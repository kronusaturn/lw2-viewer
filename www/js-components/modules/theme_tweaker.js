function injectThemeTweaker() {
	GWLog("injectThemeTweaker");

	let themeTweakerUI = addUIElement({{{parts/theme_tweaker_ui}}});

	// Clicking the background overlay closes the theme tweaker.
	themeTweakerUI.addActivateEvent(GW.themeTweaker.UIOverlayClicked = (event) => {
		GWLog("GW.themeTweaker.UIOverlayClicked");

		if (event.type == 'mousedown') {
			themeTweakerUI.style.opacity = "0.01";
		} else {
			toggleThemeTweakerUI();
			themeTweakerUI.style.opacity = "1.0";
			themeTweakReset();
		}
	}, true);

	// Intercept clicks, so they don’t “fall through” the background overlay.
	queryAll("#theme-tweaker-ui > div").forEach(div => {
		div.addActivateEvent((event) => { event.stopPropagation(); }, true);
	});

	let sampleTextContainer = query("#theme-tweaker-ui #theme-tweak-section-sample-text .sample-text-container");
	themeTweakerUI.queryAll("input").forEach(field => {
		// All input types in the theme tweaker receive a ‘change’ event when
		// their value is changed. (Range inputs, in particular, receive this 
		// event when the user lets go of the handle.) This means we should
		// update the filters for the entire page, to match the new setting.
		field.addEventListener("change", GW.themeTweaker.fieldValueChanged = (event) => {
			GWLog("GW.themeTweaker.fieldValueChanged");

			if (event.target.id == 'theme-tweak-control-invert') {
				GW.currentFilters['invert'] = event.target.checked ? '100%' : '0%';
			} else if (event.target.type == 'range') {
				let sliderName = /^theme-tweak-control-(.+)$/.exec(event.target.id)[1];
				query("#theme-tweak-label-" + sliderName).innerText = event.target.value + event.target.dataset["labelSuffix"];
				GW.currentFilters[sliderName] = event.target.value + event.target.dataset["valueSuffix"];
			} else if (event.target.id == 'theme-tweak-control-clippy') {
				query(".clippy-container").style.display = event.target.checked ? "block" : "none";
			}
			// Clear the sample text filters.
			sampleTextContainer.style.filter = "";
			// Apply the new filters globally.
			applyFilters(GW.currentFilters);
			// Adjust text rendering when inverted.
			document.body.toggleClass("filter-inverted", GW.currentFilters["invert"] == "100%");
		});

		// Range inputs receive an ‘input’ event while being scrubbed, updating
		// “live” as the handle is moved. We don’t want to change the filters 
		// for the actual page while this is happening, but we do want to change
		// the filters for the *sample text*, so the user can see what effects
		// his changes are having, live, without having to let go of the handle.
		if (field.type == "range") field.addEventListener("input", GW.themeTweaker.fieldInputReceived = (event) => {
			GWLog("GW.themeTweaker.fieldInputReceived");

			var sampleTextFilters = GW.currentFilters;

			let sliderName = /^theme-tweak-control-(.+)$/.exec(event.target.id)[1];
			query("#theme-tweak-label-" + sliderName).innerText = event.target.value + event.target.dataset["labelSuffix"];
			sampleTextFilters[sliderName] = event.target.value + event.target.dataset["valueSuffix"];

			sampleTextContainer.style.filter = filterStringFromFilters(sampleTextFilters);
		});
	});

	themeTweakerUI.query(".minimize-button").addActivateEvent(GW.themeTweaker.minimizeButtonClicked = (event) => {
		GWLog("GW.themeTweaker.minimizeButtonClicked");

		themeTweakerUI.toggleClass("minimized");
	});
	themeTweakerUI.query(".help-button").addActivateEvent(GW.themeTweaker.helpButtonClicked = (event) => {
		GWLog("GW.themeTweaker.helpButtonClicked");

		themeTweakerUI.query("#theme-tweak-control-clippy").checked = JSON.parse(localStorage.getItem("theme-tweaker-settings") || '{ "showClippy": true }')["showClippy"];
		toggleThemeTweakerHelpWindow();
	});
	themeTweakerUI.query(".reset-defaults-button").addActivateEvent(GW.themeTweaker.resetDefaultsButtonClicked = (event) => {
		GWLog("GW.themeTweaker.resetDefaultsButtonClicked");

		themeTweakerUI.query("#theme-tweak-control-invert").checked = false;
		[ "saturate", "brightness", "contrast", "hue-rotate" ].forEach(sliderName => {
			let slider = themeTweakerUI.query("#theme-tweak-control-" + sliderName);
			slider.value = slider.dataset['defaultValue'];
			themeTweakerUI.query("#theme-tweak-label-" + sliderName).innerText = slider.value + slider.dataset['labelSuffix'];
		});
		GW.currentFilters = { };
		applyFilters(GW.currentFilters);

		GW.currentTextZoom = "1.0";
		setTextZoom(GW.currentTextZoom);

		setSelectedTheme("default");
	});
	themeTweakerUI.query(".main-theme-tweaker-window .cancel-button").addActivateEvent(GW.themeTweaker.cancelButtonClicked = (event) => {
		GWLog("GW.themeTweaker.cancelButtonClicked");

		toggleThemeTweakerUI();
		themeTweakReset();
	});
	themeTweakerUI.query(".main-theme-tweaker-window .ok-button").addActivateEvent(GW.themeTweaker.OKButtonClicked = (event) => {
		GWLog("GW.themeTweaker.OKButtonClicked");

		toggleThemeTweakerUI();
		themeTweakSave();
	});
	themeTweakerUI.query(".help-window .cancel-button").addActivateEvent(GW.themeTweaker.helpWindowCancelButtonClicked = (event) => {
		GWLog("GW.themeTweaker.helpWindowCancelButtonClicked");

		toggleThemeTweakerHelpWindow();
		themeTweakerResetSettings();
	});
	themeTweakerUI.query(".help-window .ok-button").addActivateEvent(GW.themeTweaker.helpWindowOKButtonClicked = (event) => {
		GWLog("GW.themeTweaker.helpWindowOKButtonClicked");

		toggleThemeTweakerHelpWindow();
		themeTweakerSaveSettings();
	});

	themeTweakerUI.queryAll(".notch").forEach(notch => {
		notch.addActivateEvent(GW.themeTweaker.sliderNotchClicked = (event) => {
			GWLog("GW.themeTweaker.sliderNotchClicked");

			let slider = event.target.parentElement.query("input[type='range']");
			slider.value = slider.dataset['defaultValue'];
			event.target.parentElement.query(".theme-tweak-control-label").innerText = slider.value + slider.dataset['labelSuffix'];
			GW.currentFilters[/^theme-tweak-control-(.+)$/.exec(slider.id)[1]] = slider.value + slider.dataset['valueSuffix'];
			applyFilters(GW.currentFilters);
		});
	});

	themeTweakerUI.query(".clippy-close-button").addActivateEvent(GW.themeTweaker.clippyCloseButtonClicked = (event) => {
		GWLog("GW.themeTweaker.clippyCloseButtonClicked");

		themeTweakerUI.query(".clippy-container").style.display = "none";
		localStorage.setItem("theme-tweaker-settings", JSON.stringify({ 'showClippy': false }));
		themeTweakerUI.query("#theme-tweak-control-clippy").checked = false;
	});

	query("head").insertAdjacentHTML("beforeend","<style id='theme-tweaker-style'></style>");

	themeTweakerUI.query(".theme-selector").innerHTML = query("#theme-selector").innerHTML;
	themeTweakerUI.queryAll(".theme-selector button").forEach(button => {
		button.addActivateEvent(GW.themeSelectButtonClicked);
	});

	themeTweakerUI.queryAll("#theme-tweak-section-text-size-adjust button").forEach(button => {
		button.addActivateEvent(GW.themeTweaker.textSizeAdjustButtonClicked);
	});

	// Add event listeners for Escape and Enter, for the theme tweaker.
	document.addEventListener("keyup", GW.themeTweaker.keyPressed = (event) => {
		if (event.key == "Escape") {
			if (themeTweakerUI.query(".help-window").style.display != "none") {
				toggleThemeTweakerHelpWindow();
				themeTweakerResetSettings();
			} else if (themeTweakerUI.style.display != "none") {
				toggleThemeTweakerUI();
				themeTweakReset();
			}
		} else if (event.key == "Enter") {
			if (themeTweakerUI.query(".help-window").style.display != "none") {
				toggleThemeTweakerHelpWindow();
				themeTweakerSaveSettings();
			} else if (themeTweakerUI.style.display != "none") {
				toggleThemeTweakerUI();
				themeTweakSave();
			}
		}
	});

	let themeTweakerToggle = addUIElement({{{parts/theme_tweaker_toggle}}});
	themeTweakerToggle.query("button").addActivateEvent(GW.themeTweaker.toggleButtonClicked = (event) => {
		GWLog("GW.themeTweaker.toggleButtonClicked");

		GW.themeTweakerStyleSheetAvailable = () => {
			GWLog("GW.themeTweakerStyleSheetAvailable");

			themeTweakerUI.query(".current-theme span").innerText = (readCookie("theme") || "default");

			themeTweakerUI.query("#theme-tweak-control-invert").checked = (GW.currentFilters['invert'] == "100%");
			[ "saturate", "brightness", "contrast", "hue-rotate" ].forEach(sliderName => {
				let slider = themeTweakerUI.query("#theme-tweak-control-" + sliderName);
				slider.value = /^[0-9]+/.exec(GW.currentFilters[sliderName]) || slider.dataset['defaultValue'];
				themeTweakerUI.query("#theme-tweak-label-" + sliderName).innerText = slider.value + slider.dataset['labelSuffix'];
			});

			toggleThemeTweakerUI();
			event.target.disabled = true;
		};

		if (query("link[href^='/css/theme_tweaker.css']")) {
			// Theme tweaker CSS is already loaded.
			GW.themeTweakerStyleSheetAvailable();
		} else {
			// Load the theme tweaker CSS (if not loaded).
			let themeTweakerStyleSheet = document.createElement('link');
			themeTweakerStyleSheet.setAttribute('rel', 'stylesheet');
			themeTweakerStyleSheet.setAttribute('href', GW.assetVersions['/css/theme_tweaker.css']);
			themeTweakerStyleSheet.addEventListener('load', GW.themeTweakerStyleSheetAvailable);
			query("head").insertBefore(themeTweakerStyleSheet, query("head").firstElementChild);
		}

		if (GW.mediaQueries.mobileMax.matches)
			toggleAppearanceAdjustUI();
	});
}
function toggleThemeTweakerUI() {
	GWLog("toggleThemeTweakerUI");

	let themeTweakerUI = query("#theme-tweaker-ui");
	themeTweakerUI.style.display = (themeTweakerUI.style.display == "none") ? "block" : "none";
	query("#theme-tweaker-style").innerHTML = (themeTweakerUI.style.display == "none") ? "" : 
		`#content, #ui-elements-container > div:not(#theme-tweaker-ui) {
			pointer-events: none;
		}`;
	if (themeTweakerUI.style.display != "none") {
		// Save selected theme.
		GW.currentTheme = (readCookie("theme") || "default");
		// Focus invert checkbox.
		query("#theme-tweaker-ui #theme-tweak-control-invert").focus();
		// Show sample text in appropriate font.
		updateThemeTweakerSampleText();
		// Disable tab-selection of the search box.
		setSearchBoxTabSelectable(false);
		// Disable scrolling of the page.
		togglePageScrolling(false);
		// Set the minimize button state.
		themeTweakerUI.query(".minimize-button").removeClass("maximize");
		themeTweakerUI.query(".minimize-button").addClass("minimize");
	} else {
		query("#theme-tweaker-toggle button").disabled = false;
		// Re-enable tab-selection of the search box.
		setSearchBoxTabSelectable(true);
		// Re-enable scrolling of the page.
		togglePageScrolling(true);
	}
	// Set theme tweaker assistant visibility.
	query(".clippy-container").style.display = JSON.parse(
			localStorage.getItem("theme-tweaker-settings") || 
			'{ "showClippy": true }'
		)["showClippy"] ? 
		"block" : 
		"none";
}
function toggleThemeTweakerHelpWindow() {
	GWLog("toggleThemeTweakerHelpWindow");

	let themeTweakerHelpWindow = query("#theme-tweaker-ui .help-window");
	themeTweakerHelpWindow.style.display = (themeTweakerHelpWindow.style.display == "none") ? "block" : "none";
	if (themeTweakerHelpWindow.style.display != "none") {
		// Focus theme tweaker assistant checkbox.
		query("#theme-tweaker-ui #theme-tweak-control-clippy").focus();
		// Disable interaction on main theme tweaker window.
		query("#theme-tweaker-ui").style.pointerEvents = "none";
		query("#theme-tweaker-ui .main-theme-tweaker-window").style.pointerEvents = "none";
	} else {
		// Re-enable interaction on main theme tweaker window.
		query("#theme-tweaker-ui").style.pointerEvents = "auto";
		query("#theme-tweaker-ui .main-theme-tweaker-window").style.pointerEvents = "auto";
	}
}
function themeTweakReset() {
	GWLog("themeTweakReset");

	setSelectedTheme(GW.currentTheme);
	GW.currentFilters = JSON.parse(localStorage.getItem("theme-tweaks") || "{ }");
	applyFilters(GW.currentFilters);
	GW.currentTextZoom = `${parseFloat(localStorage.getItem("text-zoom")) || 1.0}`;
	setTextZoom(GW.currentTextZoom);
}
function themeTweakSave() {
	GWLog("themeTweakSave");

	GW.currentTheme = (readCookie("theme") || "default");
	localStorage.setItem("theme-tweaks", JSON.stringify(GW.currentFilters));
	localStorage.setItem("text-zoom", GW.currentTextZoom);
}
function themeTweakerResetSettings() {
	GWLog("themeTweakerResetSettings");

	query("#theme-tweak-control-clippy").checked = JSON.parse(localStorage.getItem("theme-tweaker-settings") || '{ "showClippy": true }')['showClippy'];
	query(".clippy-container").style.display = query("#theme-tweak-control-clippy").checked ? "block" : "none";
}
function themeTweakerSaveSettings() {
	GWLog("themeTweakerSaveSettings");

	localStorage.setItem("theme-tweaker-settings", JSON.stringify({ 'showClippy': query("#theme-tweak-control-clippy").checked }));
}
function updateThemeTweakerSampleText() {
	GWLog("updateThemeTweakerSampleText");

	let sampleText = query("#theme-tweaker-ui #theme-tweak-section-sample-text .sample-text");

	// This causes the sample text to take on the properties of the body text of a post.
	let bodyTextElement = query(".post-body") || query(".comment-body");
	sampleText.style.color = bodyTextElement ? 
								getComputedStyle(bodyTextElement).color : 
									getComputedStyle(query("#content")).color;

	// Here we find out what is the actual background color that will be visible
	// behind the body text of posts, and set the sample text’s background to that.
	var backgroundElement = query("#content");
	let searchField = query("#nav-item-search input");
	if (!(getComputedStyle(searchField).backgroundColor == "" || 
		  getComputedStyle(searchField).backgroundColor == "rgba(0, 0, 0, 0)"))
		backgroundElement = searchField;
	else while (getComputedStyle(backgroundElement).backgroundColor == "" || 
				getComputedStyle(backgroundElement).backgroundColor == "rgba(0, 0, 0, 0)")
				backgroundElement = backgroundElement.parentElement;
	sampleText.parentElement.style.backgroundColor = getComputedStyle(backgroundElement).backgroundColor;
}
