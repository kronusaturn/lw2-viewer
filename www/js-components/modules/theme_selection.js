function injectThemeSelector() {
	GWLog("injectThemeSelector");

	let currentTheme = readCookie("theme") || "default";
	let themeSelector = addUIElement({{{parts/theme_selector}}});
	themeSelector.queryAll("button").forEach(button => {
		button.addActivateEvent(GW.themeSelectButtonClicked = (event) => {
			GWLog("GW.themeSelectButtonClicked");

			let themeName = /select-theme-([^\s]+)/.exec(event.target.className)[1];
			setSelectedTheme(themeName);
			if (GW.mediaQueries.mobileWide.matches) toggleAppearanceAdjustUI();
		});
	});

	// Inject transitions CSS, if animating changes is enabled.
	if (GW.adjustmentTransitions) {
		query("head").insertAdjacentHTML("beforeend", {{{parts/adjustment_transitions_styles}}});
	}
}
function setSelectedTheme(themeName) {
	GWLog("setSelectedTheme");

	queryAll(".theme-selector button").forEach(button => {
		button.removeClass("selected");
		button.disabled = false;
	});
	queryAll(".theme-selector button.select-theme-" + themeName).forEach(button => {
		button.addClass("selected");
		button.disabled = true;
	});
	setTheme(themeName);
	query("#theme-tweaker-ui .current-theme span").innerText = themeName;
}
function setTheme(newThemeName) {
	GWLog("setTheme");

	var themeUnloadCallback = '';
	var oldThemeName = '';
	if (typeof(newThemeName) == 'undefined') {
		newThemeName = readCookie('theme');
		if (!newThemeName) return;
	} else {
		themeUnloadCallback = GW['themeUnloadCallback_' + (readCookie('theme') || 'default')];
		oldThemeName = readCookie('theme') || 'default';

		if (newThemeName == 'default') setCookie('theme', '');
		else setCookie('theme', newThemeName);
	}
	if (themeUnloadCallback != null) themeUnloadCallback(newThemeName);

	let styleSheetNameSuffix = (newThemeName == 'default') ? '' : ('-' + newThemeName);
	let currentStyleSheetNameComponents = /style[^\.]*(\.[^?]+)/.exec(query("head link[href*='/css/style'][href*='.css']").href);

	let newStyle = document.createElement('link');
	newStyle.setAttribute('rel', 'stylesheet');
	newStyle.setAttribute('href', GW.assetVersions['/css/style' + styleSheetNameSuffix + currentStyleSheetNameComponents[1]]);

	let oldStyle = query("head link[href*='/css/style'][href*='.css']");
	newStyle.addEventListener('load', () => { removeElement(oldStyle); });
	newStyle.addEventListener('load', () => { postSetThemeHousekeeping(oldThemeName, newThemeName); });

	if (GW.adjustmentTransitions) {
		pageFadeTransition(false);
		setTimeout(() => {
			query('head').insertBefore(newStyle, oldStyle.nextSibling);
		}, 500);
	} else {
		query('head').insertBefore(newStyle, oldStyle.nextSibling);
	}
}
function postSetThemeHousekeeping(oldThemeName = "", newThemeName = (readCookie('theme') || 'default')) {
	GWLog("postSetThemeHousekeeping");

	recomputeUIElementsContainerHeight(true);

	let themeLoadCallback = GW['themeLoadCallback_' + newThemeName];
	if (themeLoadCallback != null) themeLoadCallback(oldThemeName);

	recomputeUIElementsContainerHeight();
	adjustUIForWindowSize();
	if (GW.contentContainsImages) recomputeImagesOverlayLayout();
	window.addEventListener('resize', GW.windowResized = (event) => {
		GWLog("GW.windowResized");

		requestAnimationFrame(() => {
			adjustUIForWindowSize();
			recomputeUIElementsContainerHeight();

			if (GW.contentContainsImages) {
				recomputeImagesOverlayLayout();
				resetFocusedImagePosition();
			}
		});
	});

	// Adjust text rendering when inverted.
	document.body.toggleClass("filter-inverted", GW.currentFilters["invert"] == "100%");

	if (window.adjustmentTransitions) pageFadeTransition(true);
	updateThemeTweakerSampleText();

	setTimeout(realignHash, 0);
}

function pageFadeTransition(fadeIn) {
	if (fadeIn) {
		query("body").removeClass("transparent");
	} else {
		query("body").addClass("transparent");
	}
}

GW.themeLoadCallback_less = (fromTheme = "") => {
	GWLog("themeLoadCallback_less");

	injectSiteNavUIToggle();

	registerInitializer('shortenDate', true, () => (query(".top-post-meta") != null), () => {
		let dtf = new Intl.DateTimeFormat([], 
			matchMedia("(max-width: 1100px)").matches ? 
				{ month: 'short', day: 'numeric', year: 'numeric' } : 
					{ month: 'long', day: 'numeric', year: 'numeric' });
		let postDate = query(".top-post-meta .date");
		postDate.innerHTML = dtf.format(new Date(+ postDate.dataset.jsDate));
	});

	// First row placeholder on mobile (smartphone width).
	doWhenMatchMedia(GW.mediaQueries.mobileNarrow, "themeLessMobileFirstRowPlaceholder", (mediaQuery) => {
		query("#content").insertAdjacentHTML("beforeend", "<div id='theme-less-mobile-first-row-placeholder'></div>");
	}, (mediaQuery) => {
		removeElement("#theme-less-mobile-first-row-placeholder");
	}, (mediaQuery) => {
		removeElement("#theme-less-mobile-first-row-placeholder");
	});

	// Spans (to make post-meta date & comment count fixed on desktop).
	let elementsToFloatWithSpans = queryAll(".top-post-meta .date, .top-post-meta .comment-count");
	function removeSpansIfNeeded() {
		if (query(".top-post-meta .date span") == null) return;
		elementsToFloatWithSpans.forEach(element => {
			element.innerHTML = element.firstChild.innerHTML;
		});
	}
	registerInitializer('less_addSpansActiveMediaQuery', true, () => (query(".top-post-meta") != null), () => {
		doWhenMatchMedia(GW.mediaQueries.mobileWide, "themeLessAddSpans", (mediaQuery) => {
			removeSpansIfNeeded();
		}, (mediaQuery) => {
			elementsToFloatWithSpans.forEach(element => {
				element.innerHTML = "<span>" + element.innerHTML + "</span>";
			});
		}, (mediaQuery) => {
			removeSpansIfNeeded();
		});
	});

	if (!GW.isMobile) {
		if (localStorage.getItem("appearance-adjust-ui-toggle-engaged") == null) {
			// If state is not set (user has never clicked on the Less theme’s appearance
			// adjustment UI toggle) then show it, but then hide it after a short time.
			registerInitializer('engageAppearanceAdjustUI', true, () => (query("#ui-elements-container") != null), () => {
				toggleAppearanceAdjustUI();
				setTimeout(toggleAppearanceAdjustUI, 3000);
			});
		} else if (localStorage.getItem("appearance-adjust-ui-toggle-engaged") == "true") {
			registerInitializer('engageAppearanceAdjustUI', true, () => (query("#ui-elements-container") != null), () => {
				toggleAppearanceAdjustUI();
			});
		}

		if (fromTheme != "") {
			allUIToggles = queryAll("#ui-elements-container div[id$='-ui-toggle']");
			setTimeout(() => {
				allUIToggles.forEach(toggle => { toggle.addClass("highlighted"); });
			}, 300);
			setTimeout(() => {
				allUIToggles.forEach(toggle => { toggle.removeClass("highlighted"); });
			}, 1800);
		}

		// Unset the height of the #ui-elements-container.
		query("#ui-elements-container").style.height = "";

		// Due to filters vs. fixed elements, we need to be smarter about 
		// selecting which elements to filter...
		GW.themeTweaker.filtersExclusionPaths.themeLess = [
			"#content #secondary-bar",
			"#content .post .top-post-meta .date",
			"#content .post .top-post-meta .comment-count",
		];
		applyFilters(GW.currentFilters);
	}

	// Leave the “Sequences” tab as a word.
	adjustSequencesTab(false);

	// Change theme tweaker toggle icon.
	queryAll("#theme-tweaker-toggle button").forEach(button => {
		button.dataset.defaultIcon = button.innerHTML;
		button.innerHTML = "&#xf1de;";
	});

	// We pre-query the relevant elements, so we don’t have to run queryAll on
	// every firing of the scroll listener.
	GW.scrollState = {
		"lastScrollTop":					window.pageYOffset || document.documentElement.scrollTop,
		"unbrokenDownScrollDistance":		0,
		"unbrokenUpScrollDistance":			0,
		"siteNavUIToggleButton":			query("#site-nav-ui-toggle button"),
		"siteNavUIElements":				queryAll("#primary-bar, #secondary-bar, .page-toolbar"),
		"appearanceAdjustUIToggleButton":	query("#appearance-adjust-ui-toggle button")
	};
	addScrollListener(updateSiteNavUIState, "updateSiteNavUIStateScrollListener");
}

// Hide the post-nav-ui toggle if none of the elements to be toggled are 
// visible; otherwise, show it.
function updatePostNavUIVisibility() {
	GWLog("updatePostNavUIVisibility");

	var hidePostNavUIToggle = true;
	queryAll("#quick-nav-ui a, #new-comment-nav-ui").forEach(element => {
		if (!(element.style.visibility == "hidden" || getComputedStyle(element).display == "none"))
			hidePostNavUIToggle = false;
	});
	queryAll("#quick-nav-ui, #post-nav-ui-toggle").forEach(element => {
		element.style.visibility = hidePostNavUIToggle ? "hidden" : "";
	});
}

/*	Hide the site nav and appearance adjust UIs on scroll down; show them on 
	scroll up.

	NOTE: The UIs are re-shown on scroll up ONLY if the user has them set to be 
	engaged; if they’re manually disengaged, they are not re-engaged by scroll.

	Called by the ‘updateSiteNavUIStateScrollListener’ scroll listener.
	*/
function updateSiteNavUIState(event) {
	GWLog("updateSiteNavUIState");

	let newScrollTop = window.pageYOffset || document.documentElement.scrollTop;
	GW.scrollState.unbrokenDownScrollDistance = (newScrollTop > GW.scrollState.lastScrollTop) ? 
														(GW.scrollState.unbrokenDownScrollDistance + newScrollTop - GW.scrollState.lastScrollTop) : 
													 	0;
	GW.scrollState.unbrokenUpScrollDistance = (newScrollTop < GW.scrollState.lastScrollTop) ?
													 (GW.scrollState.unbrokenUpScrollDistance + GW.scrollState.lastScrollTop - newScrollTop) :
													 0;
	GW.scrollState.lastScrollTop = newScrollTop;

	// Hide site nav UI and appearance adjust UI when scrolling a full page down.
	if (GW.scrollState.unbrokenDownScrollDistance > window.innerHeight) {
		if (GW.scrollState.siteNavUIToggleButton.hasClass("engaged")) toggleSiteNavUI();
		if (GW.scrollState.appearanceAdjustUIToggleButton.hasClass("engaged")) toggleAppearanceAdjustUI();
	}

	// On mobile, make site nav UI translucent on ANY scroll down.
	if (GW.mediaQueries.mobileNarrow.matches)
		GW.scrollState.siteNavUIElements.forEach(element => {
			if (GW.scrollState.unbrokenDownScrollDistance > 0) element.addClass("translucent-on-scroll");
			else element.removeClass("translucent-on-scroll");
		});

	// Show site nav UI when scrolling a full page up, or to the top.
	if ((GW.scrollState.unbrokenUpScrollDistance > window.innerHeight || 
		 GW.scrollState.lastScrollTop == 0) &&
		(!GW.scrollState.siteNavUIToggleButton.hasClass("engaged") && 
		 localStorage.getItem("site-nav-ui-toggle-engaged") != "false")) toggleSiteNavUI();

	// On desktop, show appearance adjust UI when scrolling to the top.
	if ((!GW.mediaQueries.mobileNarrow.matches) && 
		(GW.scrollState.lastScrollTop == 0) &&
		(!GW.scrollState.appearanceAdjustUIToggleButton.hasClass("engaged")) && 
		(localStorage.getItem("appearance-adjust-ui-toggle-engaged") != "false")) toggleAppearanceAdjustUI();
}

GW.themeUnloadCallback_less = (toTheme = "") => {
	GWLog("themeUnloadCallback_less");

	removeSiteNavUIToggle();
	document.removeEventListener("scroll", GW["updateSiteNavUIStateScrollListener"]);

	cancelDoWhenMatchMedia("themeLessMobileFirstRowPlaceholder");
	cancelDoWhenMatchMedia("themeLessAddSpans");

	(query(".top-post-meta .date")||{}).innerHTML = (query(".bottom-post-meta .date")||{}).innerHTML;

	// Put the “Sequences” tab back.
	adjustSequencesTab();

	// Change theme tweaker toggle icon back.
	queryAll("#theme-tweaker-toggle button").forEach(button => {
		button.innerHTML = button.dataset.defaultIcon;
		button.dataset.defaultIcon = "";
	});

	// Reset filtered elements selector to default.
	delete GW.themeTweaker.filtersExclusionPaths.themeLess;
	applyFilters(GW.currentFilters);
}

GW.themeLoadCallback_dark = (fromTheme = "") => {
	GWLog("themeLoadCallback_dark");

	// Add white glow to images.
	if (GW.contentContainsImages) {
		registerInitializer('makeImagesGlow', true, () => (query("#images-overlay") != null), () => {
			queryAll(GW.imageFocus.overlayImagesSelector).forEach(image => {
				image.style.filter = "drop-shadow(0 0 0 #000) drop-shadow(0 0 0.5px #fff) drop-shadow(0 0 1px #fff) drop-shadow(0 0 2px #fff)";
				image.style.width = parseInt(image.style.width) + 12 + "px";
				image.style.height = parseInt(image.style.height) + 12 + "px";
				image.style.top = parseInt(image.style.top) - 6 + "px";
				image.style.left = parseInt(image.style.left) - 6 + "px";
			});
		});
	}

	// Dark theme should NOT have text rendering adjusted for invert filter.
	query("head").insertAdjacentHTML("beforeend", {{{parts/dark_theme_adjustments_styles}}});
}
GW.themeUnloadCallback_dark = (toTheme = "") => {
	GWLog("themeUnloadCallback_dark");

	// Remove image glow.
	if (GW.contentContainsImages) {
		queryAll(GW.imageFocus.overlayImagesSelector).forEach(image => {
			image.style.filter = "";
			image.style.width = parseInt(image.style.width) - 12 + "px";
			image.style.height = parseInt(image.style.height) - 12 + "px";
			image.style.top = parseInt(image.style.top) + 6 + "px";
			image.style.left = parseInt(image.style.left) + 6 + "px";
		});
	}

	// Remove adjustments.
	removeElement("style#dark-theme-adjustments");
}

GW.themeLoadCallback_zero = (fromTheme = "") => {
	queryAll("h1.listing + .post-meta .comment-count").forEach(commentCount => {
		let parts = /(.+?)( comments)(.+)/.exec(commentCount.title);
		commentCount.innerHTML = `${parts[1]}<span>${parts[2]}</span><span>${parts[3]}</span>`;
	});
}
GW.themeUnloadCallback_zero = (toTheme = "") => {
	queryAll("h1.listing + .post-meta .comment-count").forEach(commentCount => {
		let parts = /(.+?)( comments)(.+)/.exec(commentCount.title);
		commentCount.innerHTML = `${parts[1]}<span>${parts[2]}</span>`;
	});
}

GW.themeLoadCallback_brutalist = (fromTheme = "") => {
	GWLog("themeLoadCallback_brutalist");

	// “N comments (M new)”
	queryAll("h1.listing + .post-meta .comment-count").forEach(commentCount => {
		let parts = /(.+?)( comments)(.+)/.exec(commentCount.title);
		commentCount.innerHTML = `${parts[1]}<span>${parts[2]}</span><span>${parts[3]}</span>`;
	});

	// Theme selector close button.
	query("#theme-selector .theme-selector-close-button").innerHTML = "&#xf00d;";
}
GW.themeUnloadCallback_brutalist = (toTheme = "") => {
	GWLog("themeUnloadCallback_brutalist");

	// “N comments”
	queryAll("h1.listing + .post-meta .comment-count").forEach(commentCount => {
		let parts = /(.+?)( comments)(.+)/.exec(commentCount.title);
		commentCount.innerHTML = `${parts[1]}<span>${parts[2]}</span>`;
	});

	// Theme selector close button.
	query("#theme-selector .theme-selector-close-button").innerHTML = "&#xf057;";
}

GW.themeLoadCallback_rts = (fromTheme = "") => {
	GWLog("themeLoadCallback_rts");

	// “N comments (M new)”
	queryAll("h1.listing + .post-meta .comment-count").forEach(commentCount => {
		let parts = /(.+?)( comments)(.+)/.exec(commentCount.title);
		commentCount.innerHTML = `${parts[1]}<span>${parts[2]}</span><span>${parts[3]}</span>`;
	});
}
GW.themeUnloadCallback_rts = (toTheme = "") => {
	GWLog("themeUnloadCallback_rts");

	// “N comments”
	queryAll("h1.listing + .post-meta .comment-count").forEach(commentCount => {
		let parts = /(.+?)( comments)(.+)/.exec(commentCount.title);
		commentCount.innerHTML = `${parts[1]}<span>${parts[2]}</span>`;
	});
}

GW.themeLoadCallback_classic = (fromTheme = "") => {
	GWLog("themeLoadCallback_classic");

	queryAll(".comment-item .comment-controls .action-button").forEach(button => {
		button.innerHTML = "";
	});
}
GW.themeUnloadCallback_classic = (toTheme = "") => {
	GWLog("themeUnloadCallback_classic");

	queryAll(".comment-item .comment-controls .action-button").forEach(button => {
		button.innerHTML = button.dataset.label;
	});
}
