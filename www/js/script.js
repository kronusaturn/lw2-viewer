/***************************/
/* INITIALIZATION REGISTRY */
/***************************/

/*	Polyfill for requestIdleCallback in Apple and Microsoft browsers. */
if (!window.requestIdleCallback) {
	window.requestIdleCallback = (fn) => { setTimeout(fn, 0); };
}

/*	TBC. */
GW.initializersDone = { };
GW.initializers = { };
function registerInitializer(name, tryEarly, precondition, fn) {
	GW.initializersDone[name] = false;
	GW.initializers[name] = fn;
	let wrapper = function () {
		if (GW.initializersDone[name]) return;
		if (!precondition()) {
			if (tryEarly) {
				setTimeout(() => requestIdleCallback(wrapper, { timeout: 1000 }), 50);
			} else {
				document.addEventListener("readystatechange", wrapper, { once: true });
			}
			return;
		}
		GW.initializersDone[name] = true;
		fn();
	};
	if (tryEarly) {
		requestIdleCallback(wrapper, { timeout: 1000 });
	} else {
		document.addEventListener("readystatechange", wrapper, { once: true });
		requestIdleCallback(wrapper);
	}
}
function forceInitializer(name) {
	if (GW.initializersDone[name]) return;
	GW.initializersDone[name] = true;
	GW.initializers[name]();
}

/***********/
/* COOKIES */
/***********/

/*	Sets a cookie. */
function setCookie(name, value, days) {
	var expires = "";
	if (!days) days = 36500;
	if (days) {
		var date = new Date();
		date.setTime(date.getTime() + (days*24*60*60*1000));
		expires = "; expires=" + date.toUTCString();
	}
	document.cookie = name + "=" + (value || "")  + expires + "; path=/";
}

/*	Reads the value of named cookie.
	Returns the cookie as a string, or null if no such cookie exists. */
function readCookie(name) {
	var nameEQ = name + "=";
	var ca = document.cookie.split(';');
	for(var i = 0; i < ca.length; i++) {
		var c = ca[i];
		while (c.charAt(0)==' ') c = c.substring(1, c.length);
		if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length, c.length);
	}
	return null;
}

/*******************************/
/* EVENT LISTENER MANIPULATION */
/*******************************/

/*	Adds an event listener to a button (or other clickable element), attaching 
	it to both ‘click’ and ‘keyup’ events (for use with keyboard navigation).
	Optionally also attaches the listener to the ‘mousedown’ event, making the 
	element activate on mouse down instead of mouse up. */
Element.prototype.addActivateEvent = function(func, includeMouseDown) {
	let ael = this.activateEventListener = (event) => { if (event.button === 0 || event.key === ' ') func(event) };
	if (includeMouseDown) this.addEventListener("mousedown", ael);
	this.addEventListener("click", ael);
	this.addEventListener("keyup", ael);
}

/*	Removes event listener from a clickable element, automatically detaching it
	from all relevant event types. */
Element.prototype.removeActivateEvent = function() {
	let ael = this.activateEventListener;
	this.removeEventListener("mousedown", ael);
	this.removeEventListener("click", ael);
	this.removeEventListener("keyup", ael);
}

/*	Adds a scroll event listener to the page. */
function addScrollListener(fn, name) {
	let wrapper = (event) => {
		requestAnimationFrame(() => {
			fn(event);
			document.addEventListener("scroll", wrapper, { once: true, passive: true });
		});
	}
	document.addEventListener("scroll", wrapper, { once: true, passive: true });

	// Retain a reference to the scroll listener, if a name is provided.
	if (typeof name != "undefined")
		GW[name] = wrapper;
}

/************************/
/* ACTIVE MEDIA QUERIES */
/************************/

/*	This function provides two slightly different versions of its functionality,
	depending on whether it gets two arguments or three.

	If two arguments are given (a media query and a function), then the function 
	is called whenever the media query changes (in either direction).

	If three arguments are given (a media query and two functions), then the 
	first function is called whenever the media query starts matching, and the 
	second function is called whenever the media query stops matching.

	If you want to call a function for a change in one direction only, pass an
	empty closure (NOT null!) as one of the function arguments.

	There is also an optional fourth argument. This should be a function to be 
	called when the active media query is canceled.
	*/
function doWhenMatchMedia(mediaQuery, name, ifMatchesOrAlwaysDo, otherwiseDo = null, whenCanceledDo = null) {
	if (typeof GW.mediaQueryResponders == "undefined")
		GW.mediaQueryResponders = { };

	let mediaQueryResponder = (event, canceling = false) => {
		if (canceling) {
			GWLog(`Canceling media query “${name}”`);

			if (whenCanceledDo != null)
				whenCanceledDo(mediaQuery);
		} else {
			let matches = (typeof event == "undefined") ? mediaQuery.matches : event.matches;

			GWLog(`Media query “${name}” triggered (matches: ${matches ? "YES" : "NO"})`);

			if (otherwiseDo == null || matches) ifMatchesOrAlwaysDo(mediaQuery);
			else otherwiseDo(mediaQuery);
		}
	};
	mediaQueryResponder();
	mediaQuery.addListener(mediaQueryResponder);

	GW.mediaQueryResponders[name] = mediaQueryResponder;
}

function cancelDoWhenMatchMedia(name) {
	GW.mediaQueryResponders[name](null, true);

	for ([ key, mediaQuery ] of Object.entries(GW.mediaQueries))
		mediaQuery.removeListener(GW.mediaQueryResponders[name]);

	GW.mediaQueryResponders[name] = null;
}

/****************/
/* MISC HELPERS */
/****************/

/*	Returns the passed object if it’s truthy, or a newly created HTMLElement.
	Æ(x) is the element analogue of (x||{}).
	*/
function Æ(x) {
	return x || document.createElement(null);
}

/*	If top of element is not at or above the top of the screen, scroll it into
	view. */
Element.prototype.scrollIntoViewIfNeeded = function() {
	GWLog("scrollIntoViewIfNeeded");
	let rect = this.getBoundingClientRect();
	if ((rect.bottom > window.innerHeight && rect.top > 0) ||
		rect.top < 0) {
		this.scrollIntoView(true);
	}
}

/*	Get the comment ID of the item (if it’s a comment) or of its containing 
	comment (if it’s a child of a comment).
	*/
Element.prototype.getCommentId = function() {
	let item = (this.className == "comment-item" ? this : this.closest(".comment-item"));
	if (item) {
		return /^comment-(.*)/.exec(item.id)[1];
	} else {
		return false;
	}
}

/*	Return the currently selected text, as HTML (rather than unstyled text).
	*/
function getSelectionHTML() {
	var container = document.createElement("div");
	container.appendChild(window.getSelection().getRangeAt(0).cloneContents());
	return container.innerHTML;
}

/*	Return the value of a GET (i.e., URL) parameter.
	*/
function getQueryVariable(variable) {
	var query = window.location.search.substring(1);
	var vars = query.split("&");
	for (var i = 0; i < vars.length; i++) {
		var pair = vars[i].split("=");
		if (pair[0] == variable)
			return pair[1];
	}

	return false;
}

/*	Given an HTML string, creates an element from that HTML, adds it to 
	#ui-elements-container (creating the latter if it does not exist), and 
	returns the created element.
	*/
function addUIElement(element_html) {
	var ui_elements_container = query("#ui-elements-container");
	if (!ui_elements_container) {
		ui_elements_container = document.createElement("nav");
		ui_elements_container.id = "ui-elements-container";
		query("body").appendChild(ui_elements_container);
	}

	ui_elements_container.insertAdjacentHTML("beforeend", element_html);
	return ui_elements_container.lastElementChild;
}

/*	Given an element or a selector, removes that element (or the element 
	identified by the selector).

	If multiple elements match the selector, only the first is removed.
	*/
function removeElement(elementOrSelector, ancestor = document) {
	if (typeof elementOrSelector == "string") elementOrSelector = ancestor.query(elementOrSelector);
	if (elementOrSelector) elementOrSelector.parentElement.removeChild(elementOrSelector);
}

/*	Returns true if the string begins with the given prefix.
	*/
String.prototype.hasPrefix = function (prefix) {
	return (this.lastIndexOf(prefix, 0) === 0);
}

/*	Toggles whether the page is scrollable.
	*/
function togglePageScrolling(enable) {
	if (!enable) {
		window.addEventListener('keydown', GW.scrollingDisabledKeyDown = (event) => {
			let forbiddenKeys = [ " ", "Spacebar", "ArrowUp", "ArrowDown", "Up", "Down" ];
			if (forbiddenKeys.contains(event.key) &&
				event.target == document.body) {
				event.preventDefault();
			}
		});
	} else {
		window.removeEventListener('keydown', GW.scrollingDisabledKeyDown);
	}
}

/*	Toggles whether the search box in the nav UI is selectable via the tab key.
	*/
function setSearchBoxTabSelectable(selectable) {
	GWLog("setSearchBoxTabSelectable");

	query("input[type='search']").tabIndex = selectable ? "" : "-1";
	query("input[type='search'] + button").tabIndex = selectable ? "" : "-1";
}

/*	Copies a string to the clipboard.
	*/
function copyTextToClipboard(string) {
	let scratchpad = query("#scratchpad");
	scratchpad.value = string;
	scratchpad.select();
	document.execCommand("copy");
}

/********************/
/* DEBUGGING OUTPUT */
/********************/

function GWLog (string) {
	if (GW.loggingEnabled == true || (GW.loggingEnabled == null && localStorage.getItem("logging-enabled") == "true"))
		console.log(string);
}
GW.enableLogging = (permanently = false) => {
	if (permanently)
		localStorage.setItem("logging-enabled", "true");
	else
		GW.loggingEnabled = true;
};
GW.disableLogging = (permanently = false) => {
	if (permanently)
		localStorage.removeItem("logging-enabled");
	else
		GW.loggingEnabled = false;
};

/***********************************/
/* COMMENT THREAD MINIMIZE BUTTONS */
/***********************************/

Element.prototype.setCommentThreadMaximized = function(toggle, userOriginated = true, force) {
	GWLog("setCommentThreadMaximized");

	if (!this.hasClass("comment-item")) return;

	let commentItem = this;
	let storageName = "thread-minimized-" + commentItem.getCommentId();
	let minimize_button = commentItem.query(".comment-minimize-button");
	let deepComment = Æ(commentItem.parentElement.previousElementSibling).hasClass("deep-comment-threshold");
	let savedState = localStorage.getItem(storageName);
	let maximize = force || 
				   (toggle ? 
				   		minimize_button.hasClass("minimized") : 
				   		!(savedState == "true" || 
				   		  commentItem.hasClass("ignored") ||
				   		  (deepComment && savedState != "false")
				   		 )
				   );
	if (userOriginated) {
		var save = (maximize && deepComment) || (!maximize && !deepComment);
		if (save) {
			localStorage.setItem(storageName, !maximize);
		} else {
			localStorage.removeItem(storageName);
		}
	}

	[ commentItem, minimize_button ].forEach(element => {
		element.removeClass(maximize ? "minimized" : "maximized");
		element.addClass(maximize ? "maximized" : "minimized");
	});

	minimize_button.innerHTML = maximize ? "&#xf146;" : "&#xf0fe;";
	minimize_button.title = `${(maximize ? "Collapse" : "Expand")} comment`;
	if (getQueryVariable("chrono") != "t") {
		minimize_button.title += ` thread (${minimize_button.dataset["childCount"]} child comments)`;
	}
}

/*************************************/
/* POST LISTINGS KEYBOARD NAVIGATION */
/*************************************/

function getCurrentVisibleListingsRange(partial = false) {
	let listings = queryAll(".listings h1, .listings .comment-meta");
	let range = [ 0, 0 ];

	for (i = 0; i < listings.length; i++) {
		let listingRect = listings[i].getBoundingClientRect();
		if (listingRect.top > 0 || 
			(partial && listingRect.bottom > 0)) {
			range[0] = i;
			break;
		}
	}
	for (; i < listings.length; i++) {
		let listingRect = listings[i].getBoundingClientRect();
		if (listingRect.bottom < window.innerHeight || 
			(partial && listingRect.top < window.innerHeight)) {
			range[1]++;
		} else {
			break;
		}
	}

	return range;
}

/*****************************************/
/* NEW COMMENT HIGHLIGHTING & NAVIGATION */
/*****************************************/

/*	Returns the JavaScript timestamp of the comment item associated with the
	given element (i.e., the closest .comment-item ancestor), or NaN if the
	given element is not associated with any comment item.
	*/
Element.prototype.getCommentDate = function() {
	return parseInt(Æ(Æ(this.closest(".comment-item")).query(".date")).dataset["jsDate"]);
}

function getCurrentVisibleComment() {
	let px = window.innerWidth/2, py = 5;
	let commentItem = document.elementFromPoint(px, py).closest(".comment-item") || document.elementFromPoint(px, py+60).closest(".comment-item"); // Mind the gap between threads
	let atbottom = query("#comments").getBoundingClientRect().bottom < window.innerHeight;
	if (atbottom) {
		let hashci = location.hash && query(location.hash);
		if (hashci && /comment-item/.test(hashci.className) && hashci.getBoundingClientRect().top > 0) {
			commentItem = hashci;
		}
	}
	return commentItem;
}

function allCommentItems() {
	if (!GW.allCommentItems) GW.allCommentItems = queryAll(".comment-item");
	return GW.allCommentItems;
}

function getCurrentVisibleCommentsRange(partialTop = false, partialBottom = true) {
	let comments = allCommentItems();
	let range = [ 0, 0 ];

	for (i = 0; i < comments.length; i++) {
		let commentRect = comments[i].query(".comment-body").getBoundingClientRect();
		if (commentRect.top > 0 || 
			(partialTop && commentRect.bottom > 0)) {
			range[0] = i;
			break;
		}
	}
	for (; i < comments.length; i++) {
		let commentRect = comments[i].query(".comment-body").getBoundingClientRect();
		if (commentRect.bottom < window.innerHeight || 
			(partialBottom && commentRect.top < window.innerHeight)) {
			range[1]++;
		} else {
			break;
		}
	}

	return range;
}

function highlightCommentsSince(date) {
	GWLog("highlightCommentsSince");

	var newCommentsCount = 0;
	GW.newComments = [ ];
	let oldCommentsStack = [ ];
	let prevNewComment;
	allCommentItems().forEach(commentItem => {
		commentItem.prevNewComment = prevNewComment;
		if (commentItem.getCommentDate() > date) {
			commentItem.addClass("new-comment");
			newCommentsCount++;
			GW.newComments.push(commentItem.getCommentId());
			oldCommentsStack.forEach(oldci => { oldci.nextNewComment = commentItem });
			oldCommentsStack = [ commentItem ];
			prevNewComment = commentItem;
		} else {
			commentItem.removeClass("new-comment");
			oldCommentsStack.push(commentItem);
		}
	});

	GW.updateNewCommentNavButtonState = (commentItem) => {
		query("#new-comment-nav-ui .new-comment-previous").disabled = commentItem ? !commentItem.prevNewComment : true;
		query("#new-comment-nav-ui .new-comment-next").disabled = commentItem ? !commentItem.nextNewComment : (GW.newComments.length == 0);
	};
	GW.newCommentScrollListener = () => {
		Æ(query(".comment-item.focused")).removeClass("focused");
		let visibleCommentsRange = getCurrentVisibleCommentsRange();
		if (visibleCommentsRange[1] > 0) {
			let firstVisibleCommentItem = allCommentItems()[visibleCommentsRange[0]];
			if (firstVisibleCommentItem.hasClass("new-comment")) firstVisibleCommentItem.addClass("focused");
			GW.updateNewCommentNavButtonState(firstVisibleCommentItem);
		}
	}

	addScrollListener(GW.newCommentScrollListener);

	if (document.readyState == "complete") {
		GW.newCommentScrollListener();
	} else {
		let commentItem = location.hash && /^#comment-/.test(location.hash) && query(location.hash);
		GW.updateNewCommentNavButtonState(commentItem);
	}

	registerInitializer("initializeCommentScrollPosition", false, () => (document.readyState == "complete"), GW.newCommentScrollListener);

	return newCommentsCount;
}

function scrollToNewComment(next) {
	GWLog("scrollToNewComment");

	let commentItem = getCurrentVisibleComment();
	let targetComment = null;
	let targetCommentID = null;
	if (commentItem) {
		targetComment = (next ? commentItem.nextNewComment : commentItem.prevNewComment);
		if (targetComment) {
			targetCommentID = targetComment.getCommentId();
		}
	} else {
		if (GW.newComments[0]) {
			targetCommentID = GW.newComments[0];
			targetComment = query("#comment-" + targetCommentID);
		}
	}
	if (targetComment) {
		queryAll(".comment-item.focused").forEach(focusedCommentItem => {
			focusedCommentItem.removeClass("focused");
		});

		expandAncestorsOf(targetCommentID);
		history.replaceState(null, null, "#comment-" + targetCommentID);
		targetComment.scrollIntoView();

		targetComment.addClass("focused");
	}

	GW.newCommentScrollListener();
}

function getPostHash() {
	let postHash = /^\/posts\/([^\/]+)/.exec(location.pathname);
	return (postHash ? postHash[1] : false);
}
function getLastVisitedDate() {
	// Get the last visited date (or, if posting a comment, the previous last visited date).
	let aCommentHasJustBeenPosted = (query(".just-posted-comment") != null);
	let storageName = (aCommentHasJustBeenPosted ? "previous-last-visited-date_" : "last-visited-date_") + getPostHash();
	return localStorage.getItem(storageName);
}
function setLastVisitedDate(date) {
	GWLog("setLastVisitedDate");

	// If NOT posting a comment, save the previous value for the last-visited-date 
	// (to recover it in case of posting a comment).
	let aCommentHasJustBeenPosted = (query(".just-posted-comment") != null);
	if (!aCommentHasJustBeenPosted) {
		let previousLastVisitedDate = (localStorage.getItem("last-visited-date_" + getPostHash()) || 0);
		localStorage.setItem("previous-last-visited-date_" + getPostHash(), previousLastVisitedDate);
	}

	// Set the new value.
	localStorage.setItem("last-visited-date_" + getPostHash(), date);
}

function updateSavedCommentCount() {
	GWLog("updateSavedCommentCount");

	let commentCount = queryAll(".comment").length;
	localStorage.setItem("comment-count_" + getPostHash(), commentCount);
}
function badgePostsWithNewComments() {
	GWLog("badgePostsWithNewComments");

	if (getQueryVariable("show") == "conversations") return;

	queryAll("h1.listing a[href^='/posts']").forEach(postLink => {
		let postHash = /posts\/(.+?)\//.exec(postLink.href)[1];

		let savedCommentCount = localStorage.getItem("comment-count_" + postHash);
		let commentCountDisplay = postLink.parentElement.nextSibling.query(".comment-count");
		let currentCommentCount = /([0-9]+)/.exec(commentCountDisplay.textContent)[1];

		if (currentCommentCount > savedCommentCount)
			commentCountDisplay.addClass("new-comments");
		commentCountDisplay.title = `${currentCommentCount} comments (${currentCommentCount - savedCommentCount} new)`;
	});
}

/***********************************/
/* CONTENT COLUMN WIDTH ADJUSTMENT */
/***********************************/

function injectContentWidthSelector() {
	GWLog("injectContentWidthSelector");

	// Get saved width setting (or default).
	let currentWidth = localStorage.getItem("selected-width") || 'normal';

	// Inject the content width selector widget and activate buttons.
	let widthSelector = addUIElement("<div id='width-selector'>" +
		String.prototype.concat.apply("", GW.widthOptions.map(widthOption => {
			let [name, desc, abbr] = widthOption;
			let selected = (name == currentWidth ? ' selected' : '');
			let disabled = (name == currentWidth ? ' disabled' : '');
			return `<button type='button' class='select-width-${name}${selected}'${disabled} title='${desc}' tabindex='-1' data-name='${name}'><svg><use xlink:href='${GW.assetVersions['/assets/icons.svg']}#width-${name}'/></svg></button>`;
		}))
+ "</div>");
	widthSelector.queryAll("button").forEach(button => {
		button.addActivateEvent(GW.widthAdjustButtonClicked = (event) => {
			GWLog("GW.widthAdjustButtonClicked");

			// Determine which setting was chosen (i.e., which button was clicked).
			let selectedWidth = button.dataset.name;

			// Save the new setting.
			if (selectedWidth == "normal") localStorage.removeItem("selected-width");
			else localStorage.setItem("selected-width", selectedWidth);

			// Actually change the content width.
			setContentWidth(selectedWidth);
			button.parentElement.childNodes.forEach(aButton => {
				aButton.removeClass("selected");
				aButton.disabled = false;
			});
			button.addClass("selected");
			button.disabled = true;

			// Make sure the accesskey (to cycle to the next width) is on the right button.
			setWidthAdjustButtonsAccesskey();

			// Recompute position & styling of images in overlay.
			recomputeImagesOverlayLayout();

			// Realign hash.
			realignHash();
		});
	});

	// Make sure the accesskey (to cycle to the next width) is on the right button.
	setWidthAdjustButtonsAccesskey();

	// Inject transitions CSS, if animating changes is enabled.
	if (GW.adjustmentTransitions) {
		query("head").insertAdjacentHTML("beforeend", `<style id='width-transition'>
	#content,
	#ui-elements-container,
	#images-overlay {
		transition:
			max-width 0.3s ease;
	}
</style>`);
	}
}
function setWidthAdjustButtonsAccesskey() {
	GWLog("setWidthAdjustButtonsAccesskey");

	let widthSelector = query("#width-selector");
	widthSelector.queryAll("button").forEach(button => {
		button.removeAttribute("accesskey");
		button.title = /(.+?)( \['\])?$/.exec(button.title)[1];
	});
	let selectedButton = widthSelector.query("button.selected");
	let nextButtonInCycle = (selectedButton == selectedButton.parentElement.lastChild) ? selectedButton.parentElement.firstChild : selectedButton.nextSibling;
	nextButtonInCycle.accessKey = "'";
	nextButtonInCycle.title += " [']";
}

/*******************/
/* THEME SELECTION */
/*******************/

function injectThemeSelector() {
	GWLog("injectThemeSelector");

	let currentTheme = readCookie("theme") || "default";
	let themeSelector = addUIElement("<div id='theme-selector' class='theme-selector'>" +
	String.prototype.concat.apply("", GW.themeOptions.map(themeOption => {
		let [name, desc, letter] = themeOption;
		let selected = (name == currentTheme ? ' selected' : '');
		let disabled = (name == currentTheme ? ' disabled' : '');
		let accesskey = letter.charCodeAt(0) - 'A'.charCodeAt(0) + 1;
		return `<button type='button' class='select-theme-${name}${selected}'${disabled} title="${desc} [${accesskey}]" data-theme-name="${name}" data-theme-description="${desc}" accesskey='${accesskey}' tabindex='-1'>${letter}</button>`;
	}))
+ "</div>");
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
		query("head").insertAdjacentHTML("beforeend", `<style id='theme-fade-transition'>
	body {
		transition:
			opacity 0.5s ease-out,
			background-color 0.3s ease-out;
	}
	body.transparent {
		background-color: #777;
		opacity: 0.0;
		transition:
			opacity 0.5s ease-in,
			background-color 0.3s ease-in;
	}
</style>`);
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

	doWhenMatchMedia(GW.mediaQueries.mobileNarrow, "themeLessMobileFirstRowPlaceholder", () => {
		query("#content").insertAdjacentHTML("beforeend", "<div id='theme-less-mobile-first-row-placeholder'></div>");
	}, () => {
		removeElement("#theme-less-mobile-first-row-placeholder");
	}, () => {
		removeElement("#theme-less-mobile-first-row-placeholder");
	});

	if (!GW.isMobile) {
		registerInitializer('addSpans', true, () => (query(".top-post-meta") != null), () => {
			queryAll(".top-post-meta .date, .top-post-meta .comment-count").forEach(element => {
				element.innerHTML = "<span>" + element.innerHTML + "</span>";
			});
		});

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

	if (!GW.isMobile) {
		// Remove spans.
		queryAll(".top-post-meta .date, .top-post-meta .comment-count").forEach(element => {
			element.innerHTML = element.firstChild.innerHTML;
		});
	}

	(query(".top-post-meta .date")||{}).innerHTML = (query(".bottom-post-meta .date")||{}).innerHTML;

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
	query("head").insertAdjacentHTML("beforeend", `<style id='dark-theme-adjustments'>
	body.filter-inverted {
		text-shadow: none;
	}
	body.filter-inverted .body-text {
		text-shadow:
			0 0 1px var(--GW-content-background-color);
	}
</style>`);
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


/********************************************/
/* APPEARANCE CUSTOMIZATION (THEME TWEAKER) */
/********************************************/

function injectThemeTweaker() {
	GWLog("injectThemeTweaker");

	let themeTweakerUI = addUIElement(`<div id='theme-tweaker-ui' style='display: none;'>
	<div class='main-theme-tweaker-window'>
		<h1>Customize appearance</h1>
		<button type='button' class='minimize-button minimize' tabindex='-1'></button>
		<button type='button' class='help-button' tabindex='-1'></button>
		<p class='current-theme'>Current theme: <span>${(readCookie("theme") || "default")}</span></p>
		<p class='theme-selector'></p>
		<div class='controls-container'>
			<div id='theme-tweak-section-sample-text' class='section' data-label='Sample text'>
				<div class='sample-text-container'><span class='sample-text body-text'>
					<p>Less Wrong (text)</p>
					<p><a href="#">Less Wrong (link)</a></p>
				</span></div>
			</div>
			<div id='theme-tweak-section-text-size-adjust' class='section' data-label='Text size'>
				<button type='button' class='text-size-adjust-button decrease' title='Decrease text size'></button>
				<button type='button' class='text-size-adjust-button default' title='Reset to default text size'></button>
				<button type='button' class='text-size-adjust-button increase' title='Increase text size'></button>
			</div>
			<div id='theme-tweak-section-invert' class='section' data-label='Invert (photo-negative)'>
				<input type='checkbox' id='theme-tweak-control-invert'></input>
				<label for='theme-tweak-control-invert'>Invert colors</label>
			</div>
			<div id='theme-tweak-section-saturate' class='section' data-label='Saturation'>
				<input type="range" id="theme-tweak-control-saturate" min="0" max="300" data-default-value="100" data-value-suffix="%" data-label-suffix="%">
				<p class="theme-tweak-control-label" id="theme-tweak-label-saturate"></p>
				<div class='notch theme-tweak-slider-notch-saturate' title='Reset saturation to default value (100%)'></div>
			</div>
			<div id='theme-tweak-section-brightness' class='section' data-label='Brightness'>
				<input type="range" id="theme-tweak-control-brightness" min="0" max="300" data-default-value="100" data-value-suffix="%" data-label-suffix="%">
				<p class="theme-tweak-control-label" id="theme-tweak-label-brightness"></p>
				<div class='notch theme-tweak-slider-notch-brightness' title='Reset brightness to default value (100%)'></div>
			</div>
			<div id='theme-tweak-section-contrast' class='section' data-label='Contrast'>
				<input type="range" id="theme-tweak-control-contrast" min="0" max="300" data-default-value="100" data-value-suffix="%" data-label-suffix="%">
				<p class="theme-tweak-control-label" id="theme-tweak-label-contrast"></p>
				<div class='notch theme-tweak-slider-notch-contrast' title='Reset contrast to default value (100%)'></div>
			</div>
			<div id='theme-tweak-section-hue-rotate' class='section' data-label='Hue rotation'>
				<input type="range" id="theme-tweak-control-hue-rotate" min="0" max="360" data-default-value="0" data-value-suffix="deg" data-label-suffix="°">
				<p class="theme-tweak-control-label" id="theme-tweak-label-hue-rotate"></p>
				<div class='notch theme-tweak-slider-notch-hue-rotate' title='Reset hue to default (0° away from standard colors for theme)'></div>
			</div>
		</div>
		<div class='buttons-container'>
			<button type="button" class="reset-defaults-button">Reset to defaults</button>
			<button type='button' class='ok-button default-button'>OK</button>
			<button type='button' class='cancel-button'>Cancel</button>
		</div>
	</div>
	<div class="clippy-container">
		<span class="hint">Hi, I'm Bobby the Basilisk! Click on the minimize button (<span></span>) to minimize the theme tweaker window, so that you can see what the page looks like with the current tweaked values. (But remember, <span>the changes won't be saved until you click "OK"!</span></span>)
		<div class='clippy'></div>
		<button type='button' class='clippy-close-button' tabindex='-1' title='Hide theme tweaker assistant (you can bring him back by clicking the ? button in the title bar)'></button>
	</div>
	<div class='help-window' style='display: none;'>
		<h1>Theme tweaker help</h1>
		<div id='theme-tweak-section-clippy' class='section' data-label='Theme Tweaker Assistant'>
			<input type='checkbox' id='theme-tweak-control-clippy' checked='checked'></input>
			<label for='theme-tweak-control-clippy'>Show Bobby the Basilisk</label>
		</div>
		<div class='buttons-container'>
			<button type='button' class='ok-button default-button'>OK</button>
			<button type='button' class='cancel-button'>Cancel</button>
		</div>
	</div>
</div>`);

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

	let themeTweakerToggle = addUIElement(`<div id='theme-tweaker-toggle'><button type='button' tabindex='-1' title="Customize appearance [;]" accesskey=';'>&#xf3f0;</button></div>`);
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


/*********************/
/* PAGE QUICK-NAV UI */
/*********************/

function injectQuickNavUI() {
	GWLog("injectQuickNavUI");

	let quickNavContainer = addUIElement(`<div id='quick-nav-ui'>
	<a href='#top' title="Up to top [,]" accesskey=','>&#xf106;</a>
	<a href='#answers' title="Answers [/]">&#xf4a2;</a>
	<a href='#comments' title="Comments [/]">&#xf036;</a>
	<a href='#bottom-bar' title="Down to bottom [.]" accesskey='.'>&#xf107;</a>
</div>`);

	// Set initial accesskey.
	quickNavContainer.query(`a[href='${query("#answers") ? "#answers" : "#comments"}']`).accessKey = '/';

	// Correct for bug where this stays focused when clicked.
	quickNavContainer.query("a[href='#top']").addActivateEvent(GW.quickNavTopButtonClicked = (event) => {
		event.target.blur();
	});

	// Add flash indicator for accesskey activation.
	quickNavContainer.queryAll("a").forEach(button => {
		button.addActivateEvent(GW.quickNavButtonClicked = (event) => {
			GWLog("GW.quickNavButtonClicked");

			if (/no-/.test(button.className) == true) return;
			clearTimeout(button.fadeOutTimer);
			button.addClass("highlighted");
			button.fadeOutTimer = setTimeout(() => {
				button.removeClass("highlighted");
			}, 150);
		});
	});
}

/**********************/
/* NEW COMMENT NAV UI */
/**********************/

function injectNewCommentNavUI(newCommentsCount) {
	GWLog("injectNewCommentNavUI");

	let newCommentNavUIContainer = addUIElement(`<div id='new-comment-nav-ui'>
	<button type='button' class='new-comment-sequential-nav-button new-comment-previous' title='Previous new comment (,)' tabindex='-1'>&#xf0d8;</button>
	<span class='new-comments-count'></span>
	<button type='button' class='new-comment-sequential-nav-button new-comment-next' title='Next new comment (.)' tabindex='-1'>&#xf0d7;</button>
</div>`);

	newCommentNavUIContainer.queryAll(".new-comment-sequential-nav-button").forEach(button => {
		button.addActivateEvent(GW.commentQuicknavButtonClicked = (event) => {
			GWLog("GW.commentQuicknavButtonClicked");

			scrollToNewComment(event.target.hasClass("new-comment-next"));
			event.target.blur();
		});
	});

	document.addEventListener("keyup", GW.commentQuicknavKeyPressed = (event) => { 
		if (event.shiftKey || event.ctrlKey || event.altKey) return;
		if (!(event.key == "," || event.key == ".")) return;

		GWLog("GW.commentQuicknavKeyPressed");

		if (event.key == ",") scrollToNewComment(false);
		if (event.key == ".") scrollToNewComment(true)

		let button = query("#new-comment-nav-ui .new-comment-" + (event.key == "," ? "previous" : "next"));
		if (button.disabled == true) return;
		clearTimeout(button.fadeOutTimer);
		button.addClass("highlighted");
		button.fadeOutTimer = setTimeout(() => {
			button.removeClass("highlighted");
		}, 150);
	});

	let hnsDatePicker = addUIElement(`<div id='hns-date-picker'>
	<span>Since:</span>
	<input type='text' class='hns-date'></input>
</div>`);

	window.addEventListener("resize", GW.hnsDatePickerFlipToFit = (event) => {
		GWLog("hnsDatePickerFlipToFit");

		hnsDatePicker.toggleClass("flipped", (GW.mediaQueries.mobileMax.matches == false) && (query("#content").clientWidth > (window.innerWidth - 535)));
	});
	GW.hnsDatePickerFlipToFit();

	hnsDatePicker.query("input").addEventListener("input", GW.hnsDatePickerValueChanged = (event) => {
		GWLog("GW.hnsDatePickerValueChanged");

		let hnsDate = time_fromHuman(event.target.value);
		let newCommentsCount = highlightCommentsSince(hnsDate);
		updateNewCommentNavUI(newCommentsCount);
	}, false);

	newCommentNavUIContainer.query(".new-comments-count").addActivateEvent(GW.newCommentsCountClicked = (event) => {
		GWLog("GW.newCommentsCountClicked");

		let hnsDatePickerVisible = (getComputedStyle(hnsDatePicker).display != "none");
		hnsDatePicker.style.display = hnsDatePickerVisible ? "none" : "block";
	});
}

// time_fromHuman() function copied from https://bakkot.github.io/SlateStarComments/ssc.js
function time_fromHuman(string) {
	/* Convert a human-readable date into a JS timestamp */
	if (string.match(/^[0-9]{4}-[0-9]{2}-[0-9]{2}/)) {
		string = string.replace(' ', 'T');  // revert nice spacing
		string += ':00.000Z';  // complete ISO 8601 date
		time = Date.parse(string);  // milliseconds since epoch

		// browsers handle ISO 8601 without explicit timezone differently
		// thus, we have to fix that by hand
		time += (new Date()).getTimezoneOffset() * 60e3;
	} else {
		string = string.replace(' at', '');
		time = Date.parse(string);  // milliseconds since epoch
	}
	return time;
}

function updateNewCommentNavUI(newCommentsCount, hnsDate = -1) {
	GWLog("updateNewCommentNavUI");

	// Update the new comments count.
	let newCommentsCountLabel = query("#new-comment-nav-ui .new-comments-count");
	newCommentsCountLabel.innerText = newCommentsCount;
	newCommentsCountLabel.title = `${newCommentsCount} new comments`;

	// Update the date picker field.
	if (hnsDate != -1) {
		query("#hns-date-picker input").value = (new Date(+ hnsDate - (new Date()).getTimezoneOffset() * 60e3)).toISOString().slice(0, 16).replace('T', ' ');
	}
}

/***************************/
/* TEXT SIZE ADJUSTMENT UI */
/***************************/

GW.themeTweaker.textSizeAdjustButtonClicked = (event) => {
	GWLog("GW.themeTweaker.textSizeAdjustButtonClicked");

	var zoomFactor = parseFloat(GW.currentTextZoom) || 1.0;
	if (event.target.hasClass("decrease")) {
		zoomFactor = (zoomFactor - 0.05).toFixed(2);
	} else if (event.target.hasClass("increase")) {
		zoomFactor = (zoomFactor + 0.05).toFixed(2);
	} else {
		zoomFactor = 1.0;
	}
	setTextZoom(zoomFactor);
	GW.currentTextZoom = `${zoomFactor}`;

	if (event.target.parentElement.id == "text-size-adjustment-ui") {
		localStorage.setItem("text-zoom", GW.currentTextZoom);
	}
};

function injectTextSizeAdjustmentUIReal() {
	GWLog("injectTextSizeAdjustmentUIReal");

	let textSizeAdjustmentUIContainer = addUIElement(`<div id='text-size-adjustment-ui'>
	<button type='button' class='text-size-adjust-button decrease' title="Decrease text size [-]" tabindex='-1' accesskey='-'>&#xf068;</button>
	<button type='button' class='text-size-adjust-button default' title="Reset to default text size [0]" tabindex='-1' accesskey='0'>A</button>
	<button type='button' class='text-size-adjust-button increase' title="Increase text size [=]" tabindex='-1' accesskey='='>&#xf067;</button>
</div>`);

	textSizeAdjustmentUIContainer.queryAll("button").forEach(button => {
		button.addActivateEvent(GW.themeTweaker.textSizeAdjustButtonClicked);
	});

	GW.currentTextZoom = `${parseFloat(localStorage.getItem("text-zoom")) || 1.0}`;
}

function injectTextSizeAdjustmentUI() {
	GWLog("injectTextSizeAdjustmentUI");

	if (query("#text-size-adjustment-ui") != null) return;
	if (query("#content.post-page") != null) injectTextSizeAdjustmentUIReal();
	else document.addEventListener("DOMContentLoaded", () => {
		if (!(query(".post-body") == null && query(".comment-body") == null)) injectTextSizeAdjustmentUIReal();
	}, {once: true});
}

/********************************/
/* COMMENTS VIEW MODE SELECTION */
/********************************/

// { { { modules/chrono_mode } } }

/********************************/
/* COMMENTS LIST MODE SELECTION */
/********************************/

function injectCommentsListModeSelector() {
	GWLog("injectCommentsListModeSelector");

	if (query(".listings .comment-thread") == null) return;

	query(".listings").insertAdjacentHTML("beforebegin", `<div id='comments-list-mode-selector'>
	<button type='button' class='expanded' title='Expanded comments view' tabindex='-1'><svg><use xlink:href='${GW.assetVersions['/assets/icons.svg']}#comments-expanded'/></svg></button>
	<button type='button' class='compact' title='Compact comments view' tabindex='-1'><svg><use xlink:href='${GW.assetVersions['/assets/icons.svg']}#comments-compact'/></svg></button>
</div>`);
	let commentsListModeSelector = query("#comments-list-mode-selector");

	commentsListModeSelector.queryAll("button").forEach(button => {
		button.addActivateEvent(GW.commentsListModeSelectButtonClicked = (event) => {
			GWLog("GW.commentsListModeSelectButtonClicked");

			button.parentElement.queryAll("button").forEach(aButton => {
				aButton.removeClass("selected");
				aButton.disabled = false;
				aButton.accessKey = '`';
			});
			localStorage.setItem("comments-list-mode", button.className);
			button.addClass("selected");
			button.disabled = true;
			button.removeAttribute("accesskey");

			if (button.hasClass("expanded")) {
				query("#content").removeClass("compact");
			} else {
				query("#content").addClass("compact");
			}
		});
	});

	let savedMode = (localStorage.getItem("comments-list-mode") == "compact") ? "compact" : "expanded";
	if (savedMode == "compact")
		query("#content").addClass("compact");
	commentsListModeSelector.query(`.${savedMode}`).addClass("selected");
	commentsListModeSelector.query(`.${savedMode}`).disabled = true;
	commentsListModeSelector.query(`.${(savedMode == "compact" ? "expanded" : "compact")}`).accessKey = '`';

	if (!GW.mediaQueries.hover.matches) {
		queryAll(".comment-thread").forEach(commentThread => {
			commentThread.addActivateEvent(GW.commentThreadInListingTouched = (event) => {
				GWLog("commentThreadInListingTouched");

				Æ(event.target.closest("#content.compact .comment-thread")).toggleClass("expanded");
			});
		});
	}
}

/**********************/
/* SITE NAV UI TOGGLE */
/**********************/

function injectSiteNavUIToggle() {
	GWLog("injectSiteNavUIToggle");

	let siteNavUIToggle = addUIElement("<div id='site-nav-ui-toggle'><button type='button' tabindex='-1'>&#xf0c9;</button></div>");
	siteNavUIToggle.query("button").addActivateEvent(GW.siteNavUIToggleButtonClicked = (event) => {
		GWLog("GW.siteNavUIToggleButtonClicked");
		toggleSiteNavUI();
		localStorage.setItem("site-nav-ui-toggle-engaged", event.target.hasClass("engaged"));
	});

	if (!GW.isMobile && localStorage.getItem("site-nav-ui-toggle-engaged") == "true") toggleSiteNavUI();
}
function removeSiteNavUIToggle() {
	GWLog("removeSiteNavUIToggle");

	queryAll("#primary-bar, #secondary-bar, .page-toolbar, #site-nav-ui-toggle button").forEach(element => {
		element.removeClass("engaged");
	});
	removeElement("#site-nav-ui-toggle");
}
function toggleSiteNavUI() {
	GWLog("toggleSiteNavUI");

	queryAll("#primary-bar, #secondary-bar, .page-toolbar, #site-nav-ui-toggle button").forEach(element => {
		element.toggleClass("engaged");
		element.removeClass("translucent-on-scroll");
	});
}

/**********************/
/* POST NAV UI TOGGLE */
/**********************/

function injectPostNavUIToggle() {
	GWLog("injectPostNavUIToggle");

	let postNavUIToggle = addUIElement("<div id='post-nav-ui-toggle'><button type='button' tabindex='-1'>&#xf14e;</button></div>");
	postNavUIToggle.query("button").addActivateEvent(GW.postNavUIToggleButtonClicked = (event) => {
		GWLog("GW.postNavUIToggleButtonClicked");
		togglePostNavUI();
		localStorage.setItem("post-nav-ui-toggle-engaged", localStorage.getItem("post-nav-ui-toggle-engaged") != "true");
	});

	GW.postNavUIToggleTargetsSelector = "#quick-nav-ui, #new-comment-nav-ui, #hns-date-picker, #post-nav-ui-toggle button";

	// Prevent “flashing” of elements when resizing window.
	doWhenMatchMedia(GW.mediaQueries.mobileMax, "preventPostNavUIFlashingWhenWindowResized", () => {
		queryAll(GW.postNavUIToggleTargetsSelector + ", #quick-nav-ui > *").forEach(element => {
			element.style.transition = "none";
			setTimeout(() => {
				element.style.transition = "";
			});
			element.toggleClass("engaged", false);
		});
	});

	if (localStorage.getItem("post-nav-ui-toggle-engaged") == "true") togglePostNavUI();
}
function removePostNavUIToggle() {
	GWLog("removePostNavUIToggle");

	queryAll(GW.postNavUIToggleTargetsSelector).forEach(element => {
		element.removeClass("engaged");
	});
	removeElement("#post-nav-ui-toggle");
}
function togglePostNavUI() {
	GWLog("togglePostNavUI");

	queryAll(GW.postNavUIToggleTargetsSelector).forEach(element => {
		element.toggleClass("engaged");
	});
}

/*******************************/
/* APPEARANCE ADJUST UI TOGGLE */
/*******************************/

function injectAppearanceAdjustUIToggle() {
	GWLog("injectAppearanceAdjustUIToggle");

	let appearanceAdjustUIToggle = addUIElement("<div id='appearance-adjust-ui-toggle'><button type='button' tabindex='-1'>&#xf013;</button></div>");
	appearanceAdjustUIToggle.query("button").addActivateEvent(GW.appearanceAdjustUIToggleButtonClicked = (event) => {
		GWLog("GW.appearanceAdjustUIToggleButtonClicked");
		toggleAppearanceAdjustUI();
		localStorage.setItem("appearance-adjust-ui-toggle-engaged", event.target.hasClass("engaged"));
	});

	let themeSelectorCloseButton = appearanceAdjustUIToggle.query("button").cloneNode(true);
	themeSelectorCloseButton.addClass("theme-selector-close-button");
	themeSelectorCloseButton.innerHTML = "&#xf057;";
	query("#theme-selector").appendChild(themeSelectorCloseButton);
	themeSelectorCloseButton.addActivateEvent(GW.appearanceAdjustUIToggleButtonClicked);

	GW.appearanceAdjustUIToggleTargetsSelector = "#comments-view-mode-selector, #theme-selector, #width-selector, #text-size-adjustment-ui, #theme-tweaker-toggle, #appearance-adjust-ui-toggle, #appearance-adjust-ui-toggle button";

	// Prevent “flashing” of elements when resizing window.
	doWhenMatchMedia(GW.mediaQueries.mobileMax, "preventAppearanceAdjustUIFlashingWhenWindowResized", () => {
		queryAll(GW.appearanceAdjustUIToggleTargetsSelector).forEach(element => {
			element.style.transition = "none";
			setTimeout(() => {
				element.style.transition = "";
			});
			element.toggleClass("engaged", false);
		});
	});
}

function removeAppearanceAdjustUIToggle() {
	GWLog("removeAppearanceAdjustUIToggle");

	queryAll(GW.appearanceAdjustUIToggleTargetsSelector).forEach(element => {
		element.removeClass("engaged");
	});
	removeElement("#appearance-adjust-ui-toggle");
}

function toggleAppearanceAdjustUI(show) {
	GWLog("toggleAppearanceAdjustUI");

	if (query(GW.appearanceAdjustUIToggleTargetsSelector).hasClass("engaged")) {
		document.removeEventListener("keyup", GW.mobileThemeTweakerHideKeyPressed);
	} else {
		document.addEventListener("keyup", GW.mobileThemeTweakerHideKeyPressed = (event) => {
			if (event.key == 'Escape') {
				GWLog("GW.mobileThemeTweakerHideKeyPressed");

				toggleAppearanceAdjustUI(false);
			}
		});
	}

	queryAll(GW.appearanceAdjustUIToggleTargetsSelector).forEach(element => {
		if (typeof show == "undefined") {
			element.toggleClass("engaged");
		} else {
			element.toggleClass("engaged", show);
		}
	});
}

/*****************************/
/* MINIMIZED THREAD HANDLING */
/*****************************/

function expandAncestorsOf(commentID) {
	GWLog("expandAncestorsOf");

	let comment = query("#comment-" + commentID);
	if (!comment) {
		GWLog("Comment with ID " + commentID + " does not exist, so we can’t expand its ancestors.");
		return;
	}

	// Expand collapsed comments.
	Æ(comment.closest(".comment-item.minimized")).setCommentThreadMaximized(true, false, true);
}

/**************************/
/* WORD COUNT & READ TIME */
/**************************/

function toggleReadTimeOrWordCount(addWordCountClass) {
	GWLog("toggleReadTimeOrWordCount");

	queryAll(".post-meta .read-time").forEach(element => {
		element.toggleClass("word-count", addWordCountClass);

		let titleParts = /(\S+)(.+)$/.exec(element.title);
		[ element.innerHTML, element.title ] = [ `${titleParts[1]}<span>${titleParts[2]}</span>`, element.textContent ];
	});
}

/***************************/
/* ORIGINAL POSTER BADGING */
/***************************/

function markOriginalPosterComments() {
	GWLog("markOriginalPosterComments");

	let postAuthor = query(".post .author");
	if (postAuthor == null) return;

	queryAll(".comment-item .author, .comment-item .inline-author").forEach(author => {
		if (author.dataset.userid == postAuthor.dataset.userid ||
			(author.hash != "" && query(`${author.hash} .author`).dataset.userid == postAuthor.dataset.userid)) {
			author.addClass("original-poster");
			author.title += "Original poster";
		}
	});
}

/*****************/
/* ANTI-KIBITZER */
/*****************/

function numToAlpha(n) {
	let ret = "";
	do {
		ret = String.fromCharCode('A'.charCodeAt(0) + (n % 26)) + ret;
		n = Math.floor((n / 26) - 1);
	} while (n >= 0);
	return ret;
}

function injectAntiKibitzer() {
	GWLog("injectAntiKibitzer");

	// Inject anti-kibitzer toggle controls.
	let antiKibitzerToggle = addUIElement("<div id='anti-kibitzer-toggle'><button type='button' tabindex='-1' accesskey='g' title='Toggle anti-kibitzer (show/hide authors & karma values) [g]'></button>");
	antiKibitzerToggle.query("button").addActivateEvent(GW.antiKibitzerToggleButtonClicked = (event) => {
		GWLog("GW.antiKibitzerToggleButtonClicked");
		if (query("#anti-kibitzer-toggle").hasClass("engaged") && 
			!event.shiftKey &&
			!confirm("Are you sure you want to turn OFF the anti-kibitzer?\n\n(This will reveal the authors and karma values of all posts and comments!)")) {
			event.target.blur();
			return;
		}

		toggleAntiKibitzerMode();
		event.target.blur();
	});

	// Activate anti-kibitzer mode (if needed).
	if (localStorage.getItem("antikibitzer") == "true")
		toggleAntiKibitzerMode();

	// Remove temporary CSS that hides the authors and karma values.
	removeElement("#antikibitzer-temp");
}

function toggleAntiKibitzerMode() {
	GWLog("toggleAntiKibitzerMode");

	// This will be the URL of the user’s own page, if logged in, or the URL of
	// the login page otherwise.
	let userTabTarget = query("#nav-item-login .nav-inner").href;
	let pageHeadingElement = query("h1.page-main-heading");

	let userCount = 0;
	let userFakeName = { };

	let appellation = (query(".comment-thread-page") ? "Commenter" : "User");

	let postAuthor = query(".post-page .post-meta .author");
	if (postAuthor) userFakeName[postAuthor.dataset["userid"]] = "Original Poster";

	let antiKibitzerToggle = query("#anti-kibitzer-toggle");
	if (antiKibitzerToggle.hasClass("engaged")) {
		localStorage.setItem("antikibitzer", "false");

		let redirectTarget = pageHeadingElement && pageHeadingElement.dataset["kibitzerRedirect"];
		if (redirectTarget) {
			window.location = redirectTarget;
			return;
		}

		// Individual comment page title and header.
		if (query(".individual-thread-page")) {
			let replacer = (node) => {
				if (!node) return;
				node.firstChild.replaceWith(node.dataset["trueContent"]);
			}
			replacer(query("title:not(.fake-title)"));
			replacer(query("#content > h1"));
		}

		// Author names/links.
		queryAll(".author.redacted, .inline-author.redacted").forEach(author => {
			author.textContent = author.dataset["trueName"];
			if (/\/user/.test(author.href)) author.href = author.dataset["trueLink"];

			author.removeClass("redacted");
		});
		// Post/comment karma values.
		queryAll(".karma-value.redacted").forEach(karmaValue => {
			karmaValue.innerHTML = karmaValue.dataset["trueValue"] + karmaValue.lastChild.outerHTML;
			karmaValue.lastChild.textContent = (parseInt(karmaValue.dataset["trueValue"]) == 1) ? " point" : " points";

			karmaValue.removeClass("redacted");
		});
		// Link post domains.
		queryAll(".link-post-domain.redacted").forEach(linkPostDomain => {
			linkPostDomain.textContent = linkPostDomain.dataset["trueDomain"];

			linkPostDomain.removeClass("redacted");
		});

		antiKibitzerToggle.removeClass("engaged");
	} else {
		localStorage.setItem("antikibitzer", "true");

		let redirectTarget = pageHeadingElement && pageHeadingElement.dataset["antiKibitzerRedirect"];
		if (redirectTarget) {
			window.location = redirectTarget;
			return;
		}

		// Individual comment page title and header
		if (query(".individual-thread-page")) {
			let replacer = (node) => {
				if (!node) return;
				node.dataset["trueContent"] = node.firstChild.wholeText;
				let newText = node.firstChild.wholeText.replace(/^.* comments/, "REDACTED comments");
				node.firstChild.replaceWith(newText);
			}
			replacer(query("title:not(.fake-title)"));
			replacer(query("#content > h1"));
		}

		removeElement("title.fake-title");

		// Author names/links.
		queryAll(".author, .inline-author").forEach(author => {
			// Skip own posts/comments.
			if (author.hasClass("own-user-author"))
				return;

			let userid = author.dataset["userid"] || query(`${author.hash} .author`).dataset["userid"];

			author.dataset["trueName"] = author.textContent;
			author.textContent = userFakeName[userid] || (userFakeName[userid] = appellation + " " + numToAlpha(userCount++));

			if (/\/user/.test(author.href)) {
				author.dataset["trueLink"] = author.pathname;
				author.href = "/user?id=" + author.dataset["userid"];
			}

			author.addClass("redacted");
		});
		// Post/comment karma values.
		queryAll(".karma-value").forEach(karmaValue => {
			// Skip own posts/comments.
			if ((karmaValue.closest(".comment-item") || karmaValue.closest(".post-meta")).query(".author").hasClass("own-user-author"))
				return;

			karmaValue.dataset["trueValue"] = karmaValue.firstChild.textContent;
			karmaValue.innerHTML = "##" + karmaValue.lastChild.outerHTML;
			karmaValue.lastChild.textContent = " points";

			karmaValue.addClass("redacted");
		});
		// Link post domains.
		queryAll(".link-post-domain").forEach(linkPostDomain => {
			// Skip own posts/comments.
			if (userTabTarget == linkPostDomain.closest(".post-meta").query(".author").href)
				return;

			linkPostDomain.dataset["trueDomain"] = linkPostDomain.textContent;
			linkPostDomain.textContent = "redacted.domain.tld";

			linkPostDomain.addClass("redacted");
		});

		antiKibitzerToggle.addClass("engaged");
	}
}

/*******************************/
/* COMMENT SORT MODE SELECTION */
/*******************************/

var CommentSortMode = Object.freeze({
	TOP:		"top",
	NEW:		"new",
	OLD:		"old",
	HOT:		"hot"
});
function sortComments(section, mode) {
	GWLog("sortComments");

	let commentsContainer = query(`#${section}`);

	commentsContainer.removeClass(/(sorted-\S+)/.exec(commentsContainer.className)[1]);
	commentsContainer.addClass("sorting");

	GW.commentValues = { };
	let clonedCommentsContainer = commentsContainer.cloneNode(true);
	clonedCommentsContainer.queryAll(".comment-thread").forEach(commentThread => {
		var comparator;
		switch (mode) {
		case CommentSortMode.NEW:
			comparator = (a,b) => commentDate(b) - commentDate(a);
			break;
		case CommentSortMode.OLD:
			comparator = (a,b) => commentDate(a) - commentDate(b);
			break;
		case CommentSortMode.HOT:
			comparator = (a,b) => commentVoteCount(b) - commentVoteCount(a);
			break;
		case CommentSortMode.TOP:
		default:
			comparator = (a,b) => commentKarmaValue(b) - commentKarmaValue(a);
			break;
		}
		Array.from(commentThread.childNodes).sort(comparator).forEach(commentItem => { commentThread.appendChild(commentItem); })
	});
	removeElement(commentsContainer.lastChild);
	commentsContainer.appendChild(clonedCommentsContainer.lastChild);
	GW.commentValues = { };

	if (loggedInUserId) {
		// Re-activate vote buttons.
		commentsContainer.queryAll("button.vote").forEach(voteButton => {
			voteButton.addActivateEvent(voteButtonClicked);
		});

		// Re-activate comment action buttons.
		commentsContainer.queryAll(".action-button").forEach(button => {
			button.addActivateEvent(GW.commentActionButtonClicked);
		});
	}

	// Re-activate comment-minimize buttons.
	commentsContainer.queryAll(".comment-minimize-button").forEach(button => {
		button.addActivateEvent(GW.commentMinimizeButtonClicked);
	});

	// Re-add comment parent popups.
	addCommentParentPopups();

	// Redo new-comments highlighting.
	highlightCommentsSince(time_fromHuman(query("#hns-date-picker input").value));

	requestAnimationFrame(() => {
		commentsContainer.removeClass("sorting");
		commentsContainer.addClass("sorted-" + mode);
	});
}
function commentKarmaValue(commentOrSelector) {
	if (typeof commentOrSelector == "string") commentOrSelector = query(commentOrSelector);
	return GW.commentValues[commentOrSelector.id] || (GW.commentValues[commentOrSelector.id] = parseInt(commentOrSelector.query(".karma-value").firstChild.textContent));
}
function commentDate(commentOrSelector) {
	if (typeof commentOrSelector == "string") commentOrSelector = query(commentOrSelector);
	return GW.commentValues[commentOrSelector.id] || (GW.commentValues[commentOrSelector.id] = parseInt(commentOrSelector.query(".date").dataset.jsDate));
}
function commentVoteCount(commentOrSelector) {
	if (typeof commentOrSelector == "string") commentOrSelector = query(commentOrSelector);
	return GW.commentValues[commentOrSelector.id] || (GW.commentValues[commentOrSelector.id] = parseInt(commentOrSelector.query(".karma-value").title.split(" ")[0]));
}

function injectCommentsSortModeSelector() {
	GWLog("injectCommentsSortModeSelector");

	[ "comments", "answers" ].forEach(section => {
		let topThread = query(`#${section} > .comment-thread`);
		if (topThread == null) return;

		// Do not show sort mode selector if there is no branching in comment tree.
		if (topThread.query(".comment-item + .comment-item") == null) return;

		let sortModeSelectorHTML = `<div id='${section}-sort-mode-selector' class='sublevel-nav sort'>` + 
			Object.values(CommentSortMode).map(sortMode => `<button type='button' class='sublevel-item sort-mode-${sortMode}' tabindex='-1' title='Sort by ${sortMode}'>${sortMode}</button>`).join("") +  
			"</div>";
		topThread.insertAdjacentHTML("beforebegin", sortModeSelectorHTML);

		queryAll(`#${section}-sort-mode-selector button`).forEach(button => {
			button.addActivateEvent(GW[`${section}SortModeSelectButtonClicked`] = (event) => {
				GWLog(`GW.${section}SortModeSelectButtonClicked`);

				event.target.parentElement.queryAll("button").forEach(button => {
					button.removeClass("selected");
					button.disabled = false;
				});
				event.target.addClass("selected");
				event.target.disabled = true;

				setTimeout(() => { sortComments(section, /sort-mode-(\S+)/.exec(event.target.className)[1]); });
				setCommentsSortModeSelectButtonsAccesskey(section);
			});
		});
	});

	[ "comments", "answers" ].forEach(section => {
		let sortModeSelector = query(`#${section}-sort-mode-selector`);
		if (sortModeSelector == null) return;

		// TODO: Make this actually get the current sort mode (if that’s saved).
		// TODO: Also change the condition here to properly get chrono/threaded mode,
		// when that is properly done with cookies.
		let currentSortMode = (location.href.search("chrono=t") == -1) ? CommentSortMode.TOP : CommentSortMode.OLD;
		query(`#${section}`).addClass("sorted-" + currentSortMode);
		sortModeSelector.query(`.sort-mode-${currentSortMode}`).disabled = true;
		sortModeSelector.query(`.sort-mode-${currentSortMode}`).addClass("selected");
		setCommentsSortModeSelectButtonsAccesskey(section);
	});
}

function setCommentsSortModeSelectButtonsAccesskey(section) {
	GWLog("setCommentsSortModeSelectButtonsAccesskey");

	queryAll(`#${section}-sort-mode-selector button`).forEach(button => {
		button.removeAttribute("accesskey");
		button.title = /(.+?)( \[[zx]\])?$/.exec(button.title)[1];
	});
	let selectedButton = query(`#${section}-sort-mode-selector button.selected`);
	let nextButtonInCycle = (selectedButton == selectedButton.parentElement.lastChild) ? selectedButton.parentElement.firstChild : selectedButton.nextSibling;
	nextButtonInCycle.accessKey = (section == "comments") ? 'z' : 'x';
	nextButtonInCycle.title += ` [${nextButtonInCycle.accessKey}]`;
}

/*************************/
/* COMMENT PARENT POPUPS */
/*************************/

function addCommentParentPopups() {
	GWLog("addCommentParentPopups");

	if (!query("#content").hasClass("comment-thread-page")) return;

	queryAll(".comment-meta a.comment-parent-link, .comment-meta a.comment-child-link").forEach(commentParentLink => {
		commentParentLink.addEventListener("mouseover", GW.commentParentLinkMouseOver = (event) => {
			GWLog("GW.commentParentLinkMouseOver");

			let parentID = commentParentLink.getAttribute("href");
			var parent, popup;
			if (!(parent = (query(parentID)||{}).firstChild)) return;
			var highlightClassName;
			if (parent.getBoundingClientRect().bottom < 10 || parent.getBoundingClientRect().top > window.innerHeight + 10) {
				parentHighlightClassName = "highlight-faint";
				popup = parent.cloneNode(true);
				popup.addClasses([ "comment-popup", "highlight" ]);
				commentParentLink.addEventListener("mouseout", GW.commentParentLinkMouseOut = (event) => {
					removeElement(popup);
				}, {once: true});
				commentParentLink.closest(".comments > .comment-thread").appendChild(popup);
			} else {
				parentHighlightClassName = "highlight";
			}
			parent.parentNode.addClass(parentHighlightClassName);
			commentParentLink.addEventListener("mouseout", (event) => {
				parent.parentNode.removeClass(parentHighlightClassName);
			}, {once: true});
		});
	});

	queryAll(".comment-meta a.comment-parent-link").forEach(commentParentLink => {
		commentParentLink.innerHTML = "<span></span>";
	});

	// Due to filters vs. fixed elements, we need to be smarter about selecting which elements to filter...
	GW.themeTweaker.filtersExclusionPaths.commentParentPopups = [
		"#content .comments .comment-thread"
	];
	applyFilters(GW.currentFilters);
}

/*****************/
/* KEYBOARD HELP */
/*****************/

function keyboardHelpSetup() {
	let keyboardHelpOverlay = addUIElement(`<nav id='keyboard-help-overlay'>
	<div class='keyboard-help-container'>
		<button type='button' title='Close keyboard shortcuts' class='close-keyboard-help'>&#xf00d;</button>
		<h1>Keyboard shortcuts</h1>
		<p class='note'>Keys shown in yellow (e.g., <code class='ak'>]</code>) are <a href='https://en.wikipedia.org/wiki/Access_key#Access_in_different_browsers' target='_blank'>accesskeys</a>, and require a browser-specific modifier key (or keys).</p>
		<p class='note'>Keys shown in grey (e.g., <code>?</code>) do not require any modifier keys.</p>
		<div class='keyboard-shortcuts-lists'>` + 
		[ [
			"General",
			[ [ '?' ], "Show keyboard shortcuts" ],
			[ [ 'Esc' ], "Hide keyboard shortcuts" ]
		], [
			"Site navigation",
			[ [ 'ak-h' ], "Go to Home (a.k.a. “Frontpage”) view" ],
			[ [ 'ak-f' ], "Go to Featured (a.k.a. “Curated”) view" ],
			[ [ 'ak-a' ], "Go to All (a.k.a. “Community”) view" ],
			[ [ 'ak-m' ], "Go to Meta view" ],
			[ [ 'ak-c' ], "Go to Recent Comments view" ],
			[ [ 'ak-r' ], "Go to Archive view" ],
			[ [ 'ak-q' ], "Go to Sequences view" ],
			[ [ 'ak-t' ], "Go to About page" ],
			[ [ 'ak-u' ], "Go to User or Login page" ],
			[ [ 'ak-o' ], "Go to Inbox page" ]
		], [
			"Page navigation",
			[ [ 'ak-,' ], "Jump up to top of page" ],
			[ [ 'ak-.' ], "Jump down to bottom of page" ],
			[ [ 'ak-/' ], "Jump to top of answers section" ],
			[ [ 'ak-/' ], "Jump to top of comments section" ],
			[ [ ';' ], "Focus external link (on link posts)" ],
			[ [ 'ak-s' ], "Search" ]
		], [
			"Page actions",
			[ [ 'ak-n' ], "New post or comment" ],
			[ [ 'ak-w' ], "New answer (on question posts)" ],
			[ [ 'ak-e' ], "Edit current post" ],
			[ [ 'ak-z' ], "Switch to next sort order (comments)" ],
			[ [ 'ak-x' ], "Switch to next sort order (answers)" ]
		], [
			"Sequences",
			[ [ 'ak-]' ], "Go to next post in sequence" ],
			[ [ 'ak-[' ], "Go to previous post in sequence" ],
			[ [ 'ak-\\' ], "Go to sequence index" ]
		], [
			"Post/comment list views",
			[ [ '.' ], "Focus next entry (post/comment)" ],
			[ [ ',' ], "Focus previous entry (post/comment)" ],
			[ [ ';' ], "Cycle between links in focused entry" ],
			[ [ 'Enter' ], "Go to currently focused entry" ],
			[ [ 'Esc' ], "Unfocus currently focused entry" ],
			[ [ 'ak-]' ], "Go to next page" ],
			[ [ 'ak-[' ], "Go to previous page" ],
			[ [ 'ak-\\' ], "Go to first page" ],
			[ [ 'ak-e' ], "Edit currently focused post" ],
			[ [ 'ak-z' ], "Switch post sort order" ]
		], [
			"Editor",
			[ [ 'ak-k' ], "Bold text" ],
			[ [ 'ak-i' ], "Italic text" ],
			[ [ 'ak-l' ], "Insert hyperlink" ],
			[ [ 'ak-q' ], "Blockquote text" ],
			[ [ 'ak-n' ], "Footnote" ]
		], [
			"Miscellaneous",
			[ [ 'ak-x' ], "Switch to next view on user page" ],
			[ [ 'ak-z' ], "Switch to previous view on user page" ],
			[ [ 'ak-`' ], "Toggle compact comment list view" ],
			[ [ 'ak-g' ], "Toggle anti-kibitzer mode" ]
		], [
			"Appearance",
			[ [ 'ak-=' ], "Increase text size" ],
			[ [ 'ak--' ], "Decrease text size" ],
			[ [ 'ak-0' ], "Reset to default text size" ],
			[ [ 'ak-\'' ], "Cycle through content width settings" ],
			[ [ 'ak-1' ], "Switch to default theme [A]" ],
			[ [ 'ak-2' ], "Switch to dark theme [B]" ],
			[ [ 'ak-3' ], "Switch to grey theme [C]" ],
			[ [ 'ak-4' ], "Switch to ultramodern theme [D]" ],
			[ [ 'ak-5' ], "Switch to simple theme [E]" ],
			[ [ 'ak-6' ], "Switch to brutalist theme [F]" ],
			[ [ 'ak-7' ], "Switch to ReadTheSequences theme [G]" ],
			[ [ 'ak-8' ], "Switch to classic Less Wrong theme [H]" ],
			[ [ 'ak-9' ], "Switch to modern Less Wrong theme [I]" ],
			[ [ 'ak-;' ], "Open theme tweaker" ],
			[ [ 'Enter' ], "Save changes and close theme tweaker"],
			[ [ 'Esc' ], "Close theme tweaker (without saving)" ]
		], [
			"Slide shows",
			[ [ 'ak-l' ], "Start/resume slideshow" ],
			[ [ 'Esc' ], "Exit slideshow" ],
			[ [ '&#x2192;', '&#x2193;' ], "Next slide" ],
			[ [ '&#x2190;', '&#x2191;' ], "Previous slide" ],
			[ [ 'Space' ], "Reset slide zoom" ]
		] ].map(section => 
		`<ul><li class='section'>${section[0]}</li>` + section.slice(1).map(entry =>
			`<li>
				<span class='keys'>` + 
				entry[0].map(key =>
					(key.hasPrefix("ak-")) ? `<code class='ak'>${key.substring(3)}</code>` : `<code>${key}</code>`
				).join("") + 
				`</span>
				<span class='action'>${entry[1]}</span>
			</li>`
		).join("\n") + `</ul>`).join("\n") + `
		</ul></div>
	</div>
</nav>`);

	// Add listener to show the keyboard help overlay.
	document.addEventListener("keypress", GW.keyboardHelpShowKeyPressed = (event) => {
		if (event.key != '?') return;

		GWLog("GW.keyboardHelpShowKeyPressed");

		toggleKeyboardHelpOverlay(true);
	});

	// Clicking the background overlay closes the keyboard help overlay.
	keyboardHelpOverlay.addActivateEvent(GW.keyboardHelpOverlayClicked = (event) => {
		GWLog("GW.keyboardHelpOverlayClicked");

		if (event.type == 'mousedown') {
			keyboardHelpOverlay.style.opacity = "0.01";
		} else {
			toggleKeyboardHelpOverlay(false);
			keyboardHelpOverlay.style.opacity = "1.0";
		}
	}, true);

	// Intercept clicks, so they don’t “fall through” the background overlay.
	Æ(query("#keyboard-help-overlay .keyboard-help-container")).addActivateEvent((event) => { event.stopPropagation(); }, true);

	// Clicking the close button closes the keyboard help overlay.
	keyboardHelpOverlay.query("button.close-keyboard-help").addActivateEvent(GW.closeKeyboardHelpButtonClicked = (event) => {
		GWLog("GW.closeKeyboardHelpButtonClicked");

		toggleKeyboardHelpOverlay(false);
	});

	// Add button to open keyboard help.
	query("#nav-item-about").insertAdjacentHTML("beforeend", "<button type='button' tabindex='-1' class='open-keyboard-help' title='Keyboard shortcuts'>&#xf11c;</button>");
	query("#nav-item-about button.open-keyboard-help").addActivateEvent(GW.openKeyboardHelpButtonClicked = (event) => {
		GWLog("GW.openKeyboardHelpButtonClicked");

		toggleKeyboardHelpOverlay(true);
		event.target.blur();
	});
}

function toggleKeyboardHelpOverlay(show) {
	GWLog("toggleKeyboardHelpOverlay");

	let keyboardHelpOverlay = query("#keyboard-help-overlay");
	show = (typeof show != "undefined") ? show : (getComputedStyle(keyboardHelpOverlay) == "hidden");
	keyboardHelpOverlay.style.visibility = show ? "visible" : "hidden";

	// Prevent scrolling the document when the overlay is visible.
	togglePageScrolling(!show);

	// Focus the close button as soon as we open.
	keyboardHelpOverlay.query("button.close-keyboard-help").focus();

	if (show) {
		// Add listener to show the keyboard help overlay.
		document.addEventListener("keyup", GW.keyboardHelpHideKeyPressed = (event) => {
			GWLog("GW.keyboardHelpHideKeyPressed");
			if (event.key == 'Escape')
				toggleKeyboardHelpOverlay(false);
		});
	} else {
		document.removeEventListener("keyup", GW.keyboardHelpHideKeyPressed);
	}

	// Disable / enable tab-selection of the search box.
	setSearchBoxTabSelectable(!show);
}

/**********/
/* POPUPS */
/**********/

function popupsSetup() {
	document.body.insertAdjacentHTML("beforeend", "<div id='popups-overlay-fixed'></div>");
	document.body.insertAdjacentHTML("beforeend", "<div id='popups-overlay-scrolling'></div>");
}

/******************/
/* INITIALIZATION */
/******************/

registerInitializer('earlyInitialize', true, () => (query("#content") != null), () => {
	GWLog("INITIALIZER earlyInitialize");
	// Check to see whether we’re on a mobile device (which we define as a touchscreen^W narrow viewport).
// 	GW.isMobile = /Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini/i.test(navigator.userAgent);
//	GW.isMobile = ('ontouchstart' in document.documentElement);
	GW.mediaQueries = {
		mobileNarrow: matchMedia("(max-width: 520px)"),
		mobileWide: matchMedia("(max-width: 900px)"),
		mobileMax: matchMedia("(max-width: 960px)"),
		hover: matchMedia("only screen and (hover: hover) and (pointer: fine)")
	};
	GW.isMobile = GW.mediaQueries.mobileMax.matches;
	GW.isFirefox = navigator.userAgent.toLowerCase().indexOf('firefox') > -1;

	// Backward compatibility.
	let storedTheme = localStorage.getItem('selected-theme');
	if (storedTheme) {
		setTheme(storedTheme);
		localStorage.removeItem('selected-theme');
	}

	// Animate width & theme adjustments?
	GW.adjustmentTransitions = false;

	// Add the content width selector.
	injectContentWidthSelector();
	// Add the text size adjustment widget.
	injectTextSizeAdjustmentUI();
	// Add the theme selector.
	injectThemeSelector();
	// Add the theme tweaker.
	injectThemeTweaker();
	// Add the quick-nav UI.
	injectQuickNavUI();

	// If client is logged in...
	if (loggedInUserId) {
		let luserJS = document.createElement("script");
		luserJS.src = GW.assetVersions['/js/luser.js'];
		luserJS.addEventListener('load', (event) => {
			GWLog("user.js loaded");
			luserJS.dataset.loaded = 1;

			// Check for notifications.
			updateInbox();

			// Do various things only relevant to logged-in users.
			registerInitializer('user-initialize', false, () => (document.readyState != 'loading'), userInitialize);
		});
		query("head").appendChild(luserJS);
	}
});

function userInitialize() {
	GWLog("INITIALIZER userInitialize");

	// On edit post pages and conversation pages, add GUIEdit buttons to the 
	// textarea, and markdownify the existing text, if any (this is needed if a 
	// post was last edited on LW).
	queryAll(".with-markdown-editor textarea").forEach(textarea => {
		textarea.addTextareaFeatures();
		textarea.value = MarkdownFromHTML(textarea.value);
	});

	// Set the “submit” button on the edit post page to something more helpful.
	setEditPostPageSubmitButtonText();

	// Add upvote/downvote buttons.
	if (typeof postVote != 'undefined') {
		queryAll(".post-meta .karma-value").forEach(karmaValue => {
			addVoteButtons(karmaValue, postVote, 'Posts');
			karmaValue.parentElement.addClass("active-controls");
		});
	}
	if (typeof commentVotes != 'undefined') {
		queryAll(".comment-meta .karma-value").forEach(karmaValue => {
			let commentID = karmaValue.getCommentId();
			addVoteButtons(karmaValue, commentVotes[commentID], 'Comments');
			karmaValue.parentElement.addClass("active-controls");
		});
	}

	// Activate the vote buttons.
	queryAll("button.vote").forEach(voteButton => {
		voteButton.addActivateEvent(voteButtonClicked);
	});

	// For all comment containers...
	queryAll(".comments").forEach((commentsContainer) => {
		// Add reply buttons.
		commentsContainer.queryAll(".comment").forEach(comment => {
			comment.insertAdjacentHTML("afterend", "<div class='comment-controls posting-controls'></div>");
			comment.parentElement.query(".comment-controls").constructCommentControls();
		});

		// Add top-level new comment form.
		if (!query(".individual-thread-page")) {
			commentsContainer.insertAdjacentHTML("afterbegin", "<div class='comment-controls posting-controls'></div>");
			commentsContainer.query(".comment-controls").constructCommentControls();
		}
	});

	// Hash realignment is needed because adding the above elements almost
	// certainly caused the page to reflow, and now client is no longer
	// scrolled to the place indicated by the hash.
	GW.needHashRealignment = true;
}

registerInitializer('initialize', false, () => (document.readyState != 'loading'), () => {
	GWLog("INITIALIZER initialize");
	forceInitializer('earlyInitialize');

	// Create scratchpad for copy operations.
	addUIElement("<textarea id='scratchpad'></textarea>");

	// This is for “qualified hyperlinking”, i.e. “link without comments” and/or
	// “link without nav bars”.
	if (getQueryVariable("comments") == "false")
		query("#content").addClass("no-comments");
	if (getQueryVariable("hide-nav-bars") == "true") {
		query("#content").addClass("no-nav-bars");
		let auxAboutLink = addUIElement("<div id='aux-about-link'><a href='/about' accesskey='t' target='_new'>&#xf129;</a></div>");
	}

	// If the page cannot have comments, remove the accesskey from the #comments
	// quick-nav button; and if the page can have comments, but does not, simply 
	// disable the #comments quick nav button. (Ditto for answers.)
	let content = query("#content");
	[ "comments", "answers" ].forEach(section => {
		if (content.query(`#${section}`) == null) {
			query(`#quick-nav-ui a[href='#${section}']`).accessKey = '';
		} else if (content.query(`#${section} .comment-thread`) == null) {
			query(`#quick-nav-ui a[href='#${section}']`).addClass(`no-${section}`);
		}
	});
	// Set up accesskey sharing between questions and comments, if need be.
	if (query("#answers")) {
		queryAll("#quick-nav-ui a[href='#comments'], #quick-nav-ui a[href='#answers']").forEach(button => {
			button.addActivateEvent(GW.quickNavCommentsOrQuestionsButtonClicked = (event) => {
				GWLog("GW.quickNavCommentsOrQuestionsButtonClicked");

				event.target.accessKey = '';
				let otherSection = (event.target.hash == "#comments") ? "#answers" : "#comments";
				query(`#quick-nav-ui a[href='${otherSection}']`).accessKey = '/';
			});
		});
	}

	// Links to comments generated by LW have a hash that consists of just the 
	// comment ID, which can start with a number. Prefix it with “comment-”.
	if (location.hash.length == 18) {
		location.hash = "#comment-" + location.hash.substring(1);
	}

	// If the viewport is wide enough to fit the desktop-size content column,
	// use a long date format; otherwise, a short one.
	let useLongDate = (GW.mediaQueries.mobileWide.matches == false);
	let dtf = new Intl.DateTimeFormat([], 
		( useLongDate ? 
			{ month: 'short', day: 'numeric', year: 'numeric', hour: 'numeric', minute: 'numeric' }
				: { month: 'numeric', day: 'numeric', year: '2-digit', hour: 'numeric', minute: 'numeric' } ));
	queryAll(".date").forEach(date => {
		let d = date.dataset.jsDate;
		if (d) { date.innerHTML = dtf.format(new Date(+ d)); }
	});

	GW.needHashRealignment = false;

	// If this is a post page...
	let postMeta = query(".post .post-meta");
	if (postMeta) {
		// Add “qualified hyperlinking” toolbar.
		let postPermalink = location.protocol + "//" + location.host + location.pathname;
		postMeta.insertAdjacentHTML("beforeend", `<div class='qualified-linking'>
	<input type='checkbox' tabindex='-1' id='qualified-linking-toolbar-toggle-checkbox'><label for='qualified-linking-toolbar-toggle-checkbox'><span>&#xf141;</span></label>
	<div class='qualified-linking-toolbar'>
	<a href='${postPermalink}'>Post permalink</a>
	<button type='button' tabindex='-1' class='copy-link permalink' title='Copy post permalink to clipboard'>&#xf0c5;</button>
	<a href='${postPermalink}?comments=false'>Link without comments</a>
	<button type='button' tabindex='-1' class='copy-link no-comments' title='Copy link without comments to clipboard'>&#xf0c5;</button>
	<a href='${postPermalink}?hide-nav-bars=true'>Link without top nav bars</a>
	<button type='button' tabindex='-1' class='copy-link hide-nav-bars' title='Copy link without top nav bars to clipboard'>&#xf0c5;</button>
	<a href='${postPermalink}?comments=false&hide-nav-bars=true'>Link without comments or top nav bars</a>
	<button type='button' tabindex='-1' class='copy-link no-comments hide-nav-bars' title='Copy link without comments or top nav bars to clipboard'>&#xf0c5;</button>
	</div>
</div>`);

		// Activate copy-to-clipboard icons in qualified linking toolbar.
		queryAll(".qualified-linking button.copy-link").forEach(button => {
			button.addActivateEvent(GW.copyQualifiedLinkButtonClicked = (event) => {
				GWLog("GW.copyQualifiedLinkButtonClicked");

				copyTextToClipboard(event.target.previousElementSibling.href);
			});
		});

		// Replicate .post-meta at bottom of post.
		let clonedPostMeta = postMeta.cloneNode(true);
		postMeta.addClass("top-post-meta");
		clonedPostMeta.addClass("bottom-post-meta");
		clonedPostMeta.query("input[type='checkbox']").id += "-bottom";
		clonedPostMeta.query("label").htmlFor += "-bottom";
		query(".post").appendChild(clonedPostMeta);
	}

	// Clean up LW2 link in .post-meta.
	updateLW2Link();

	// Clean up ToC.
	queryAll(".contents-list li a").forEach(tocLink => {
		tocLink.innerText = tocLink.innerText.replace(/^[0-9]+\. /, '');
		tocLink.innerText = tocLink.innerText.replace(/^[0-9]+: /, '');
		tocLink.innerText = tocLink.innerText.replace(/^M{0,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})\. /i, '');
		tocLink.innerText = tocLink.innerText.replace(/^[A-Z]\. /, '');
	});

	// Rectify mathjax block container heights.
	queryAll(".mathjax-block-container .mjx-chtml").forEach(mjxContainer => {
		mjxContainer.style.height = mjxContainer.offsetHeight + "px";
	});

	// Mark answers as such.
	queryAll("#answers > .comment-thread > .comment-item").forEach(answerItem => {
		answerItem.addClass("answer-item");
	});

	// If we’re on a comment thread page...
	if (query(".comments") && !query("#content").hasClass("individual-thread-page")) {
		// Add comment-minimize buttons to every comment.
		queryAll(".comment-meta").forEach(commentMeta => {
			if (!commentMeta.lastChild.hasClass("comment-minimize-button"))
				commentMeta.insertAdjacentHTML("beforeend", "<div class='comment-minimize-button maximized'>&#xf146;</div>");
		});

		// Format and activate comment-minimize buttons.
		queryAll(".comment-minimize-button").forEach(button => {
			button.closest(".comment-item").setCommentThreadMaximized(false);

			button.addActivateEvent(GW.commentMinimizeButtonClicked = (event) => {
				GWLog("GW.commentMinimizeButtonClicked");

				event.target.closest(".comment-item").setCommentThreadMaximized(true);
			});
		});

		if (getQueryVariable("chrono") == "t") {
			query("head").insertAdjacentHTML("beforeend", "<style>.comment-minimize-button::after { display: none; }</style>");
		}
	}

	if (query(".comment-thread")) {
		// On mobile, wrap authors to limit tappable area.
		doWhenMatchMedia(GW.mediaQueries.mobileNarrow, "wrapAuthorsInCommentListings", () => {
			queryAll(".comment-meta > .author").forEach(author => {
				author.outerHTML = `<span class='author-wrapper'>${author.outerHTML}</span>`;
			});
		}, () => {
			queryAll(".author-wrapper").forEach(wrapper => {
				wrapper.outerHTML = wrapper.innerHTML;
			});
		});
	}

	// Scroll down to a comment, if the URL points to one.
	let urlParts = document.URL.split('#comment-');
	if (urlParts.length > 1) {
		expandAncestorsOf(urlParts[1]);
		GW.needHashRealignment = true;
	}

	/*	On mobile, replace the labels for the checkboxes on the edit post form with icons, to save space.
		*/
	if (query(".edit-post-page")) {
		doWhenMatchMedia(GW.mediaQueries.mobileNarrow, "editPostFormCheckboxLabels", (mediaQuery) => {
			query("label[for='link-post']").innerHTML = mediaQuery.matches ? "&#xf0c1" : "Link post";
			query("label[for='question']").innerHTML = mediaQuery.matches ? "&#xf128" : "Question post";
		});
	}

	// Widgets should not stay focused when clicked.
	if (query(".edit-post-page")) {
		queryAll("input[type='checkbox'] + label, input[type='radio'] + label").forEach(widget => {
			widget.addEventListener("mouseup", (event) => {
				setTimeout(() => { event.target.previousElementSibling.blur(); });
			});
		});
	}

	/*	On desktop, replace the “Sequences” tab name with the book icon.
		*/
	doWhenMatchMedia(GW.mediaQueries.mobileWide, "setSequencesNavTabText", (mediaQuery) => {
		query("#nav-item-sequences .nav-inner").innerHTML = mediaQuery.matches ? "Sequences" : "&#xf5db;";
	});

	// Add error message (as placeholder) if user tries to click Search with
	// an empty search field.
	query("#nav-item-search form").addEventListener("submit", GW.siteSearchFormSubmitted = (event) => {
		let searchField = event.target.query("input");
		if (searchField.value == "") {
			event.preventDefault();
			event.target.blur();
			searchField.placeholder = "Enter a search string!";
			searchField.focus();
		}
	});
	// Remove the placeholder / error on any input.
	query("#nav-item-search input").addEventListener("input", GW.siteSearchFieldValueChanged = (event) => {
		event.target.placeholder = "";
	});

	// Prevent conflict between various single-hotkey listeners and text fields.
	queryAll("input[type='text'], input[type='search'], input[type='password']").forEach(inputField => {
		inputField.addEventListener("keyup", (event) => { event.stopPropagation(); });
		inputField.addEventListener("keypress", (event) => { event.stopPropagation(); });
	});

	if (content.hasClass("post-page")) {
		// Read and update last-visited-date.
		let lastVisitedDate = getLastVisitedDate();
		setLastVisitedDate(Date.now());

		// Save the number of comments this post has when it's visited.
		updateSavedCommentCount();

		if (content.query(".comments .comment-thread") != null) {
			// Add the new comments count & navigator.
			injectNewCommentNavUI();

			// Get the highlight-new-since date (as specified by URL parameter, if 
			// present, or otherwise the date of the last visit).
			let hnsDate = parseInt(getQueryVariable("hns")) || lastVisitedDate;

			// Highlight new comments since the specified date.			 
			let newCommentsCount = highlightCommentsSince(hnsDate);

			// Update the comment count display.
			updateNewCommentNavUI(newCommentsCount, hnsDate);
		}
	} else {
		// On listing pages, make comment counts more informative.
		badgePostsWithNewComments();
	}

	// Add the comments list mode selector widget (expanded vs. compact).
	injectCommentsListModeSelector();

	// Add the comments view selector widget (threaded vs. chrono).
// 	injectCommentsViewModeSelector();

	// Add the comments sort mode selector (top, hot, new, old).
	injectCommentsSortModeSelector();

	// Add the toggle for the post nav UI elements on mobile layouts.
	injectPostNavUIToggle();

	// Add the toggle for the appearance adjustment UI elements on mobile layouts.
	injectAppearanceAdjustUIToggle();

	// Add the antikibitzer.
	injectAntiKibitzer();

	// Add comment parent popups.
	addCommentParentPopups();

	// Mark original poster’s comments with a special class.
	markOriginalPosterComments();

	// On the All view, mark posts with non-positive karma with a special class.
	if (query("#content").hasClass("all-index-page")) {
		queryAll("#content.index-page h1.listing + .post-meta .karma-value").forEach(karmaValue => {
			if (parseInt(karmaValue.textContent.replace("−", "-")) > 0) return;

			karmaValue.closest(".post-meta").previousSibling.addClass("spam");
		});
	}

	// Compute the text of the pagination UI tooltip text.
	var currentPage = query("#top-nav-bar .page-number");
	if (currentPage) {
		currentPage = parseInt(currentPage.lastChild.textContent);
		queryAll("#top-nav-bar a.nav-item-next, #bottom-bar #nav-item-next a").forEach(nextPageLink => {
			nextPageLink.dataset.targetPage = currentPage + 1;
		});
		queryAll("#top-nav-bar a.nav-item-prev, #bottom-bar #nav-item-prev a").forEach(prevPageLink => {
			prevPageLink.dataset.targetPage = currentPage - 1;
		});
		queryAll("#top-nav-bar a.nav-item-first, #bottom-bar #nav-item-first a").forEach(prevPageLink => {
			prevPageLink.dataset.targetPage = 1;
		});
	}

	// On the comment retry form, add reassurance message.
	let retryForm = query(".error-retry-form");
	if (retryForm) {
		let savedCommentContent = retryForm.query("input[name='text']").value;
		query(".gw-error").insertAdjacentHTML("beforeend", `<div class='reassurance'>
	<p class='message'><strong>Your comment could not be posted, but it was <em>not</em> lost!</strong></p>
	<p class='message'>Click the “<strong>Retry</strong>” button below to try posting it again.</p>
	<p class='saved-comment-content body-text'>${savedCommentContent}</p>
</div>`);
	}

	// Add event listener for . , ; (for navigating listings pages).
	let listings = queryAll("h1.listing a[href^='/posts/'], h1.listing a[href^='/s/'], .listings .comment-thread .comment-meta .date");
	if (listings.length > 0) {
		document.addEventListener("keyup", GW.postListingsNavKeyPressed = (event) => { 
			if (event.ctrlKey || event.shiftKey || event.altKey || !(event.key == "," || event.key == "." || event.key == ';' || event.key == "Escape")) return;

			if (event.key == "Escape") {
				if (document.activeElement.parentElement.hasClass("listing") || 
					document.activeElement.closest(".expanded"))
					document.activeElement.blur();
				return;
			}

			if (event.key == ';') {
				if (document.activeElement.parentElement.hasClass("link-post-listing")) {
					let links = document.activeElement.parentElement.queryAll("a");
					links[document.activeElement == links[0] ? 1 : 0].focus();
				} else if (document.activeElement.parentElement.hasClass("comment-meta")) {
					let links = document.activeElement.parentElement.queryAll("a.date, a.permalink");
					links[document.activeElement == links[0] ? 1 : 0].focus();
					document.activeElement.closest(".comment-item").addClasses([ "focused", "expanded" ]);
				}
				return;
			}

			var indexOfActiveListing = -1;
			for (i = 0; i < listings.length; i++) {
				let parentOfFocusedElement = document.activeElement.parentElement;
				if (parentOfFocusedElement.hasClass("listing") && 
					listings[i] === parentOfFocusedElement.query("a[href^='/posts/'], a[href^='/s/']")) {
					indexOfActiveListing = i;
					break;
				} else if (parentOfFocusedElement.hasClass("comment-meta") && 
					listings[i] === parentOfFocusedElement.query("a.date")) {
					indexOfActiveListing = i;
					break;
				}
			}

			// Remove edit accesskey from currently highlighted post by active user, if applicable.
			if (indexOfActiveListing > -1) {
				delete (listings[indexOfActiveListing].parentElement.query(".edit-post-link")||{}).accessKey;
			} else {
				let range = getCurrentVisibleListingsRange();
				indexOfActiveListing = (event.key == "." ? range[0] - 1 : range[0] + range[1]);
			}

			let indexOfNextListing = (event.key == "." ? ++indexOfActiveListing % listings.length : (--indexOfActiveListing + listings.length)) % listings.length;
			listings[indexOfNextListing].focus();

			if (listings[indexOfNextListing].closest(".comment-item")) {
				listings[indexOfNextListing].closest(".comment-item").addClasses([ "expanded", "focused" ]);
				listings[indexOfNextListing].closest(".comment-item").scrollIntoViewIfNeeded();
			}

			// Add edit accesskey to newly highlighted post by active user, if applicable.
			if (listings[indexOfActiveListing] != null)
				(listings[indexOfActiveListing].parentElement.query(".edit-post-link")||{}).accessKey = 'e';
		});
		queryAll(".listings .comment-thread .comment-meta a.date, .listings .comment-thread .comment-meta a.permalink").forEach(link => {
			link.addEventListener("blur", GW.commentListingsHyperlinkUnfocused = (event) => {
				event.target.closest(".comment-item").removeClasses([ "expanded", "focused" ]);
			});
		});
	}
	// Add event listener for ; (to focus the link on link posts).
	if (query("#content").hasClass("post-page") && 
		query(".post").hasClass("link-post")) {
		document.addEventListener("keyup", GW.linkPostLinkFocusKeyPressed = (event) => {
			if (!(event.key == ';' || event.key == 'Escape')) return;
			GWLog("GW.linkPostLinkFocusKeyPressed");

			let linkPostLink = query("a.link-post-link");

			if (document.activeElement === linkPostLink) linkPostLink.blur();
			else if (event.key == ';') linkPostLink.focus();
		});
	}

	// Add accesskeys to user page view selector.
	let viewSelector = query("#content.user-page > .sublevel-nav");
	if (viewSelector) {
		let currentView = viewSelector.query("span");
		(currentView.nextSibling || viewSelector.firstChild).accessKey = 'x';
		(currentView.previousSibling || viewSelector.lastChild).accessKey = 'z';
	}

	// Add accesskey to index page sort selector.
	(query("#content.index-page .sublevel-nav.sort a")||{}).accessKey = 'z';

	// Move MathJax style tags to <head>.
	var aggregatedStyles = "";
	queryAll("#content style").forEach(styleTag => {
		aggregatedStyles += styleTag.innerHTML;
		removeElement("style", styleTag.parentElement);
	});
	if (aggregatedStyles != "") {
		query("head").insertAdjacentHTML("beforeend", "<style id='mathjax-styles'>" + aggregatedStyles + "</style>");
	}

	// Add listeners to switch between word count and read time.
	if (localStorage.getItem("display-word-count")) toggleReadTimeOrWordCount(true);
	queryAll(".post-meta .read-time").forEach(element => {
		element.addActivateEvent(GW.readTimeOrWordCountClicked = (event) => {
			let displayWordCount = localStorage.getItem("display-word-count");
			toggleReadTimeOrWordCount(!displayWordCount);
			if (displayWordCount) localStorage.removeItem("display-word-count");
			else localStorage.setItem("display-word-count", true);
		});
	});

	// Add copy listener to strip soft hyphens (inserted by server-side hyphenator).
	query("#content").addEventListener("copy", GW.textCopied = (event) => {
		event.preventDefault();
		const selectedHTML = getSelectionHTML();
		const selectedText = getSelection().toString();
		event.clipboardData.setData("text/plain", selectedText.replace(/\u00AD|\u200b/g, ""));
		event.clipboardData.setData("text/html", selectedHTML.replace(/\u00AD|\u200b/g, ""));
	});

	// Fix markup issues.
	rectifyMarkup();

	// Set up Image Focus feature, if there are images.
	if (query("#content img")) {
		GW.contentContainsImages = true;

		let imageFocusJS = document.createElement("script");
		imageFocusJS.src = GW.assetVersions['/js/image-focus.js'];
		imageFocusJS.addEventListener('load', (event) => {
			GWLog("image-focus.js loaded");

			imageFocusSetup();

			registerInitializer('pageLayoutFinishedProcessImages', false, () => (document.readyState == "complete"), () => {
				GWLog("INITIALIZER pageLayoutFinishedProcessImages");

				// Construct the images overlay (which also calls imageFocusSetup() again).
				generateImagesOverlay();

				// If the URL hash specifies an image to focus, focus it.
				focusImageSpecifiedByURL();
			});
		});
		query("head").appendChild(imageFocusJS);
	}

	// Set up keyboard shortcuts guide overlay.
	keyboardHelpSetup();

	// Set up popups overlay.
// 	popupsSetup();
});

/************************/
/* MARKUP RECTIFICATION */
/************************/

/*	This function fixes bad markup generated by Less Wrong.
	*/
function rectifyMarkup() {
	GWLog("rectifyMarkup");

	let content = query("#content");

	// Unwrap <pre> blocks from extraneous divs.
	content.queryAll(".body-text div > pre").forEach(pre => {
		pre.parentElement.outerHTML = pre.parentElement.innerHTML;
	});

	// Convert bold paragraphs into headings.
	// NOTE: This will not result in a ToC (as that is generated server-side).
	content.queryAll(".body-text p > strong:only-child, .body-text p > b:only-child").forEach(strong => {
		if (strong === strong.parentElement.firstChild &&
			strong === strong.parentElement.lastChild)
			strong.parentElement.outerHTML = `<h3>${strong.innerHTML}</h3>`;
	});

	// Remove extraneous rules.
	content.queryAll(".body-text h1 + hr, .body-text h2 + hr, .body-text h3 + hr").forEach(rule => {
		removeElement(rule);
	});

	// Remove extraneous breaks.
	content.queryAll(".body-text > br").forEach(br => {
		removeElement(br);
	});

	// Remove empty lists.
	content.queryAll(".body-text ul > li:only-child").forEach(listItem => {
		if (!/\S/.test(listItem.innerHTML)) removeElement(listItem.parentElement);
	});
}

/*************************/
/* POST-LOAD ADJUSTMENTS */
/*************************/

registerInitializer('pageLayoutFinished', false, () => (document.readyState == "complete"), () => {
	GWLog("INITIALIZER pageLayoutFinished");

	forceInitializer('initialize');

	realignHashIfNeeded();

	postSetThemeHousekeeping();

	// Adjust state of text input fields.
	registerInitializer('adjustInputFieldsState', true, () => (Æ(query("script[src*='user.js']")).dataset.loaded), () => {
		doWhenMatchMedia(GW.mediaQueries.mobileNarrow, "editorInputFields", () => {
			queryAll("#content textarea").forEach(textarea => {
				textarea.blur();
				expandTextarea(textarea);
			});
		}, () => {
			queryAll(".with-markdown-editor textarea").forEach(textarea => {
				expandTextarea(textarea);
			});

			// Focus appropriate input field.
			let appropriateInputField = (getQueryVariable("post-id") ? "#edit-post-form textarea" : "#edit-post-form input[name='title']") + 
										", .conversation-page textarea";
			queryAll(appropriateInputField).forEach(field => {
				field.focus();
			});
		});
	});
});

function adjustUIForWindowSize() {
	GWLog("adjustUIForWindowSize");

	var bottomBarOffset;

	// Adjust bottom bar state.
	let bottomBar = query("#bottom-bar");
	bottomBarOffset = bottomBar.hasClass("decorative") ? 16 : 30;
	if (query("#content").clientHeight > window.innerHeight + bottomBarOffset) {
		bottomBar.removeClass("decorative");

		bottomBar.query("#nav-item-top").style.display = "";
	} else if (bottomBar) {
		if (bottomBar.childElementCount > 1) bottomBar.removeClass("decorative");
		else bottomBar.addClass("decorative");

		bottomBar.query("#nav-item-top").style.display = "none";
	}

	// Show quick-nav UI up/down buttons if content is taller than window.
	bottomBarOffset = bottomBar.hasClass("decorative") ? 16 : 30;
	queryAll("#quick-nav-ui a[href='#top'], #quick-nav-ui a[href='#bottom-bar']").forEach(element => {
		element.style.visibility = (query("#content").clientHeight > window.innerHeight + bottomBarOffset) ? "" : "hidden";
	});

	// Move anti-kibitzer toggle if content is very short.
	if (query("#content").clientHeight < 400) (query("#anti-kibitzer-toggle")||{}).style.bottom = "125px";

	// Update the visibility of the post nav UI.
	updatePostNavUIVisibility();
}

function recomputeUIElementsContainerHeight(force = false) {
	GWLog("recomputeUIElementsContainerHeight");

	if (force || query("#ui-elements-container").style.height != "") {
		/*	At viewport width where we show the mobile versions of the fixed UI 
			elements, #ui-elements-container is static, not fixed; and its 
			children are fixed, not absolute. This is done in order to prevent 
			position jitter during scrolling on mobile devices.
			*/
		if (GW.mediaQueries.mobileMax.matches) {
			query("#ui-elements-container").style.height = "";
		} else {
			let bottomBarOffset = query("#bottom-bar").hasClass("decorative") ? 16 : 30;
			query("#ui-elements-container").style.height = (query("#content").clientHeight <= window.innerHeight + bottomBarOffset) ? 
															query("#content").clientHeight + "px" :
															"";
		}
	}
}

function updateLW2Link() {
	queryAll(".post .post-meta .lw2-link").forEach(lw2Link => {
		lw2Link.innerHTML = "";
	});
}

function realignHashIfNeeded() {
	if (GW.needHashRealignment)
		realignHash();
}
function realignHash() {
	GWLog("realignHash");

	if (!location.hash) return;

	let targetElement = query(location.hash);
	if (targetElement) targetElement.scrollIntoView(true);
	GW.needHashRealignment = false;
}
