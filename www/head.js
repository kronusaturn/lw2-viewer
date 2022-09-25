
// FOR TESTING ONLY, COMMENT WHEN DEPLOYING.
// GW.loggingEnabled = true;

// Links to comments generated by LW have a hash that consists of just the 
// comment ID, which can start with a number. Prefix it with "comment-".
if (location.hash.length == 18) {
	location.hash = "#comment-" + location.hash.substring(1);
}

/****************************************************/
/* CSS CLASS MANIPULATION (polyfill for .classList) */
/****************************************************/

Element.prototype.addClass = function(className) {
	if (!this.hasClass(className))
		this.className = (this.className + " " + className).trim();
}
Element.prototype.addClasses = function(classNames) {
	let elementClassNames = this.className.trim().split(/\s/);
	
	classNames.forEach(className => {
		if (!this.hasClass(className))
			elementClassNames.push(className);
	});
	
	this.className = elementClassNames.join(" ");
}
Element.prototype.removeClass = function(className) {
	this.className = this.className.replace(new RegExp("(^|\\s+)" + className + "(\\s+|$)"), "$1").trim();
	if (this.className == "") this.removeAttribute("class");
}
Element.prototype.removeClasses = function(classNames) {
	classNames.forEach(className => {
		this.className = this.className.replace(new RegExp("(^|\\s+)" + className + "(\\s+|$)"), "$1").trim();
	});
	if (this.className == "") this.removeAttribute("class");
}
Element.prototype.hasClass = function(className) {
	return (new RegExp("(^|\\s+)" + className + "(\\s+|$)")).test(this.className);
}
/*	True if the element has _all_ of the classes in the argument (which may be
	a space-separated string or an array); false otherwise.
 */
Element.prototype.hasClasses = function (classes) {
	if (typeof classes == "string")
		classes = classes.split(" ");

	for (let aClass of classes)
		if (false == this.hasClass(aClass))
			return false;

	return true;
};
Element.prototype.toggleClass = function(className) {
	if (this.hasClass(className))
		this.removeClass(className);
	else
		this.addClass(className);
}

/*  Swap classes on the given element.

    First argument is an array with two string elements (the classes).
    Second argument is 0 or 1 (index of class to add; the other is removed).

    Note that the first class in the array is always removed/added first, and 
    then the second class in the array is added/removed; thus these two calls 
    have different effects:
    
    anElement.swapClasses([ "foo", "bar" ], 1);
    anElement.swapClasses([ "bar", "foo" ], 0);

	The first call removes "foo" and then adds "bar"; the second call adds "bar"
	and then removes "foo". (This can have different visual or other side 
	effects in many circumstances. It also results in a different end state in 
	the cases where the two classes are the same.)
 */
Element.prototype.swapClasses = function (classes, whichToAdd) {
	let op1 = whichToAdd ? "removeClass" : "addClass";
	let op2 = whichToAdd ? "addClass" : "removeClass";
	
	this[op1](classes[0]);
	this[op2](classes[1]);
};

/* DOM helpers */

function insertHeadHTML(html) {
	document.head.insertAdjacentHTML("beforeend", html.replace(/\s+/, " "));
}

/********************/
/* QUERYING THE DOM */
/********************/

function queryAll(selector, context) {
    context = context || document;
    // Redirect simple selectors to the more performant function
    if (/^(#?[\w-]+|\.[\w-.]+)$/.test(selector)) {
        switch (selector.charAt(0)) {
            case '#':
                // Handle ID-based selectors
                let element = document.getElementById(selector.substr(1));
                return element ? [ element ] : [ ];
            case '.':
                // Handle class-based selectors
                // Query by multiple classes by converting the selector 
                // string into single spaced class names
                var classes = selector.substr(1).replace(/\./g, ' ');
                return [].slice.call(context.getElementsByClassName(classes));
            default:
                // Handle tag-based selectors
                return [].slice.call(context.getElementsByTagName(selector));
        }
    }
    // Default to `querySelectorAll`
    return [].slice.call(context.querySelectorAll(selector));
}
function query(selector, context) {
	let all = queryAll(selector, context);
	return (all.length > 0) ? all[0] : null;
}
Object.prototype.queryAll = function (selector) {
	return queryAll(selector, this);
}
Object.prototype.query = function (selector) {
	return query(selector, this);
}

/********/
/* MISC */
/********/

Object.prototype.isEmpty = function () {
    for (var prop in this) if (this.hasOwnProperty(prop)) return false;
    return true;
};
Object.prototype.keys = function () {
	return Object.keys(this);
}
Array.prototype.contains = function (element) {
	return (this.indexOf(element) !== -1);
}
Array.prototype.clone = function() {
	return JSON.parse(JSON.stringify(this));
};

/********************************************************************/
/*	Reads the value of named cookie.
	Returns the cookie as a string, or null if no such cookie exists.
 */
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

/***************************************************************************/
/*	Create and return a new element with the specified tag name, attributes,
	and object properties.
 */
function newElement(tagName, attributes = { }, properties = { }) {
	let element = document.createElement(tagName);
	for (const attrName in attributes)
		if (attributes.hasOwnProperty(attrName))
			element.setAttribute(attrName, attributes[attrName]);
	for (const propName in properties)
		if (properties.hasOwnProperty(propName))
			element[propName] = properties[propName];
	return element;
}

/****************************/
/* APPEARANCE CUSTOMIZATION */
/****************************/

Appearance = {
	/*****************/
	/*	Configuration.
	 */

	defaultTheme: "default",
	defaultFilters: { },
	defaultWidth: "normal",
	defaultTextZoom: 1.0,

	minTextZoom: 0.5,
	maxTextZoom: 1.5,

	widthOptions: [
		[ "normal",	"Narrow (fixed-width) content column",	"N" ],
		[ "wide",	"Wide (fixed-width) content column",	"W" ],
		[ "fluid",	"Full-width (fluid) content column",	"F" ]
	],

	textSizeAdjustTargetElementsSelector: [
		".post", 
		".comment", 
		".posting-controls", 
		".sample-text"
	].join(", "),

	themeOptions: [
		[ "default",		"Default theme (dark text on light background)",	"A" ],
		[ "dark",			"Dark theme (light text on dark background)",		"B" ],
		[ "grey",			"Grey theme (more subdued than default theme)",		"C" ],
		[ "ultramodern",	"Ultramodern theme (very hip)",						"D" ],
		[ "zero",			"Theme zero (plain and simple)",					"E" ],
		[ "brutalist",		"Brutalist theme (the Motherland calls!)",			"F" ],
		[ "rts",			"ReadTheSequences.com theme",						"G" ],
		[ "classic",		"Classic Less Wrong theme",							"H" ],
		[ "less",			"Less theme (serenity now)",						"I" ]
	],

	defaultFiltersExclusionTree: [ "#content", [ ] ],

	defaultThemeTweakerClippyState: true,

	defaultAppearanceAdjustUIToggleState: false,

	themeLessAppearanceAdjustUIElementsSelector: [
		"#comments-view-mode-selector", 
		"#theme-selector", 
		"#dark-mode-selector",
		"#width-selector", 
		"#text-size-adjustment-ui", 
		"#theme-tweaker-toggle", 
		"#appearance-adjust-ui-toggle button"
	].join(", "),

	/******************/
	/*	Infrastructure.
	 */

	currentTheme: null,
	currentFilters: null,
	currentWidth: null,
	currentTextZoom: null,

	filtersExclusionPaths: { },

	themeTweakStyleBlock: null,
	textZoomStyleBlock: null,

	/*****************/
	/*	Functionality.
	 */

	/*	Themes.
	 */

	getSavedTheme: () => {
		return (readCookie("theme") || Appearance.defaultTheme);
	},

	/*	Filters (theme tweaks).
	 */

	getSavedFilters: () => {
		return (JSON.parse(localStorage.getItem("theme-tweaks")) || Appearance.defaultFilters);
	},

	saveCurrentFilters: () => {
		GWLog("Appearance.saveCurrentFilters");

		if (Appearance.currentFilters == { })
			localStorage.removeItem("theme-tweaks");
		else
			localStorage.setItem("theme-tweaks", JSON.stringify(Appearance.currentFilters));
	},

	applyFilters: (filters) => {
		GWLog("Appearance.applyFilters");

		if (typeof filters == "undefined")
			filters = Appearance.currentFilters;

		let fullStyleString = "";
		if (!filters.isEmpty()) {
			let filtersExclusionTree = (   Appearance.exclusionTreeFromExclusionPaths(Appearance.filtersExclusionPaths) 
										|| Appearance.defaultFiltersExclusionTree);
			fullStyleString = `body::before { content: ""; } body > #content::before { z-index: 0; }`
							+ ` ${Appearance.selectorFromExclusionTree(filtersExclusionTree)}`
							+ ` { filter: ${Appearance.filterStringFromFilters(filters)}; }`;
		}
	
		//	Update the style tag.
		Appearance.themeTweakStyleBlock.innerHTML = fullStyleString;

		//	Update the current filters.
		Appearance.currentFilters = filters;
	},

	exclusionTreeFromExclusionPaths: (paths) => {
		if (!paths)
			return null;

		let tree = Appearance.defaultFiltersExclusionTree.clone();
		paths.keys().flatMap(key => paths[key]).forEach(path => {
			var currentNodeInTree = tree;
			path.split(" ").slice(1).forEach(step => {
				if (currentNodeInTree[1] == null)
					currentNodeInTree[1] = [ ];

				var indexOfMatchingChild = currentNodeInTree[1].findIndex(child => { return child[0] == step; });
				if (indexOfMatchingChild == -1) {
					currentNodeInTree[1].push([ step, [ ] ]);
					indexOfMatchingChild = currentNodeInTree[1].length - 1;
				}

				currentNodeInTree = currentNodeInTree[1][indexOfMatchingChild];
			});
		});

		return tree;
	},

	selectorFromExclusionTree: (tree) => {
		let selectorParts = [
			"body::before, #ui-elements-container > div:not(#theme-tweaker-ui), #theme-tweaker-ui #theme-tweak-section-sample-text .sample-text-container"
		];
	
		function selectorFromExclusionTreeNode(node, path = [ ]) {
			let [ value, children ] = node;

			let newPath = path.clone();
			newPath.push(value);

			if (!children) {
				return value;
			} else if (children.length == 0) {
				return `${newPath.join(" > ")} > *, ${newPath.join(" > ")}::before, ${newPath.join(" > ")}::after`;
			} else {
				return `${newPath.join(" > ")} > *:not(${children.map(child => child[0]).join("):not(")}), ${newPath.join(" > ")}::before, ${newPath.join(" > ")}::after, ` + children.map(child => selectorFromExclusionTreeNode(child, newPath)).join(", ");
			}
		}
	
		return selectorParts + ", " + selectorFromExclusionTreeNode(tree);
	},

	filterStringFromFilters: (filters) => {
		let filterString = "";
		for (key of Object.keys(filters)) {
			let value = filters[key];
			filterString += ` ${key}(${value})`;
		}
		return filterString;
	},

	/*	Content column width.
	 */

	getSavedWidth: () => {
		return (localStorage.getItem("selected-width") || Appearance.defaultWidth);
	},

	saveCurrentWidth: () => {
		GWLog("Appearance.saveCurrentWidth");

		if (Appearance.currentWidth == "normal")
			localStorage.removeItem("selected-width");
		else
			localStorage.setItem("selected-width", Appearance.currentWidth);
	},

	setContentWidth: (widthOption) => {
		GWLog("Appearance.setContentWidth");

		document.head.removeClasses(Appearance.widthOptions.map(wo => "content-width-" + wo[0]));
		document.head.addClass("content-width-" + (widthOption || Appearance.currentWidth));
	},

	/*	Text zoom.
	 */

	getSavedTextZoom: () => {
		return (parseFloat(localStorage.getItem("text-zoom")) || Appearance.defaultTextZoom);
	},

	saveCurrentTextZoom: () => {
		GWLog("Appearance.saveCurrentTextZoom");

		if (Appearance.currentTextZoom == 1.0)
			localStorage.removeItem("text-zoom");
		else
			localStorage.setItem("text-zoom", Appearance.currentTextZoom);
	},

	setTextZoom: (zoomFactor, save = true) => {
		GWLog("Appearance.setTextZoom");

		if (!zoomFactor)
			return;

		if (zoomFactor <= Appearance.minTextZoom) {
			zoomFactor = Appearance.minTextZoom;
			queryAll(".text-size-adjust-button.decrease").forEach(button => {
				button.disabled = true;
			});
		} else if (zoomFactor >= Appearance.maxTextZoom) {
			zoomFactor = Appearance.maxTextZoom;
			queryAll(".text-size-adjust-button.increase").forEach(button => {
				button.disabled = true;
			});
		} else {
			queryAll(".text-size-adjust-button").forEach(button => {
				button.disabled = false;
			});
		}

		Appearance.currentTextZoom = zoomFactor;

		Appearance.textZoomStyleBlock.innerHTML = `${Appearance.textSizeAdjustTargetElementsSelector} { zoom: ${zoomFactor}; }`;

		if (window.generateImagesOverlay) {
			requestAnimationFrame(() => {
				generateImagesOverlay();
			});
		}

		if (save)
			Appearance.saveCurrentTextZoom();
	},

	/*********/
	/*	Setup.
	 */

	//	Set up appearance system and apply saved settings.
	setup: () => {
		GWLog("Appearance.setup");

		Appearance.currentTheme = Appearance.getSavedTheme();
		Appearance.currentFilters = Appearance.getSavedFilters();
		Appearance.currentWidth = Appearance.getSavedWidth();
		Appearance.currentTextZoom = Appearance.getSavedTextZoom();

		insertHeadHTML("<style id='theme-tweak'></style>");
		insertHeadHTML("<style id='text-zoom'></style>");

		Appearance.themeTweakStyleBlock = document.head.query("#theme-tweak");
		Appearance.textZoomStyleBlock = document.head.query("#text-zoom");

		Appearance.applyFilters();
		Appearance.setContentWidth();
		Appearance.setTextZoom();
	}
};

Appearance.setup();

/*****************/
/* ANTI-KIBITZER */
/*****************/

// While everything's being loaded, hide the authors and karma values.
if (localStorage.getItem("antikibitzer") == "true") {
	insertHeadHTML("<style id='antikibitzer-temp'>" +
	`.author, .inline-author, .karma-value, .individual-thread-page > h1 { visibility: hidden; }` + 
	"</style>");

	if (document.location.pathname.match(new RegExp("/posts/.*/comment/"))) {
		insertHeadHTML("<"+"title class='fake-title'></title>");
	}
}

/****************/
/* DEBUG OUTPUT */
/****************/

function GWLog (string) {
	if (GW.loggingEnabled || localStorage.getItem("logging-enabled") == "true")
		console.log(string);
}

/****************/
/* MISC HELPERS */
/****************/

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

/*	Get the comment ID of the item (if it's a comment) or of its containing 
	comment (if it's a child of a comment).
	*/
Element.prototype.getCommentId = function() {
	let item = (this.className == "comment-item" ? this : this.closest(".comment-item"));
	if (item) {
		return (/^comment-(.*)/.exec(item.id)||[])[1];
	} else {
		return false;
	}
}

/***********************************/
/* COMMENT THREAD MINIMIZE BUTTONS */
/***********************************/

Element.prototype.setCommentThreadMaximized = function(toggle, userOriginated = true, force) {
	GWLog("setCommentThreadMaximized");
	let commentItem = this;
	let storageName = "thread-minimized-" + commentItem.getCommentId();
	let minimize_button = commentItem.query(".comment-minimize-button");
	let maximize = force || (toggle ? /minimized/.test(minimize_button.className) : !(localStorage.getItem(storageName) || commentItem.hasClass("ignored")));
	if (userOriginated) {
		if (maximize) {
			localStorage.removeItem(storageName);
		} else {
			localStorage.setItem(storageName, true);
		}
	}

	commentItem.style.height = maximize ? 'auto' : '38px';
	commentItem.style.overflow = maximize ? 'visible' : 'hidden';

	minimize_button.className = "comment-minimize-button " + (maximize ? "maximized" : "minimized");
	minimize_button.innerHTML = maximize ? "&#xf146;" : "&#xf0fe;";
	minimize_button.title = `${(maximize ? "Collapse" : "Expand")} comment`;
	if (getQueryVariable("chrono") != "t") {
		minimize_button.title += ` thread (${minimize_button.dataset["childCount"]} child comments)`;
	}
}

/*****************************/
/* MINIMIZED THREAD HANDLING */
/*****************************/

function expandAncestorsOf(comment) {
	GWLog("expandAncestorsOf");
	if (typeof comment == "string") {
		comment = /(?:comment-)?(.+)/.exec(comment)[1];
		comment = query("#comment-" + comment);
	}
	if (!comment) {
		GWLog("Comment with ID " + comment.id + " does not exist, so we can’t expand its ancestors.");
		return;
	}

	// Expand collapsed comment threads.
	let parentOfContainingCollapseCheckbox = (comment.closest("label[for^='expand'] + .comment-thread")||{}).parentElement;
	if (parentOfContainingCollapseCheckbox) parentOfContainingCollapseCheckbox.query("input[id^='expand']").checked = true;

	// Expand collapsed comments.
	let containingTopLevelCommentItem = comment.closest(".comments > ul > li");
	if (containingTopLevelCommentItem) containingTopLevelCommentItem.setCommentThreadMaximized(true, false, true);
}

/********************/
/* COMMENT CONTROLS */
/********************/

/*	Adds an event listener to a button (or other clickable element), attaching 
	it to both "click" and "keyup" events (for use with keyboard navigation).
	Optionally also attaches the listener to the 'mousedown' event, making the 
	element activate on mouse down instead of mouse up. */
Element.prototype.addActivateEvent = function(func, includeMouseDown) {
	let ael = this.activateEventListener = (event) => { if (event.button === 0 || event.key === ' ') func(event) };
	if (includeMouseDown) this.addEventListener("mousedown", ael);
	this.addEventListener("click", ael);
	this.addEventListener("keyup", ael);
}

Element.prototype.updateCommentControlButton = function() {
	GWLog("updateCommentControlButton");
	let retractFn = () => {
		if(this.closest(".comment-item").firstChild.hasClass("retracted"))
			return [ "unretract-button", "Un-retract", "Un-retract this comment" ];
		else
			return [ "retract-button", "Retract", "Retract this comment (without deleting)" ];
	};
	let classMap = {
		"delete-button": () => { return [ "delete-button", "Delete", "Delete this comment" ] },
		"retract-button": retractFn,
		"unretract-button": retractFn,
		"edit-button": () => { return [ "edit-button", "Edit", "Edit this comment" ] }
	};
	classMap.keys().forEach((testClass) => {
		if (this.hasClass(testClass)) {
			let [ buttonClass, buttonLabel, buttonAltText ] = classMap[testClass]();
			this.className = "";
			this.addClasses([ buttonClass, "action-button" ]);
			if (this.innerHTML || !this.dataset.label) this.innerHTML = buttonLabel;
			this.dataset.label = buttonLabel;
			this.title = buttonAltText;
			this.tabIndex = '-1';
			return;
		}
	});
}

Element.prototype.constructCommentControls = function() {
	GWLog("constructCommentControls");
	let commentControls = this;

	if(commentControls.parentElement.hasClass("comments") && !commentControls.parentElement.hasClass("replies-open")) {
		return;
	}
	
	let commentType = commentControls.parentElement.id.replace(/s$/, "");
	commentControls.innerHTML = "";
	let replyButton;
	if (commentControls.parentElement.hasClass("comments")) {
		replyButton = newElement("BUTTON", {
			"class": "new-comment-button action-button",
			"accesskey": (commentType == "comment" ? "n" : ""),
			"title": ("Post new " + commentType + (commentType == "comment" ? " [n]" : "")),
			"tabindex": "-1"
		}, {
			"innerHTML": (commentType == "nomination" ? "Add nomination" : "Post new " + commentType)
		})
	} else {
		if (commentControls.parentElement.query(".comment-body").hasAttribute("data-markdown-source")) {
			let buttonsList = [];
			if (!commentControls.parentElement.query(".comment-thread"))
				buttonsList.push("delete-button");
			buttonsList.push("retract-button", "edit-button");
			buttonsList.forEach(buttonClass => {
				let button = commentControls.appendChild(newElement("BUTTON", { "class": buttonClass }));
				button.updateCommentControlButton();
			});
		}
		replyButton = newElement("BUTTON", {
			"class": "reply-button action-button",
			"data-label": "Reply",
			"tabindex": "-1"
		}, {
			"innerHTML": "Reply"
		});
	}
	commentControls.appendChild(replyButton);

	// Activate buttons.
	commentControls.queryAll(".action-button").forEach(button => {
		button.addActivateEvent(GW.commentActionButtonClicked);
	});

	// Replicate voting controls at the bottom of comments.
	if (commentControls.parentElement.hasClass("comments")) return;
	let votingControls = commentControls.parentElement.queryAll(".comment-meta .voting-controls");
	if (!votingControls) return;
	votingControls.forEach(control => {
		let controlCloned = control.cloneNode(true);
		commentControls.appendChild(controlCloned);
	});
	
	if(commentControls.query(".active-controls")) {
		commentControls.queryAll("button.vote").forEach(voteButton => {
			voteButton.addActivateEvent(voteButtonClicked);
		});
	}
}

GW.commentActionButtonClicked = (event) => {
	GWLog("commentActionButtonClicked");
	if (event.target.hasClass("edit-button") ||
		event.target.hasClass("reply-button") ||
		event.target.hasClass("new-comment-button")) {
		queryAll("textarea").forEach(textarea => {
			let commentControls = textarea.closest(".comment-controls");
			if(commentControls) hideReplyForm(commentControls);
		});
	}

	if (event.target.hasClass("delete-button")) {
		let commentItem = event.target.closest(".comment-item");
		if (confirm("Are you sure you want to delete this comment?" + "\n\n" +
					"COMMENT DATE: " + commentItem.query(".date.").innerHTML + "\n" + 
					"COMMENT ID: " + /comment-(.+)/.exec(commentItem.id)[1] + "\n\n" + 
					"COMMENT TEXT:" + "\n" + commentItem.query(".comment-body").dataset.markdownSource))
			doCommentAction("delete", commentItem);
	} else if (event.target.hasClass("retract-button")) {
		doCommentAction("retract", event.target.closest(".comment-item"));
	} else if (event.target.hasClass("unretract-button")) {
		doCommentAction("unretract", event.target.closest(".comment-item"));
	} else if (event.target.hasClass("edit-button")) {
		showCommentEditForm(event.target.closest(".comment-item"));
	} else if (event.target.hasClass("reply-button")) {
		showReplyForm(event.target.closest(".comment-item"));
	} else if (event.target.hasClass("new-comment-button")) {
		showReplyForm(event.target.closest(".comments"));
	}

	event.target.blur();
};

function initializeCommentControls() {
	e = newElement("DIV", { "class": "comment-controls posting-controls" });
	document.currentScript.insertAdjacentElement("afterend", e);
	e.constructCommentControls();

	if (window.location.hash) {
		let comment = e.closest(".comment-item");
		if(comment && window.location.hash == "#" + comment.id)
			expandAncestorsOf(comment);
	}
}

/****************/
/* PRETTY DATES */
/****************/

// If the viewport is wide enough to fit the desktop-size content column,
// use a long date format; otherwise, a short one.
let useLongDate = window.innerWidth > 900;
let dtf = new Intl.DateTimeFormat([], 
				  ( useLongDate ? 
				    { month: 'short', day: 'numeric', year: 'numeric', hour: 'numeric', minute: 'numeric' }
				    : { month: 'numeric', day: 'numeric', year: 'numeric', hour: 'numeric', minute: 'numeric' } ));

function prettyDate() {
	let dateElement = document.currentScript.parentElement;
	let jsDate = dateElement.dataset.jsDate;
	if (jsDate) {
		let pretty = dtf.format(new Date(+ jsDate));
		window.requestAnimationFrame(() => {
			dateElement.innerHTML = pretty;
			dateElement.removeClass('hide-until-init');
		});
	}
}


// Hide elements that require javascript until ready.
insertHeadHTML("<style>.only-without-js { display: none; }</style><style id='hide-until-init'>.hide-until-init { visibility: hidden; }</style>");

/****************/
/* SERVER CALLS */
/****************/

let deferredCalls = [];

function callWithServerData(fname, uri) {
	deferredCalls.push([fname, uri]);
}

/************/
/* TRIGGERS */
/************/

/*	Polyfill for requestIdleCallback in Apple and Microsoft browsers. */
if (!window.requestIdleCallback) {
	window.requestIdleCallback = (fn) => { setTimeout(fn, 0) };
}

GW.triggers = {};

function invokeTrigger(args) {
	if(args.priority < 0) {
		args.fn();
	} else if(args.priority > 0) {
		requestIdleCallback(args.fn, {timeout: args.priority});
	} else {
		setTimeout(args.fn, 0);
	}
}

function addTriggerListener(name, args) {
	if(typeof(GW.triggers[name])=="string") return invokeTrigger(args);
	if(!GW.triggers[name]) GW.triggers[name] = [];
	GW.triggers[name].push(args);
}

function activateTrigger(name) {
	if(Array.isArray(GW.triggers[name])) {
		GW.triggers[name].forEach(invokeTrigger);
	}
	GW.triggers[name] = "done";
}

function addMultiTriggerListener(triggers, args) {
	if(triggers.length == 1) {
		addTriggerListener(triggers[0], args);
	} else {
		let trigger = triggers.pop();
		addMultiTriggerListener(triggers, {immediate: args["immediate"], fn: () => addTriggerListener(trigger, args)});
	}
}

