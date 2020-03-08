
// FOR TESTING ONLY, COMMENT WHEN DEPLOYING.
// GW.loggingEnabled = true;

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
Element.prototype.toggleClass = function(className) {
	if (this.hasClass(className))
		this.removeClass(className);
	else
		this.addClass(className);
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

/***********************************/
/* CONTENT COLUMN WIDTH ADJUSTMENT */
/***********************************/

GW.widthOptions = [
	['normal', 'Narrow (fixed-width) content column', 'N'],
	['wide', 'Wide (fixed-width) content column', 'W'],
	['fluid', 'Full-width (fluid) content column', 'F']
];

function setContentWidth(widthOption) {
	let currentWidth = localStorage.getItem("selected-width") || 'normal';
	let head = query('head');
	head.removeClasses(GW.widthOptions.map(wo => 'content-width-' + wo[0]));
	head.addClass('content-width-' + (widthOption || 'normal'));
}
setContentWidth(localStorage.getItem('selected-width'));

/********************************************/
/* APPEARANCE CUSTOMIZATION (THEME TWEAKER) */
/********************************************/

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

GW.themeTweaker = { };
GW.themeTweaker.filtersExclusionPaths = { };
GW.themeTweaker.defaultFiltersExclusionTree = [ "#content", [ ] ];

function exclusionTreeFromExclusionPaths(paths) {
	if (!paths) return null;

	let tree = GW.themeTweaker.defaultFiltersExclusionTree.clone();
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
}
function selectorFromExclusionTree(tree) {
	var selectorParts = [
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
}
function filterStringFromFilters(filters) {
	var filterString = "";
	for (key of Object.keys(filters)) {
		let value = filters[key];
		filterString += ` ${key}(${value})`;
	}
	return filterString;
}
function applyFilters(filters) {
	var fullStyleString = "";
	
	if (!filters.isEmpty()) {
		let filtersExclusionTree = exclusionTreeFromExclusionPaths(GW.themeTweaker.filtersExclusionPaths) || GW.themeTweaker.defaultFiltersExclusionTree;
		fullStyleString = `body::before { content: ""; } body > #content::before { z-index: 0; } ${selectorFromExclusionTree(filtersExclusionTree)} { filter: ${filterStringFromFilters(filters)}; }`;
	}
	
	// Update the style tag (if it’s already been loaded).
	(query("#theme-tweak")||{}).innerHTML = fullStyleString;
}
query("head").insertAdjacentHTML("beforeend", "<style id='theme-tweak'></style>");	
GW.currentFilters = JSON.parse(localStorage.getItem("theme-tweaks") || "{ }");
applyFilters(GW.currentFilters);

/************************/
/* TEXT SIZE ADJUSTMENT */
/************************/

query("head").insertAdjacentHTML("beforeend", "<style id='text-zoom'></style>");
function setTextZoom(zoomFactor) {
	if (!zoomFactor) return;

	let minZoomFactor = 0.5;
	let maxZoomFactor = 1.5;
	
	if (zoomFactor <= minZoomFactor) {
		zoomFactor = minZoomFactor;
		queryAll(".text-size-adjust-button.decrease").forEach(function (button) {
			button.disabled = true;
		});
	} else if (zoomFactor >= maxZoomFactor) {
		zoomFactor = maxZoomFactor;
		queryAll(".text-size-adjust-button.increase").forEach(function (button) {
			button.disabled = true;
		});
	} else {
		queryAll(".text-size-adjust-button").forEach(function (button) {
			button.disabled = false;
		});
	}

	let textZoomStyle = query("#text-zoom");
	textZoomStyle.innerHTML = 
		`.post, .comment, .comment-controls {
			zoom: ${zoomFactor};
		}`;

	if (window.generateImagesOverlay) setTimeout(generateImagesOverlay);
}
GW.currentTextZoom = localStorage.getItem('text-zoom');
setTextZoom(GW.currentTextZoom);

/**********/
/* THEMES */
/**********/

GW.themeOptions = [
	['default', 'Default theme (dark text on light background)', 'A'],
	['dark', 'Dark theme (light text on dark background)', 'B'],
	['grey', 'Grey theme (more subdued than default theme)', 'C'],
	['ultramodern', 'Ultramodern theme (very hip)', 'D'],
	['zero', 'Theme zero (plain and simple)', 'E'],
	['brutalist', 'Brutalist theme (the Motherland calls!)', 'F'],
	['rts', 'ReadTheSequences.com theme', 'G'],
	['classic', 'Classic Less Wrong theme', 'H'],
	['less', 'Less theme (serenity now)', 'I']
];

/*****************/
/* ANTI-KIBITZER */
/*****************/

// While everything's being loaded, hide the authors and karma values.
if (localStorage.getItem("antikibitzer") == "true") {
	query("head").insertAdjacentHTML("beforeend", "<style id='antikibitzer-temp'>" +
	`.author, .inline-author, .karma-value, .individual-thread-page > h1 { visibility: hidden; }` + 
	"</style>");

	if(document.location.pathname.match(new RegExp("/posts/.*/comment/"))) {
		query("head").insertAdjacentHTML("beforeend", "<"+"title class='fake-title'></title>");
	}
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
	let commentControls = this;

	if(commentControls.parentElement.id == "nominations" || commentControls.parentElement.id == "reviews") {
		return; // Too late to add nominations or reviews.
	}
	
	let commentType = commentControls.parentElement.id.replace(/s$/, "");
	commentControls.innerHTML = "";
	let replyButton = document.createElement("button");
	if (commentControls.parentElement.hasClass("comments")) {
		replyButton.className = "new-comment-button action-button";
		replyButton.innerHTML = (commentType == "nomination" ? "Add nomination" : "Post new " + commentType);
		replyButton.setAttribute("accesskey", (commentType == "comment" ? "n" : ""));
		replyButton.setAttribute("title", "Post new " + commentType + (commentType == "comment" ? " [n]" : ""));
	} else {
		if (commentControls.parentElement.query(".comment-body").hasAttribute("data-markdown-source")) {
			let buttonsList = [];
			if(!commentControls.parentElement.query(".comment-thread"))
				buttonsList.push("delete-button");
			buttonsList.push("retract-button", "edit-button");
			buttonsList.forEach(buttonClass => {
				let button = commentControls.appendChild(document.createElement("button"));
				button.addClass(buttonClass);
				button.updateCommentControlButton();
			});
		}
		replyButton.className = "reply-button action-button";
		replyButton.innerHTML = "Reply";
		replyButton.dataset.label = "Reply";
	}
	commentControls.appendChild(replyButton);
	replyButton.tabIndex = '-1';

	// On mobile, hide labels for all but the Reply button.
	if (GW.isMobile && window.innerWidth <= 900) {
		commentControls.queryAll(".delete-button, .retract-button, .unretract-button, .edit-button").forEach(button => {
			button.innerHTML = "";
		});
	}

	// Activate buttons.
	commentControls.queryAll(".action-button").forEach(button => {
		button.addActivateEvent(GW.commentActionButtonClicked);
	});

	// Replicate karma controls at the bottom of comments.
	if (commentControls.parentElement.hasClass("comments")) return;
	let karmaControls = commentControls.parentElement.query(".comment-meta .karma");
	if (!karmaControls) return;
	let karmaControlsCloned = karmaControls.cloneNode(true);
	commentControls.appendChild(karmaControlsCloned);
	if(commentControls.query(".active-controls")) {
		commentControls.queryAll("button.vote").forEach(voteButton => {
			voteButton.addActivateEvent(voteButtonClicked);
		});
	}
}

GW.commentActionButtonClicked = (event) => {
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
	e = document.createElement("div");
	e.className = "comment-controls posting-controls";
	document.currentScript.insertAdjacentElement("afterend", e);
	e.constructCommentControls();
}

// Hide elements that require javascript until ready.
query("head").insertAdjacentHTML("beforeend", "<style>.only-without-js { display: none; }</style><style id='hide-until-init'>.hide-until-init { visibility: hidden; }</style>");
