
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
                return [document.getElementById(selector.substr(1))];
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

Object.prototype.isEmpty = function() {
    for (var prop in this) if (this.hasOwnProperty(prop)) return false;
    return true;
};

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
		let selector = GW.filtersTargetSelector || "body::before, #content, #ui-elements-container > div:not(#theme-tweaker-ui), #theme-tweaker-ui #theme-tweak-section-sample-text .sample-text-container";
		fullStyleString = `body::before { content: ""; } ${selector} { filter: ${filterStringFromFilters(filters)}; }`;
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
		`.post-body, .comment-body {
			zoom: ${zoomFactor};
		}`;
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
		query("head").insertAdjacentHTML("beforeend", "<title class='fake-title'></title>");
	}
}
