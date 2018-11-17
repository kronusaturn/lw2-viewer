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

/***********************************/
/* CONTENT COLUMN WIDTH ADJUSTMENT */
/***********************************/

var widthOptions = [
	['normal', 'Narrow (fixed-width) content column', 'N'],
	['wide', 'Wide (fixed-width) content column', 'W'],
	['fluid', 'Full-width (fluid) content column', 'F']
];

function setContentWidth(widthOption) {
	let currentWidth = localStorage.getItem("selected-width") || 'normal';
	let head = document.querySelector('head');
	head.removeClasses(widthOptions.map(wo => 'content-width-' + wo[0]));
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
		let selector = window.filtersTargetSelector || "body::before, #content, #ui-elements-container > div:not(#theme-tweaker-ui), #theme-tweaker-ui #theme-tweak-section-sample-text .sample-text-container";
		fullStyleString = `body::before { content: ""; } ${selector} { filter: ${filterStringFromFilters(filters)}; }`;
	}
	
	// Update the style tag (if it’s already been loaded).
	(document.querySelector("#theme-tweak")||{}).innerHTML = fullStyleString;
}
document.querySelector("head").insertAdjacentHTML("beforeend", "<style id='theme-tweak'></style>");	
window.currentFilters = JSON.parse(localStorage.getItem("theme-tweaks") || "{ }");
applyFilters(window.currentFilters);

/************************/
/* TEXT SIZE ADJUSTMENT */
/************************/

document.querySelector("head").insertAdjacentHTML("beforeend", "<style id='text-zoom'></style>");
function setTextZoom(zoomFactor) {
	if (!zoomFactor) return;

	let minZoomFactor = 0.5;
	let maxZoomFactor = 1.5;
	
	if (zoomFactor <= minZoomFactor) {
		zoomFactor = minZoomFactor;
		document.querySelectorAll(".text-size-adjust-button.decrease").forEach(function (button) {
			button.disabled = true;
		});
	} else if (zoomFactor >= maxZoomFactor) {
		zoomFactor = maxZoomFactor;
		document.querySelectorAll(".text-size-adjust-button.increase").forEach(function (button) {
			button.disabled = true;
		});
	} else {
		document.querySelectorAll(".text-size-adjust-button").forEach(function (button) {
			button.disabled = false;
		});
	}

	let textZoomStyle = document.querySelector("#text-zoom");
	textZoomStyle.innerHTML = 
		`.post-body, .comment-body {
			zoom: ${zoomFactor};
		}`;
}
window.currentTextZoom = localStorage.getItem('text-zoom');
setTextZoom(window.currentTextZoom);

/**********/
/* THEMES */
/**********/

window.themeOptions = [
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
	document.querySelector("head").insertAdjacentHTML("beforeend", "<style id='antikibitzer-temp'>" +
	`.author, .inline-author, .karma-value, .individual-thread-page > h1 { visibility: hidden; }` + 
	"</style>");

	if(document.location.pathname.match(new RegExp("/posts/.*/comment/"))) {
		document.querySelector("head").insertAdjacentHTML("beforeend", "<title class='fake-title'></title>");
	}
}
