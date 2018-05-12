document.querySelector("head").insertAdjacentHTML("beforeend", "<style id='hide-bottom-bar'>#bottom-bar { display: none }</style>");

function setTheme(themeName) {
	if (typeof(themeName) == 'undefined') {
		themeName = window.localStorage.getItem('selected-theme');
		if (!themeName) return;
	} else {
		if (themeName == 'default') window.localStorage.removeItem('selected-theme');
		else window.localStorage.setItem('selected-theme', themeName);
	}
	
	let styleSheetNameSuffix = (themeName == 'default') ? '' : ('-' + themeName);
	let currentStyleSheetNameComponents = /style[^\.]*(\..+)$/.exec(document.querySelector("head link[href*='.css']").href);
	
	let newStyle = document.createElement('link');
	newStyle.setAttribute('rel', 'stylesheet');
	newStyle.setAttribute('href', '/style' + styleSheetNameSuffix + currentStyleSheetNameComponents[1]);
	
	let oldStyle = document.querySelector("head link[href*='.css']");
	newStyle.addEventListener('load', function() { oldStyle.parentElement.removeChild(oldStyle); });
	document.querySelector('head').insertBefore(newStyle, oldStyle.nextSibling);
	
	if (themeName == 'dark') {
		document.querySelector("head").insertAdjacentHTML("beforeend", "<style id='dark-theme-adjustments'>" + 
		`.markdown-reference-link a { color: #d200cf; filter: invert(100%); }` + "</style>");
	} else {
		document.querySelectorAll("#dark-theme-adjustments").forEach(function(e) {e.parentNode.removeChild(e)});
	}
}
setTheme();

function setContentWidth(widthString) {
	if (!widthString) return;
	let widthAdjustStyle = document.querySelector('#width-adjust');
	widthAdjustStyle.innerHTML = 
		`#content, #ui-elements-container { 
			max-width: calc(${widthString});
		}
		#ui-elements-container {
			left: calc((100% - ${widthString}) / 2);
		}`;
}
setContentWidth(window.localStorage.getItem('selected-width'));

Object.prototype.isEmpty = function() {
    for (var prop in this) if (this.hasOwnProperty(prop)) return false;
    return true;
};

function applyFilters(filters) {
	var fullStyleString = "";
	
	if (!filters.isEmpty()) {
		var filterString = "";
		for (key of Object.keys(filters)) {
			let value = filters[key];
			filterString += ` ${key}(${value})`;
		}
		fullStyleString = `body::before { content: ""; } body::before, #content, #ui-elements-container > div:not(#theme-tweaker-ui) { filter: ${filterString}; }`;
	}
	
	// Update the style tag (if it’s already been loaded).
	document.querySelectorAll("#theme-tweak").forEach(function (styleBlock) { styleBlock.innerHTML = fullStyleString; });
}
document.querySelector("head").insertAdjacentHTML("beforeend", "<style id='theme-tweak'></style>");	
window.currentFilters = JSON.parse(window.localStorage.getItem("theme-tweaks") || "{ }");
applyFilters(window.currentFilters);

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
		`.post-body, .comment-body, #theme-tweaker-ui #theme-tweak-section-text-size-adjust .sample-text {
			zoom: ${zoomFactor};
		}`;


}
window.currentTextZoom = window.localStorage.getItem('text-zoom');
setTextZoom(window.currentTextZoom);