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
		`.markdown-reference-link a::before { filter: invert(100%); }` + "</style>");
	}
}
setTheme();

function setContentWidth(widthString) {
	if(!widthString) return;
	let widthAdjustStyle = document.querySelector('#width-adjust');
	widthAdjustStyle.innerHTML = 
		`#content { 
			max-width: calc(${widthString});
		}
		#bottom-bar a[href='#top']::after, 
		.post-meta a[href='#comments']::after, 
		.post-meta a[href='#bottom-bar']::after {
			right: calc((100vw - ${widthString}) / 2 - 75px);
		}
		.post-meta .new-comments-count {
			right: calc((100vw - ${widthString}) / 2 - 139px);
		}
		.post-meta .new-comment-sequential-nav-button {
			right: calc((100vw - ${widthString}) / 2 - 148px);
		}
		#width-selector {
			right: calc((100% - ${widthString}) / 2 - 78px);
		}
		#theme-selector {
			left: calc((100% - ${widthString}) / 2 - 41px);
		}
		#theme-tweaker-toggle {
			left: calc((100% - ${widthString}) / 2 - 75px);
		}`;
}
setContentWidth(window.localStorage.getItem('selected-width'));

Object.prototype.isEmpty = function() {
    for (var prop in this) if (this.hasOwnProperty(prop)) return false;
    return true;
};

function applyFilters(filters) {
	if (filters.isEmpty()) return;
	
	var filterString = "";
	for (const [ key, value ] of filters.entries()) {
		filterString += ` ${key}(${value})`;
	}
	var fullStyleString = "#content, #ui-elements-container > div:not(#theme-tweaker-ui) { filter:" + filterString + "; }";
	
	// Special cases require additional stuff.
	if (filters.invert == '100%') {
		fullStyleString += "\nbody { background-color: #000; }";
	}
	
	// Update the style tag.
	document.querySelector("#theme-tweak").innerHTML = fullStyleString;
}
window.currentFilters = JSON.parse(window.localStorage.getItem("theme-tweaks") || "{ }");
applyFilters(window.currentFilters);