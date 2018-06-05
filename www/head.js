document.querySelector("head").insertAdjacentHTML("beforeend", "<style id='hide-bottom-bar'>#bottom-bar { display: none }</style>");

var widthOptions = [
	['normal', 'Narrow (fixed-width) content column', 'N', '900px'],
	['wide', 'Wide (fixed-width) content column', 'W', '1150px'],
	['fluid', 'Full-width (fluid) content column', 'F', '(100% - 300px)']
];

var widthDict = {'(100vw - 300px)': 'fluid'};
widthOptions.map((wo) => {
	widthDict[wo[0]] = wo[3];
	widthDict[wo[3]] = wo[3];
});

function setContentWidth(widthString) {
	let width = widthDict[widthString];
	if (!width) return;
	document.querySelector('#width-adjust').innerHTML = 
		`#content, #ui-elements-container, #images-overlay { 
			max-width: calc(${width});
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
