/******************/
/* ANALYSIS TOOLS */
/******************/

GWLog("Analysis tools loaded.");

/****************************/
/* CHARACTER USAGE ANALYSIS */
/****************************/

function catalogCharactersUsedIn(element) {
	if (element.textContent == "") return;
	if (element.nodeType == 3) {
		// Analysis...
		GWLog("Analyzing element of type " + element.parentElement.tagName + " [" + /\"?(.+?)\"?(?:,|$)/.exec(getComputedStyle(element.parentElement).fontFamily)[1] + "]");
		GWLog("“" + element.textContent + "”");
		
		return;
	}
	element.childNodes.forEach(childNode => {
		catalogCharactersUsedIn(childNode);
	});
}

function constructCharacterUsageTable() {
	GW.characterUsageTable = { };
	catalogCharactersUsedIn(query("body"));

	console.log("Character usage table constructed.");
}

constructCharacterUsageTable();