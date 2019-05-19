"<div id='theme-selector' class='theme-selector'>" +
	String.prototype.concat.apply("", GW.themeOptions.map(themeOption => {
		let [name, desc, letter] = themeOption;
		let selected = (name == currentTheme ? ' selected' : '');
		let disabled = (name == currentTheme ? ' disabled' : '');
		let accesskey = letter.charCodeAt(0) - 'A'.charCodeAt(0) + 1;
		return `<button type='button' class='select-theme-${name}${selected}'${disabled} title="${desc} [${accesskey}]" data-theme-name="${name}" data-theme-description="${desc}" accesskey='${accesskey}' tabindex='-1'>${letter}</button>`;
	}))
+ "</div>"