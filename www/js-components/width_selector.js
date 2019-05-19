"<div id='width-selector'>" +
		String.prototype.concat.apply("", GW.widthOptions.map(widthOption => {
			let [name, desc, abbr] = widthOption;
			let selected = (name == currentWidth ? ' selected' : '');
			let disabled = (name == currentWidth ? ' disabled' : '');
			return `<button type='button' class='select-width-${name}${selected}'${disabled} title='${desc}' tabindex='-1' data-name='${name}'><svg><use xlink:href='${GW.assetVersions['/assets/icons.svg']}#width-${name}'/></svg></button>`;
		}))
+ "</div>"