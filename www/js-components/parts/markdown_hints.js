`<input type='checkbox' id='markdown-hints-checkbox' tabindex='-1'>
	<label for='markdown-hints-checkbox'></label>
	<div id='markdown-hints'>` + [
		[ '&#xf032;', 'Bold', '**Bold**', 'font-weight:bold' ],
		[ '&#xf033;', 'Italic', '*Italic*', 'font-style:italic' ],
		[ '&#xf0cc;', '<s>Strikethrough</s>', '~~Strikethrough~~', '' ],
		[ '&#xf0c1;', '<a href=#>Link</a>', '[Link](http://example.com)', '' ],
		[ '&#xf03e;', 'Image', '![Alt-text](http://example.com/pic.gif)', '' ],
		[ '&#xf1dc;<sup>1</sup>', 'Heading 1', '# Heading 1', '' ],
		[ '&#xf1dc;<sup>2</sup>', 'Heading 2', '## Heading 2', '' ],
		[ '&#xf1dc;<sup>3</sup>', 'Heading 3', '### Heading 3', '' ],
		[ '&#xf10e;', 'Blockquote', '&gt; Blockquote', '' ],
		[ '&#xf0ca;', 'Bulleted list', '* List item', '' ],
		[ '&#xf0cb;', 'Numbered list', '1. List item', '' ],
	].map(row => `<div class='markdown-hints-row'><span class='guiedit'>${row[0]}</span><span${row[3] ? " style='" + row[3] + "'" : ""}>${row[1]}</span><code>${row[2]}</code></div>`).join('')
+ `</div>`