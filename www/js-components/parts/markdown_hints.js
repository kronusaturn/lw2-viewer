`<input type='checkbox' id='markdown-hints-checkbox' tabindex='-1'>
	<label for='markdown-hints-checkbox'></label>
	<div id='markdown-hints'>` + [
		[ 'Bold', '**Bold**', 'font-weight:bold' ],
		[ 'Italic', '*Italic*', 'font-style:italic' ],
		[ '<s>Strikethrough</s>', '~~Strikethrough~~', '' ],
		[ '<a href=#>Link</a>', '[Link](http://example.com)', '' ],
		[ 'Image', '![Alt-text](http://example.com/pic.gif)', '' ],
		[ 'Heading 1', '# Heading 1', '' ],
		[ 'Heading 2', '## Heading 2', '' ],
		[ 'Heading 2', '### Heading 3', '' ],
		[ 'Blockquote', '&gt; Blockquote', '' ],
		[ 'Bulleted list', '* List item', '' ],
		[ 'Numbered list', '1. List item', '' ],
	].map(row => `<div class='markdown-hints-row'><span${row[2] ? " style='" + row[2] + "'" : ""}>${row[0]}</span><code>${row[1]}</code></div>`).join('')
+ `</div>`