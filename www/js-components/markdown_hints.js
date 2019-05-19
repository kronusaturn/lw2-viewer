`<input type='checkbox' id='markdown-hints-checkbox' tabindex='-1'>
	<label for='markdown-hints-checkbox'></label>
	<div id='markdown-hints'>` + 
	[	"<span style='font-weight: bold;'>Bold</span><code>**Bold**</code>", 
		"<span style='font-style: italic;'>Italic</span><code>*Italic*</code>",
		"<span><a href=#>Link</a></span><code>[Link](http://example.com)</code>",
		"<span>Heading 1</span><code># Heading 1</code>",
		"<span>Heading 2</span><code>## Heading 1</code>",
		"<span>Heading 3</span><code>### Heading 1</code>",
		"<span>Blockquote</span><code>&gt; Blockquote</code>" 
	].map(row => "<div class='markdown-hints-row'>" + row + "</div>").join("")
+ `</div>`