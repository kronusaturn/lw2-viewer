`<nav id='keyboard-help-overlay'>
	<div class='keyboard-help-container'>
		<button type='button' title='Close keyboard shortcuts' class='close-keyboard-help'>&#xf00d;</button>
		<h1>Keyboard shortcuts</h1>
		<p class='note'>Keys shown in yellow (e.g., <code class='ak'>]</code>) are <a href='https://en.wikipedia.org/wiki/Access_key#Access_in_different_browsers' target='_blank'>accesskeys</a>, and require a browser-specific modifier key (or keys).</p>
		<p class='note'>Keys shown in grey (e.g., <code>?</code>) do not require any modifier keys.</p>
		<div class='keyboard-shortcuts-lists'>` + 
		[ [
			"General",
			[ [ '?' ], "Show keyboard shortcuts" ],
			[ [ 'Esc' ], "Hide keyboard shortcuts" ]
		], [
			"Site navigation",
			[ [ 'ak-h' ], "Go to Home (a.k.a. “Frontpage”) view" ],
			[ [ 'ak-f' ], "Go to Featured (a.k.a. “Curated”) view" ],
			[ [ 'ak-a' ], "Go to All (a.k.a. “Community”) view" ],
			[ [ 'ak-m' ], "Go to Meta view" ],
			[ [ 'ak-c' ], "Go to Recent Comments view" ],
			[ [ 'ak-r' ], "Go to Archive view" ],
			[ [ 'ak-q' ], "Go to Sequences view" ],
			[ [ 'ak-t' ], "Go to About page" ],
			[ [ 'ak-u' ], "Go to User or Login page" ],
			[ [ 'ak-o' ], "Go to Inbox page" ]
		], [
			"Page navigation",
			[ [ 'ak-,' ], "Jump up to top of page" ],
			[ [ 'ak-.' ], "Jump down to bottom of page" ],
			[ [ 'ak-/' ], "Jump to top of answers section" ],
			[ [ 'ak-/' ], "Jump to top of comments section" ],
			[ [ ';' ], "Focus external link (on link posts)" ],
			[ [ 'ak-s' ], "Search" ]
		], [
			"Page actions",
			[ [ 'ak-n' ], "New post or comment" ],
			[ [ 'ak-w' ], "New answer (on question posts)" ],
			[ [ 'ak-e' ], "Edit current post" ],
			[ [ 'ak-z' ], "Switch to next sort order (comments)" ],
			[ [ 'ak-x' ], "Switch to next sort order (answers)" ]
		], [
			"Sequences",
			[ [ 'ak-]' ], "Go to next post in sequence" ],
			[ [ 'ak-[' ], "Go to previous post in sequence" ],
			[ [ 'ak-\\' ], "Go to sequence index" ]
		], [
			"Post/comment list views",
			[ [ '.' ], "Focus next entry (post/comment)" ],
			[ [ ',' ], "Focus previous entry (post/comment)" ],
			[ [ ';' ], "Cycle between links in focused entry" ],
			[ [ 'Enter' ], "Go to currently focused entry" ],
			[ [ 'Esc' ], "Unfocus currently focused entry" ],
			[ [ 'ak-]' ], "Go to next page" ],
			[ [ 'ak-[' ], "Go to previous page" ],
			[ [ 'ak-\\' ], "Go to first page" ],
			[ [ 'ak-e' ], "Edit currently focused post" ],
			[ [ 'ak-z' ], "Switch post sort order" ]
		], [
			"Editor",
			[ [ 'ak-k' ], "Bold text" ],
			[ [ 'ak-i' ], "Italic text" ],
			[ [ 'ak-l' ], "Insert hyperlink" ],
			[ [ 'ak-q' ], "Blockquote text" ],
			[ [ 'ak-n' ], "Footnote" ]
		], [
			"Miscellaneous",
			[ [ 'ak-x' ], "Switch to next view on user page" ],
			[ [ 'ak-z' ], "Switch to previous view on user page" ],
			[ [ 'ak-`' ], "Toggle compact comment list view" ],
			[ [ 'ak-g' ], "Toggle anti-kibitzer mode" ]
		], [
			"Appearance",
			[ [ 'ak-=' ], "Increase text size" ],
			[ [ 'ak--' ], "Decrease text size" ],
			[ [ 'ak-0' ], "Reset to default text size" ],
			[ [ 'ak-\'' ], "Cycle through content width settings" ],
			[ [ 'ak-1' ], "Switch to default theme [A]" ],
			[ [ 'ak-2' ], "Switch to dark theme [B]" ],
			[ [ 'ak-3' ], "Switch to grey theme [C]" ],
			[ [ 'ak-4' ], "Switch to ultramodern theme [D]" ],
			[ [ 'ak-5' ], "Switch to simple theme [E]" ],
			[ [ 'ak-6' ], "Switch to brutalist theme [F]" ],
			[ [ 'ak-7' ], "Switch to ReadTheSequences theme [G]" ],
			[ [ 'ak-8' ], "Switch to classic Less Wrong theme [H]" ],
			[ [ 'ak-9' ], "Switch to modern Less Wrong theme [I]" ],
			[ [ 'ak-;' ], "Open theme tweaker" ],
			[ [ 'Enter' ], "Save changes and close theme tweaker"],
			[ [ 'Esc' ], "Close theme tweaker (without saving)" ]
		], [
			"Slide shows",
			[ [ 'ak-l' ], "Start/resume slideshow" ],
			[ [ 'Esc' ], "Exit slideshow" ],
			[ [ '&#x2192;', '&#x2193;' ], "Next slide" ],
			[ [ '&#x2190;', '&#x2191;' ], "Previous slide" ],
			[ [ 'Space' ], "Reset slide zoom" ]
		] ].map(section => 
		`<ul><li class='section'>${section[0]}</li>` + section.slice(1).map(entry =>
			`<li>
				<span class='keys'>` + 
				entry[0].map(key =>
					(key.hasPrefix("ak-")) ? `<code class='ak'>${key.substring(3)}</code>` : `<code>${key}</code>`
				).join("") + 
				`</span>
				<span class='action'>${entry[1]}</span>
			</li>`
		).join("\n") + `</ul>`).join("\n") + `
		</ul></div>
	</div>
</nav>`