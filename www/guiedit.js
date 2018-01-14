function insMarkup() {
	var mopen = '', mclose = '', mtext = '', func = false;
	if (typeof arguments[0] == 'function') {
		func = arguments[0];
	} else {
		mopen = arguments[0];
		mclose = arguments[1];
		mtext = arguments[2];
	}

	var tarea = document.querySelector(".cancel-comment-button ~ form textarea");
	tarea.focus();
	var p0 = tarea.selectionStart;
	var p1 = tarea.selectionEnd;
	var cur0 = cur1 = p0;
	
	var str = (p0 == p1) ? mtext : tarea.value.substring(p0, p1);
	str = func ? func(str, p0) : (mopen + str + mclose);
	
	// Determine selection.
	if (!func) {
		cur0 += (p0 == p1) ? mopen.length : str.length;
		cur1 = (p0 == p1) ? (cur0 + mtext.length) : cur0;
	} else {
		cur0 = str[1];
		cur1 = str[2];
		str = str[0];
	}

	// Update textarea contents.
	tarea.value = tarea.value.substring(0, p0) + str + tarea.value.substring(p1);
	
	// Set selection.
	tarea.selectionStart = cur0;
	tarea.selectionEnd = cur1;
	
	return;
}

var guiEditButtons = [
	[ 'strong', 'Strong (bold)', 'k', '**', '**', 'Bold text', '&#xf032;' ],
	[ 'em', 'Emphasized (italic)', 'i', '*', '*', 'Italicized text', '&#xf033;' ],
	[ 'link', 'Hyperlink', 'l', hyperlink, '', '', '&#xf0c1;' ],
// 	[ 'image', 'Image', '', '![', '](image url)', 'Image alt-text', '&#xf03e;' ],
	[ 'heading1', 'Heading level 1', '', '\\n# ', '', 'Heading', '&#xf1dc;<sup>1</sup>' ],
	[ 'heading2', 'Heading level 2', '', '\\n## ', '', 'Heading', '&#xf1dc;<sup>2</sup>' ],
	[ 'heading3', 'Heading level 3', '', '\\n### ', '', 'Heading', '&#xf1dc;<sup>3</sup>' ],
	[ 'blockquote', 'Blockquote', '', blockquote, '', '', '&#xf10e;' ],
	[ 'bulleted-list', 'Bulleted list', '', '\\n* ', '', 'List item', '&#xf0ca' ],
	[ 'numbered-list', 'Numbered list', '', '\\n1. ', '', 'List item', '&#xf0cb' ],
	[ 'horizontal-rule', 'Horizontal rule', '', '\\n\\n---\\n\\n', '', '', '&#xf068' ],
	[ 'inline-code', 'Inline code', '', '`', '`', 'Code', '&#xf121' ],
	[ 'code-block', 'Code block', '', '```\\n', '\\n```', 'Code', '&#xf1c9' ]
// 	[ 'formula', 'LaTeX', '', '$', '$', 'LaTeX formula', '&#xf155' ]
];

function blockquote(text, startpos) {
	if (text == '') {
		return "> Quoted text";
	} else {
		return "> " + text.split("\n").join("\n> ") + "\n";
	}
}

function hyperlink(text, startpos) {
	var url = '', link_text = text;
	if (text.search(/^https?/) != -1) {
		url = text;
		link_text = "link text";
	} else {
		url = prompt("Link address (URL):") || "";
	}	
	
	return "[" + link_text + "](" + url + ")";
}
