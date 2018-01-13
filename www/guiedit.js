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
	var str = (p0 == p1) ? mtext : tarea.value.substring(p0, p1);
	str = func ? func(str) : (mopen + str + mclose);
	tarea.value = tarea.value.substring(0, p0) + str + tarea.value.substring(p1);
	return;
}

var guiEditButtons = [
	[ 'strong', 'Strong (bold)', 'k', '**', '**', 'Bold text', '&#xf032;' ],
	[ 'em', 'Emphasized (italic)', 'i', '*', '*', 'Italicized text', '&#xf033;' ],
	[ 'link', 'Hyperlink', 'l', '[', '](link url)', 'link text', '&#xf0c1;' ],
	[ 'image', 'Image', '', '![', '](image url)', 'Image alt-text', '&#xf03e;' ],
	[ 'heading1', 'Heading level 1', '', '# ', '', 'Heading', '&#xf1dc;' ],
	[ 'heading2', 'Heading level 2', '', '## ', '', 'Heading', '&#xf1dc;' ],
	[ 'heading3', 'Heading level 3', '', '### ', '', 'Heading', '&#xf1dc;' ],
	[ 'blockquote', 'Blockquote', '', '> ', '', 'Quoted text', '&#xf10e;' ],
	[ 'bulleted-list', 'Bulleted list', '', '* ', '', 'List item', '&#xf0ca' ],
	[ 'numbered-list', 'Numbered list', '', '1. ', '', 'List item', '&#xf0cb' ],
	[ 'horizontal-rule', 'Horizontal rule', '', '---', '', '', '&#xf068' ],
	[ 'inline-code', 'Inline code', '', '`', '`', 'Code', '&#xf121' ],
	[ 'code-block', 'Code block', '', '```\n', '\n```', 'Code', '&#xf1c9' ]
];
