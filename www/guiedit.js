function insMarkup() {
	var mopen = arguments[0], mclose = arguments[1];

	var tarea = document.querySelector(".cancel-comment-button ~ form textarea");
	tarea.focus();
	var p0 = tarea.selectionStart;
	var p1 = tarea.selectionEnd;
	var str = tarea.value.substring(p0, p1);
	tarea.value = tarea.value.substring(0, p0)
		+ mopen + str + mclose
		+ tarea.value.substring(p1);
	return;
}

var guiEditButtons = [
	[ 'strong', 'Strong (bold)', 'k', '**', '**', '&#xf032;' ],
	[ 'em', 'Emphasized (italic)', 'i', '*', '*', '&#xf033;' ],
	[ 'link', 'Hyperlink', 'l', '[', '](link_url_goes_here)', '&#xf0c1;' ],
	[ 'heading1', 'Heading level 1', '', '# ', '', '&#xf1dc;' ],
	[ 'heading2', 'Heading level 2', '', '## ', '', '&#xf1dc;' ],
	[ 'heading3', 'Heading level 3', '', '### ', '', '&#xf1dc;' ],
	[ 'blockquote', 'Blockquote', '', '> ', '', '&#xf10e;' ]
];
