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
	[ 'strong', '**', '**', '&#xf032;' ],
	[ 'em', '*', '*', '&#xf033;' ]
];
