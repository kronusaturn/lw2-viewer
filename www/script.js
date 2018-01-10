document.addEventListener("DOMContentLoaded", function() {
	if(document.getElementById("content").clientHeight <= window.innerHeight + 30) {
		let e = document.getElementById("bottom-bar");
		e.parentNode.removeChild(e);
	}
	let urlParts = document.URL.split('#');
	if (urlParts.length > 1) {
		document.querySelector('#'+urlParts[1]).closest("label[for^='expand'] + .comment-thread").parentElement.querySelector("input[id^='expand']").checked = true;
		location.hash = '';
		location.hash = urlParts[1];
	}
});
