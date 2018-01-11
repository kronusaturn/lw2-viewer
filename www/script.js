document.addEventListener("DOMContentLoaded", function() {
	window.requestAnimationFrame(function() {
		var content = document.querySelector("#content");
		if (content.clientHeight <= window.innerHeight + 30) {
			content.removeChild(document.querySelector("#bottom-bar"));
			document.querySelector(".post .post-meta").removeChild(document.querySelector(".post .post-meta a[href='#bottom-bar']"));
		} 	
		if (content.clientHeight <= window.innerHeight + 30 || 
			document.querySelector("#comments").childNodes.length == 0) {
			document.styleSheets[1].insertRule('.post .post-meta .comment-count::after { display: none; }', document.styleSheets[1].cssRules.length);
		}

		let urlParts = document.URL.split('#');
		if (urlParts.length > 1) {
			try { document.querySelector('#'+urlParts[1]).closest("label[for^='expand'] + .comment-thread").parentElement.querySelector("input[id^='expand']").checked = true; }
			catch (e) { }
			location.hash = '';
			location.hash = urlParts[1];
		}
	})
});
