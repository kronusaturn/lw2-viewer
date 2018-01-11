function readCookie(name) {
	var nameEQ = name + "=";
	var ca = document.cookie.split(';');
	for(var i=0;i < ca.length;i++) {
		var c = ca[i];
		while (c.charAt(0)==' ') c = c.substring(1,c.length);
		if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length,c.length);
	}
	return null;
}

function injectReplyForm(e, withparent) {
	e.innerHTML = '<form method="post"><textarea name="text"></textarea>' +
		(withparent ? '<input type="hidden" name="parent-comment-id" value="'+e.parentElement.parentElement.id+'">':'') +
		'<input type="hidden" name="csrf-token" value="'+window.csrfToken+'"><input type="submit" value="Submit"></form>';
	if(withparent) {
		e.querySelector("textarea").focus();
	}
}

function showReplyForm(event) {
	injectReplyForm(event.target.parentElement, true);
}

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

		if(readCookie("lw2-auth-token")) {
			document.querySelectorAll("#comments .comment").forEach(function(e) {
				let r = document.createElement("div");
				r.className = "comment-controls";
				r.innerHTML = '<div class="comment-reply-container"><button class="reply-button">Reply</button></div>';
				e.insertAdjacentElement("afterend", r);
				r.querySelector(".reply-button").addEventListener("mouseup", window.showReplyForm);
				r.querySelector(".reply-button").addEventListener("keyup", window.showReplyForm);
			});
			let r = document.createElement("div");
			r.className = "comment-reply-container";
			document.querySelector("#comments").insertAdjacentElement("afterbegin", r);
			injectReplyForm(r, false);
		}
	})
}, {once: true});
