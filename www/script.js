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
	e.outerHTML = "<button class='cancel-comment-button'>Cancel</button>";
	e.insertAdjacentHTML(
		"<form method='post'><textarea name='text'></textarea>" +
		(withparent ? "<input type='hidden' name='parent-comment-id' value='" + e.parentElement.parentElement.id + "'>" : "") +
		"<input type='hidden' name='csrf-token' value='" + window.csrfToken + "'>" +
		"<span>You can use <a href='http://commonmark.org/help/' target='_blank'>Markdown</a> here.</span><input type='submit' value='Submit'></form>"
		);
	if(withparent) {
		e.parentElement.querySelector("textarea").focus();
	}
	e.addEventListener("mouseup", window.hideReplyForm);
	e.addEventListener("keyup", window.hideReplyForm);
}

function removeReplyForm(e) {
	e.parentElement.removeChild(e.parentElement.querySelector("form"));
	e.outerHTML = "<button class='reply-button'>Reply</button>";
}

function showReplyForm(event) {
	document.querySelectorAll(".comment-controls form").forEach(function (e) {
		removeReplyForm(e.parentElement.querySelector(".cancel-comment-button"));
	});

	injectReplyForm(event.target, (event.target.parentElement.parentElement.id == 'comments' ? false : true));
}

function hideReplyForm(event) {
	removeReplyForm(event.target);
}

document.addEventListener("DOMContentLoaded", function() {
	window.requestAnimationFrame(function() {
		var content = document.querySelector("#content");
		if (content.clientHeight <= window.innerHeight + 30) {
			content.removeChild(document.querySelector("#bottom-bar"));
			document.querySelector(".post .post-meta").removeChild(document.querySelector(".post .post-meta a[href='#bottom-bar']"));
		} 	
		if (content.clientHeight <= window.innerHeight + 30 || 
			(document.querySelector("#comments") && document.querySelector("#comments").childNodes.length == 0)) {
			document.styleSheets[1].insertRule('.post .post-meta .comment-count::after { display: none; }', document.styleSheets[1].cssRules.length);
		}

		let needHashRealignment = false;

		let urlParts = document.URL.split('#');
		if (urlParts.length > 1) {
			try { document.querySelector('#'+urlParts[1]).closest("label[for^='expand'] + .comment-thread").parentElement.querySelector("input[id^='expand']").checked = true; }
			catch (e) { }
			needHashRealignment = true;
		}

		if(readCookie("lw2-auth-token")) {
			// Add upvote/downvote buttons.
			document.querySelectorAll(".comment-meta .karma").forEach(function (e) {
				e.insertAdjacentHTML('beforebegin', "<button type='button' class='vote vote-up'></button>");
				e.insertAdjacentHTML('afterend', "<button type='button' class='vote vote-down'></button>");
			});
			
			// Add reply buttons.
			document.querySelectorAll("#comments .comment").forEach(function (e) {
				e.insertAdjacentHTML("afterend", "<div class='comment-controls'><button class='reply-button'>Reply</button></div>");				

				e.parentElement.querySelector(".reply-button").addEventListener("mouseup", window.showReplyForm);
				e.parentElement.querySelector(".reply-button").addEventListener("keyup", window.showReplyForm);
			});
			
			// Add top-level new comment form.
			document.querySelector("#comments").insertAdjacentHTML("afterbegin", "<div class='comment-controls'><button class='new-comment-button'>Post new comment</button></div>");
			document.querySelector("#comments .new-comment-button").addEventListener("mouseup", window.showReplyForm);
			document.querySelector("#comments .new-comment-button").addEventListener("keyup", window.showReplyForm);

			needHashRealignment = true;
		}

		let h = location.hash;
		if(needHashRealignment && h) {
			location.hash = '';
			location.hash = h;
		}
	})
}, {once: true});
