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

Element.prototype.addActivateEvent = function(func) {
	this.addEventListener("mouseup", func);
	this.addEventListener("keyup", func);
}

Element.prototype.injectReplyForm = function() {
	let e = this;
	let withparent = (e.parentElement.id != 'comments');
	e.innerHTML = "<button class='cancel-comment-button'>Cancel</button>" +
		"<form method='post'><textarea name='text'></textarea>" +
		(withparent ? "<input type='hidden' name='parent-comment-id' value='" + e.parentElement.id + "'>" : "") +
		"<input type='hidden' name='csrf-token' value='" + window.csrfToken + "'>" +
		"<span>You can use <a href='http://commonmark.org/help/' target='_blank'>Markdown</a> here.</span><input type='submit' value='Submit'></form>";
	
	e.querySelector(".cancel-comment-button").addActivateEvent(window.hideReplyForm);
	if(e.getBoundingClientRect().bottom > window.innerHeight) {
		e.scrollIntoView(false);
	}
	e.querySelector("textarea").focus();
	
	e.querySelector(".cancel-comment-button + form").insertAdjacentHTML("afterbegin", "<div class='guiedit-buttons-container'></div>")
	var buttons_container = e.querySelector(".guiedit-buttons-container");
	for (var button of guiEditButtons) {
		buttons_container.insertAdjacentHTML("beforeend", 
			"<button type='button' class='guiedit guiedit-" 
			+ button[0]
			+ "' title='"
			+ button[1] + ((button[2] != "") ? (" [accesskey: " + button[2] + "]") : "")
			+ "' accesskey='"
			+ button[2]
			+ "' onclick='insMarkup(\""
			+ button[3]
			+ "\",\""
			+ button[4]
			+ "\",\""
			+ button[5]
			+ "\");'>"
			+ button[6]
			+ "</button>"
		);
	}
}

Element.prototype.injectReplyButton = function() {
	let e = this;
	e.innerHTML = "";
	let button = e.appendChild(document.createElement("button"));
	if(e.parentElement.id == 'comments') {
		button.className="new-comment-button";
		button.innerHTML="Post new comment";
	} else {
		button.className="reply-button";
		button.innerHTML="Reply";
	}
	button.addActivateEvent(window.showReplyForm);
}

function showReplyForm(event) {
	let commentControls = event.target.parentElement;
	document.querySelectorAll(".comment-controls").forEach(function (e) {
		e.injectReplyButton();
	});

	commentControls.injectReplyForm();
}

function hideReplyForm(event) {
	event.target.parentElement.injectReplyButton();
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
				e.insertAdjacentHTML("afterend", "<div class='comment-controls'></div>");
				e.parentElement.querySelector(".comment-controls").injectReplyButton();
			});
			
			// Add top-level new comment form.
			document.querySelector("#comments").insertAdjacentHTML("afterbegin", "<div class='comment-controls'></div>");
			document.querySelector("#comments .comment-controls").injectReplyButton();

			needHashRealignment = true;
		}

		let h = location.hash;
		if(needHashRealignment && h) {
			location.hash = '';
			location.hash = h;
		}
	})
}, {once: true});
