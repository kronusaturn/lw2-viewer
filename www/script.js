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

Element.prototype.injectReplyForm = function(editMarkdownSource) {
	let e = this;
	let editCommentId = (editMarkdownSource ? e.parentElement.id : false);
	let withparent = (!editMarkdownSource && e.parentElement.id != 'comments');
	e.innerHTML = "<button class='cancel-comment-button' tabindex='-1'>Cancel</button>" +
		"<form method='post'><textarea name='text'></textarea>" +
		(withparent ? "<input type='hidden' name='parent-comment-id' value='" + e.parentElement.id + "'>" : "") +
		(editCommentId ? "<input type='hidden' name='edit-comment-id' value='" + editCommentId + "'>" : "") +
		"<input type='hidden' name='csrf-token' value='" + window.csrfToken + "'>" +
		"<span class='markdown-reference-link'>You can use <a href='http://commonmark.org/help/' target='_blank'>Markdown</a> here.</span><input type='submit' value='Submit'></form>";
	
	e.querySelector(".cancel-comment-button").addActivateEvent(window.hideReplyForm);
	if(e.getBoundingClientRect().bottom > window.innerHeight) {
		e.scrollIntoView(false);
	}
	e.querySelector("form").onsubmit = function(event) {
		if(!event.target.text.value) {
			alert("Please enter a comment.");
			return false;
		}
	}
	e.querySelector("textarea").value = (editMarkdownSource ? editMarkdownSource : "");
	e.querySelector("textarea").focus();
	e.querySelector("textarea").addEventListener("input", OnInputExpandTextarea, false);
	
	e.querySelector(".cancel-comment-button + form").insertAdjacentHTML("afterbegin", "<div class='guiedit-buttons-container'></div>")
	var buttons_container = e.querySelector(".guiedit-buttons-container");
	for (var button of guiEditButtons) {
		buttons_container.insertAdjacentHTML("beforeend", 
			"<button type='button' class='guiedit guiedit-" 
			+ button[0]
			+ "' tabindex='-1' title='"
			+ button[1] + ((button[2] != "") ? (" [accesskey: " + button[2] + "]") : "")
			+ "' data-tooltip='" + button[1]
			+ "' accesskey='"
			+ button[2]
			+ "' onclick='insMarkup("
			+ ((typeof button[3] == 'function') ?
				button[3].name : 
				("\"" + button[3]  + "\",\"" + button[4] + "\",\"" + button[5] + "\""))
			+ ");'>"
			+ button[6]
			+ "</button>"
		);
	}
	
	var markdown_hints = "<input type='checkbox' id='markdown-hints-checkbox'><label for='markdown-hints-checkbox'></label>";
	markdown_hints += "<div class='markdown-hints'>";
	markdown_hints += "<div class='markdown-hints-row'><span style='font-weight: bold;'>Bold</span><code>**Bold**</code></div>";
	markdown_hints += "<div class='markdown-hints-row'><span style='font-style: italic;'>Bold</span><code>*Italic*</code></div>";
	markdown_hints += "<div class='markdown-hints-row'><span><a href=#>Link</a></span><code>[Link](http://example.com)</code></div>";
	markdown_hints += "</div>";
	e.querySelector("form span").insertAdjacentHTML("afterend", markdown_hints);
}

Element.prototype.injectCommentButtons = function() {
	let e = this;
	e.innerHTML = "";
	let replyButton = document.createElement("button");
	if(e.parentElement.id == 'comments') {
		replyButton.className="new-comment-button action-button";
		replyButton.innerHTML="Post new comment";
	} else {
		if(e.parentElement.querySelector(".comment-body").hasAttribute("data-markdown-source")) {
			let editButton = e.appendChild(document.createElement("button"));
			editButton.className="edit-button action-button";
			editButton.innerHTML="Edit";
			editButton.addActivateEvent(window.showCommentEditForm);
		}
		replyButton.className="reply-button action-button";
		replyButton.innerHTML="Reply";
	}
	e.appendChild(replyButton);
	replyButton.tabIndex = '-1';
	replyButton.addActivateEvent(window.showReplyForm);
}

function showCommentEditForm(event) {
	let commentControls = event.target.parentElement;
	let commentBody = commentControls.parentElement.querySelector(".comment-body");
	commentBody.setAttribute("style", "display: none;");
	commentControls.injectReplyForm(commentBody.getAttribute("data-markdown-source"));
	ExpandTextarea(commentControls.querySelector("textarea"));
}

function showReplyForm(event) {
	let commentControls = event.target.parentElement;
	document.querySelectorAll(".comment-controls").forEach(function (e) {
		e.injectCommentButtons();
	});

	commentControls.injectReplyForm();
}

function hideReplyForm(event) {
	try { event.target.parentElement.parentElement.querySelector(".comment-body").removeAttribute("style"); }
	catch (e) { }
	event.target.parentElement.injectCommentButtons();
}

function OnInputExpandTextarea() {
	ExpandTextarea(this);
}
function ExpandTextarea(textarea) {
	textarea.style.height = 'auto';
	textarea.style.height = textarea.scrollHeight + 30 + 'px';
}

document.addEventListener("DOMContentLoaded", function() {
	window.requestAnimationFrame(function() {
		var content = document.querySelector("#content");
		if (content.clientHeight <= window.innerHeight + 30) {
			removeElement("#bottom-bar", content);
			removeElement(".post .post-meta a[href='#bottom-bar']", content);
		} 	
		if (content.clientHeight <= window.innerHeight + 30 || 
			(content.querySelector("#comments") && content.querySelector("#comments").childNodes.length == 0)) {
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
				e.insertAdjacentHTML('beforebegin', "<button type='button' class='vote vote-up' tabindex='-1'></button>");
				e.insertAdjacentHTML('afterend', "<button type='button' class='vote vote-down' tabindex='-1'></button>");
			});
			
			var comments_container = document.querySelector("#comments");
			if (comments_container) {
				// Add reply buttons.
				comments_container.querySelectorAll(".comment").forEach(function (e) {
					e.insertAdjacentHTML("afterend", "<div class='comment-controls'></div>");
					e.parentElement.querySelector(".comment-controls").injectCommentButtons();
				});
			
				// Add top-level new comment form.
				comments_container.insertAdjacentHTML("afterbegin", "<div class='comment-controls'></div>");
				comments_container.querySelector(".comment-controls").injectCommentButtons();
			}			

			needHashRealignment = true;
		}

		let h = location.hash;
		if(needHashRealignment && h) {
			location.hash = '';
			location.hash = h;
		}
	})
}, {once: true});

function removeElement(selector, ancestor = document) {
	var element = ancestor.querySelector(selector);
	if (element) element.parentElement.removeChild(element);
}