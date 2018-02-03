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

Element.prototype.addClass = function(className) {
	if (!this.className.match(new RegExp("(^|\\s)" + className + "(\\s|$)")))
		this.className += " " + className;
}
Element.prototype.removeClass = function(className) {
	this.className = this.className.replace(new RegExp("(^|\\s)" + className + "(\\s|$)"), "");
}

Element.prototype.addActivateEvent = function(func, waitForMouseUp = true) {
	this.addEventListener((waitForMouseUp ? "mouseup" : "mousedown"), func);
	this.addEventListener("keyup", func);
}

Element.prototype.removeActivateEvent = function(func) {
	this.removeEventListener("mousedown", func);
	this.removeEventListener("mouseup", func);
	this.removeEventListener("keyup", func);
}

Element.prototype.scrollIntoViewIfNeeded = function() {
	if(this.getBoundingClientRect().bottom > window.innerHeight) {
		this.scrollIntoView(false);
	}
}

Element.prototype.getCommentId = function() {
	let item = (this.className == "comment-item" ? this : this.closest(".comment-item"));
	if(item) {
		return /^comment-(.*)/.exec(item.id)[1];
	} else {
		return false;
	}
}

Element.prototype.addTextareaFeatures = function() {
	let textarea = this;

	textarea.addEventListener("focus", function(e){e.target.parentElement.parentElement.scrollIntoViewIfNeeded()});
	textarea.addEventListener("scroll", OnInputExpandTextarea, false);
	textarea.addEventListener("keyup", function(e){e.stopPropagation()});
	
	textarea.insertAdjacentHTML("beforebegin", "<div class='guiedit-buttons-container'></div>");
	var buttons_container = textarea.parentElement.querySelector(".guiedit-buttons-container");
	for (var button of guiEditButtons) {
		buttons_container.insertAdjacentHTML("beforeend", 
			"<button type='button' class='guiedit guiedit-" 
			+ button[0]
			+ "' tabindex='-1' title='"
			+ button[1] + ((button[2] != "") ? (" [accesskey: " + button[2] + "]") : "")
			+ "' data-tooltip='" + button[1]
			+ "' accesskey='"
			+ button[2]
			+ "' onclick='insMarkup(event,"
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
	markdown_hints += "<div class='markdown-hints-row'><span style='font-style: italic;'>Italic</span><code>*Italic*</code></div>";
	markdown_hints += "<div class='markdown-hints-row'><span><a href=#>Link</a></span><code>[Link](http://example.com)</code></div>";
	markdown_hints += "<div class='markdown-hints-row'><span>Heading 1</span><code># Heading 1</code></div>";
	markdown_hints += "<div class='markdown-hints-row'><span>Heading 2</span><code>## Heading 1</code></div>";
	markdown_hints += "<div class='markdown-hints-row'><span>Heading 3</span><code>### Heading 1</code></div>";
	markdown_hints += "<div class='markdown-hints-row'><span>Blockquote</span><code>&gt; Blockquote</code></div>";
	markdown_hints += "</div>";
	textarea.parentElement.querySelector("span").insertAdjacentHTML("afterend", markdown_hints);
}

Element.prototype.injectReplyForm = function(editMarkdownSource) {
	let e = this;
	let editCommentId = (editMarkdownSource ? e.getCommentId() : false);
	let withparent = (!editMarkdownSource && e.getCommentId());
	e.innerHTML = "<button class='cancel-comment-button' tabindex='-1'>Cancel</button>" +
		"<form method='post'><textarea name='text'></textarea>" +
		(withparent ? "<input type='hidden' name='parent-comment-id' value='" + e.getCommentId() + "'>" : "") +
		(editCommentId ? "<input type='hidden' name='edit-comment-id' value='" + editCommentId + "'>" : "") +
		"<input type='hidden' name='csrf-token' value='" + window.csrfToken + "'>" +
		"<span class='markdown-reference-link'>You can use <a href='http://commonmark.org/help/' target='_blank'>Markdown</a> here.</span><input type='submit' value='Submit'></form>";
	
	e.querySelector(".cancel-comment-button").addActivateEvent(window.hideReplyForm);
	e.scrollIntoViewIfNeeded();
	e.querySelector("form").onsubmit = function(event) {
		if(!event.target.text.value) {
			alert("Please enter a comment.");
			return false;
		}
	}
	let textarea = e.querySelector("textarea");
	textarea.value = (editMarkdownSource ? editMarkdownSource : "");
	textarea.addTextareaFeatures();
	textarea.focus();
}

Element.prototype.injectCommentButtons = function() {
	let e = this;
	e.innerHTML = "";
	let replyButton = document.createElement("button");
	if (e.parentElement.id == 'comments') {
		replyButton.className = "new-comment-button action-button";
		replyButton.innerHTML = "Post new comment";
	} else {
		if (e.parentElement.querySelector(".comment-body").hasAttribute("data-markdown-source")) {
			let editButton = e.appendChild(document.createElement("button"));
			editButton.className = "edit-button action-button";
			editButton.innerHTML = "Edit";
			editButton.tabIndex = '-1';
			editButton.addActivateEvent(window.showCommentEditForm);
		}
		replyButton.className = "reply-button action-button";
		replyButton.innerHTML = "Reply";
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
	window.requestAnimationFrame(function() {
		textarea.style.height = 'auto';
		textarea.style.height = textarea.scrollHeight + 30 + 'px';
		textarea.parentElement.parentElement.scrollIntoViewIfNeeded();
	});
}

function makeVoteCompleteEvent(buttonTarget, karmaTarget) {
	return function(e) {
		buttonTarget.parentNode.querySelectorAll("button.vote").forEach(function(b) { b.style.pointerEvents = "" });
		buttonTarget.parentNode.querySelectorAll(".karma").forEach(function(x) { x.style.opacity = "" });
		if(e.target.status == 200) {
			let res = JSON.parse(e.target.responseText);
			let karmaText = res[0], voteType = res[1];
			karmaTarget.innerHTML = karmaText;
			buttonTarget.parentNode.querySelectorAll("button.vote").forEach(function(b) {
				b.className = 'vote ' + b.getAttribute("data-vote-type") + ((b.getAttribute('data-vote-type') == voteType) ? ' selected' : '');
			});
		}
	}
}

function sendVoteRequest(targetId, targetType, voteType, onFinish) {
	let req = new XMLHttpRequest();
	req.addEventListener("load", onFinish);
	req.open("POST", "/karma-vote");
	req.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");
	req.send("csrf-token="+encodeURIComponent(csrfToken)+"&target="+encodeURIComponent(targetId)+"&target-type="+encodeURIComponent(targetType)+"&vote-type="+encodeURIComponent(voteType));
}

function voteEvent(e) {
	e.target.blur();
	e.target.parentNode.querySelectorAll("button.vote").forEach(function(b) { b.style.pointerEvents = "none" });
	e.target.parentNode.querySelectorAll(".karma").forEach(function(x) { x.style.opacity = "0.5" });
	let targetType = e.target.getAttribute("data-target-type");
	let targetId = ((targetType == 'Comments') ? e.target.getCommentId() : e.target.parentNode.getAttribute("data-post-id"));
	let voteType = e.target.getAttribute("data-vote-type");
	let oldVoteType;
	if(targetType == "Posts") {
		oldVoteType = postVote;
		postVote = ((voteType == oldVoteType) ? null : voteType);
	} else {
		oldVoteType = commentVotes[targetId];
		commentVotes[targetId] = ((voteType == oldVoteType) ? null : voteType);
	}
	let f = function() { sendVoteRequest(targetId, targetType, voteType, makeVoteCompleteEvent(e.target, e.target.parentNode.querySelector(".karma-value"))) };
	if(oldVoteType && (oldVoteType != voteType)) {
		sendVoteRequest(targetId, targetType, oldVoteType, f);
	} else {
		f();
	}
}

function commentMinimizeButtonClicked(event) {
	event.target.closest(".comment-item").setCommentThreadMaximized(true);
}
Element.prototype.setCommentThreadMaximized = function(toggle, userOriginated = true) {
	let ci = this;
	let storageName = "thread-minimized-" + ci.getCommentId();
	let minimize_button = ci.querySelector(".comment-minimize-button");
	let maximize = (toggle ? /minimized/.test(minimize_button.className) : !window.localStorage.getItem(storageName));
	if (userOriginated) {
		if (maximize) {
			window.localStorage.removeItem(storageName);
		} else {
			window.localStorage.setItem(storageName, true);
		}
	}

	ci.style.height = maximize ? 'auto' : '38px';
	ci.style.overflow = maximize ? 'visible' : 'hidden';

	minimize_button.className = "comment-minimize-button " + (maximize ? "maximized" : "minimized");
	minimize_button.innerHTML = maximize ? "&#xf146;" : "&#xf0fe;";
	minimize_button.title = (maximize ? "Collapse" : "Expand") + 
							" comment thread (" + 
							minimize_button.dataset["childCount"] + 
							" child comments)";
}

Element.prototype.getCommentDate = function() {
	let item = (this.className == "comment-item") ? this : this.closest(".comment-item");
	return (item ? parseInt(item.querySelector(".date").dataset["jsDate"]) : false);
}
function getCurrentVisibleComment() {
	let px = window.innerWidth/2, py = window.innerHeight/10;
	let ci = document.elementFromPoint(px, py).closest(".comment-item") || document.elementFromPoint(px, py+60).closest(".comment-item"); // Mind the gap between threads
	let atbottom = document.querySelector("#comments").getBoundingClientRect().bottom < window.innerHeight;
	if(atbottom) {
		let hashci = location.hash && document.querySelector(location.hash);
		if(hashci && /comment-item/.test(hashci.className) && hashci.getBoundingClientRect().top > 0) {
			ci = hashci;
		}
	}
	return ci;
}

function highlightCommentsSince(date) {
	var newCommentsCount = 0;
	window.newComments = [ ];
	let oldCommentsStack = [ ];
	let prevNewComment;
	document.querySelectorAll(".comment-item").forEach(function (ci) {
		ci.prevNewComment = prevNewComment;
		if (ci.getCommentDate() > date) {
			ci.addClass("new-comment");
			newCommentsCount++;
			window.newComments.push(ci.getCommentId());
			oldCommentsStack.forEach(function (oldci) { oldci.nextNewComment = ci });
			oldCommentsStack = [ ci ];
			prevNewComment = ci;
		} else {
			ci.removeClass("new-comment");
			oldCommentsStack.push(ci);
		}
	});
	window.scrollListener = function(e) {
		window.requestAnimationFrame(function () {
			let ci = getCurrentVisibleComment();
			if(ci) {
				document.querySelector(".post-meta .new-comment-previous").disabled = !ci.prevNewComment;
				document.querySelector(".post-meta .new-comment-next").disabled = !ci.nextNewComment;
			} else {
				document.querySelector(".post-meta .new-comment-previous").disabled = true;
				document.querySelector(".post-meta .new-comment-next").disabled = (window.newComments.length == 0);
			}
			document.addEventListener("scroll", scrollListener, {once: true, passive: true});
		});
	}

	scrollListener();

	return newCommentsCount;
}

function scrollToNewComment(next) {
	let ci = getCurrentVisibleComment();
	if (ci) {
		let targetComment = (next ? ci.nextNewComment : ci.prevNewComment);
		if (targetComment) {
			location.hash = "comment-" + targetComment.getCommentId();
			expandAncestorsOf(location.hash);
		}
	} else {
		if (window.newComments[0]) {
			location.hash = "comment-" + window.newComments[0];
			expandAncestorsOf(location.hash);
		}
		realignHash();
	}
	scrollListener();
}

function commentQuicknavButtonClicked(event) {
	scrollToNewComment(/next/.test(event.target.className));
}

function getPostHash() {
	let postHash = /^\/posts\/([^\/]+)/.exec(location.pathname);
	return (postHash ? postHash[1] : false);
}
function getLastVisitedDate() {
	let storageName = "last-visited-date_" + getPostHash();
	return window.localStorage.getItem(storageName);
}
function setLastVisitedDate(date) {
	let storageName = "last-visited-date_" + getPostHash();
	window.localStorage.setItem(storageName, date);
}

function injectContentWidthSelector() {
	document.querySelector("head").insertAdjacentHTML("beforeend", "<style id='width-adjust'></style>");
	let widthSelector = addUIElement(
		"<div id='width-selector'>" + 
		"<button type='button' class='select-width-normal selected' title='Narrow (fixed-width) content column' tabindex='-1'>N</button>" + 
		"<button type='button' class='select-width-wide' title='Wide (fluid) content column' tabindex='-1'>W</button>" + 
		"</div>");
	widthSelector.querySelectorAll("button").forEach(function (button) {
		button.addActivateEvent(widthAdjustButtonClicked);
	});
}
function widthAdjustButtonClicked(event) {
	setContentWidth((event.target.className == "select-width-normal" ? "900px" : "(100vw - 300px)"));
	event.target.parentElement.childNodes.forEach(function (button) {
		button.removeClass("selected");
		button.disabled = false;
	});
	event.target.addClass("selected");
	event.target.disabled = true;
}
function setContentWidth(widthString) {
	let widthAdjustStyle = document.querySelector("#width-adjust");
	widthAdjustStyle.innerHTML = 
		`#content { 
			max-width: calc(${widthString});
		}
		#bottom-bar a[href='#top']::after, 
		.post-meta a[href='#comments']::after, 
		.post-meta a[href='#bottom-bar']::after {
			right: calc((100vw - ${widthString}) / 2 - 75px);
		}
		.post-meta .new-comments-count {
			right: calc((100vw - ${widthString}) / 2 - 139px);
		}
		.post-meta .new-comment-sequential-nav-button {
			right: calc((100vw - ${widthString}) / 2 - 148px);
		}
		#width-selector {
			right: calc((100% - ${widthString}) / 2 - 50px);
		}`;
		
	let themeSelectStyle = document.querySelector("#theme-select");
	themeSelectStyle.innerHTML = 
		`#theme-selector {
			left: calc((100% - 900px) / 2 - 41px);
		}`;
}

function injectThemeSelector() {
	document.querySelector("head").insertAdjacentHTML("beforeend", "<style id='theme-select'></style>");
	let themeSelector = addUIElement(
		"<div id='theme-selector'>" + 
		"<button type='button' class='select-theme-default selected' title='Default theme (dark text on light background)' tabindex='-1'>A</button>" + 
		"<button type='button' class='select-theme-dark' title='Dark theme (light text on dark background)' tabindex='-1'>A</button>" + 
		"</div>");
	themeSelector.querySelectorAll("button").forEach(function (button) {
		button.addActivateEvent(themeSelectButtonClicked);
	});
}
function themeSelectButtonClicked(event) {
	setTheme(/select-theme-([^\s]+)/.exec(event.target.className)[1]);
	event.target.parentElement.childNodes.forEach(function (button) {
		button.removeClass("selected");
		button.disabled = false;
	});
	event.target.addClass("selected");
	event.target.disabled = true;
}
function setTheme(themeName) {
	let styleSheetNameSuffix = (themeName == 'default') ? '' : '-dark';
	let currentStyleSheetNameComponents = /style[^\.]*(\..+)$/.exec(document.querySelector("head link[href*='.css']").href);
	document.querySelector("head link[href*='.css']").href = "/style" + styleSheetNameSuffix + currentStyleSheetNameComponents[2];
}

function expandAncestorsOf(commentId) {
	try { document.querySelector('#'+commentId).closest("label[for^='expand'] + .comment-thread").parentElement.querySelector("input[id^='expand']").checked = true; }
	catch (e) { }
	try { document.querySelector('#'+commentId).closest("#comments > ul > li").setCommentThreadMaximized(true, false); }
	catch (e) { }
}

function getQueryVariable(variable)
{
	var query = window.location.search.substring(1);
	var vars = query.split("&");
	for (var i = 0; i < vars.length; i++) {
		var pair = vars[i].split("=");
		if(pair[0] == variable)
			return pair[1];
	}
	
	return false;
}

function addUIElement(element_html) {
	var ui_elements_container = document.querySelector("#ui-elements-container");
	if (!ui_elements_container) {
		ui_elements_container = document.createElement("div");
		ui_elements_container.id = "ui-elements-container";
		document.querySelector("body").appendChild(ui_elements_container);
	}
	
	ui_elements_container.insertAdjacentHTML("beforeend", element_html);
	return ui_elements_container.lastElementChild;
}

function initialize() {
	window.requestAnimationFrame(function() {
		if(location.hash.length == 18) {
			location.hash = "#comment-" + location.hash.substring(1);
		}
		var content = document.querySelector("#content");
		if (content.clientHeight <= window.innerHeight + 30) {
			removeElement("#bottom-bar", content);
			removeElement(".post .post-meta a[href='#bottom-bar']", content);
		} 	
		if (content.clientHeight <= window.innerHeight + 30 || 
			(content.querySelector("#comments") && content.querySelector("#comments").childNodes.length == 0)) {
			document.styleSheets[1].insertRule('.post .post-meta .comment-count::after { display: none; }', document.styleSheets[1].cssRules.length);
		}

		try {
			let dtf = new Intl.DateTimeFormat([], {month: 'short', day: 'numeric', year: 'numeric', hour: 'numeric', minute: 'numeric'});
			document.querySelectorAll(".date").forEach(function (e) {
				let d = e.getAttribute("data-js-date");
				if(d) { e.innerHTML = dtf.format(new Date(+ d)); }
			});
		}
		catch(e) { }

		window.needHashRealignment = false;

		let urlParts = document.URL.split('#');
		if (urlParts.length > 1) {
			expandAncestorsOf(urlParts[1]);
			window.needHashRealignment = true;
		}

		document.querySelectorAll(".comment-meta .comment-parent-link, .comment-meta .comment-child-links a").forEach(function (cpl) {
			cpl.addEventListener("mouseover", function(e) {
				let parent = document.querySelector(cpl.getAttribute("href")).firstChild;
				let parentCI = parent.parentNode;
				var highlight_cn;
				if(parent.getBoundingClientRect().bottom < 10 || parent.getBoundingClientRect().top > window.innerHeight + 10) {
					highlight_cn = "comment-item-highlight-faint";
					parent = parent.cloneNode(true);
					parent.addClass("comment-popup")
					parent.addClass("comment-item-highlight");
					cpl.addEventListener("mouseout", function(e) {
						parent.parentNode.removeChild(parent);
					}, {once: true});
					cpl.parentNode.parentNode.appendChild(parent);
				} else {
					highlight_cn = "comment-item-highlight";
				}
				let cn = parentCI.className;
				parentCI.className = cn + " " + highlight_cn;
				cpl.addEventListener("mouseout", function(e) { parentCI.className = cn; }, {once: true});
			});
		});

		document.querySelectorAll("#edit-post-form textarea").forEach(function (textarea) { textarea.addTextareaFeatures(); });
		document.querySelectorAll((getQueryVariable("post-id")) ? "#edit-post-form textarea" : "#edit-post-form input[name='title']").forEach(function (field) { field.focus(); });

		if(readCookie("lw2-auth-token")) {
			// Add upvote/downvote buttons.
			if(typeof(postVote) != 'undefined') {
				let e = document.querySelector(".post-meta .karma-value");
				let voteType = postVote;
				e.insertAdjacentHTML('beforebegin', "<button type='button' class='vote upvote"+(voteType=='upvote'?' selected':'')+"' data-vote-type='upvote' data-target-type='Posts' tabindex='-1'></button>");
				e.insertAdjacentHTML('afterend', "<button type='button' class='vote downvote"+(voteType=='downvote'?' selected':'')+"' data-vote-type='downvote' data-target-type='Posts' tabindex='-1'></button>");
			}
			if(typeof(commentVotes) != 'undefined') {
				document.querySelectorAll(".comment-meta .karma-value").forEach(function (e) {
					let cid = e.getCommentId();
					let voteType = commentVotes[cid];
					e.insertAdjacentHTML('beforebegin', "<button type='button' class='vote upvote"+(voteType=='upvote'?' selected':'')+"' data-vote-type='upvote' data-target-type='Comments' tabindex='-1'></button>");
					e.insertAdjacentHTML('afterend', "<button type='button' class='vote downvote"+(voteType=='downvote'?' selected':'')+"' data-vote-type='downvote' data-target-type='Comments' tabindex='-1'></button>");
				});
			}
			document.querySelectorAll("button.vote").forEach(function(e) {
				e.addActivateEvent(voteEvent);
			});

			var comments_container = document.querySelector("#comments");
			if (comments_container) {
				// Add reply buttons.
				comments_container.querySelectorAll(".comment").forEach(function (e) {
					e.insertAdjacentHTML("afterend", "<div class='comment-controls posting-controls'></div>");
					e.parentElement.querySelector(".comment-controls").injectCommentButtons();
				});
			
				// Add top-level new comment form.
				comments_container.insertAdjacentHTML("afterbegin", "<div class='comment-controls posting-controls'></div>");
				comments_container.querySelector(".comment-controls").injectCommentButtons();
			}			

			window.needHashRealignment = true;
		}

		// Clean up ToC
		document.querySelectorAll(".contents-list li a").forEach(function (a) {
			a.innerText = a.innerText.replace(/^[0-9]+\. /, '');
			a.innerText = a.innerText.replace(/^M{0,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})\. /i, '');
			a.innerText = a.innerText.replace(/^[A-Z]\. /, '');
		});

		if(document.readyState != "complete") {
			document.addEventListener("load", whenLoaded, {once: true});
		} else {
			whenLoaded();
		}
		
		// Format and activate comment-minimize buttons.
		document.querySelectorAll(".comment-minimize-button").forEach(function (b) {
			b.closest(".comment-item").setCommentThreadMaximized(false);
			b.addActivateEvent(commentMinimizeButtonClicked);
		});
		
		// Read and update last-visited-date.
		if(getPostHash()) {
			let lastVisitedDate = getLastVisitedDate();
			setLastVisitedDate(Date.now());

			// Highlight new comments (as specified by URL parameter, if present, or otherwise
			// all the new ones since last visit).
			let hns = parseInt(getQueryVariable("hns"));
			let newCommentsCount = highlightCommentsSince(hns || lastVisitedDate);

			// Add the new comments count & navigator.
			document.querySelector(".post .post-meta").insertAdjacentHTML("beforeend", 
				("<button type='button' class='new-comment-sequential-nav-button new-comment-previous' title='Previous new comment [,]' tabindex='-1' accesskey=',' disabled>&#xf0d8;</button> " + 
				 "<span class='new-comments-count' title='" + newCommentsCount + " new comments'>" + newCommentsCount + "</span> " +
				 "<button type='button' class='new-comment-sequential-nav-button new-comment-next' title='Next new comment [.]' tabindex='-1' accesskey='.'" + 
				 (newCommentsCount == 0 ? " disabled" : "") + 
				 ">&#xf0d7;</button>"));
			if (newCommentsCount > 0) {
				document.querySelector(".post-meta .new-comment-previous").addActivateEvent(commentQuicknavButtonClicked);
				document.querySelector(".post-meta .new-comment-next").addActivateEvent(commentQuicknavButtonClicked);
				document.querySelector(".post-meta .new-comment-previous").dataset['targetComment'] = -2;
				document.querySelector(".post-meta .new-comment-next").dataset['targetComment'] = 0;

				document.addEventListener("keyup", function(e) { if(e.key == ",") scrollToNewComment(false); if(e.key == ".") scrollToNewComment(true)});
			}
		}
		
		// Add the content width selector.
		injectContentWidthSelector();
	})
}

function whenLoaded() {
	window.requestAnimationFrame(function () {
		if (window.needHashRealignment)
			realignHash();
	});
}
function realignHash() {
	let h = location.hash;
	if (h)
		document.querySelector(h).scrollIntoView(true);
}

function removeElement(selector, ancestor = document) {
	var element = ancestor.querySelector(selector);
	if (element) element.parentElement.removeChild(element);
}

if(document.readyState == "loading") {
	document.addEventListener("DOMContentLoaded", initialize, {once: true});
} else {
	initialize();
}
