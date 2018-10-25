/***************************/
/* INITIALIZATION REGISTRY */
/***************************/

if (!window.requestIdleCallback) {
	window.requestIdleCallback = (fn) => { window.setTimeout(fn, 0) };
}

var initializersDone = {};
var initializers = {};
function registerInitializer(name, tryEarly, precondition, fn) {
	initializersDone[name] = false;
	initializers[name] = fn;
	let wrapper = function () {
		if (initializersDone[name]) return;
		if (!precondition()) {
			if (tryEarly) {
				window.setTimeout(() => window.requestIdleCallback(wrapper, {timeout: 1000}), 50);
			} else {
				document.addEventListener("readystatechange", wrapper, {once: true});
			}
			return;
		}
		initializersDone[name] = true;
		fn();
	};
	if (tryEarly) {
		window.requestIdleCallback(wrapper, {timeout: 1000});
	} else {
		document.addEventListener("readystatechange", wrapper, {once: true});
		window.requestIdleCallback(wrapper);
	}
}
function forceInitializer(name) {
	if (initializersDone[name]) return;
	initializersDone[name] = true;
	initializers[name]();
}

/***********/
/* COOKIES */
/***********/

function setCookie(name, value, days) {
	var expires = "";
	if (!days) days = 36500;
	if (days) {
		var date = new Date();
		date.setTime(date.getTime() + (days*24*60*60*1000));
		expires = "; expires=" + date.toUTCString();
	}
	document.cookie = name + "=" + (value || "")  + expires + "; path=/";
}
function readCookie(name) {
	var nameEQ = name + "=";
	var ca = document.cookie.split(';');
	for(var i = 0; i < ca.length; i++) {
		var c = ca[i];
		while (c.charAt(0)==' ') c = c.substring(1, c.length);
		if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length, c.length);
	}
	return null;
}

/****************************************************/
/* CSS CLASS MANIPULATION (polyfill for .classList) */
/****************************************************/

Element.prototype.addClass = function(className) {
	if (!this.hasClass(className))
		this.className += " " + className;
}
Element.prototype.addClasses = function(classNames) {
	let element = this;
	let elementClassNames = this.className.trim().split(/\s/);
	
	classNames.forEach(className => {
		if (!element.hasClass(className))
			elementClassNames.push(className);
	});
	
	this.className = elementClassNames.join(" ");
}
Element.prototype.removeClass = function(className) {
	this.className = this.className.replace(new RegExp("(^|\\s+)" + className + "(\\s+|$)"), "$1").trim();
}
Element.prototype.removeClasses = function(classNames) {
	let elementClassNames = this.className;
	classNames.forEach(className => {
		elementClassNames = elementClassNames.replace(new RegExp("(^|\\s+)" + className + "(\\s+|$)"), "$1").trim();
	});
	this.className = elementClassNames;
}
Element.prototype.hasClass = function(className) {
	return (new RegExp("(^|\\s+)" + className + "(\\s+|$)")).test(this.className);
}
Element.prototype.toggleClass = function(className) {
	if (this.hasClass(className))
		this.removeClass(className);
	else
		this.addClass(className);
}

/*******************************/
/* EVENT LISTENER MANIPULATION */
/*******************************/

Element.prototype.addActivateEvent = function(func, includeMouseDown) {
	let ael = this.activateEventListener = (event) => { if (event.button === 0 || event.key === ' ') func(event) };
	if (includeMouseDown) this.addEventListener("mousedown", ael);
	this.addEventListener("click", ael);
// 	this.addEventListener("mouseup", ael);
	this.addEventListener("keyup", ael);
}

Element.prototype.removeActivateEvent = function() {
	let ael = this.activateEventListener;
	this.removeEventListener("mousedown", ael);
// 	this.removeEventListener("mouseup", ael);
	this.removeEventListener("click", ael);
	this.removeEventListener("keyup", ael);
}

function addScrollListener(fn, name) {
	let wrapper = (event) => {
		window.requestAnimationFrame(() => {
			fn(event);
			document.addEventListener("scroll", wrapper, {once: true, passive: true});
		});
	}
	document.addEventListener("scroll", wrapper, {once: true, passive: true});
	
	// Retain a reference to the scroll listener, if a name is provided.
	if (typeof name != "undefined")
		window[name] = wrapper;
}

/****************/
/* MISC HELPERS */
/****************/

Element.prototype.scrollIntoViewIfNeeded = function() {
	if (this.getBoundingClientRect().bottom > window.innerHeight) {
		this.scrollIntoView(false);
	}
}

Element.prototype.getCommentId = function() {
	let item = (this.className == "comment-item" ? this : this.closest(".comment-item"));
	if (item) {
		return /^comment-(.*)/.exec(item.id)[1];
	} else {
		return false;
	}
}

/*******************/
/* INBOX INDICATOR */
/*******************/

function updateInbox() {
	let onFinish = (event) => {
		if (event.target.status == 200) {
			let response = JSON.parse(event.target.responseText);
			if (response) {
				let element = document.querySelector('#inbox-indicator');
				element.className = 'new-messages';
				element.title = 'New messages [o]';
			}
		}
	};

	if (loggedInUserId) {
		let request = new XMLHttpRequest();
		request.addEventListener("load", onFinish);
		request.open("GET", "/check-notifications");
		request.send();
	}
}

/**************/
/* COMMENTING */
/**************/

function GUIEditMobileHelpButtonClicked(event) {
	toggleMarkdownHintsBox();
	event.target.toggleClass("active");
	document.querySelector(".posting-controls:focus-within textarea").focus();
}
function toggleMarkdownHintsBox() {
	let markdownHintsBox = document.querySelector(".markdown-hints");
	markdownHintsBox.style.display = (window.getComputedStyle(markdownHintsBox).display == "none") ? "block" : "none";
}
function removeMarkdownHintsBox() {
	let markdownHintsBox = document.querySelector(".markdown-hints");
	if (window.getComputedStyle(markdownHintsBox).display != "none") markdownHintsBox.style.display = "none";
}
function GUIEditMobileExitButtonClicked(event) {
	event.target.blur();
	removeMarkdownHintsBox();
}

Element.prototype.addTextareaFeatures = function() {
	let textarea = this;

	textarea.addEventListener("focus", (event) => { event.target.closest("form").scrollIntoViewIfNeeded(); });
	textarea.addEventListener("input", OnInputExpandTextarea, false);
	textarea.addEventListener("input", OnInputRemoveMarkdownHints, false);
	textarea.addEventListener("keyup", (event) => { event.stopPropagation(); });

	textarea.insertAdjacentHTML("beforebegin", "<div class='guiedit-buttons-container'></div>");
	var buttons_container = textarea.parentElement.querySelector(".guiedit-buttons-container");
	for (var button of guiEditButtons) {
		let [ name, desc, accesskey, m_before_or_func, m_after, placeholder, icon ] = button;
		buttons_container.insertAdjacentHTML("beforeend", 
			"<button type='button' class='guiedit guiedit-" 
			+ name
			+ "' tabindex='-1' title='"
			+ desc + ((accesskey != "") ? (" [" + accesskey + "]") : "")
			+ "' data-tooltip='" + desc
			+ "' ["
			+ accesskey
			+ "] onclick='insMarkup(event,"
			+ ((typeof m_before_or_func == 'function') ?
				m_before_or_func.name : 
				("\"" + m_before_or_func  + "\",\"" + m_after + "\",\"" + placeholder + "\""))
			+ ");'><div>"
			+ icon
			+ "</div></button>"
		);
	}

	var markdown_hints = 
	`<input type='checkbox' id='markdown-hints-checkbox'>
	<label for='markdown-hints-checkbox'></label>
	<div class='markdown-hints'>` + 
	[	"<span style='font-weight: bold;'>Bold</span><code>**Bold**</code>", 
		"<span style='font-style: italic;'>Italic</span><code>*Italic*</code>",
		"<span><a href=#>Link</a></span><code>[Link](http://example.com)</code>",
		"<span>Heading 1</span><code># Heading 1</code>",
		"<span>Heading 2</span><code>## Heading 1</code>",
		"<span>Heading 3</span><code>### Heading 1</code>",
		"<span>Blockquote</span><code>&gt; Blockquote</code>" ].map(row => "<div class='markdown-hints-row'>" + row + "</div>").join("") +
	`</div>`;
	textarea.parentElement.querySelector("span").insertAdjacentHTML("afterend", markdown_hints);
	
	let guiEditMobileHelpButton = document.querySelector(".guiedit-mobile-help-button");
	if (guiEditMobileHelpButton) {
		guiEditMobileHelpButton.addActivateEvent(GUIEditMobileHelpButtonClicked);
	}
	
	let guiEditMobileExitButton = document.querySelector(".guiedit-mobile-exit-button");
	if (guiEditMobileExitButton) {
		guiEditMobileExitButton.addActivateEvent(GUIEditMobileExitButtonClicked);
	}
}

Element.prototype.injectReplyForm = function(editMarkdownSource) {
	let element = this;
	let editCommentId = (editMarkdownSource ? element.getCommentId() : false);
	let withparent = (!editMarkdownSource && element.getCommentId());
	element.innerHTML = "<button class='cancel-comment-button' tabindex='-1'>Cancel</button>" +
		"<form method='post'>" + 
		"<div class='textarea-container'>" + 
		"<textarea name='text' onkeyup='enableBeforeUnload();' onchange='enableBeforeUnload();'></textarea>" +
		(withparent ? "<input type='hidden' name='parent-comment-id' value='" + element.getCommentId() + "'>" : "") +
		(editCommentId ? "<input type='hidden' name='edit-comment-id' value='" + editCommentId + "'>" : "") +
		"<span class='markdown-reference-link'>You can use <a href='http://commonmark.org/help/' target='_blank'>Markdown</a> here.</span>" + 
		`<button type="button" class="guiedit-mobile-auxiliary-button guiedit-mobile-help-button">Help</button>` + 
		`<button type="button" class="guiedit-mobile-auxiliary-button guiedit-mobile-exit-button">Exit</button>` + 
		"</div><div>" + 
		"<input type='hidden' name='csrf-token' value='" + window.csrfToken + "'>" +
		"<input type='submit' value='Submit'>" + 
		"</div></form>";
	
	element.querySelector(".cancel-comment-button").addActivateEvent(window.hideReplyForm);
	element.scrollIntoViewIfNeeded();
	element.querySelector("form").onsubmit = (event) => {
		if (!event.target.text.value) {
			alert("Please enter a comment.");
			return false;
		}
	}
	let textarea = element.querySelector("textarea");
	textarea.value = editMarkdownSource || "";
	textarea.addTextareaFeatures();
	textarea.focus();
}

Element.prototype.injectCommentButtons = function() {
	let element = this;
	element.innerHTML = "";
	let replyButton = document.createElement("button");
	if (element.parentElement.id == 'comments') {
		replyButton.className = "new-comment-button action-button";
		replyButton.innerHTML = "Post new comment";
		replyButton.setAttribute("accesskey", "n");
		replyButton.setAttribute("title", "Post new comment [n]");
	} else {
		if (element.parentElement.querySelector(".comment-body").hasAttribute("data-markdown-source")) {
			let editButton = element.appendChild(document.createElement("button"));
			editButton.className = "edit-button action-button";
			editButton.innerHTML = "Edit";
			editButton.tabIndex = '-1';
			editButton.addActivateEvent(window.showCommentEditForm);
		}
		replyButton.className = "reply-button action-button";
		replyButton.innerHTML = "Reply";
	}
	element.appendChild(replyButton);
	replyButton.tabIndex = '-1';
	replyButton.addActivateEvent(window.showReplyForm);
}

function showCommentEditForm(event) {
	let commentControls = event.target.parentElement;
	let commentBody = commentControls.parentElement.querySelector(".comment-body");
	commentBody.setAttribute("style", "display: none;");
	commentControls.injectReplyForm(commentBody.getAttribute("data-markdown-source"));
	commentControls.querySelector("form").addClass("edit-existing-comment");
	ExpandTextarea(commentControls.querySelector("textarea"));
}

function showReplyForm(event) {
	let commentControls = event.target.parentElement;
	document.querySelectorAll(".comment-controls").forEach(commentControls => {
		commentControls.injectCommentButtons();
	});

	commentControls.injectReplyForm();
}

function hideReplyForm(event) {
	try { event.target.parentElement.parentElement.querySelector(".comment-body").removeAttribute("style"); }
	catch (ex) { console.log(ex); }
	event.target.parentElement.injectCommentButtons();
}

function OnInputExpandTextarea() {
	if (window.innerWidth <= 520) return;
	let totalBorderHeight = (this.closest("#conversation-form") == null) ? 30 : 2;
	if ((this.offsetHeight - totalBorderHeight) < this.scrollHeight) {
		ExpandTextarea(this);
	}
}
function ExpandTextarea(textarea) {
	window.requestAnimationFrame(() => {
		textarea.style.height = 'auto';
		let totalBorderHeight = (textarea.closest("#conversation-form") == null) ? 30 : 2;
		textarea.style.height = textarea.scrollHeight + totalBorderHeight + 'px';
		if (textarea.clientHeight < window.innerHeight) {
			textarea.parentElement.parentElement.scrollIntoViewIfNeeded();
		}
	});
}

function OnInputRemoveMarkdownHints() {
	if (window.innerWidth > 520) return;
	removeMarkdownHintsBox();
	document.querySelector(".guiedit-mobile-help-button").removeClass("active");
}

/**********/
/* VOTING */
/**********/

function parseVoteType(voteType) {
	let value = {};
	if (!voteType) return value;
	value.up = /[Uu]pvote$/.test(voteType);
	value.down = /[Dd]ownvote$/.test(voteType);
	value.big = /^big/.test(voteType);
	return value;
}

function makeVoteType(value) {
	return (value.big ? 'big' : 'small') + (value.up ? 'Up' : 'Down') + 'vote';
}

function makeVoteClass(vote) {
	if (vote.up || vote.down) {
		return (vote.big ? 'selected big-vote' : 'selected');
	} else {
		return '';
	}
}

function addVoteButtons(element, voteType, targetType) {
	let vote = parseVoteType(voteType);
	let voteClass = makeVoteClass(vote);
	element.insertAdjacentHTML('beforebegin', "<button type='button' class='vote upvote"+(vote.up ?' '+voteClass:'')+"' data-vote-type='upvote' data-target-type='"+targetType+"' tabindex='-1'></button>");
	element.insertAdjacentHTML('afterend', "<button type='button' class='vote downvote"+(vote.down ?' '+voteClass:'')+"' data-vote-type='downvote' data-target-type='"+targetType+"' tabindex='-1'></button>");
}

function makeVoteCompleteEvent(target) {
	return (event) => {
		var buttonTargets, karmaTargets;
		if (target === null) {
			buttonTargets = document.querySelectorAll(".post-meta .karma");
			karmaTargets = document.querySelectorAll(".post-meta .karma-value");
		} else {
			let commentItem = target.closest(".comment-item")
			buttonTargets = [ commentItem.querySelector(".comment-meta .karma"), commentItem.querySelector(".comment-controls .karma") ];
			karmaTargets = [ commentItem.querySelector(".comment-meta .karma-value"), commentItem.querySelector(".comment-controls .karma-value") ];
		}
		buttonTargets.forEach(buttonTarget => {
			buttonTarget.removeClass("waiting");
		});
		if (event.target.status == 200) {
			let response = JSON.parse(event.target.responseText);
			let karmaText = response[0], voteType = response[1];

			let vote = parseVoteType(voteType);
			let voteUpDown = (vote.up ? 'upvote' : (vote.down ? 'downvote' : ''));
			let voteClass = makeVoteClass(vote);

			karmaTargets.forEach(karmaTarget => {
				karmaTarget.innerHTML = karmaText;
				if (karmaTarget.hasClass("redacted")) {
					karmaTarget.dataset["trueValue"] = karmaTarget.firstChild.textContent;
					karmaTarget.firstChild.textContent = "##";
				}
			});
			buttonTargets.forEach(buttonTarget => {
				buttonTarget.querySelectorAll("button.vote").forEach(button => {
					button.removeClasses([ "clicked-once", "clicked-twice", "selected", "big-vote" ]);
					if (button.getAttribute('data-vote-type') == voteUpDown) button.addClass(voteClass);
				});
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

function voteButtonClicked(event) {
	let voteButton = event.target;
	
	// 500 ms (0.5 s) double-click timeout.	
	let doubleClickTimeout = 500;
	
	if (!voteButton.clickedOnce) {
		voteButton.clickedOnce = true;
		voteButton.addClass("clicked-once");

		window.setTimeout(vbDoubleClickTimeoutCallback, doubleClickTimeout, vb);
	} else {
		voteButton.clickedOnce = false;
		
		// Do double-click code.
		voteEvent(voteButton, 2);
		voteButton.removeClass("clicked-once");
		voteButton.addClass("clicked-twice");
	}
}
function vbDoubleClickTimeoutCallback(voteButton) {
	if (!voteButton.clickedOnce) return;
	
	// Do single-click code.
	voteButton.clickedOnce = false;
	voteEvent(voteButton, 1);
}
function voteEvent(voteButton, numClicks) {
	voteButton.blur();
	voteButton.parentNode.addClass("waiting");
	let targetType = voteButton.getAttribute("data-target-type");
	let targetId = ((targetType == 'Comments') ? voteButton.getCommentId() : voteButton.parentNode.getAttribute("data-post-id"));
	let voteUpDown = voteButton.getAttribute("data-vote-type");
	let vote = parseVoteType(voteUpDown);
	vote.big = (numClicks == 2);
	let voteType = makeVoteType(vote);
	let oldVoteType;
	if (targetType == "Posts") {
		oldVoteType = postVote;
		postVote = ((voteType == oldVoteType) ? null : voteType);
	} else {
		oldVoteType = commentVotes[targetId];
		commentVotes[targetId] = ((voteType == oldVoteType) ? null : voteType);
	}
	let f = () => { sendVoteRequest(targetId, targetType, voteType, makeVoteCompleteEvent((targetType == 'Comments' ? voteButton.parentNode : null))) };
	if (oldVoteType && (oldVoteType != voteType)) {
		sendVoteRequest(targetId, targetType, oldVoteType, f);
	} else {
		f();
	}
}

/***********************************/
/* COMMENT THREAD MINIMIZE BUTTONS */
/***********************************/

function commentMinimizeButtonClicked(event) {
	event.target.closest(".comment-item").setCommentThreadMaximized(true);
}
Element.prototype.setCommentThreadMaximized = function(toggle, userOriginated = true, force) {
	let commentItem = this;
	let storageName = "thread-minimized-" + commentItem.getCommentId();
	let minimize_button = commentItem.querySelector(".comment-minimize-button");
	let maximize = force || (toggle ? /minimized/.test(minimize_button.className) : !window.localStorage.getItem(storageName));
	if (userOriginated) {
		if (maximize) {
			window.localStorage.removeItem(storageName);
		} else {
			window.localStorage.setItem(storageName, true);
		}
	}

	commentItem.style.height = maximize ? 'auto' : '38px';
	commentItem.style.overflow = maximize ? 'visible' : 'hidden';

	minimize_button.className = "comment-minimize-button " + (maximize ? "maximized" : "minimized");
	minimize_button.innerHTML = maximize ? "&#xf146;" : "&#xf0fe;";
	minimize_button.title = `${(maximize ? "Collapse" : "Expand")} comment`;
	if (getQueryVariable("chrono") != "t") {
		minimize_button.title += ` thread (${minimize_button.dataset["childCount"]} child comments)`;
	}
}

/*****************************************/
/* NEW COMMENT HIGHLIGHTING & NAVIGATION */
/*****************************************/

Element.prototype.getCommentDate = function() {
	let item = (this.className == "comment-item") ? this : this.closest(".comment-item");
	return (item ? parseInt(item.querySelector(".date").dataset["jsDate"]) : false);
}
function getCurrentVisibleComment() {
	let px = window.innerWidth/2, py = window.innerHeight/10;
	let commentItem = document.elementFromPoint(px, py).closest(".comment-item") || document.elementFromPoint(px, py+60).closest(".comment-item"); // Mind the gap between threads
	let atbottom = document.querySelector("#comments").getBoundingClientRect().bottom < window.innerHeight;
	if (atbottom) {
		let hashci = location.hash && document.querySelector(location.hash);
		if (hashci && /comment-item/.test(hashci.className) && hashci.getBoundingClientRect().top > 0) {
			commentItem = hashci;
		}
	}
	return commentItem;
}

function highlightCommentsSince(date) {
	var newCommentsCount = 0;
	window.newComments = [ ];
	let oldCommentsStack = [ ];
	let prevNewComment;
	document.querySelectorAll(".comment-item").forEach(commentItem => {
		commentItem.prevNewComment = prevNewComment;
		if (commentItem.getCommentDate() > date) {
			commentItem.addClass("new-comment");
			newCommentsCount++;
			window.newComments.push(commentItem.getCommentId());
			oldCommentsStack.forEach(oldci => { oldci.nextNewComment = commentItem });
			oldCommentsStack = [ commentItem ];
			prevNewComment = commentItem;
		} else {
			commentItem.removeClass("new-comment");
			oldCommentsStack.push(commentItem);
		}
	});

	window.newCommentScrollSet = (commentItem) => {
		document.querySelector("#new-comment-nav-ui .new-comment-previous").disabled = commentItem ? !commentItem.prevNewComment : true;
		document.querySelector("#new-comment-nav-ui .new-comment-next").disabled = commentItem ? !commentItem.nextNewComment : (window.newComments.length == 0);
	};
	window.newCommentScrollListener = () => {
		let commentItem = getCurrentVisibleComment();
		newCommentScrollSet(commentItem);
	}

	addScrollListener(newCommentScrollListener);

	if (document.readyState=="complete") {
		newCommentScrollListener();
	} else {
		let commentItem = location.hash && /^#comment-/.test(location.hash) && document.querySelector(location.hash);
		newCommentScrollSet(commentItem);
	}

	registerInitializer("initializeCommentScrollPosition", false, () => document.readyState == "complete", newCommentScrollListener);

	return newCommentsCount;
}

function scrollToNewComment(next) {
	let commentItem = getCurrentVisibleComment();
	let targetComment = null;
	let targetCommentID = null;
	if (commentItem) {
		targetComment = (next ? commentItem.nextNewComment : commentItem.prevNewComment);
		if (targetComment) {
			targetCommentID = targetComment.getCommentId();
		}
	} else {
		if (window.newComments[0]) {
			targetCommentID = window.newComments[0];
			targetComment = document.querySelector("#comment-" + targetCommentID);
		}
	}
	if (targetComment) {
		expandAncestorsOf(targetCommentID);
		history.replaceState(null, null, "#comment-" + targetCommentID);
		targetComment.scrollIntoView();
	}

	newCommentScrollListener();
}

function commentQuicknavButtonClicked(event) {
	scrollToNewComment(/next/.test(event.target.className));
}

function getPostHash() {
	let postHash = /^\/posts\/([^\/]+)/.exec(location.pathname);
	return (postHash ? postHash[1] : false);
}
function getLastVisitedDate() {
	// Get the last visited date (or, if posting a comment, the previous last visited date).
	let aCommentHasJustBeenPosted = (document.querySelector(".just-posted-comment") != null);
	let storageName = (aCommentHasJustBeenPosted ? "previous-last-visited-date_" : "last-visited-date_") + getPostHash();
	return window.localStorage.getItem(storageName);
}
function setLastVisitedDate(date) {
	// If NOT posting a comment, save the previous value for the last-visited-date 
	// (to recover it in case of posting a comment).
	let aCommentHasJustBeenPosted = (document.querySelector(".just-posted-comment") != null);
	if (!aCommentHasJustBeenPosted) {
		let previousLastVisitedDate = (window.localStorage.getItem("last-visited-date_" + getPostHash()) || 0);
		window.localStorage.setItem("previous-last-visited-date_" + getPostHash(), previousLastVisitedDate);
	}
	
	// Set the new value.
	window.localStorage.setItem("last-visited-date_" + getPostHash(), date);
}

function updateSavedCommentCount() {
	let commentCount = document.querySelectorAll(".comment").length;
	window.localStorage.setItem("comment-count_" + getPostHash(), commentCount);
}
function badgePostsWithNewComments() {
	if (getQueryVariable("show") == "conversations") return;
	
	document.querySelectorAll("h1.listing a[href^='/']").forEach(postLink => {
		let postHash = /posts\/(.+?)\//.exec(postLink.href)[1];

		let savedCommentCount = window.localStorage.getItem("comment-count_" + postHash);
		let commentCountDisplay = postLink.parentElement.nextSibling.querySelector(".comment-count");
		let currentCommentCount = /([0-9]+)/.exec(commentCountDisplay.textContent)[1];

		if (currentCommentCount > savedCommentCount)
			commentCountDisplay.addClass("new-comments");
		commentCountDisplay.title = `${currentCommentCount} comments (${currentCommentCount - savedCommentCount} new)`;
	});
}

/***********************************/
/* CONTENT COLUMN WIDTH ADJUSTMENT */
/***********************************/

function injectContentWidthSelector() {
	let currentWidth = widthDict[window.localStorage.getItem("selected-width")] || widthDict['normal'];
	let widthSelector = addUIElement(
		"<div id='width-selector'>" +
		String.prototype.concat.apply("", widthOptions.map(function (widthOption) {
			let [name, desc, abbr, width] = widthOption;
			let selected = (width == currentWidth ? ' selected' : '');
			let disabled = (width == currentWidth ? ' disabled' : '');
			return `<button type='button' class='select-width-${name}${selected}'${disabled} title='${desc}' tabindex='-1' data-name='${name}'>${abbr}</button>`})) +
		"</div>");
	widthSelector.querySelectorAll("button").forEach(button => {
		button.addActivateEvent(widthAdjustButtonClicked);
	});
	
	setWidthAdjustButtonsAccesskey();
	
	if (window.adjustmentTransitions) {
		document.querySelector("head").insertAdjacentHTML("beforeend", 
			"<style id='width-transition'>" + 
			`#content,
			#ui-elements-container,
			#images-overlay {
				transition:
					max-width 0.3s ease;
			}` + 
			"</style>");
	}
}
function setWidthAdjustButtonsAccesskey() {
	document.querySelectorAll("#width-selector button").forEach(button => {
		button.accessKey = "";
		button.title = /(.+?)( \['\])?$/.exec(button.title)[1];
	});
	let selectedButton = document.querySelector("#width-selector button.selected");
	let nextButtonInCycle = (selectedButton == selectedButton.parentElement.lastChild) ? selectedButton.parentElement.firstChild : selectedButton.nextSibling;
	nextButtonInCycle.accessKey = "'";
	nextButtonInCycle.title += ` [\']`;
}
function widthAdjustButtonClicked(event) {
	let selectedWidth = event.target.getAttribute("data-name");
	if (selectedWidth == "normal") window.localStorage.removeItem("selected-width"); else window.localStorage.setItem("selected-width", selectedWidth);
	setContentWidth(selectedWidth);
	event.target.parentElement.childNodes.forEach(button => {
		button.removeClass("selected");
		button.disabled = false;
	});
	event.target.addClass("selected");
	event.target.disabled = true;
	
	setWidthAdjustButtonsAccesskey();

	// Regenerate images overlay.
	generateImagesOverlay();
}

/*******************/
/* THEME SELECTION */
/*******************/

function injectThemeSelector() {
	let currentTheme = readCookie("theme") || "default";
	let themeSelector = addUIElement(
		"<div id='theme-selector' class='theme-selector'>" +
		String.prototype.concat.apply("", window.themeOptions.map(function (to) {
			let [name, desc, letter] = to;
			let selected = (name == currentTheme ? ' selected' : '');
			let disabled = (name == currentTheme ? ' disabled' : '');
			let accesskey = letter.charCodeAt(0) - 'A'.charCodeAt(0) + 1;
			return `<button type='button' class='select-theme-${name}${selected}'${disabled} title="${desc} [${accesskey}]" data-theme-name="${name}" data-theme-description="${desc}" accesskey='${accesskey}' tabindex='-1'>${letter}</button>`;})) +
		"</div>");
	themeSelector.querySelectorAll("button").forEach(button => {
		button.addActivateEvent(themeSelectButtonClicked);
	});
	
	if (window.adjustmentTransitions) {
		document.querySelector("head").insertAdjacentHTML("beforeend", 
			"<style id='theme-fade-transition'>" + 
			`body {
				transition:
					opacity 0.5s ease-out,
					background-color 0.3s ease-out;
			}
			body.transparent {
				background-color: #777;
				opacity: 0.0;
				transition:
					opacity 0.5s ease-in,
					background-color 0.3s ease-in;
			}` + 
			"</style>");
	}
}
function themeSelectButtonClicked(event) {
	let themeName = /select-theme-([^\s]+)/.exec(event.target.className)[1];
	setSelectedTheme(themeName);
}
function setSelectedTheme(themeName) {
	document.querySelectorAll(".theme-selector button").forEach(button => {
		button.removeClass("selected");
		button.disabled = false;
	});
	document.querySelectorAll(".theme-selector button.select-theme-" + themeName).forEach(button => {
		button.addClass("selected");
		button.disabled = true;
	});
	setTheme(themeName);
	document.querySelector("#theme-tweaker-ui .current-theme span").innerText = themeName;
}
function setTheme(newThemeName) {
	var themeUnloadCallback = '';
	var oldThemeName = '';
	if (typeof(newThemeName) == 'undefined') {
		newThemeName = readCookie('theme');
		if (!newThemeName) return;
	} else {
		themeUnloadCallback = window['themeUnloadCallback_' + (readCookie('theme') || 'default')];
		oldThemeName = readCookie('theme') || 'default';
		
		if (newThemeName == 'default') setCookie('theme', '');
		else setCookie('theme', newThemeName);
	}	
	if (themeUnloadCallback != null) themeUnloadCallback(newThemeName);
	
	let styleSheetNameSuffix = (newThemeName == 'default') ? '' : ('-' + newThemeName);
	let currentStyleSheetNameComponents = /style[^\.]*(\..+)$/.exec(document.querySelector("head link[href*='.css']").href);
	
	let newStyle = document.createElement('link');
	newStyle.setAttribute('rel', 'stylesheet');
	newStyle.setAttribute('href', '/style' + styleSheetNameSuffix + currentStyleSheetNameComponents[1]);
	
	let oldStyle = document.querySelector("head link[href*='.css']");
	newStyle.addEventListener('load', function() { removeElement(oldStyle); });
	newStyle.addEventListener('load', function() { postSetThemeHousekeeping(oldThemeName, newThemeName); });

	if (window.adjustmentTransitions) {
		pageFadeTransition(false);
		window.setTimeout(function () { document.querySelector('head').insertBefore(newStyle, oldStyle.nextSibling); }, 500);
	} else {
		document.querySelector('head').insertBefore(newStyle, oldStyle.nextSibling);
	}
}
function postSetThemeHousekeeping(oldThemeName = "", newThemeName = (readCookie('theme') || 'default')) {
	recomputeUIElementsContainerHeight();
	adjustUIForWindowSize();
	window.addEventListener('resize', adjustUIForWindowSize);

	let themeLoadCallback = window['themeLoadCallback_' + newThemeName];
	if (themeLoadCallback != null) themeLoadCallback(oldThemeName);
	
	generateImagesOverlay();
	if (window.adjustmentTransitions) pageFadeTransition(true);
	updateThemeTweakerSampleText();
}

function pageFadeTransition(fadeIn) {
	if (fadeIn) {
		document.querySelector("body").removeClass("transparent");
	} else {
		document.querySelector("body").addClass("transparent");
	}
}

function themeLoadCallback_less(fromTheme = "") {
	injectSiteNavUIToggle();
	if (!window.isMobile) {
		injectPostNavUIToggle();
		injectAppearanceAdjustUIToggle();
	}

	registerInitializer('shortenDate', true, () => document.querySelector(".top-post-meta") != null, function () {
		let dtf = new Intl.DateTimeFormat([], 
			(window.innerWidth < 1100) ? 
				{ month: 'short', day: 'numeric', year: 'numeric' } : 
					{ month: 'long', day: 'numeric', year: 'numeric' });
		let postDate = document.querySelector(".top-post-meta .date");
		postDate.innerHTML = dtf.format(new Date(+ postDate.getAttribute("data-js-date")));
	});

	if (window.isMobile) {
		document.querySelector("#content").insertAdjacentHTML("beforeend", "<div id='theme-less-mobile-first-row-placeholder'></div>");
	}
	
	if (!window.isMobile) {
		registerInitializer('addSpans', true, () => document.querySelector(".top-post-meta") != null, function () {
			document.querySelectorAll(".top-post-meta .date, .top-post-meta .comment-count").forEach(element => {
				element.innerHTML = "<span>" + element.innerHTML + "</span>";
			});
		});

		if (window.localStorage.getItem("appearance-adjust-ui-toggle-engaged") == null) {
			// If state is not set (user has never clicked on the Less theme's appearance
			// adjustment UI toggle) then show it, but then hide it after a short time.
			registerInitializer('engageAppearanceAdjustUI', true, () => document.querySelector("#ui-elements-container") != null, function () {
				toggleAppearanceAdjustUI();
				window.setTimeout(toggleAppearanceAdjustUI, 3000);
			});
		}

		if (fromTheme != "") {
			allUIToggles = document.querySelectorAll("#ui-elements-container div[id$='-ui-toggle']");
			window.setTimeout(function () {
				allUIToggles.forEach(toggle => { toggle.addClass("highlighted"); });
			}, 300);
			window.setTimeout(function () {
				allUIToggles.forEach(toggle => { toggle.removeClass("highlighted"); });
			}, 1800);
		}

		// Unset the height of the #ui-elements-container
		document.querySelector("#ui-elements-container").style.height = "";
		
		registerInitializer('updatePostNavUIToggleVisibility', false, () => document.readyState == "complete", updatePostNavUIToggleVisibility);
		window.addEventListener('resize', updatePostNavUIToggleVisibility);		

		// Due to filters vs. fixed elements, we need to be smarter about selecting which elements to filter...
		window.filtersTargetSelector = "body::before, #content > *:not(#secondary-bar):not(.post), #secondary-bar > *, .post > *:not(.top-post-meta), .top-post-meta > *:not(.date):not(.comment-count), .top-post-meta .date span, .top-post-meta .comment-count > span, #ui-elements-container > div:not(#theme-tweaker-ui), #theme-tweaker-ui #theme-tweak-section-sample-text .sample-text-container";
		applyFilters(window.currentFilters);
	}
	
	window.scrollState = {
		"lastScrollTop":					window.pageYOffset || document.documentElement.scrollTop,
		"unbrokenDownScrollDistance":		0,
		"unbrokenUpScrollDistance":			0,
		"siteNavUIToggleButton":			document.querySelector("#site-nav-ui-toggle button"),
		"appearanceAdjustUIToggleButton":	document.querySelector("#appearance-adjust-ui-toggle button")
	};
	addScrollListener(updateSiteNavUIState, "updateSiteNavUIStateScrollListener");
}

// Hide the post-nav-ui toggle if none of the elements to be toggled are visible; 
// otherwise, show it.
function updatePostNavUIToggleVisibility() {
	var hidePostNavUIToggle = true;
	document.querySelectorAll("#ui-elements-container #quick-nav-ui a, #ui-elements-container #new-comment-nav-ui").forEach(element => {
		if (window.getComputedStyle(element).visibility == "visible") hidePostNavUIToggle = false;
	});
	try { document.querySelector("#ui-elements-container #post-nav-ui-toggle").style.visibility = hidePostNavUIToggle ? "hidden" : ""; } catch (ex) { console.log(ex); }
}

// Hide the site nav and appearance adjust UIs on scroll down; show them on scroll up.
// NOTE: The UIs are re-shown on scroll up ONLY if the user has them set to be 
// engaged; if they're manually disengaged, they are not re-engaged by scroll.
function updateSiteNavUIState(event) {
	let newScrollTop = window.pageYOffset || document.documentElement.scrollTop;
	window.scrollState.unbrokenDownScrollDistance = (newScrollTop > window.scrollState.lastScrollTop) ? 
														(window.scrollState.unbrokenDownScrollDistance + newScrollTop - window.scrollState.lastScrollTop) : 
													 	0;
	window.scrollState.unbrokenUpScrollDistance = (newScrollTop < window.scrollState.lastScrollTop) ?
													(window.scrollState.unbrokenUpScrollDistance + window.scrollState.lastScrollTop - newScrollTop) :
													0;
	window.scrollState.lastScrollTop = newScrollTop;

	// Hide site nav UI and appearance adjust UI when scrolling a full page down.
	if (window.scrollState.unbrokenDownScrollDistance > window.innerHeight) {
		if (window.scrollState.siteNavUIToggleButton.hasClass("engaged")) toggleSiteNavUI();
		if (window.scrollState.appearanceAdjustUIToggleButton.hasClass("engaged")) toggleAppearanceAdjustUI();
	}

	// Show site nav UI when scrolling a full page up, or to the top.
	if ((window.scrollState.unbrokenUpScrollDistance > window.innerHeight || 
		 window.scrollState.lastScrollTop == 0) &&
		(!window.scrollState.siteNavUIToggleButton.hasClass("engaged") && 
		 window.localStorage.getItem("site-nav-ui-toggle-engaged") != "false")) toggleSiteNavUI();

	// Show appearance adjust UI when scrolling to the top.
	if ((window.scrollState.lastScrollTop == 0) &&
		(!window.scrollState.appearanceAdjustUIToggleButton.hasClass("engaged") && 
		 window.localStorage.getItem("appearance-adjust-ui-toggle-engaged") != "false")) toggleAppearanceAdjustUI();

	console.log("Scrolling...");
}

function themeUnloadCallback_less(toTheme = "") {
	removeSiteNavUIToggle();
	if (!window.isMobile) {
		removePostNavUIToggle();
		removeAppearanceAdjustUIToggle();
	}
	window.removeEventListener('resize', updatePostNavUIToggleVisibility);
	
	document.removeEventListener("scroll", window["updateSiteNavUIStateScrollListener"]);
	
	removeElement("#theme-less-mobile-first-row-placeholder");

	if (!window.isMobile) {
		// Remove spans
		document.querySelectorAll(".top-post-meta .date, .top-post-meta .comment-count").forEach(element => {
			element.innerHTML = element.firstChild.innerHTML;
		});
	}
	
	(document.querySelector(".top-post-meta .date")||{}).innerHTML = (document.querySelector(".bottom-post-meta .date")||{}).innerHTML;
	
	// Reset filtered elements selector to default.
	window.filtersTargetSelector = "";
	applyFilters(window.currentFilters);
}

function themeLoadCallback_dark(fromTheme = "") {
	document.querySelector("head").insertAdjacentHTML("beforeend", 
		"<style id='dark-theme-adjustments'>" + 
		`.markdown-reference-link a { color: #d200cf; filter: invert(100%); }` + 
		`#bottom-bar.decorative::before { filter: invert(100%); }` +
		"</style>");
}
function themeUnloadCallback_dark(toTheme = "") {
	removeElement("#dark-theme-adjustments");
}

/********************************************/
/* APPEARANCE CUSTOMIZATION (THEME TWEAKER) */
/********************************************/

function injectThemeTweaker() {
	let themeTweakerToggle = addUIElement(`<div id='theme-tweaker-toggle'><button type='button' tabindex='-1' title="Customize appearance [;]" accesskey=';'>&#xf1de;</button></div>`);
	themeTweakerToggle.querySelector("button").addActivateEvent(themeTweakerToggleButtonClicked);
	
	let themeTweakerUI = addUIElement("<div id='theme-tweaker-ui' style='display: none;'>" + 
	`<div class='main-theme-tweaker-window'>
		<h1>Customize appearance</h1>
		<button type='button' class='minimize-button minimize' tabindex='-1'></button>
		<button type='button' class='help-button' tabindex='-1'></button>
		<p class='current-theme'>Current theme: <span>` + 
		(readCookie("theme") || "default") + 
		`</span></p>
		<p class='theme-selector'></p>
		<div class='controls-container'>
			<div id='theme-tweak-section-sample-text' class='section' data-label='Sample text'>
				<div class='sample-text-container'><span class='sample-text'>
					<p>Less Wrong (text)</p>
					<p><a href="#">Less Wrong (link)</a></p>
				</span></div>
			</div>
			<div id='theme-tweak-section-text-size-adjust' class='section' data-label='Text size'>
				<button type='button' class='text-size-adjust-button decrease' title='Decrease text size'></button>
				<button type='button' class='text-size-adjust-button default' title='Reset to default text size'></button>
				<button type='button' class='text-size-adjust-button increase' title='Increase text size'></button>
			</div>
			<div id='theme-tweak-section-invert' class='section' data-label='Invert (photo-negative)'>
				<input type='checkbox' id='theme-tweak-control-invert'></input>
				<label for='theme-tweak-control-invert'>Invert colors</label>
			</div>
			<div id='theme-tweak-section-saturate' class='section' data-label='Saturation'>
				<input type="range" id="theme-tweak-control-saturate" min="0" max="300" data-default-value="100" data-value-suffix="%" data-label-suffix="%">
				<p class="theme-tweak-control-label" id="theme-tweak-label-saturate"></p>
				<div class='notch theme-tweak-slider-notch-saturate' title='Reset saturation to default value (100%)'></div>
			</div>
			<div id='theme-tweak-section-brightness' class='section' data-label='Brightness'>
				<input type="range" id="theme-tweak-control-brightness" min="0" max="300" data-default-value="100" data-value-suffix="%" data-label-suffix="%">
				<p class="theme-tweak-control-label" id="theme-tweak-label-brightness"></p>
				<div class='notch theme-tweak-slider-notch-brightness' title='Reset brightness to default value (100%)'></div>
			</div>
			<div id='theme-tweak-section-contrast' class='section' data-label='Contrast'>
				<input type="range" id="theme-tweak-control-contrast" min="0" max="300" data-default-value="100" data-value-suffix="%" data-label-suffix="%">
				<p class="theme-tweak-control-label" id="theme-tweak-label-contrast"></p>
				<div class='notch theme-tweak-slider-notch-contrast' title='Reset contrast to default value (100%)'></div>
			</div>
			<div id='theme-tweak-section-hue-rotate' class='section' data-label='Hue rotation'>
				<input type="range" id="theme-tweak-control-hue-rotate" min="0" max="360" data-default-value="0" data-value-suffix="deg" data-label-suffix="°">
				<p class="theme-tweak-control-label" id="theme-tweak-label-hue-rotate"></p>
				<div class='notch theme-tweak-slider-notch-hue-rotate' title='Reset hue to default (0° away from standard colors for theme)'></div>
			</div>
		</div>
		<div class='buttons-container'>
			<button type="button" class="reset-defaults-button">Reset to defaults</button>
			<button type='button' class='ok-button default-button'>OK</button>
			<button type='button' class='cancel-button'>Cancel</button>
		</div>
	</div>
	<div class="clippy-container">
		<span class="hint">Hi, I'm Bobby the Basilisk! Click on the minimize button (<img src='data:image/gif;base64,R0lGODlhFgAUAPIAMQAAAAMDA394f7+4v9/Y3//4/2JX/38AACwAAAAAFgAUAAADRki63B6kyEkrFbCMzbvnWPSNXqiRqImm2Uqq7gfH3Uxv9p3TNuD/wFqLAywChCKi8Yc83XDD52AXCwmu2KxWG+h6v+BwNwEAOw==' />) to minimize the theme tweaker window, so that you can see what the page looks like with the current tweaked values. (But remember, <span>the changes won't be saved until you click "OK"!</span>)
		<img class='clippy' src='data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAZAAAAEnCAMAAACwruUzAAADAFBMVEUAAADz5NjSv7L37ub7///89/Lex7n////+/v3+/flKLyXp2tD69O7h0MXIs6c1JiBKKyM8KCJlOCr+/fzRu627ppnEraDKs6YxIh/v0rxQNSvpvJ3Mt6ojGRdQPjXy5tu+qZymlIr8/Pt2Qi+ERjC1nIzZqYZ5ZFq0n5CcgXJiMyWFdm5iUkr9/PqDUTwfFxRlTUDAppZeQjPIflSNUDO/kXXamWzZxLUXERCkZ0ljQjWfhXTEYzDl08aIZE9zXlCsk4Lhl1mljX326NykWTCRdmT58erTe0G/aDWlXzfs1cJLOjLMq5RzWUrz4dMLCAePc2G+hlvw4dfbiE/duaC3eU3Ml23axbfornepb0bgp3rv0bo5KiLwv5kAAQAiGBgWEhMgFRQpGhgVDg4QDQ5SLiQuHh0cFhc0IiBKKiQHBQUpHh08JiAxIBwDAgJQKiAQCgoKCQkbERAkHBsvIyI3JiQ3JB5LLidAJB1LJx5EKidgMSJGKiFeMilXNCtlNCkuGxc7IRs2HxpEJiKCQytAJSN5Pi5qOSdBKSFZLSdzPCUmFRNwOy5ZMiNpOy9hOSw6IiJYKx9RKyhPMyo9KieXUioNBgWKRi+NUDOTSS+XfGqnkIFFJRubgXCNSyihjH2pYSybTS9yQzB7QiZoU0R7Ri9vNCWzWi94OCihVSq4oJGBOii+YzCfVDStmYySdWKeXTGoWDGFTS29qZ2rjXi4bzWHb16MeWu1ZCuVVTSynI5dSDukh3Syl4aKPyqvk3+olIebhnhFMSt0UTyCSTanYjh2WkZpMSC4pJdmSTdaPzGlgmp/aFiwaDbEr6JQPTP9/vyNaE+/pZXLtqqAYUuSQiefeF2WcFi4nIp0X1GnTSzDcTCeQSvPhUfBej3Hq5ixTizNaDCxoJSEWUCwcES+f03GWizOdjXZk1S1imjoo1vadjLBn4mXYTzGilvNp4y8TC707+zTmWW/l3vhwKrus3DYsJjep3idbEj2woKtfVyrOivp3triij3u08DVysJmExTsAAABAHRSTlMAVX9VKlV/KlUqqlUqVX+q1NSqf6p/1KrUVdR/1NSqKqp/qtSqqn9/1H/Uf3/Uf6qq1NR/1H9/qtR/f9TUf6qq1Kqqqqqqf6qq1Kp/qtR/1NSq1NTU1NTU1Kqq1H+q+f//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////lVdJBQAAlLBJREFUeJzsnAtwU3d+7+vYRhIQwqPpjXGTdAn1DqSwd+PHLpvHvSwsYDbJxBBYHpuxLTi2Ti3NyWK9j46OJR29jvWWTnXUq9HoMZKssdUraauONLf17bBSNcletW6p0y6OVFKkRTFrsxiMccYh939Me3s7287dFtLeJueDBwvJCPH/nt/v+/v9///z/5VfYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWH5f8HZzONx/r0/BMs/wGlqbuWwivz/A6e5vYnLafknX2sBwfNPv8LyucHhvvnPKtLS3MRlBfk3g8Nr4XA4Ldwd7e2t3JZ/lLU2b+YxT7Q0tXOBwXA4z7M2828Aj/uQHYc6gCI85qnNmzktzANeU1sreMRp6mnmcsAPtjWzgnz+tDS3t7+5A7Dr9c71SACCtLW1Mopwmre0tQKt2jo3MIK0dmxiff/zhAcyUgunpfXVgwffOXRo165vnO1p47YAt+C1tTMPWlq4b3YwWj3R2QFMpKWVefnf+0N/IeGBsglc65ufBNd/a2vrjkNnTp45f+LEibOdPU2MIpymjp428AK39chxRqrXj/a0chhB2rlshHwO8HhMHuK0bG7qAAFw5M03d+27V6msPDjRdxSEAhhzDndDZ2f7kTePHDly6MzbJ/Z9/WhPUwuvpbWzu5WNkM8BTlNbezsIjubmHb/ad/zQ8ePHT9y+W63W3/762YPARkDRxT1y7ujBd44fApxfeTBz+ihIVlxua+eBJsZEQHi1sMI8RnjN7T2dnd0gBN489L1Tp8+ff/v8g5l7Ux9VT/UePdjZxAz9juMnT558G6Sx8ydW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' />
		<button type='button' class='clippy-close-button' tabindex='-1' title='Hide theme tweaker assistant (you can bring him back by clicking the ? button in the title bar)'></button>
	</div>
	<div class='help-window' style='display: none;'>
		<h1>Theme tweaker help</h1>
		<div id='theme-tweak-section-clippy' class='section' data-label='Theme Tweaker Assistant'>
			<input type='checkbox' id='theme-tweak-control-clippy' checked='checked'></input>
			<label for='theme-tweak-control-clippy'>Show Bobby the Basilisk</label>
		</div>
		<div class='buttons-container'>
			<button type='button' class='ok-button default-button'>OK</button>
			<button type='button' class='cancel-button'>Cancel</button>
		</div>
	</div>
	` + "</div>");
	themeTweakerUI.addActivateEvent(themeTweakerUIOverlayClicked, true);
	
	(document.querySelector("#theme-tweaker-ui > div")||{}).addActivateEvent(clickInterceptor, true);
	
	themeTweakerUI.querySelectorAll("input").forEach(field => {
		field.addEventListener("change", themeTweakerFieldValueChanged);
		if (field.type == "range") field.addEventListener("input", themeTweakerFieldInputReceived);
	});
	
	themeTweakerUI.querySelector(".minimize-button").addActivateEvent(themeTweakerMinimizeButtonClicked);
	themeTweakerUI.querySelector(".help-button").addActivateEvent(themeTweakerHelpButtonClicked);
	themeTweakerUI.querySelector(".reset-defaults-button").addActivateEvent(themeTweakerResetDefaultsButtonClicked);
	themeTweakerUI.querySelector(".main-theme-tweaker-window .cancel-button").addActivateEvent(themeTweakerCancelButtonClicked);
	themeTweakerUI.querySelector(".main-theme-tweaker-window .ok-button").addActivateEvent(themeTweakerOKButtonClicked);
	themeTweakerUI.querySelector(".help-window .cancel-button").addActivateEvent(themeTweakerHelpWindowCancelButtonClicked);
	themeTweakerUI.querySelector(".help-window .ok-button").addActivateEvent(themeTweakerHelpWindowOKButtonClicked);
	
	themeTweakerUI.querySelectorAll(".notch").forEach(notch => {
		notch.addActivateEvent(function (event) {
			let slider = event.target.parentElement.querySelector("input[type='range']");
			slider.value = slider.dataset['defaultValue'];
			event.target.parentElement.querySelector(".theme-tweak-control-label").innerText = slider.value + slider.dataset['labelSuffix'];
			window.currentFilters[/^theme-tweak-control-(.+)$/.exec(slider.id)[1]] = slider.value + slider.dataset['valueSuffix'];
			applyFilters(window.currentFilters);
		});
	});
	
	themeTweakerUI.querySelector(".clippy-close-button").addActivateEvent(themeTweakerClippyCloseButtonClicked);
	
	document.querySelector("head").insertAdjacentHTML("beforeend","<style id='theme-tweaker-style'></style>");
	
	document.querySelector("#theme-tweaker-ui .theme-selector").innerHTML = document.querySelector("#theme-selector").innerHTML;
	document.querySelectorAll("#theme-tweaker-ui .theme-selector button").forEach(button => {
		button.addActivateEvent(themeSelectButtonClicked);
	});
	
	document.querySelectorAll("#theme-tweaker-ui #theme-tweak-section-text-size-adjust button").forEach(button => {
		button.addActivateEvent(themeTweakerTextSizeAdjustButtonClicked);
	});
}
function toggleThemeTweakerUI() {
	let themeTweakerUI = document.querySelector("#theme-tweaker-ui");
	themeTweakerUI.style.display = (themeTweakerUI.style.display == "none") ? "block" : "none";
	document.querySelector("#theme-tweaker-style").innerHTML = (themeTweakerUI.style.display == "none") ? "" : 
		`#content, #ui-elements-container > div:not(#theme-tweaker-ui) {
			pointer-events: none;
		}`;
	if (themeTweakerUI.style.display != "none") {
		// Save selected theme.
		window.currentTheme = (readCookie("theme") || "default");
		// Focus invert checkbox.
		document.querySelector("#theme-tweaker-ui #theme-tweak-control-invert").focus();
		// Show sample text in appropriate font.
		updateThemeTweakerSampleText();
		// Disable tab-selection of the search box.
		setSearchBoxTabSelectable(false);
	} else {
		document.querySelector("#theme-tweaker-toggle button").disabled = false;
		// Re-enable tab-selection of the search box.
		setSearchBoxTabSelectable(true);
	}
	// Set theme tweaker assistant visibility.
	document.querySelector(".clippy-container").style.display = JSON.parse(window.localStorage.getItem("theme-tweaker-settings") || '{ "showClippy": true }')["showClippy"] ? "block" : "none";
}
function setSearchBoxTabSelectable(selectable) {
	document.querySelector("input[type='search']").tabIndex = selectable ? "" : "-1";
	document.querySelector("input[type='search'] + button").tabIndex = selectable ? "" : "-1";
}
function themeTweakerToggleButtonClicked(event) {
	document.querySelector("#theme-tweaker-ui .current-theme span").innerText = (readCookie("theme") || "default");
	
	document.querySelector("#theme-tweak-control-invert").checked = (window.currentFilters['invert'] == "100%");	
	[ "saturate", "brightness", "contrast", "hue-rotate" ].forEach(sliderName => {
		let slider = document.querySelector("#theme-tweak-control-" + sliderName);
		slider.value = /^[0-9]+/.exec(window.currentFilters[sliderName]) || slider.dataset['defaultValue'];
		document.querySelector("#theme-tweak-label-" + sliderName).innerText = slider.value + slider.dataset['labelSuffix'];
	});

	toggleThemeTweakerUI();
	event.target.disabled = true;
}
function themeTweakerUIOverlayClicked(event) {
	if (event.type == 'mousedown') {
		document.querySelector("#theme-tweaker-ui").style.opacity = "0.01";
	} else {
		toggleThemeTweakerUI();	
		document.querySelector("#theme-tweaker-ui").style.opacity = "1.0";
		themeTweakReset();
	}
}
function themeTweakerMinimizeButtonClicked(event) {
	let themeTweakerStyle = document.querySelector("#theme-tweaker-style");

	if (event.target.hasClass("minimize")) {
		event.target.removeClass("minimize");
		themeTweakerStyle.innerHTML = 
			`#theme-tweaker-ui .main-theme-tweaker-window {
				width: 320px;
				height: 31px;
				overflow: hidden;
				padding: 30px 0 0 0;
				top: 20px;
				right: 20px;
				left: auto;
			}
			#theme-tweaker-ui::after {
				top: 27px;
				right: 27px;
			}
			#theme-tweaker-ui::before {
				opacity: 0.0;
				height: 0;
			}
			#theme-tweaker-ui .clippy-container {
				opacity: 1.0;
			}
			#theme-tweaker-ui .clippy-container .hint span {
				color: #c00;
			}
			#theme-tweaker-ui {
				height: 0;
			}
			#content, #ui-elements-container > div:not(#theme-tweaker-ui) {
				pointer-events: none;
			}`;
		event.target.addClass("maximize");
	} else {
		event.target.removeClass("maximize");
		themeTweakerStyle.innerHTML = 
			`#content, #ui-elements-container > div:not(#theme-tweaker-ui) {
				pointer-events: none;
			}`;
		event.target.addClass("minimize");
	}
}
function toggleThemeTweakerHelpWindow() {
	let themeTweakerHelpWindow = document.querySelector("#theme-tweaker-ui .help-window");
	themeTweakerHelpWindow.style.display = (themeTweakerHelpWindow.style.display == "none") ? "block" : "none";
	if (themeTweakerHelpWindow.style.display != "none") {
		// Focus theme tweaker assistant checkbox.
		document.querySelector("#theme-tweaker-ui #theme-tweak-control-clippy").focus();
		// Disable interaction on main theme tweaker window.
		document.querySelector("#theme-tweaker-ui").style.pointerEvents = "none";
		document.querySelector("#theme-tweaker-ui .main-theme-tweaker-window").style.pointerEvents = "none";
	} else {
		// Re-enable interaction on main theme tweaker window.
		document.querySelector("#theme-tweaker-ui").style.pointerEvents = "auto";
		document.querySelector("#theme-tweaker-ui .main-theme-tweaker-window").style.pointerEvents = "auto";
	}
}
function themeTweakerHelpButtonClicked(event) {
	document.querySelector("#theme-tweak-control-clippy").checked = JSON.parse(window.localStorage.getItem("theme-tweaker-settings") || '{ "showClippy": true }')["showClippy"];
	toggleThemeTweakerHelpWindow();
}
function themeTweakerResetDefaultsButtonClicked(event) {
	document.querySelector("#theme-tweak-control-invert").checked = false;
	[ "saturate", "brightness", "contrast", "hue-rotate" ].forEach(sliderName => {
		let slider = document.querySelector("#theme-tweak-control-" + sliderName);
		slider.value = slider.dataset['defaultValue'];
		document.querySelector("#theme-tweak-label-" + sliderName).innerText = slider.value + slider.dataset['labelSuffix'];
	});
	window.currentFilters = { };
	applyFilters(window.currentFilters);
	
	window.currentTextZoom = 1;
	setTextZoom(window.currentTextZoom);
	
	setSelectedTheme("default");
}
function themeTweakerCancelButtonClicked(event) {
	toggleThemeTweakerUI();
	themeTweakReset();
}
function themeTweakerOKButtonClicked(event) {
	toggleThemeTweakerUI();
	themeTweakSave();
}
function themeTweakReset() {
	setSelectedTheme(window.currentTheme);
	window.currentFilters = JSON.parse(window.localStorage.getItem("theme-tweaks") || "{ }");
	applyFilters(window.currentFilters);
	window.currentTextZoom = window.localStorage.getItem("text-zoom");
	setTextZoom(window.currentTextZoom);
}
function themeTweakSave() {
	window.currentTheme = (readCookie("theme") || "default");
	window.localStorage.setItem("theme-tweaks", JSON.stringify(window.currentFilters));
	window.localStorage.setItem("text-zoom", window.currentTextZoom);
}
function clickInterceptor(event) {
	event.stopPropagation();
}

function themeTweakerFieldInputReceived(event) {
	var sampleTextFilters = window.currentFilters;
	
	let sliderName = /^theme-tweak-control-(.+)$/.exec(event.target.id)[1];
	document.querySelector("#theme-tweak-label-" + sliderName).innerText = event.target.value + event.target.dataset["labelSuffix"];
	sampleTextFilters[sliderName] = event.target.value + event.target.dataset["valueSuffix"];
	
	document.querySelector("#theme-tweaker-ui #theme-tweak-section-sample-text .sample-text-container").style.filter = filterStringFromFilters(sampleTextFilters);
}
function themeTweakerFieldValueChanged(event) {
	if (event.target.id == 'theme-tweak-control-invert') {
		window.currentFilters['invert'] = event.target.checked ? '100%' : '0%';
	} else if (event.target.type == 'range') {
		let sliderName = /^theme-tweak-control-(.+)$/.exec(event.target.id)[1];
		document.querySelector("#theme-tweak-label-" + sliderName).innerText = event.target.value + event.target.dataset["labelSuffix"];
		window.currentFilters[sliderName] = event.target.value + event.target.dataset["valueSuffix"];
	} else if (event.target.id == 'theme-tweak-control-clippy') {
		document.querySelector(".clippy-container").style.display = event.target.checked ? "block" : "none";
	}
	// Clear the sample text filters.
	document.querySelector("#theme-tweaker-ui #theme-tweak-section-sample-text .sample-text-container").style.filter = "";
	// Apply the new filters globally.
	applyFilters(window.currentFilters);
}
function themeTweakerHelpWindowCancelButtonClicked(event) {
	toggleThemeTweakerHelpWindow();
	themeTweakerResetSettings();
}
function themeTweakerHelpWindowOKButtonClicked(event) {
	toggleThemeTweakerHelpWindow();
	themeTweakerSaveSettings();
}
function themeTweakerResetSettings() {
	document.querySelector("#theme-tweak-control-clippy").checked = JSON.parse(window.localStorage.getItem("theme-tweaker-settings") || '{ "showClippy": true }')['showClippy'];
	document.querySelector(".clippy-container").style.display = document.querySelector("#theme-tweak-control-clippy").checked ? "block" : "none";
}
function themeTweakerSaveSettings() {
	window.localStorage.setItem("theme-tweaker-settings", JSON.stringify({ 'showClippy': document.querySelector("#theme-tweak-control-clippy").checked }));
}
function themeTweakerClippyCloseButtonClicked() {
	document.querySelector(".clippy-container").style.display = "none";
	window.localStorage.setItem("theme-tweaker-settings", JSON.stringify({ 'showClippy': false }));
	document.querySelector("#theme-tweak-control-clippy").checked = false;
}
function themeTweakerTextSizeAdjustButtonClicked(event) {
	var zoomFactor = parseFloat(window.currentTextZoom) || 1.0;
	if (event.target.hasClass("decrease")) {
		zoomFactor = (zoomFactor - 0.05).toFixed(2);
	} else if (event.target.hasClass("increase")) {
		zoomFactor = (zoomFactor + 0.05).toFixed(2);
	} else {
		zoomFactor = 1.0;
	}	
	setTextZoom(zoomFactor);
	window.currentTextZoom = `${zoomFactor}`;
	
	if (event.target.parentElement.id == "text-size-adjustment-ui") {
		window.localStorage.setItem("text-zoom", window.currentTextZoom);
	}
}
function updateThemeTweakerSampleText() {
	let sampleText = document.querySelector("#theme-tweaker-ui #theme-tweak-section-sample-text .sample-text");

	// This causes the sample text to take on the properties of the body text of a post.
	sampleText.removeClass("post-body");
	let bodyTextElement = document.querySelector(".post-body") || document.querySelector(".comment-body");
	sampleText.addClass("post-body");	
	sampleText.style.color = bodyTextElement ? 
								window.getComputedStyle(bodyTextElement).color : 
									window.getComputedStyle(document.querySelector("#content")).color;
	
	// Here we find out what is the actual background color that will be visible behind
	// the body text of posts, and set the sample text’s background to that.
	var backgroundElement = document.querySelector("#content");
	while (window.getComputedStyle(backgroundElement).backgroundColor == "" || 
		   window.getComputedStyle(backgroundElement).backgroundColor == "rgba(0, 0, 0, 0)")
		   backgroundElement = backgroundElement.parentElement;
	sampleText.parentElement.style.backgroundColor = window.getComputedStyle(backgroundElement).backgroundColor;
}

/*********************/
/* PAGE QUICK-NAV UI */
/*********************/

function injectQuickNavUI() {
	let quickNavContainer = addUIElement("<div id='quick-nav-ui'>" +
	`<a href='#top' title="Up to top [,]" accesskey=','>&#xf106;</a>
	<a href='#comments' title="Comments [/]" accesskey='/'>&#xf036;</a>
	<a href='#bottom-bar' title="Down to bottom [.]" accesskey='.'>&#xf107;</a>
	` + "</div>");
}

/**********************/
/* NEW COMMENT NAV UI */
/**********************/

function injectNewCommentNavUI(newCommentsCount) {
	let newCommentUIContainer = addUIElement("<div id='new-comment-nav-ui'>" + 
	`<button type='button' class='new-comment-sequential-nav-button new-comment-previous' title='Previous new comment [,]' tabindex='-1'>&#xf0d8;</button>
	<span class='new-comments-count'></span>
	<button type='button' class='new-comment-sequential-nav-button new-comment-next' title='Next new comment [.]' tabindex='-1'>&#xf0d7;</button>`
	+ "</div>");

	newCommentUIContainer.querySelector(".new-comment-previous").addActivateEvent(commentQuicknavButtonClicked);
	newCommentUIContainer.querySelector(".new-comment-next").addActivateEvent(commentQuicknavButtonClicked);

	document.addEventListener("keyup", (event) => { 
		if (event.shiftKey || event.ctrlKey || event.altKey) return;
		if (event.key == ",") scrollToNewComment(false);
		if (event.key == ".") scrollToNewComment(true)
	});
	
	let hnsDatePicker = addUIElement("<div id='hns-date-picker'>"
	+ `<span>Since:</span>`
	+ `<input type='text' class='hns-date'></input>`
	+ "</div>");
	
	hnsDatePicker.querySelector("input").addEventListener("input", OnInputUpdateHNSDate, false);
	
	newCommentUIContainer.querySelector(".new-comments-count").addActivateEvent(toggleHNSDatePickerVisibility);
}

function OnInputUpdateHNSDate() {
	let hns = time_fromHuman(this.value);
	let newCommentsCount = highlightCommentsSince(hns);
	updateNewCommentNavUI(newCommentsCount);
}

// time_fromHuman() function copied from https://bakkot.github.io/SlateStarComments/ssc.js
function time_fromHuman(string) {
	/* Convert a human-readable date into a JS timestamp */
	if (string.match(/^[0-9]{4}-[0-9]{2}-[0-9]{2}/)) {
		string = string.replace(' ', 'T');  // revert nice spacing
		string += ':00.000Z';  // complete ISO 8601 date
		time = Date.parse(string);  // milliseconds since epoch

		// browsers handle ISO 8601 without explicit timezone differently
		// thus, we have to fix that by hand
		time += (new Date()).getTimezoneOffset() * 60e3;
	} else {
		string = string.replace(' at', '');
		time = Date.parse(string);  // milliseconds since epoch
	}
	return time;
}

function updateNewCommentNavUI(newCommentsCount, hns = -1) {
	// Update the new comments count.
	let newCommentsCountLabel = document.querySelector("#new-comment-nav-ui .new-comments-count");
	newCommentsCountLabel.innerText = newCommentsCount;
	newCommentsCountLabel.title = `${newCommentsCount} new comments`;
	
	// Update the date picker field.
	if (hns != -1) {
		let hnsDatePickerInputField = document.querySelector("#hns-date-picker input");
		hnsDatePickerInputField.value = (new Date(+ hns - (new Date()).getTimezoneOffset() * 60e3)).toISOString().slice(0, 16).replace('T', ' ');
	}
}

function toggleHNSDatePickerVisibility() {
	let hnsDatePicker = document.querySelector("#hns-date-picker");
	let hnsDatePickerVisible = (window.getComputedStyle(hnsDatePicker).display != "none");
	hnsDatePicker.style.display = hnsDatePickerVisible ? "none" : "block";
}

/***************************/
/* TEXT SIZE ADJUSTMENT UI */
/***************************/

function injectTextSizeAdjustmentUIReal() {
	let textSizeAdjustmentUIContainer = addUIElement("<div id='text-size-adjustment-ui'>"
	+ `<button type='button' class='text-size-adjust-button decrease' title="Decrease text size [-]" tabindex='-1' accesskey='-'>&#xf068;</button>`
	+ `<button type='button' class='text-size-adjust-button default' title="Reset to default text size [0]" tabindex='-1' accesskey='0'>A</button>`
	+ `<button type='button' class='text-size-adjust-button increase' title="Increase text size [=]" tabindex='-1' accesskey='='>&#xf067;</button>`
	+ "</div>");
	
	textSizeAdjustmentUIContainer.querySelectorAll("button").forEach(button => {
		button.addActivateEvent(themeTweakerTextSizeAdjustButtonClicked);
	});
}

function injectTextSizeAdjustmentUI() {
	if (document.querySelector("#text-size-adjustment-ui") != null) return;
	if (document.querySelector("#content.post-page") != null) injectTextSizeAdjustmentUIReal();
	else document.addEventListener("DOMContentLoaded", function() {
		if (document.querySelector(".post-body") != null || document.querySelector(".comment-body") != null) injectTextSizeAdjustmentUIReal();
	}, {once: true});
}

/********************************/
/* COMMENTS VIEW MODE SELECTION */
/********************************/

function injectCommentsViewModeSelector() {
	let commentsContainer = document.querySelector("#comments");
	if (commentsContainer == null) return;

	let currentModeThreaded = (location.href.search("chrono=t") == -1);
	let newHref = "href='" + location.pathname + location.search.replace("chrono=t","") + (currentModeThreaded ? ((location.search == "" ? "?" : "&") + "chrono=t") : "") + location.hash + "' ";

	let commentsViewModeSelector = addUIElement("<div id='comments-view-mode-selector'>"
	+ `<a class="threaded ${currentModeThreaded ? 'selected' : ''}" ${currentModeThreaded ? "" : newHref} ${currentModeThreaded ? "" : "accesskey='x' "} title='Comments threaded view${currentModeThreaded ? "" : " [x]"}'>&#xf038;</a>`
	+ `<a class="chrono ${currentModeThreaded ? '' : 'selected'}" ${currentModeThreaded ? newHref : ""} ${currentModeThreaded ? "accesskey='x' " : ""} title='Comments chronological (flat) view${currentModeThreaded ? " [x]" : ""}'>&#xf017;</a>`
	+ "</div>");
	
// 	commentsViewModeSelector.querySelectorAll("a").forEach(button => {
// 		button.addActivateEvent(commentsViewModeSelectorButtonClicked);
// 	});
	
	if (!currentModeThreaded) {
		document.querySelectorAll(".comment-meta > a.comment-parent-link").forEach(commentParentLink => {
			commentParentLink.textContent = document.querySelector(commentParentLink.hash).querySelector(".author").textContent;
			commentParentLink.addClass("inline-author");
			commentParentLink.outerHTML = "<div class='comment-parent-link'>in reply to: " + commentParentLink.outerHTML + "</div>";
		});
		
		document.querySelectorAll(".comment-child-links a").forEach(commentChildLink => {
			commentChildLink.textContent = commentChildLink.textContent.slice(1);
			commentChildLink.addClasses([ "inline-author", "comment-child-link" ]);
		});
		
		rectifyChronoModeCommentChildLinks();
		
		commentsContainer.addClass("chrono");
	} else {
		commentsContainer.addClass("threaded");
	}

	// Remove extraneous top-level comment thread in chrono mode.
	let topLevelCommentThread = document.querySelector("#comments > .comment-thread");
	if (topLevelCommentThread.children.length == 0) removeElement(topLevelCommentThread);
}

// function commentsViewModeSelectorButtonClicked(event) {
// 	event.preventDefault();
// 	
// 	var newDocument;
// 	let request = new XMLHttpRequest();
// 	request.open("GET", event.target.href);
// 	request.onreadystatechange = () => {
// 		if (request.readyState != 4) return;
// 		newDocument = htmlToElement(request.response);
// 
// 		let classes = event.target.hasClass("threaded") ? { "old": "chrono", "new": "threaded" } : { "old": "threaded", "new": "chrono" };
// 
// 		// Update the buttons.
// 		event.target.addClass("selected");
// 		event.target.parentElement.querySelector("." + classes.old).removeClass("selected");
// 
// 		// Update the #comments container.
// 		let commentsContainer = document.querySelector("#comments");
// 		commentsContainer.removeClass(classes.old);
// 		commentsContainer.addClass(classes.new);
// 		
// 		// Update the content.
// 		commentsContainer.outerHTML = newDocument.querySelector("#comments").outerHTML;
// 	};
// 	request.send();
// }
// 
// function htmlToElement(html) {
//     var template = document.createElement('template');
//     template.innerHTML = html.trim();
//     return template.content;
// }

function rectifyChronoModeCommentChildLinks() {
	document.querySelectorAll(".comment-child-links").forEach(commentChildLinksContainer => {
		let children = childrenOfComment(commentChildLinksContainer.closest(".comment-item").id);
		let childLinks = commentChildLinksContainer.querySelectorAll("a");
		childLinks.forEach((link, index) => {
			link.href = "#" + children.find(child => child.querySelector(".author").textContent == link.textContent).id;
		});
		
		// Sort by date.
		let childLinksArray = Array.from(childLinks)
		childLinksArray.sort((a,b) => document.querySelector(`${a.hash} .date`).dataset["jsDate"] - document.querySelector(`${b.hash} .date`).dataset["jsDate"]);
		commentChildLinksContainer.innerHTML = "Replies: " + childLinksArray.map(childLink => childLink.outerHTML).join("");
	});
}
function childrenOfComment(commentID) {
	return Array.from(document.querySelectorAll(`#${commentID} ~ .comment-item`)).filter(commentItem => {
		let commentParentLink = commentItem.querySelector("a.comment-parent-link");
		return ((commentParentLink||{}).hash == "#" + commentID);
	});
}

/********************************/
/* COMMENTS LIST MODE SELECTION */
/********************************/

function injectCommentsListModeSelector() {
	if (document.querySelector("#content > .comment-thread") == null) return;
	
	let commentsListModeSelectorHTML = "<div id='comments-list-mode-selector'>"
	+ `<button type='button' class='expanded' title='Expanded comments view' tabindex='-1'></button>`
	+ `<button type='button' class='compact' title='Compact comments view' tabindex='-1'></button>`
	+ "</div>";
	(document.querySelector("#content.user-page .user-stats") || document.querySelector(".page-toolbar") || document.querySelector(".active-bar")).insertAdjacentHTML("afterend", commentsListModeSelectorHTML);
	let commentsListModeSelector = document.querySelector("#comments-list-mode-selector");
	
	commentsListModeSelector.querySelectorAll("button").forEach(button => {
		button.addActivateEvent(commentsListModeSelectButtonClicked);
	});
	
	let savedMode = (window.localStorage.getItem("comments-list-mode") == "compact") ? "compact" : "expanded";
	if (savedMode == "compact")	
		document.querySelector("#content").addClass("compact");
	commentsListModeSelector.querySelector(`.${savedMode}`).addClass("selected");
	commentsListModeSelector.querySelector(`.${savedMode}`).disabled = true;
	commentsListModeSelector.querySelector(`.${(savedMode == "compact" ? "expanded" : "compact")}`).accessKey = '`';
	
	if (window.isMobile) {
		document.querySelectorAll("#comments-list-mode-selector ~ .comment-thread").forEach(commentParentLink => {
			commentParentLink.addActivateEvent(function (event) {
				let parentCommentThread = event.target.closest("#content.compact .comment-thread");
				if (parentCommentThread) parentCommentThread.toggleClass("expanded");
			}, false);
		});
	}
}

function commentsListModeSelectButtonClicked(event) {
	event.target.parentElement.querySelectorAll("button").forEach(button => {
		button.removeClass("selected");
		button.disabled = false;
		button.accessKey = '`';
	});
	window.localStorage.setItem("comments-list-mode", event.target.className);
	event.target.addClass("selected");
	event.target.disabled = true;
	event.target.accessKey = '';

	if (event.target.hasClass("expanded")) {
		document.querySelector("#content").removeClass("compact");
	} else {
		document.querySelector("#content").addClass("compact");
	}
}

/**********************/
/* SITE NAV UI TOGGLE */
/**********************/

function injectSiteNavUIToggle() {
	let siteNavUIToggle = addUIElement("<div id='site-nav-ui-toggle'><button type='button' tabindex='-1'>&#xf0c9;</button></div>");
	siteNavUIToggle.querySelector("button").addActivateEvent(siteNavUIToggleButtonClicked);
	
	if (!window.isMobile && window.localStorage.getItem("site-nav-ui-toggle-engaged") == "true") toggleSiteNavUI();
}
function removeSiteNavUIToggle() {
	document.querySelectorAll("#primary-bar, #secondary-bar, .page-toolbar, #site-nav-ui-toggle button").forEach(element => {
		element.removeClass("engaged");
	});
	removeElement("#site-nav-ui-toggle");
}
function siteNavUIToggleButtonClicked() {
	toggleSiteNavUI();
	window.localStorage.setItem("site-nav-ui-toggle-engaged", event.target.hasClass("engaged"));
//	window.localStorage.getItem("site-nav-ui-toggle-engaged") != "true");
}
function toggleSiteNavUI() {
	document.querySelectorAll("#primary-bar, #secondary-bar, .page-toolbar, #site-nav-ui-toggle button").forEach(element => {
		element.toggleClass("engaged");
	});
}

/**********************/
/* POST NAV UI TOGGLE */
/**********************/

function injectPostNavUIToggle() {
	let postNavUIToggle = addUIElement("<div id='post-nav-ui-toggle'><button type='button' tabindex='-1'>&#xf14e;</button></div>");
	postNavUIToggle.querySelector("button").addActivateEvent(postNavUIToggleButtonClicked);
	
	if (window.localStorage.getItem("post-nav-ui-toggle-engaged") == "true") togglePostNavUI();
}
function removePostNavUIToggle() {
	document.querySelectorAll("#quick-nav-ui, #new-comment-nav-ui, #hns-date-picker, #post-nav-ui-toggle button").forEach(element => {
		element.removeClass("engaged");
	});
	removeElement("#post-nav-ui-toggle");
}
function postNavUIToggleButtonClicked(event) {
	togglePostNavUI();
	window.localStorage.setItem("post-nav-ui-toggle-engaged", window.localStorage.getItem("post-nav-ui-toggle-engaged") != "true");
}
function togglePostNavUI() {
	document.querySelectorAll("#quick-nav-ui, #new-comment-nav-ui, #hns-date-picker, #post-nav-ui-toggle button").forEach(element => {
		element.toggleClass("engaged");
	});
}

/*******************************/
/* APPEARANCE ADJUST UI TOGGLE */
/*******************************/

function injectAppearanceAdjustUIToggle() {
	let appearanceAdjustUIToggle = addUIElement("<div id='appearance-adjust-ui-toggle'><button type='button' tabindex='-1'>&#xf013;</button></div>");
	appearanceAdjustUIToggle.querySelector("button").addActivateEvent(appearanceAdjustUIToggleButtonClicked);
	
	if (window.isMobile) {
		let themeSelectorCloseButton = appearanceAdjustUIToggle.querySelector("button").cloneNode(true);
		themeSelectorCloseButton.addClass("theme-selector-close-button");
		themeSelectorCloseButton.innerHTML = "&#xf057;";
		document.querySelector("#theme-selector").appendChild(themeSelectorCloseButton);
		themeSelectorCloseButton.addActivateEvent(appearanceAdjustUIToggleButtonClicked);
	} else {
		if (window.localStorage.getItem("appearance-adjust-ui-toggle-engaged") == "true") toggleAppearanceAdjustUI();
	}
}
function removeAppearanceAdjustUIToggle() {
	document.querySelectorAll("#comments-view-mode-selector, #theme-selector, #width-selector, #text-size-adjustment-ui, #theme-tweaker-toggle, #appearance-adjust-ui-toggle button").forEach(element => {
		element.removeClass("engaged");
	});
	removeElement("#appearance-adjust-ui-toggle");
}
function appearanceAdjustUIToggleButtonClicked(event) {
	toggleAppearanceAdjustUI();
	window.localStorage.setItem("appearance-adjust-ui-toggle-engaged", event.target.hasClass("engaged"));
// 	window.localStorage.getItem("appearance-adjust-ui-toggle-engaged") != "true");
}
function toggleAppearanceAdjustUI() {
	document.querySelectorAll("#comments-view-mode-selector, #theme-selector, #width-selector, #text-size-adjustment-ui, #theme-tweaker-toggle, #appearance-adjust-ui-toggle button").forEach(element => {
		element.toggleClass("engaged");
	});
}

/*****************************/
/* MINIMIZED THREAD HANDLING */
/*****************************/

function expandAncestorsOf(commentId) {
	try { document.querySelector('#comment-'+commentId).closest("label[for^='expand'] + .comment-thread").parentElement.querySelector("input[id^='expand']").checked = true; }
	catch (ex) { console.log(ex); }
	try { document.querySelector('#comment-'+commentId).closest("#comments > ul > li").setCommentThreadMaximized(true, false, true); }
	catch (ex) { console.log(ex); }
}

/**************************/
/* WORD COUNT & READ TIME */
/**************************/

function toggleReadTimeOrWordCount(addWordCountClass) {
	document.querySelectorAll(".post-meta .read-time").forEach(element => {
		if (addWordCountClass) element.addClass("word-count");
		else element.removeClass("word-count");
		
		let titleParts = /(\S+)(.+)$/.exec(element.title);
		[ element.innerHTML, element.title ] = [ `${titleParts[1]}<span>${titleParts[2]}</span>`, element.textContent ];
	});
	
	
}
function readTimeOrWordCountClicked(event) {
	let displayWordCount = window.localStorage.getItem("display-word-count");
	toggleReadTimeOrWordCount(!displayWordCount);
	if (displayWordCount) window.localStorage.removeItem("display-word-count");
	else window.localStorage.setItem("display-word-count", true);
}

/**************************/
/* PROMPT TO SAVE CHANGES */
/**************************/

function enableBeforeUnload() {
	window.onbeforeunload = function () { return true; };
}
function disableBeforeUnload() {
	window.onbeforeunload = null;
}

/***************************/
/* ORIGINAL POSTER BADGING */
/***************************/

function markOriginalPosterComments() {
	let postAuthor = document.querySelector(".post .author");
	if (postAuthor == null) return;
	
	document.querySelectorAll(".comment-item .author, .comment-item .inline-author").forEach(author => {
		if (author.dataset.userid == postAuthor.dataset.userid ||
			(author.hash != "" && document.querySelector(`${author.hash} .author`).dataset.userid == postAuthor.dataset.userid))
			author.addClass("original-poster");
			author.title += "Original poster";
	});
}

/*****************/
/* ANTI-KIBITZER */
/*****************/

function numToAlpha(n) {
	let ret = "";
	do {
		ret = String.fromCharCode('A'.charCodeAt(0) + (n % 26)) + ret;
		n = Math.floor((n / 26) - 1);
	} while (n >= 0);
	return ret;
}

function injectAntiKibitzer() {
	// Inject anti-kibitzer toggle controls.
	let antiKibitzerToggle = addUIElement("<div id='anti-kibitzer-toggle'><button type='button' tabindex='-1' accesskey='g' title='Toggle anti-kibitzer (show/hide authors & karma values) [g]'></button>");
	antiKibitzerToggle.querySelector("button").addActivateEvent(antiKibitzerToggleButtonClicked);
	
	// Activate anti-kibitzer mode (if needed).
	if (window.localStorage.getItem("antikibitzer") == "true")
		toggleAntiKibitzerMode();
	
	// Remove temporary CSS that hides the authors and karma values.
	removeElement("#antikibitzer-temp");
}

function antiKibitzerToggleButtonClicked (event) {
	if (document.querySelector("#anti-kibitzer-toggle").hasClass("engaged") && 
		!event.shiftKey &&
		!confirm("Are you sure you want to turn OFF the anti-kibitzer?\n\n(This will reveal the authors and karma values of all posts and comments!)")) {
		event.target.blur();
		return;
	}

	toggleAntiKibitzerMode();
	event.target.blur();
}
function toggleAntiKibitzerMode() {
	// This will be the URL of the user's own page, if logged in, or the URL of
	// the login page otherwise.
	let userTabTarget = document.querySelector("#nav-item-login .nav-inner").href;
	let pageHeadingElement = document.querySelector("h1.page-main-heading");

	let userCount = 0;
	let userFakeName = { };

	let appellation = (document.querySelector(".post-page") ? "Commenter" : "User");

	let postAuthor = document.querySelector(".post-page .post-meta .author");
	if (postAuthor) userFakeName[postAuthor.dataset["userid"]] = "Original Poster";

	let antiKibitzerToggle = document.querySelector("#anti-kibitzer-toggle");
	if (antiKibitzerToggle.hasClass("engaged")) {
		window.localStorage.setItem("antikibitzer", "false");

		let redirectTarget = pageHeadingElement && pageHeadingElement.dataset["kibitzerRedirect"];
		if (redirectTarget) {
			window.location = redirectTarget;
			return;
		}

		// Individual comment page title and header
		if (document.querySelector(".individual-thread-page")) {
			let replacer = (node) => {
				if (!node) return;
				node.firstChild.replaceWith(node.dataset["trueContent"]);
			}
			replacer(document.querySelector("title:not(.fake-title)"));
			replacer(document.querySelector("#content > h1"));
		}

		// Author names/links.
		document.querySelectorAll(".author.redacted, .inline-author.redacted").forEach(author => {
			author.textContent = author.dataset["trueName"];
			if (/\/user/.test(author.href)) author.href = author.dataset["trueLink"];
			
			author.removeClass("redacted");
		});
		// Post/comment karma values.
		document.querySelectorAll(".karma-value.redacted").forEach(karmaValue => {
			karmaValue.innerHTML = karmaValue.dataset["trueValue"] + karmaValue.lastChild.outerHTML;
			karmaValue.lastChild.textContent = (parseInt(karmaValue.dataset["trueValue"]) == 1) ? " point" : " points";
			
			karmaValue.removeClass("redacted");
		});
		// Link post domains.
		document.querySelectorAll(".link-post-domain.redacted").forEach(linkPostDomain => {
			linkPostDomain.textContent = linkPostDomain.dataset["trueDomain"];
			
			linkPostDomain.removeClass("redacted");
		});
		
		antiKibitzerToggle.removeClass("engaged");
	} else {
		window.localStorage.setItem("antikibitzer", "true");

		let redirectTarget = pageHeadingElement && pageHeadingElement.dataset["antiKibitzerRedirect"];
		if (redirectTarget) {
			window.location = redirectTarget;
			return;
		}

		// Individual comment page title and header
		if (document.querySelector(".individual-thread-page")) {
			let replacer = (node) => {
				if (!node) return;
				node.dataset["trueContent"] = node.firstChild.wholeText;
				let newText = node.firstChild.wholeText.replace(/^.* comments/, "REDACTED comments");
				node.firstChild.replaceWith(newText);
			}
			replacer(document.querySelector("title:not(.fake-title)"));
			replacer(document.querySelector("#content > h1"));
		}
		
		removeElement("title.fake-title");

		// Author names/links.
		document.querySelectorAll(".author, .inline-author").forEach(author => {
			// Skip own posts/comments.
			if (author.hasClass("own-user-author"))
				return;

			let userid = author.dataset["userid"] || document.querySelector(`${author.hash} .author`).dataset["userid"];

			author.dataset["trueName"] = author.textContent;
			author.textContent = userFakeName[userid] || (userFakeName[userid] = appellation + " " + numToAlpha(userCount++));
			
			if (/\/user/.test(author.href)) {
				author.dataset["trueLink"] = author.pathname;
				author.href = "/user?id=" + author.dataset["userid"];
			}
			
			author.addClass("redacted");
		});
		// Post/comment karma values.
		document.querySelectorAll(".karma-value").forEach(karmaValue => {
			// Skip own posts/comments.
			if ((karmaValue.closest(".comment-item") || karmaValue.closest(".post-meta")).querySelector(".author").hasClass("own-user-author"))
				return;
		
			karmaValue.dataset["trueValue"] = karmaValue.firstChild.textContent;
			karmaValue.innerHTML = "##" + karmaValue.lastChild.outerHTML;
			karmaValue.lastChild.textContent = " points";
			
			karmaValue.addClass("redacted");
		});
		// Link post domains.
		document.querySelectorAll(".link-post-domain").forEach(linkPostDomain => {
			// Skip own posts/comments.
			if (userTabTarget == linkPostDomain.closest(".post-meta").querySelector(".author").href)
				return;
				
			linkPostDomain.dataset["trueDomain"] = linkPostDomain.textContent;
			linkPostDomain.textContent = "redacted.domain.tld";
			
			linkPostDomain.addClass("redacted");
		});
		
		antiKibitzerToggle.addClass("engaged");
	}
}

/*******************************/
/* COMMENT SORT MODE SELECTION */
/*******************************/

var CommentSortMode = Object.freeze({
	TOP:		"top",
	NEW:		"new",
	OLD:		"old",
	HOT:		"hot"
});
function sortComments(mode) {
	let commentsContainer = document.querySelector("#comments");

	commentsContainer.removeClass(/(sorted-\S+)/.exec(commentsContainer.className)[1]);
	commentsContainer.addClass("sorting");

	let clonedCommentsContainer = commentsContainer.cloneNode(true);
	clonedCommentsContainer.querySelectorAll(".comment-thread").forEach(commentThread => {
		var comparator;
		switch (mode) {
		case CommentSortMode.NEW:
			comparator = (a,b) => commentDate(b) - commentDate(a);
			break;
		case CommentSortMode.OLD:
			comparator = (a,b) => commentDate(a) - commentDate(b);
			break;
		case CommentSortMode.HOT:
			comparator = (a,b) => commentVoteCount(b) - commentVoteCount(a);
			break;
		case CommentSortMode.TOP:
		default:
			comparator = (a,b) => commentKarmaValue(b) - commentKarmaValue(a);
			break;
		}
		commentThread.innerHTML = Array.from(commentThread.childNodes).sort(comparator).map(commentItem => commentItem.outerHTML).join("");
	});
	commentsContainer.querySelector("#comments-sort-mode-selector + .comment-thread").innerHTML = clonedCommentsContainer.querySelector("#comments-sort-mode-selector + .comment-thread").innerHTML;

	window.requestAnimationFrame(() => {
		commentsContainer.removeClass("sorting");
		commentsContainer.addClass("sorted-" + mode);
	});
}
function commentKarmaValue(commentOrSelector) {
	if (typeof commentOrSelector == "string") commentOrSelector = document.querySelector(commentOrSelector);
	return parseInt(commentOrSelector.querySelector(".karma-value").firstChild.textContent);
}
function commentDate(commentOrSelector) {
	if (typeof commentOrSelector == "string") commentOrSelector = document.querySelector(commentOrSelector);
	return parseInt(commentOrSelector.querySelector(".date").dataset.jsDate);
}
function commentVoteCount(commentOrSelector) {
	if (typeof commentOrSelector == "string") commentOrSelector = document.querySelector(commentOrSelector);
	return parseInt(commentOrSelector.querySelector(".karma-value").title.split(" ")[0]);
}

function injectCommentsSortModeSelector() {
	let topCommentThread = document.querySelector("#comments > .comment-thread");
	if (topCommentThread == null) return;

	let commentsSortModeSelectorHTML = "<div id='comments-sort-mode-selector' class='sublevel-nav sort'>" + 
		Object.values(CommentSortMode).map(sortMode => `<button type='button' class='sublevel-item sort-mode-${sortMode}' tabindex='-1' title='Sort by ${sortMode}'>${sortMode}</button>`).join("") +  
		"</div>";
	topCommentThread.insertAdjacentHTML("beforebegin", commentsSortModeSelectorHTML);
	let commentsSortModeSelector = document.querySelector("#comments-sort-mode-selector");
	
	commentsSortModeSelector.querySelectorAll("button").forEach(button => {
		button.addActivateEvent(commentsSortModeSelectButtonClicked);
	});

	// TODO: Make this actually get the current sort mode (if that's saved).
	// TODO: Also change the condition here to properly get chrono/threaded mode,
	// when that is properly done with cookies.
	let currentSortMode = (location.href.search("chrono=t") == -1) ? CommentSortMode.TOP : CommentSortMode.OLD;
	topCommentThread.parentElement.addClass("sorted-" + currentSortMode);
	commentsSortModeSelector.querySelector(".sort-mode-" + currentSortMode).disabled = true;
	commentsSortModeSelector.querySelector(".sort-mode-" + currentSortMode).addClass("selected");
	setCommentsSortModeSelectButtonsAccesskey();
}

function commentsSortModeSelectButtonClicked(event) {
	event.target.parentElement.querySelectorAll("button").forEach(button => {
		button.removeClass("selected");
		button.disabled = false;
	});
	event.target.addClass("selected");
	event.target.disabled = true;

	sortComments(/sort-mode-(\S+)/.exec(event.target.className)[1]);
	setCommentsSortModeSelectButtonsAccesskey();
}
function setCommentsSortModeSelectButtonsAccesskey() {
	document.querySelectorAll("#comments-sort-mode-selector button").forEach(button => {
		button.accessKey = "";
		button.title = /(.+?)( \[z\])?$/.exec(button.title)[1];
	});
	let selectedButton = document.querySelector("#comments-sort-mode-selector button.selected");
	let nextButtonInCycle = (selectedButton == selectedButton.parentElement.lastChild) ? selectedButton.parentElement.firstChild : selectedButton.nextSibling;
	nextButtonInCycle.accessKey = "z";
	nextButtonInCycle.title += " [z]";
}

/*********************/
/* MORE MISC HELPERS */
/*********************/

function getQueryVariable(variable)
{
	var query = window.location.search.substring(1);
	var vars = query.split("&");
	for (var i = 0; i < vars.length; i++) {
		var pair = vars[i].split("=");
		if (pair[0] == variable)
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

function removeElement(elementOrSelector, ancestor = document) {
	if (typeof elementOrSelector == "string") elementOrSelector = ancestor.querySelector(elementOrSelector);
	if (elementOrSelector) elementOrSelector.parentElement.removeChild(elementOrSelector);
}

/******************/
/* INITIALIZATION */
/******************/

registerInitializer('earlyInitialize', true, () => document.querySelector("#content") != null, function () {
	// Check to see whether we're on a mobile device (which we define as a touchscreen)
// 	window.isMobile = /Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini/i.test(navigator.userAgent);
	window.isMobile = ('ontouchstart' in document.documentElement);

	// Backward compatibility
	let storedTheme = window.localStorage.getItem('selected-theme');
	if (storedTheme) {
		setTheme(storedTheme);
		window.localStorage.removeItem('selected-theme');
	}

	// Animate width & theme adjustments?
	window.adjustmentTransitions = false;

	// Add the content width selector.
	injectContentWidthSelector();
	// Add the theme selector.
	injectThemeSelector();
	// Add the theme tweaker.
	injectThemeTweaker();
	// Add the quick-nav UI.
	injectQuickNavUI();
	// Add the text size adjustment widget.
	injectTextSizeAdjustmentUI();
	
	try { updateInbox(); }
	catch (ex) { console.log(ex); }
});

registerInitializer('initialize', false, () => document.readyState != 'loading', function () {
	forceInitializer('earlyInitialize');

	if (getQueryVariable("comments") == "false")
		document.querySelector("#content").addClass("no-comments");
	if (getQueryVariable("hide-nav-bars") == "true") {
		document.querySelector("#content").addClass("no-nav-bars");
		let auxAboutLink = addUIElement("<div id='aux-about-link'><a href='/about' accesskey='t' target='_new'>&#xf129;</a></div>");
	}

	let content = document.querySelector("#content");
	if (content.querySelector("#comments .comment-thread") == null) {
		try { document.querySelector("#quick-nav-ui a[href='#comments']").addClass("no-comments"); }
		catch (ex) { console.log(ex); }
	}

	if (location.hash.length == 18) {
		location.hash = "#comment-" + location.hash.substring(1);
	}

	let useLongDate = window.innerWidth > 900;
	try {
		let dtf = new Intl.DateTimeFormat([], 
			( useLongDate ? 
				{ month: 'short', day: 'numeric', year: 'numeric', hour: 'numeric', minute: 'numeric' }
					: { month: 'numeric', day: 'numeric', year: '2-digit', hour: 'numeric', minute: 'numeric' } ));
		document.querySelectorAll(".date").forEach(date => {
			let d = date.getAttribute("data-js-date");
			if (d) { date.innerHTML = dtf.format(new Date(+ d)); }
		});
	}
	catch (ex) { console.log(ex); }

	window.needHashRealignment = false;

	// NOTE: Commenting this out here, adding it further on - for now.
	// Add comment parent popups.
// 	document.querySelectorAll(".comment-meta a.comment-parent-link, .comment-meta a.comment-child-link").forEach(commentParentLink => {
// 		commentParentLink.addEventListener("mouseover", function(e) {
// 			let parent_id = "#comment-" + /(?:#comment-)?(.+)/.exec(commentParentLink.getAttribute("href"))[1];
// 			var parent;
// 			try { parent = document.querySelector(parent_id).firstChild; } catch (ex) { console.log(ex); return; }
// 			let parentCI = parent.parentNode;
// 			var highlight_cn;
// 			if (parent.getBoundingClientRect().bottom < 10 || parent.getBoundingClientRect().top > window.innerHeight + 10) {
// 				highlight_cn = "comment-item-highlight-faint";
// 				parent = parent.cloneNode(true);
// 				parent.addClass("comment-popup")
// 				parent.addClass("comment-item-highlight");
// 				commentParentLink.addEventListener("mouseout", function(e) {
// 					parent.parentNode.removeChild(parent);
// 				}, {once: true});
// 				commentParentLink.parentNode.parentNode.appendChild(parent);
// 			} else {
// 				highlight_cn = "comment-item-highlight";
// 			}
// 			let className = parentCI.className;
// 			parentCI.className = className + " " + highlight_cn;
// 			commentParentLink.addEventListener("mouseout", (event) => { parentCI.className = cn; }, {once: true});
// 		});
// 	});

	document.querySelectorAll(".with-markdown-editor textarea, .conversation-page textarea").forEach(textarea => { textarea.addTextareaFeatures(); });
	document.querySelectorAll(((getQueryVariable("post-id")) ? "#edit-post-form textarea" : "#edit-post-form input[name='title']") + (window.isMobile ? "" : ", .conversation-page textarea")).forEach(field => { field.focus(); });

	let postMeta = document.querySelector(".post .post-meta");
	if (postMeta) {
		// Add "qualified hyperlinking" toolbar.
		let postPermalink = location.protocol + "//" + location.host + location.pathname;
		postMeta.insertAdjacentHTML("beforeend", "<div class='qualified-linking'>" + 
		"<input type='checkbox' tabindex='-1' id='qualified-linking-toolbar-toggle-checkbox'><label for='qualified-linking-toolbar-toggle-checkbox'><span>&#xf141;</span></label>" + 
		"<div class='qualified-linking-toolbar'>" +
		`<a href='${postPermalink}'>Post permalink</a>` +
		`<a href='${postPermalink}?comments=false'>Link without comments</a>` +
		`<a href='${postPermalink}?hide-nav-bars=true'>Link without top nav bars</a>` +
		`<a href='${postPermalink}?comments=false&hide-nav-bars=true'>Link without comments or top nav bars</a>` +
		"</div>" +
		"</div>");

		// Replicate .post-meta at bottom of post.
		let clonedPostMeta = postMeta.cloneNode(true);
		postMeta.addClass("top-post-meta");
		clonedPostMeta.addClass("bottom-post-meta");
		clonedPostMeta.querySelector("input[type='checkbox']").id += "-bottom";
		clonedPostMeta.querySelector("label").htmlFor += "-bottom";
		document.querySelector(".post").appendChild(clonedPostMeta);
	}
	
	if (loggedInUserId) {
		var comments_container = document.querySelector("#comments");
		if (comments_container) {
			// Add reply buttons.
			comments_container.querySelectorAll(".comment").forEach(comment => {
				comment.insertAdjacentHTML("afterend", "<div class='comment-controls posting-controls'></div>");
				comment.parentElement.querySelector(".comment-controls").injectCommentButtons();
			});
		
			// Add top-level new comment form.
			if (!document.querySelector(".individual-thread-page")) {
				comments_container.insertAdjacentHTML("afterbegin", "<div class='comment-controls posting-controls'></div>");
				comments_container.querySelector(".comment-controls").injectCommentButtons();
			}
		}

		// Add upvote/downvote buttons.
		if (typeof postVote != 'undefined') {
			document.querySelectorAll(".post-meta .karma-value").forEach(karmaValue => {
				addVoteButtons(karmaValue, postVote, 'Posts');
			});
		}
		if (typeof commentVotes != 'undefined') {
			document.querySelectorAll(".comment-meta .karma-value").forEach(karmaValue => {
				let commentID = karmaValue.getCommentId();
				addVoteButtons(karmaValue, commentVotes[commentID], 'Comments');
				karmaValue.parentElement.addClass("active-controls");
			});
			// Replicate karma controls at the bottom of comments.
			document.querySelectorAll(".comment-meta .karma").forEach(karmaControls => {
				let karmaControlsCloned = karmaControls.cloneNode(true);
				let commentControls = karmaControls.closest(".comment").nextSibling;
				commentControls.appendChild(karmaControlsCloned);
			});
		}
		document.querySelector("head").insertAdjacentHTML("beforeend","<style id='vote-buttons'>" + 
		`.upvote:hover,
		.upvote.selected {
			color: #00d800;
		}
		.downvote:hover,
		.downvote.selected {
			color: #eb4c2a;
		}` +
		"</style>");
		document.querySelectorAll("button.vote").forEach(voteButton => {
			voteButton.addActivateEvent(voteButtonClicked);
		});

		window.needHashRealignment = true;
	}

	// Clean up ToC
	document.querySelectorAll(".contents-list li a").forEach(tocLink => {
		tocLink.innerText = tocLink.innerText.replace(/^[0-9]+\. /, '');
		tocLink.innerText = tocLink.innerText.replace(/^[0-9]+: /, '');
		tocLink.innerText = tocLink.innerText.replace(/^M{0,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})\. /i, '');
		tocLink.innerText = tocLink.innerText.replace(/^[A-Z]\. /, '');
	});

	if (document.querySelector("#comments") != null) {
		// Add comment-minimize buttons to every comment.		
		document.querySelectorAll(".comment-meta").forEach(commentMeta => {
			if (!commentMeta.lastChild.hasClass("comment-minimize-button"))
				commentMeta.insertAdjacentHTML("beforeend", "<div class='comment-minimize-button maximized'>&#xf146;</div>");
		});
		if (!document.querySelector("#content").hasClass("individual-thread-page")) {
			// Format and activate comment-minimize buttons.
			document.querySelectorAll(".comment-minimize-button").forEach(button => {
				button.closest(".comment-item").setCommentThreadMaximized(false);
				button.addActivateEvent(commentMinimizeButtonClicked);
			});
		}
	}
	if (getQueryVariable("chrono") == "t") {
		document.querySelector("head").insertAdjacentHTML("beforeend", "<style>.comment-minimize-button::after { display: none; }</style>");
	}
	let urlParts = document.URL.split('#comment-');
	if (urlParts.length > 1) {
		expandAncestorsOf(urlParts[1]);
		window.needHashRealignment = true;
	}

	// Prevent conflict between various single-hotkey listeners and text fields
	document.querySelectorAll("input[type='text'], input[type='search'], input[type='password']").forEach(inputField => {
		inputField.addEventListener("keyup", (event) => { event.stopPropagation(); });
	});
	
	if (document.querySelector("#comments") && !document.querySelector(".individual-thread-page") && getPostHash()) {
		// Read and update last-visited-date.
		let lastVisitedDate = getLastVisitedDate();
		setLastVisitedDate(Date.now());
		
		// Save the number of comments this post has when it's visited.
		updateSavedCommentCount();

		// Add the new comments count & navigator.
		injectNewCommentNavUI();

		// Get the highlight-new-since date (as specified by URL parameter, if 
		// present, or otherwise the date of the last visit).
		let hns = parseInt(getQueryVariable("hns")) || lastVisitedDate;

		// Highlight new comments since the specified date.			 
		let newCommentsCount = highlightCommentsSince(hns);
		
		// Update the comment count display.
		updateNewCommentNavUI(newCommentsCount, hns);
	} else {
		// On listing pages, make comment counts more informative.
		badgePostsWithNewComments();
	}
	
	// Add the comments list mode selector widget (expanded vs. compact).
	injectCommentsListModeSelector();

	// Add the comments view selector widget (threaded vs. chrono).
	injectCommentsViewModeSelector();
	
	// Add the comments sort mode selector (top, hot, new, old).
	injectCommentsSortModeSelector();

	// Add the toggle for the post nav UI elements on mobile.
	if (window.isMobile) injectPostNavUIToggle();
	
	// Add the toggle for the appearance adjustment UI elements on mobile.
	if (window.isMobile) injectAppearanceAdjustUIToggle();

	// Add the antikibitzer.
	injectAntiKibitzer();
	
	// Mark original poster's comments with a special class.
	markOriginalPosterComments();

	// NOTE: This is commented out earlier on, and added here - for now.
	// Add comment parent popups.
	document.querySelectorAll(".comment-meta a.comment-parent-link, .comment-meta a.comment-child-link").forEach(commentParentLink => {
		commentParentLink.addEventListener("mouseover", (event) => {
			let parent_id = "#comment-" + /(?:#comment-)?(.+)/.exec(commentParentLink.getAttribute("href"))[1];
			var parent;
			try { parent = document.querySelector(parent_id).firstChild; } catch (ex) { console.log(ex); return; }
			let parentCI = parent.parentNode;
			var highlight_cn;
			if (parent.getBoundingClientRect().bottom < 10 || parent.getBoundingClientRect().top > window.innerHeight + 10) {
				highlight_cn = "comment-item-highlight-faint";
				parent = parent.cloneNode(true);
				parent.addClass("comment-popup")
				parent.addClass("comment-item-highlight");
				commentParentLink.addEventListener("mouseout", (event) => {
					removeElement(parent);
				}, {once: true});
				commentParentLink.closest(".comment").appendChild(parent);
			} else {
				highlight_cn = "comment-item-highlight";
			}
			let className = parentCI.className;
			parentCI.className = className + " " + highlight_cn;
			commentParentLink.addEventListener("mouseout", (event) => { parentCI.className = className; }, {once: true});
		});
	});

	// Add event listeners for Escape and Enter, for the theme tweaker.
	document.addEventListener("keyup", (event) => {
		if (event.keyCode == 27) {
		// Escape key.
			if (document.querySelector("#theme-tweaker-ui .help-window").style.display != "none") {
				toggleThemeTweakerHelpWindow();
				themeTweakerResetSettings();
			} else if (document.querySelector("#theme-tweaker-ui").style.display != "none") {
				toggleThemeTweakerUI();
				themeTweakReset();
			}
		} else if (event.keyCode == 13) {
		// Enter key.
			if (document.querySelector("#theme-tweaker-ui .help-window").style.display != "none") {
				toggleThemeTweakerHelpWindow();
				themeTweakerSaveSettings();
			} else if (document.querySelector("#theme-tweaker-ui").style.display != "none") {
				toggleThemeTweakerUI();
				themeTweakSave();
			}
		}
	});
	
	// Add event listener for . , / (for navigating listings pages).
	let listings = document.querySelectorAll("h1.listing a:last-of-type");
	if (listings.length > 0) {
		document.addEventListener("keyup", (event) => { 
			if (event.ctrlKey || event.shiftKey || event.altKey || !(event.key == "," || event.key == "." || event.key == '/')) return;
			
			if (event.key == '/') {
				if (document.activeElement.parentElement.hasClass("link-post-listing")) {
					let links = document.activeElement.parentElement.querySelectorAll("a");
					links[document.activeElement == links[0] ? 1 : 0].focus();
				}
				return;
			}
	
			var indexOfActiveListing = -1;
			for (i = 0; i < listings.length; i++) {
				if (listings[i] === document.activeElement.parentElement.querySelector("a[href^='/']")) {
					indexOfActiveListing = i;
					break;
				}
			}
			let indexOfNextListing = (event.key == "." ? ++indexOfActiveListing : (--indexOfActiveListing + listings.length)) % listings.length;
			listings[indexOfNextListing].focus();
		});
	}
	// Add event listener for / (to focus the link on link posts).
	if (document.querySelector("#content").hasClass("post-page") && 
		document.querySelector(".post").hasClass("link-post")) {
		document.addEventListener("keyup", function (e) {
			if (e.key == '/') document.querySelector("a.link-post-link").focus();
		});
	}

	// Add event listener for . , / (for navigating the recent comments page).
	let comments = document.querySelectorAll("#content > .comment-thread .comment-meta a.date");
	if (comments.length > 0) {
		document.addEventListener("keyup", (event) => {
			if (event.ctrlKey || event.shiftKey || event.altKey || !(event.key == "," || event.key == "." || event.key == '/')) return;

			if (event.key == '/') {
				if (document.activeElement.parentElement.hasClass("comment-meta")) {
					let links = document.activeElement.parentElement.querySelectorAll("a.date, a.permalink");
					links[document.activeElement == links[0] ? 1 : 0].focus();
					document.activeElement.closest(".comment-item").addClass("comment-item-highlight");
				}
				return;
			}

			try { document.activeElement.closest(".comment-item").removeClass("comment-item-highlight"); } catch (ex) { console.log(ex); }

			var indexOfActiveComment = -1;
			for (i = 0; i < comments.length; i++) {
				if (document.activeElement.parentElement.hasClass("comment-meta") && 
					comments[i] === document.activeElement.parentElement.querySelector("a.date")) {
					indexOfActiveComment = i;
					break;
				}
			}
			let indexOfNextComment = (event.key == "." ? ++indexOfActiveComment : (--indexOfActiveComment + comments.length)) % comments.length;
			comments[indexOfNextComment].closest(".comment-item").addClass("comment-item-highlight");
			comments[indexOfNextComment].closest(".comment-item").scrollIntoView();
			comments[indexOfNextComment].focus();
		});
		document.querySelectorAll("#content > .comment-thread .comment-meta a.date, #content > .comment-thread .comment-meta a.permalink").forEach(link => {
			link.addEventListener("blur", (event) => {
				event.target.closest(".comment-item").removeClass("comment-item-highlight");
			});
		});
	}

	// Add accesskeys to user page view selector.
	let viewSelector = document.querySelector("#content.user-page > .sublevel-nav");
	if (viewSelector) {
		let currentView = viewSelector.querySelector("span");
		(currentView.nextSibling || viewSelector.firstChild).accessKey = 'x';
		(currentView.previousSibling || viewSelector.lastChild).accessKey = 'z';
	}
	
	// Add accesskey to index page sort selector.
	(document.querySelector("#content.index-page > .sublevel-nav.sort a")||{}).accessKey = 'z';

	// Move MathJax style tags to <head>.
	var aggregatedStyles = "";
	document.querySelectorAll("#content style").forEach(styleTag => {
		aggregatedStyles += styleTag.innerHTML;
		removeElement("style", styleTag.parentElement);
	});
	if (aggregatedStyles != "") {
		document.querySelector("head").insertAdjacentHTML("beforeend", "<style id='mathjax-styles'>" + aggregatedStyles + "</style>");
	}
	
	// Add listeners to switch between word count and read time.
	if (window.localStorage.getItem("display-word-count")) toggleReadTimeOrWordCount(true);
	document.querySelectorAll(".post-meta .read-time").forEach(element => { element.addActivateEvent(readTimeOrWordCountClicked); });
});

/*************************/
/* POST-LOAD ADJUSTMENTS */
/*************************/

registerInitializer('pageLayoutFinished', false, () => document.readyState == "complete", function () {
	forceInitializer('initialize');

	realignHashIfNeeded();

	postSetThemeHousekeeping();
	
	// FOR TESTING ONLY, COMMENT WHEN DEPLOYING.
// 	document.querySelector("input[type='search']").value = document.documentElement.clientWidth;
});

function generateImagesOverlay() {
	// Don't do this on the about page.
	if (document.querySelector(".about-page") != null) return;

	// Remove existing, if any.
	removeElement("#images-overlay");

	// Create new.
	document.querySelector("body").insertAdjacentHTML("afterbegin", "<div id='images-overlay'></div>");
	let imagesOverlay = document.querySelector("#images-overlay");
	let imagesOverlayLeftOffset = imagesOverlay.getBoundingClientRect().left;
	document.querySelectorAll(".post-body img").forEach(image => {
		let clonedImage = image.cloneNode(true);
		clonedImage.style.top = image.getBoundingClientRect().top - parseFloat(window.getComputedStyle(image).marginTop) + window.scrollY + "px";
		clonedImage.style.left = image.getBoundingClientRect().left - parseFloat(window.getComputedStyle(image).marginLeft) - imagesOverlayLeftOffset + "px";
		clonedImage.style.width = image.getBoundingClientRect().width + "px";
		clonedImage.style.height = image.getBoundingClientRect().height + "px";
		clonedImage.style.border = window.getComputedStyle(image).border;
		imagesOverlay.appendChild(clonedImage);
	});
}

function adjustUIForWindowSize() {
	// Adjust bottom bar state.
	let bottomBar = document.querySelector("#bottom-bar");
	if (document.querySelector("#content").clientHeight > window.innerHeight + 30) {
		bottomBar.removeClass("decorative");
		
		bottomBar.querySelector("#nav-item-top").style.display = "";
	} else if (bottomBar) {
		if (bottomBar.childElementCount > 1) bottomBar.removeClass("decorative");
		else bottomBar.addClass("decorative");

		bottomBar.querySelector("#nav-item-top").style.display = "none";
	}
	
	// Show quick-nav UI up/down buttons if content is taller than window.
	document.querySelectorAll("#quick-nav-ui a[href='#top'], #quick-nav-ui a[href='#bottom-bar']").forEach(button => {
		button.style.visibility = (document.querySelector("#content").clientHeight > window.innerHeight + 30) ? "unset" : "hidden";
	});
	
	// Move anti-kibitzer toggle if content is very short.
	if (document.querySelector("#content").clientHeight < 400) (document.querySelector("#anti-kibitzer-toggle")||{}).style.bottom = "125px";

	// Add "horizontal" class to sort order selector when it's specified, via CSS, to
	// be horizontal (i.e. flex-direction: row)
	document.querySelectorAll(".sublevel-nav.sort").forEach(sortSelector => {
		if (window.getComputedStyle(sortSelector).flexDirection == "row") sortSelector.addClass("horizontal");
		else sortSelector.removeClass("horizontal");
	});
}

function recomputeUIElementsContainerHeight() {
	if (!window.isMobile && document.querySelector("#content").clientHeight <= window.innerHeight + 30) {
		document.querySelector("#ui-elements-container").style.height = document.querySelector("#content").clientHeight + "px";
	}
}

function realignHashIfNeeded() {
	if (window.needHashRealignment)
		realignHash();
}
function realignHash() {
	try { document.querySelector(location.hash).scrollIntoView(true); } catch (ex) { console.log(ex); }
}

/***********/
/* GUIEDIT */
/***********/

function insMarkup(event) {
	var mopen = '', mclose = '', mtext = '', func = false;
	if (typeof arguments[1] == 'function') {
		func = arguments[1];
	} else {
		mopen = arguments[1];
		mclose = arguments[2];
		mtext = arguments[3];
	}

	var tarea = event.target.closest("form").querySelector("textarea");
	tarea.focus();
	var p0 = tarea.selectionStart;
	var p1 = tarea.selectionEnd;
	var cur0 = cur1 = p0;
	
	var str = (p0 == p1) ? mtext : tarea.value.substring(p0, p1);
	str = func ? func(str, p0) : (mopen + str + mclose);
	
	// Determine selection.
	if (!func) {
		cur0 += (p0 == p1) ? mopen.length : str.length;
		cur1 = (p0 == p1) ? (cur0 + mtext.length) : cur0;
	} else {
		cur0 = str[1];
		cur1 = str[2];
		str = str[0];
	}

	// Update textarea contents.
	tarea.value = tarea.value.substring(0, p0) + str + tarea.value.substring(p1);
	
	// Set selection.
	tarea.selectionStart = cur0;
	tarea.selectionEnd = cur1;
	
	return;
}

var guiEditButtons = [
	[ 'strong', 'Strong (bold)', 'k', '**', '**', 'Bold text', '&#xf032;' ],
	[ 'em', 'Emphasized (italic)', 'i', '*', '*', 'Italicized text', '&#xf033;' ],
	[ 'link', 'Hyperlink', 'l', hyperlink, '', '', '&#xf0c1;' ],
	[ 'image', 'Image', '', '![', '](image url)', 'Image alt-text', '&#xf03e;' ],
	[ 'heading1', 'Heading level 1', '', '\\n# ', '', 'Heading', '&#xf1dc;<sup>1</sup>' ],
	[ 'heading2', 'Heading level 2', '', '\\n## ', '', 'Heading', '&#xf1dc;<sup>2</sup>' ],
	[ 'heading3', 'Heading level 3', '', '\\n### ', '', 'Heading', '&#xf1dc;<sup>3</sup>' ],
	[ 'blockquote', 'Blockquote', '', blockquote, '', '', '&#xf10e;' ],
	[ 'bulleted-list', 'Bulleted list', '', '\\n* ', '', 'List item', '&#xf0ca' ],
	[ 'numbered-list', 'Numbered list', '', '\\n1. ', '', 'List item', '&#xf0cb' ],
	[ 'horizontal-rule', 'Horizontal rule', '', '\\n\\n---\\n\\n', '', '', '&#xf068' ],
	[ 'inline-code', 'Inline code', '', '`', '`', 'Code', '&#xf121' ],
	[ 'code-block', 'Code block', '', '```\\n', '\\n```', 'Code', '&#xf1c9' ],
	[ 'formula', 'LaTeX', '', '$', '$', 'LaTeX formula', '&#xf155' ]
];

function blockquote(text, startpos) {
	if (text == '') {
		text = "> Quoted text";
		return [ text, startpos + 2, startpos + text.length ];
	} else {
		text = "> " + text.split("\n").join("\n> ") + "\n";
		return [ text, startpos + text.length, startpos + text.length ];
	}
}

function hyperlink(text, startpos) {
	var url = '', link_text = text, endpos = startpos;
	if (text.search(/^https?/) != -1) {
		url = text;
		link_text = "link text";
		startpos = startpos + 1;
		endpos = startpos + link_text.length;
	} else {
		url = prompt("Link address (URL):");
		if (!url) {
			endpos = startpos + text.length;
			return [ text, startpos, endpos ];
		}
		startpos = startpos + text.length + url.length + 4;
		endpos = startpos;
	}	
	
	return [ "[" + link_text + "](" + url + ")", startpos, endpos ];
}
