/***************************/
/* INITIALIZATION REGISTRY */
/***************************/

/*	Polyfill for requestIdleCallback in Apple and Microsoft browsers. */
if (!window.requestIdleCallback) {
	window.requestIdleCallback = (fn) => { setTimeout(fn, 0) };
}

/*	TBC. */
GW.initializersDone = { };
GW.initializers = { };
function registerInitializer(name, tryEarly, precondition, fn) {
	GW.initializersDone[name] = false;
	GW.initializers[name] = fn;
	let wrapper = function () {
		if (GW.initializersDone[name]) return;
		if (!precondition()) {
			if (tryEarly) {
				setTimeout(() => requestIdleCallback(wrapper, {timeout: 1000}), 50);
			} else {
				document.addEventListener("readystatechange", wrapper, {once: true});
			}
			return;
		}
		GW.initializersDone[name] = true;
		fn();
	};
	if (tryEarly) {
		requestIdleCallback(wrapper, {timeout: 1000});
	} else {
		document.addEventListener("readystatechange", wrapper, {once: true});
		requestIdleCallback(wrapper);
	}
}
function forceInitializer(name) {
	if (GW.initializersDone[name]) return;
	GW.initializersDone[name] = true;
	GW.initializers[name]();
}

/***********/
/* COOKIES */
/***********/

/*	Sets a cookie. */
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

/*	Reads the value of named cookie.
	Returns the cookie as a string, or null if no such cookie exists. */
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

/*******************************/
/* EVENT LISTENER MANIPULATION */
/*******************************/

/*	Adds an event listener to a button (or other clickable element), attaching 
	it to both "click" and "keyup" events (for use with keyboard navigation).
	Optionally also attaches the listener to the 'mousedown' event, making the 
	element activate on mouse down instead of mouse up. */
Element.prototype.addActivateEvent = function(func, includeMouseDown) {
	let ael = this.activateEventListener = (event) => { if (event.button === 0 || event.key === ' ') func(event) };
	if (includeMouseDown) this.addEventListener("mousedown", ael);
	this.addEventListener("click", ael);
	this.addEventListener("keyup", ael);
}

/*	Removes event listener from a clickable element, automatically detaching it
	from all relevant event types. */
Element.prototype.removeActivateEvent = function() {
	let ael = this.activateEventListener;
	this.removeEventListener("mousedown", ael);
	this.removeEventListener("click", ael);
	this.removeEventListener("keyup", ael);
}

/*	Adds a scroll event listener to the page. */
function addScrollListener(fn, name) {
	let wrapper = (event) => {
		requestAnimationFrame(() => {
			fn(event);
			document.addEventListener("scroll", wrapper, {once: true, passive: true});
		});
	}
	document.addEventListener("scroll", wrapper, {once: true, passive: true});

	// Retain a reference to the scroll listener, if a name is provided.
	if (typeof name != "undefined")
		GW[name] = wrapper;
}

/****************/
/* MISC HELPERS */
/****************/

/*	If top of element is not at or above the top of the screen, scroll it into
	view. */
Element.prototype.scrollIntoViewIfNeeded = function() {
	GWLog("scrollIntoViewIfNeeded");
	if (this.getBoundingClientRect().bottom > window.innerHeight && 
		this.getBoundingClientRect().top > 0) {
		this.scrollIntoView(false);
	}
}

/*	Get the comment ID of the item (if it's a comment) or of its containing 
	comment (if it's a child of a comment).
	*/
Element.prototype.getCommentId = function() {
	let item = (this.className == "comment-item" ? this : this.closest(".comment-item"));
	if (item) {
		return /^comment-(.*)/.exec(item.id)[1];
	} else {
		return false;
	}
}

function doAjax(params) {
	let req = new XMLHttpRequest();
	req.addEventListener("load", (event) => {
		if(event.target.status < 400) {
			if(params["onSuccess"]) params.onSuccess();
		} else {
			if(params["onFailure"]) params.onFailure();
		}
	});
	req.open((params["method"] || "GET"), (params.location || document.location));
	if(params["method"] == "POST") {
		req.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");
		params["params"]["csrf-token"] = GW.csrfToken;
		req.send(params.params.keys().map((x) => {return "" + x + "=" + encodeURIComponent(params.params[x])}).join("&"));
	} else {
		req.send();
	}
}

/*	Return the currently selected text, as HTML (rather than unstyled text).
	*/
function getSelectionHTML() {
	var container = document.createElement("div");
	container.appendChild(window.getSelection().getRangeAt(0).cloneContents());
	return container.innerHTML;
}

/*	Return the value of a GET (i.e., URL) parameter.
	*/
function getQueryVariable(variable) {
	var query = window.location.search.substring(1);
	var vars = query.split("&");
	for (var i = 0; i < vars.length; i++) {
		var pair = vars[i].split("=");
		if (pair[0] == variable)
			return pair[1];
	}

	return false;
}

/*	Given an HTML string, creates an element from that HTML, adds it to 
	#ui-elements-container (creating the latter if it does not exist), and 
	returns the created element.
	*/
function addUIElement(element_html) {
	var ui_elements_container = query("#ui-elements-container");
	if (!ui_elements_container) {
		ui_elements_container = document.createElement("div");
		ui_elements_container.id = "ui-elements-container";
		query("body").appendChild(ui_elements_container);
	}

	ui_elements_container.insertAdjacentHTML("beforeend", element_html);
	return ui_elements_container.lastElementChild;
}

/*	Given an element or a selector, removes that element (or the element 
	identified by the selector).
	If multiple elements match the selector, only the first is removed.
	*/
function removeElement(elementOrSelector, ancestor = document) {
	if (typeof elementOrSelector == "string") elementOrSelector = ancestor.query(elementOrSelector);
	if (elementOrSelector) elementOrSelector.parentElement.removeChild(elementOrSelector);
}

/*	Returns true if the string begins with the given prefix.
	*/
String.prototype.hasPrefix = function (prefix) {
	return (this.lastIndexOf(prefix, 0) === 0);
}

/*	Toggles whether the page is scrollable.
	*/
function togglePageScrolling(enable) {
	let body = query("body");
	if (!enable) {
		GW.scrollPositionBeforeScrollingDisabled = window.scrollY;
		body.addClass("no-scroll");
		body.style.top = `-${GW.scrollPositionBeforeScrollingDisabled}px`;
	} else {
		body.removeClass("no-scroll");
		body.removeAttribute("style");
		window.scrollTo(0, GW.scrollPositionBeforeScrollingDisabled);
	}
}

/********************/
/* DEBUGGING OUTPUT */
/********************/

function GWLog (string) {
	if (GW.loggingEnabled || localStorage.getItem("logging-enabled") == "true")
		console.log(string);
}
GW.enableLogging = (permanently = false) => {
	if (permanently)
		localStorage.setItem("logging-enabled", "true");
	else
		GW.loggingEnabled = true;
};
GW.disableLogging = (permanently = false) => {
	if (permanently)
		localStorage.removeItem("logging-enabled");
	else
		GW.loggingEnabled = false;
};

/*******************/
/* INBOX INDICATOR */
/*******************/

function updateInbox() {
	GWLog("updateInbox");
	if (!loggedInUserId) return;

	let request = new XMLHttpRequest();
	request.addEventListener("load", GW.updateInboxRequestLoaded = (event) => {
		GWLog("GW.updateInboxRequestLoaded");
		if (event.target.status != 200) return;

		let response = JSON.parse(event.target.responseText);
		if (response) {
			let element = query('#inbox-indicator');
			element.className = 'new-messages';
			element.title = 'New messages [o]';
		}
	});
	request.open("GET", "/check-notifications");
	request.send();
}

/**************/
/* COMMENTING */
/**************/

function toggleMarkdownHintsBox() {
	GWLog("toggleMarkdownHintsBox");
	let markdownHintsBox = query("#markdown-hints");
	markdownHintsBox.style.display = (getComputedStyle(markdownHintsBox).display == "none") ? "block" : "none";
}
function hideMarkdownHintsBox() {
	GWLog("hideMarkdownHintsBox");
	let markdownHintsBox = query("#markdown-hints");
	if (getComputedStyle(markdownHintsBox).display != "none") markdownHintsBox.style.display = "none";
}

Element.prototype.addTextareaFeatures = function() {
	GWLog("addTextareaFeatures");
	let textarea = this;

	textarea.addEventListener("focus", GW.textareaFocused = (event) => {
		GWLog("GW.textareaFocused");
		event.target.closest("form").scrollIntoViewIfNeeded();
	});
	textarea.addEventListener("input", GW.textareaInputReceived = (event) => {
		GWLog("GW.textareaInputReceived");
		if (window.innerWidth > 520) {
			// Expand textarea if needed.
			expandTextarea(textarea);
		} else {
			// Remove markdown hints.
			hideMarkdownHintsBox();
			query(".guiedit-mobile-help-button").removeClass("active");
		}
	}, false);
	textarea.addEventListener("keyup", (event) => { event.stopPropagation(); });
	textarea.addEventListener("keypress", (event) => { event.stopPropagation(); });

	textarea.insertAdjacentHTML("beforebegin", "<div class='guiedit-buttons-container'></div>");
	let textareaContainer = textarea.closest(".textarea-container");
	var buttons_container = textareaContainer.query(".guiedit-buttons-container");
	for (var button of GW.guiEditButtons) {
		let [ name, desc, accesskey, m_before_or_func, m_after, placeholder, icon ] = button;
		buttons_container.insertAdjacentHTML("beforeend", 
			"<button type='button' class='guiedit guiedit-" 
			+ name
			+ "' tabindex='-1'"
			+ ((accesskey != "") ? (" accesskey='" + accesskey + "'") : "")
			+ " title='" + desc + ((accesskey != "") ? (" [" + accesskey + "]") : "") + "'"
			+ " data-tooltip='" + desc + ((accesskey != "") ? (" [" + accesskey + "]") : "") + "'"
			+ " onclick='insertMarkup(event,"
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
	<div id='markdown-hints'>` + 
	[	"<span style='font-weight: bold;'>Bold</span><code>**Bold**</code>", 
		"<span style='font-style: italic;'>Italic</span><code>*Italic*</code>",
		"<span><a href=#>Link</a></span><code>[Link](http://example.com)</code>",
		"<span>Heading 1</span><code># Heading 1</code>",
		"<span>Heading 2</span><code>## Heading 1</code>",
		"<span>Heading 3</span><code>### Heading 1</code>",
		"<span>Blockquote</span><code>&gt; Blockquote</code>" ].map(row => "<div class='markdown-hints-row'>" + row + "</div>").join("") +
	`</div>`;
	textareaContainer.query("span").insertAdjacentHTML("afterend", markdown_hints);

	textareaContainer.queryAll(".guiedit-mobile-auxiliary-button").forEach(button => {
		button.addActivateEvent(GW.GUIEditMobileAuxiliaryButtonClicked = (event) => {
			GWLog("GW.GUIEditMobileAuxiliaryButtonClicked");
			if (button.hasClass("guiedit-mobile-help-button")) {
				toggleMarkdownHintsBox();
				event.target.toggleClass("active");
				query(".posting-controls:focus-within textarea").focus();
			} else if (button.hasClass("guiedit-mobile-exit-button")) {
				event.target.blur();
				hideMarkdownHintsBox();
				textareaContainer.query(".guiedit-mobile-help-button").removeClass("active");
			}
		});
	});

	// On smartphone (narrow mobile) screens, when a textarea is focused (and
	// automatically fullscreened), remove all the filters from the page, and 
	// then apply them *just* to the fixed editor UI elements. This is in order
	// to get around the "children of elements with a filter applied cannot be
	// fixed" issue".
	if (GW.isMobile && window.innerWidth <= 520) {
		let fixedEditorElements = textareaContainer.queryAll("textarea, .guiedit-buttons-container, .guiedit-mobile-auxiliary-button, #markdown-hints");
		textarea.addEventListener("focus", GW.textareaFocusedMobile = (event) => {
			GWLog("GW.textareaFocusedMobile");
			GW.savedFilters = GW.currentFilters;
			GW.currentFilters = { };
			applyFilters(GW.currentFilters);
			fixedEditorElements.forEach(element => {
				element.style.filter = filterStringFromFilters(GW.savedFilters);
			});
		});
		textarea.addEventListener("blur", GW.textareaBlurredMobile = (event) => {
			GWLog("GW.textareaBlurredMobile");
			GW.currentFilters = GW.savedFilters;
			GW.savedFilters = { };
			requestAnimationFrame(() => {
				applyFilters(GW.currentFilters);
				fixedEditorElements.forEach(element => {
					element.style.filter = filterStringFromFilters(GW.savedFilters);
				});
			});
		});
	}
}

Element.prototype.injectReplyForm = function(editMarkdownSource) {
	GWLog("injectReplyForm");
	let commentControls = this;
	let editCommentId = (editMarkdownSource ? commentControls.getCommentId() : false);
	let withparent = (!editMarkdownSource && commentControls.getCommentId());
	let answer = commentControls.parentElement.id == "answers";
	let parentAnswer = commentControls.closest("#answers > .comment-thread > .comment-item");
	let withParentAnswer = (!editMarkdownSource && parentAnswer && parentAnswer.getCommentId());
	let parentCommentItem = commentControls.closest(".comment-item");
	let alignmentForum = alignmentForumAllowed && (!parentCommentItem || parentCommentItem.firstChild.querySelector(".comment-meta .alignment-forum"));
	commentControls.innerHTML = "<button class='cancel-comment-button' tabindex='-1'>Cancel</button>" +
		"<form method='post'>" + 
		"<div class='textarea-container'>" + 
		"<textarea name='text' oninput='enableBeforeUnload();'></textarea>" +
		(withparent ? "<input type='hidden' name='parent-comment-id' value='" + commentControls.getCommentId() + "'>" : "") +
		(withParentAnswer ? "<input type='hidden' name='parent-answer-id' value='" + withParentAnswer + "'>" : "") +
		(editCommentId ? "<input type='hidden' name='edit-comment-id' value='" + editCommentId + "'>" : "") +
		(answer ? "<input type='hidden' name='answer' value='t'>" : "") +
		(alignmentForum ? "<input type='hidden' name='af' value='t'>" : "") +
		"<span class='markdown-reference-link'>You can use <a href='http://commonmark.org/help/' target='_blank'>Markdown</a> here.</span>" + 
		`<button type="button" class="guiedit-mobile-auxiliary-button guiedit-mobile-help-button">Help</button>` + 
		`<button type="button" class="guiedit-mobile-auxiliary-button guiedit-mobile-exit-button">Exit</button>` + 
		"</div><div>" + 
		"<input type='hidden' name='csrf-token' value='" + GW.csrfToken + "'>" +
		"<input type='submit' value='Submit'>" + 
		"</div></form>";
	commentControls.onsubmit = disableBeforeUnload;

	commentControls.query(".cancel-comment-button").addActivateEvent(GW.cancelCommentButtonClicked = (event) => {
		GWLog("GW.cancelCommentButtonClicked");
		hideReplyForm(event.target.closest(".comment-controls"));
	});
	commentControls.scrollIntoViewIfNeeded();
	commentControls.query("form").onsubmit = (event) => {
		if (!event.target.text.value) {
			alert("Please enter a comment.");
			return false;
		}
	}
	let textarea = commentControls.query("textarea");
	textarea.value = MarkdownFromHTML(editMarkdownSource || "");
	textarea.addTextareaFeatures();
	textarea.focus();
}

Element.prototype.updateCommentControlButton = function() {
	let retractFn = () => {
		if(this.closest(".comment-item").firstChild.hasClass("retracted"))
			return [ "unretract-button", "Un-retract", "Un-retract this comment" ];
		else
			return [ "retract-button", "Retract", "Retract this comment (without deleting)" ];
	};
	let classMap = {
		"delete-button": () => { return [ "delete-button", "Delete", "Delete this comment" ] },
		"retract-button": retractFn,
		"unretract-button": retractFn,
		"edit-button": () => { return [ "edit-button", "Edit", "Edit this comment" ] }
	};
	classMap.keys().forEach((testClass) => {
		if (this.hasClass(testClass)) {
			let [ buttonClass, buttonLabel, buttonAltText ] = classMap[testClass]();
			this.className = "";
			this.addClasses([ buttonClass, "action-button" ]);
			if (this.innerHTML || !this.dataset.label) this.innerHTML = buttonLabel;
			this.dataset.label = buttonLabel;
			this.title = buttonAltText;
			this.tabIndex = '-1';
			return;
		}
	});
}

Element.prototype.constructCommentControls = function() {
	GWLog("constructCommentControls");
	let commentControls = this;
	let commentType = (commentControls.parentElement.id == "answers" ? "answer" : "comment");
	commentControls.innerHTML = "";
	let replyButton = document.createElement("button");
	if (commentControls.parentElement.hasClass("comments")) {
		replyButton.className = "new-comment-button action-button";
		replyButton.innerHTML = "Post new " + commentType;
		replyButton.setAttribute("accesskey", (commentType == "comment" ? "n" : ""));
		replyButton.setAttribute("title", "Post new " + commentType + (commentType == "comment" ? " [n]" : ""));
	} else {
		if (commentControls.parentElement.query(".comment-body").hasAttribute("data-markdown-source")) {
			let buttonsList = [];
			if(!commentControls.parentElement.query(".comment-thread"))
				buttonsList.push("delete-button");
			buttonsList.push("retract-button", "edit-button");
			buttonsList.forEach(buttonClass => {
				let button = commentControls.appendChild(document.createElement("button"));
				button.addClass(buttonClass);
				button.updateCommentControlButton();
			});
		}
		replyButton.className = "reply-button action-button";
		replyButton.innerHTML = "Reply";
		replyButton.dataset.label = "Reply";
	}
	commentControls.appendChild(replyButton);
	replyButton.tabIndex = '-1';

	// On mobile, hide labels for all but the Reply button.
	if (GW.isMobile && window.innerWidth <= 900) {
		commentControls.queryAll(".delete-button, .retract-button, .unretract-button, .edit-button").forEach(button => {
			button.innerHTML = "";
		});
	}

	// Activate buttons.
	commentControls.queryAll(".action-button").forEach(button => {
		button.addActivateEvent(GW.commentActionButtonClicked);
	});

	// Replicate karma controls at the bottom of comments.
	if (commentControls.parentElement.hasClass("comments")) return;
	let karmaControls = commentControls.parentElement.query(".comment-meta .karma");
	if (!karmaControls) return;
	let karmaControlsCloned = karmaControls.cloneNode(true);
	commentControls.appendChild(karmaControlsCloned);
	commentControls.queryAll("button.vote").forEach(voteButton => {
		voteButton.addActivateEvent(voteButtonClicked);
	});
}

GW.commentActionButtonClicked = (event) => {
	GWLog("GW.commentActionButtonClicked");
	if (event.target.hasClass("edit-button") ||
		event.target.hasClass("reply-button") ||
		event.target.hasClass("new-comment-button")) {
		queryAll("textarea").forEach(textarea => {
			hideReplyForm(textarea.closest(".comment-controls"));
		});
	}

	if (event.target.hasClass("delete-button")) {
		let commentItem = event.target.closest(".comment-item");
		if (confirm("Are you sure you want to delete this comment?" + "\n\n" +
					"COMMENT DATE: " + commentItem.query(".date.").innerHTML + "\n" + 
					"COMMENT ID: " + /comment-(.+)/.exec(commentItem.id)[1] + "\n\n" + 
					"COMMENT TEXT:" + "\n" + commentItem.query(".comment-body").dataset.markdownSource))
			doCommentAction("delete", commentItem);
	} else if (event.target.hasClass("retract-button")) {
		doCommentAction("retract", event.target.closest(".comment-item"));
	} else if (event.target.hasClass("unretract-button")) {
		doCommentAction("unretract", event.target.closest(".comment-item"));
	} else if (event.target.hasClass("edit-button")) {
		showCommentEditForm(event.target.closest(".comment-item"));
	} else if (event.target.hasClass("reply-button")) {
		showReplyForm(event.target.closest(".comment-item"));
	} else if (event.target.hasClass("new-comment-button")) {
		showReplyForm(event.target.closest(".comments"));
	}

	event.target.blur();
};

function showCommentEditForm(commentItem) {
	GWLog("showCommentEditForm");

	let commentBody = commentItem.query(".comment-body");
	commentBody.style.display = "none";

	let commentControls = commentItem.query(".comment-controls");
	commentControls.injectReplyForm(commentBody.dataset.markdownSource);
	commentControls.query("form").addClass("edit-existing-comment");
	expandTextarea(commentControls.query("textarea"));
}

function showReplyForm(commentItem) {
	GWLog("showReplyForm");

	let commentControls = commentItem.query(".comment-controls");
	commentControls.injectReplyForm(commentControls.dataset.enteredText);
}

function hideReplyForm(commentControls) {
	GWLog("hideReplyForm");
	// Are we editing a comment? If so, un-hide the existing comment body.
	let containingComment = commentControls.closest(".comment-item");
	if (containingComment) containingComment.query(".comment-body").style.display = "";

	let enteredText = commentControls.query("textarea").value;
	if (enteredText) commentControls.dataset.enteredText = enteredText;

	disableBeforeUnload();
	commentControls.constructCommentControls();
}

function expandTextarea(textarea) {
	GWLog("expandTextarea");
	if (window.innerWidth <= 520) return;

	let totalBorderHeight = 30;
	if (textarea.clientHeight == textarea.scrollHeight + totalBorderHeight) return;

	requestAnimationFrame(() => {
		textarea.style.height = 'auto';
		textarea.style.height = textarea.scrollHeight + totalBorderHeight + 'px';
		if (textarea.clientHeight < window.innerHeight) {
			textarea.parentElement.parentElement.scrollIntoViewIfNeeded();
		}
	});
}

function doCommentAction(action, commentItem) {
	GWLog("doCommentAction");
	let params = {};
	params[(action + "-comment-id")] = commentItem.getCommentId();
	doAjax({
		method: "POST",
		params: params,
		onSuccess: GW.commentActionPostSucceeded = (event) => {
			GWLog("GW.commentActionPostSucceeded");
			let fn = {
				retract: () => { commentItem.firstChild.addClass("retracted") },
				unretract: () => { commentItem.firstChild.removeClass("retracted") },
				delete: () => {
					commentItem.firstChild.outerHTML = "<div class=\"comment deleted-comment\"><div class=\"comment-meta\"><span class=\"deleted-meta\">[ ]</span></div><div class=\"comment-body\">[deleted]</div></div>";
					commentItem.removeChild(commentItem.query(".comment-controls"));
				}
			}[action];
			if(fn) fn();
			if(action != "delete")
				commentItem.query(".comment-controls").queryAll(".action-button").forEach(x => {x.updateCommentControlButton()});
		}
	});
}

/**********/
/* VOTING */
/**********/

function parseVoteType(voteType) {
	GWLog("parseVoteType");
	let value = {};
	if (!voteType) return value;
	value.up = /[Uu]pvote$/.test(voteType);
	value.down = /[Dd]ownvote$/.test(voteType);
	value.big = /^big/.test(voteType);
	return value;
}

function makeVoteType(value) {
	GWLog("makeVoteType");
	return (value.big ? 'big' : 'small') + (value.up ? 'Up' : 'Down') + 'vote';
}

function makeVoteClass(vote) {
	GWLog("makeVoteClass");
	if (vote.up || vote.down) {
		return (vote.big ? 'selected big-vote' : 'selected');
	} else {
		return '';
	}
}

function addVoteButtons(element, voteType, targetType) {
	GWLog("addVoteButtons");
	let vote = parseVoteType(voteType);
	let voteClass = makeVoteClass(vote);
	element.insertAdjacentHTML('beforebegin', "<button type='button' class='vote upvote"+(vote.up ?' '+voteClass:'')+"' data-vote-type='upvote' data-target-type='"+targetType+"' tabindex='-1'></button>");
	element.insertAdjacentHTML('afterend', "<button type='button' class='vote downvote"+(vote.down ?' '+voteClass:'')+"' data-vote-type='downvote' data-target-type='"+targetType+"' tabindex='-1'></button>");
}

function makeVoteCompleteEvent(target) {
	GWLog("makeVoteCompleteEvent");
	return (GW.voteComplete = (event) => {
		GWLog("GW.voteComplete");
		var buttonTargets, karmaTargets;
		if (target === null) {
			buttonTargets = queryAll(".post-meta .karma");
			karmaTargets = queryAll(".post-meta .karma-value");
		} else {
			let commentItem = target.closest(".comment-item")
			buttonTargets = [ commentItem.query(".comment-meta .karma"), commentItem.query(".comment-controls .karma") ];
			karmaTargets = [ commentItem.query(".comment-meta .karma-value"), commentItem.query(".comment-controls .karma-value") ];
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
				buttonTarget.queryAll("button.vote").forEach(button => {
					button.removeClasses([ "clicked-once", "clicked-twice", "selected", "big-vote" ]);
					if (button.dataset.voteType == voteUpDown) button.addClass(voteClass);
				});
			});
		}
	});
}

function sendVoteRequest(targetId, targetType, voteType, onFinish) {
	GWLog("sendVoteRequest");
	let req = new XMLHttpRequest();
	req.addEventListener("load", onFinish);
	req.open("POST", "/karma-vote");
	req.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");
	req.send("csrf-token="+encodeURIComponent(GW.csrfToken)+"&target="+encodeURIComponent(targetId)+"&target-type="+encodeURIComponent(targetType)+"&vote-type="+encodeURIComponent(voteType));
}

function voteButtonClicked(event) {
	GWLog("voteButtonClicked");
	let voteButton = event.target;

	// 500 ms (0.5 s) double-click timeout.
	let doubleClickTimeout = 500;

	if (!voteButton.clickedOnce) {
		voteButton.clickedOnce = true;
		voteButton.addClass("clicked-once");

		setTimeout(GW.vbDoubleClickTimeoutCallback = (voteButton) => {
			if (!voteButton.clickedOnce) return;

			// Do single-click code.
			voteButton.clickedOnce = false;
			voteEvent(voteButton, 1);
		}, doubleClickTimeout, voteButton);
	} else {
		voteButton.clickedOnce = false;

		// Do double-click code.
		voteEvent(voteButton, 2);
		voteButton.removeClass("clicked-once");
		voteButton.addClass("clicked-twice");
	}
}
function voteEvent(voteButton, numClicks) {
	GWLog("voteEvent");
	voteButton.blur();
	voteButton.parentNode.addClass("waiting");
	let targetType = voteButton.dataset.targetType;
	let targetId = ((targetType == 'Comments') ? voteButton.getCommentId() : voteButton.parentNode.dataset.postId);
	let voteUpDown = voteButton.dataset.voteType;
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

Element.prototype.setCommentThreadMaximized = function(toggle, userOriginated = true, force) {
	GWLog("setCommentThreadMaximized");
	let commentItem = this;
	let storageName = "thread-minimized-" + commentItem.getCommentId();
	let minimize_button = commentItem.query(".comment-minimize-button");
	let maximize = force || (toggle ? /minimized/.test(minimize_button.className) : !(localStorage.getItem(storageName) || commentItem.hasClass("ignored")));
	if (userOriginated) {
		if (maximize) {
			localStorage.removeItem(storageName);
		} else {
			localStorage.setItem(storageName, true);
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
	let dateElement = item && item.query(".date");
	return (dateElement && parseInt(dateElement.dataset["jsDate"]));
}
function getCurrentVisibleComment() {
	let px = window.innerWidth/2, py = 5;
	let commentItem = document.elementFromPoint(px, py).closest(".comment-item") || document.elementFromPoint(px, py+60).closest(".comment-item"); // Mind the gap between threads
	let atbottom = query("#comments").getBoundingClientRect().bottom < window.innerHeight;
	if (atbottom) {
		let hashci = location.hash && query(location.hash);
		if (hashci && /comment-item/.test(hashci.className) && hashci.getBoundingClientRect().top > 0) {
			commentItem = hashci;
		}
	}
	return commentItem;
}

function highlightCommentsSince(date) {
	GWLog("highlightCommentsSince");
	var newCommentsCount = 0;
	GW.newComments = [ ];
	let oldCommentsStack = [ ];
	let prevNewComment;
	queryAll(".comment-item").forEach(commentItem => {
		commentItem.prevNewComment = prevNewComment;
		if (commentItem.getCommentDate() > date) {
			commentItem.addClass("new-comment");
			newCommentsCount++;
			GW.newComments.push(commentItem.getCommentId());
			oldCommentsStack.forEach(oldci => { oldci.nextNewComment = commentItem });
			oldCommentsStack = [ commentItem ];
			prevNewComment = commentItem;
		} else {
			commentItem.removeClass("new-comment");
			oldCommentsStack.push(commentItem);
		}
	});

	GW.newCommentScrollSet = (commentItem) => {
		query("#new-comment-nav-ui .new-comment-previous").disabled = commentItem ? !commentItem.prevNewComment : true;
		query("#new-comment-nav-ui .new-comment-next").disabled = commentItem ? !commentItem.nextNewComment : (GW.newComments.length == 0);
	};
	GW.newCommentScrollListener = () => {
		let commentItem = getCurrentVisibleComment();
		GW.newCommentScrollSet(commentItem);
	}

	addScrollListener(GW.newCommentScrollListener);

	if (document.readyState=="complete") {
		GW.newCommentScrollListener();
	} else {
		let commentItem = location.hash && /^#comment-/.test(location.hash) && query(location.hash);
		GW.newCommentScrollSet(commentItem);
	}

	registerInitializer("initializeCommentScrollPosition", false, () => document.readyState == "complete", GW.newCommentScrollListener);

	return newCommentsCount;
}

function scrollToNewComment(next) {
	GWLog("scrollToNewComment");
	let commentItem = getCurrentVisibleComment();
	let targetComment = null;
	let targetCommentID = null;
	if (commentItem) {
		targetComment = (next ? commentItem.nextNewComment : commentItem.prevNewComment);
		if (targetComment) {
			targetCommentID = targetComment.getCommentId();
		}
	} else {
		if (GW.newComments[0]) {
			targetCommentID = GW.newComments[0];
			targetComment = query("#comment-" + targetCommentID);
		}
	}
	if (targetComment) {
		expandAncestorsOf(targetCommentID);
		history.replaceState(null, null, "#comment-" + targetCommentID);
		targetComment.scrollIntoView();
	}

	GW.newCommentScrollListener();
}

function getPostHash() {
	let postHash = /^\/posts\/([^\/]+)/.exec(location.pathname);
	return (postHash ? postHash[1] : false);
}
function getLastVisitedDate() {
	// Get the last visited date (or, if posting a comment, the previous last visited date).
	let aCommentHasJustBeenPosted = (query(".just-posted-comment") != null);
	let storageName = (aCommentHasJustBeenPosted ? "previous-last-visited-date_" : "last-visited-date_") + getPostHash();
	return localStorage.getItem(storageName);
}
function setLastVisitedDate(date) {
	GWLog("setLastVisitedDate");
	// If NOT posting a comment, save the previous value for the last-visited-date 
	// (to recover it in case of posting a comment).
	let aCommentHasJustBeenPosted = (query(".just-posted-comment") != null);
	if (!aCommentHasJustBeenPosted) {
		let previousLastVisitedDate = (localStorage.getItem("last-visited-date_" + getPostHash()) || 0);
		localStorage.setItem("previous-last-visited-date_" + getPostHash(), previousLastVisitedDate);
	}

	// Set the new value.
	localStorage.setItem("last-visited-date_" + getPostHash(), date);
}

function updateSavedCommentCount() {
	GWLog("updateSavedCommentCount");
	let commentCount = queryAll(".comment").length;
	localStorage.setItem("comment-count_" + getPostHash(), commentCount);
}
function badgePostsWithNewComments() {
	GWLog("badgePostsWithNewComments");
	if (getQueryVariable("show") == "conversations") return;

	queryAll("h1.listing a[href^='/posts']").forEach(postLink => {
		let postHash = /posts\/(.+?)\//.exec(postLink.href)[1];

		let savedCommentCount = localStorage.getItem("comment-count_" + postHash);
		let commentCountDisplay = postLink.parentElement.nextSibling.query(".comment-count");
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
	GWLog("injectContentWidthSelector");
	// Get saved width setting (or default).
	let currentWidth = localStorage.getItem("selected-width") || 'normal';

	// Inject the content width selector widget and activate buttons.
	let widthSelector = addUIElement(
		"<div id='width-selector'>" +
		String.prototype.concat.apply("", GW.widthOptions.map(widthOption => {
			let [name, desc, abbr] = widthOption;
			let selected = (name == currentWidth ? ' selected' : '');
			let disabled = (name == currentWidth ? ' disabled' : '');
			return `<button type='button' class='select-width-${name}${selected}'${disabled} title='${desc}' tabindex='-1' data-name='${name}'>${abbr}</button>`})) +
		"</div>");
	widthSelector.queryAll("button").forEach(button => {
		button.addActivateEvent(GW.widthAdjustButtonClicked = (event) => {
			GWLog("GW.widthAdjustButtonClicked");

			// Determine which setting was chosen (i.e., which button was clicked).
			let selectedWidth = event.target.dataset.name;

			// Save the new setting.
			if (selectedWidth == "normal") localStorage.removeItem("selected-width");
			else localStorage.setItem("selected-width", selectedWidth);

			// Actually change the content width.
			setContentWidth(selectedWidth);
			event.target.parentElement.childNodes.forEach(button => {
				button.removeClass("selected");
				button.disabled = false;
			});
			event.target.addClass("selected");
			event.target.disabled = true;

			// Make sure the accesskey (to cycle to the next width) is on the right button.
			setWidthAdjustButtonsAccesskey();

			// Regenerate images overlay.
			generateImagesOverlay();

			// Realign hash.
			realignHash();
		});
	});

	// Make sure the accesskey (to cycle to the next width) is on the right button.
	setWidthAdjustButtonsAccesskey();

	// Inject transitions CSS, if animating changes is enabled.
	if (GW.adjustmentTransitions) {
		query("head").insertAdjacentHTML("beforeend", 
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
	GWLog("setWidthAdjustButtonsAccesskey");
	let widthSelector = query("#width-selector");
	widthSelector.queryAll("button").forEach(button => {
		button.removeAttribute("accesskey");
		button.title = /(.+?)( \['\])?$/.exec(button.title)[1];
	});
	let selectedButton = widthSelector.query("button.selected");
	let nextButtonInCycle = (selectedButton == selectedButton.parentElement.lastChild) ? selectedButton.parentElement.firstChild : selectedButton.nextSibling;
	nextButtonInCycle.accessKey = "'";
	nextButtonInCycle.title += ` [\']`;
}

/*******************/
/* THEME SELECTION */
/*******************/

function injectThemeSelector() {
	GWLog("injectThemeSelector");
	let currentTheme = readCookie("theme") || "default";
	let themeSelector = addUIElement(
		"<div id='theme-selector' class='theme-selector'>" +
		String.prototype.concat.apply("", GW.themeOptions.map(themeOption => {
			let [name, desc, letter] = themeOption;
			let selected = (name == currentTheme ? ' selected' : '');
			let disabled = (name == currentTheme ? ' disabled' : '');
			let accesskey = letter.charCodeAt(0) - 'A'.charCodeAt(0) + 1;
			return `<button type='button' class='select-theme-${name}${selected}'${disabled} title="${desc} [${accesskey}]" data-theme-name="${name}" data-theme-description="${desc}" accesskey='${accesskey}' tabindex='-1'>${letter}</button>`;})) +
		"</div>");
	themeSelector.queryAll("button").forEach(button => {
		button.addActivateEvent(GW.themeSelectButtonClicked = (event) => {
			GWLog("GW.themeSelectButtonClicked");
			let themeName = /select-theme-([^\s]+)/.exec(event.target.className)[1];
			setSelectedTheme(themeName);
			if (GW.isMobile) toggleAppearanceAdjustUI();
		});
	});

	// Inject transitions CSS, if animating changes is enabled.
	if (GW.adjustmentTransitions) {
		query("head").insertAdjacentHTML("beforeend", 
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
function setSelectedTheme(themeName) {
	GWLog("setSelectedTheme");
	queryAll(".theme-selector button").forEach(button => {
		button.removeClass("selected");
		button.disabled = false;
	});
	queryAll(".theme-selector button.select-theme-" + themeName).forEach(button => {
		button.addClass("selected");
		button.disabled = true;
	});
	setTheme(themeName);
	query("#theme-tweaker-ui .current-theme span").innerText = themeName;
}
function setTheme(newThemeName) {
	var themeUnloadCallback = '';
	var oldThemeName = '';
	if (typeof(newThemeName) == 'undefined') {
		newThemeName = readCookie('theme');
		if (!newThemeName) return;
	} else {
		themeUnloadCallback = GW['themeUnloadCallback_' + (readCookie('theme') || 'default')];
		oldThemeName = readCookie('theme') || 'default';

		if (newThemeName == 'default') setCookie('theme', '');
		else setCookie('theme', newThemeName);
	}
	if (themeUnloadCallback != null) themeUnloadCallback(newThemeName);

	let styleSheetNameSuffix = (newThemeName == 'default') ? '' : ('-' + newThemeName);
	let currentStyleSheetNameComponents = /style[^\.]*(\..+)$/.exec(query("head link[href*='.css']").href);

	let newStyle = document.createElement('link');
	newStyle.setAttribute('rel', 'stylesheet');
	newStyle.setAttribute('href', '/css/style' + styleSheetNameSuffix + currentStyleSheetNameComponents[1]);

	let oldStyle = query("head link[href*='.css']");
	newStyle.addEventListener('load', () => { removeElement(oldStyle); });
	newStyle.addEventListener('load', () => { postSetThemeHousekeeping(oldThemeName, newThemeName); });

	if (GW.adjustmentTransitions) {
		pageFadeTransition(false);
		setTimeout(() => {
			query('head').insertBefore(newStyle, oldStyle.nextSibling);
		}, 500);
	} else {
		query('head').insertBefore(newStyle, oldStyle.nextSibling);
	}
}
function postSetThemeHousekeeping(oldThemeName = "", newThemeName = (readCookie('theme') || 'default')) {
	recomputeUIElementsContainerHeight(true);

	let themeLoadCallback = GW['themeLoadCallback_' + newThemeName];
	if (themeLoadCallback != null) themeLoadCallback(oldThemeName);

	recomputeUIElementsContainerHeight();
	adjustUIForWindowSize();
	window.addEventListener('resize', GW.windowResized = (event) => {
		GWLog("GW.windowResized");
		adjustUIForWindowSize();
		recomputeUIElementsContainerHeight();
	});

	generateImagesOverlay();

	if (window.adjustmentTransitions) pageFadeTransition(true);
	updateThemeTweakerSampleText();

	if (typeof(window.msMatchMedia || window.MozMatchMedia || window.WebkitMatchMedia || window.matchMedia) !== 'undefined') {
		window.matchMedia('(orientation: portrait)').addListener(generateImagesOverlay);
	}

	setTimeout(realignHash, 0);
}

function pageFadeTransition(fadeIn) {
	if (fadeIn) {
		query("body").removeClass("transparent");
	} else {
		query("body").addClass("transparent");
	}
}

GW.themeLoadCallback_less = (fromTheme = "") => {
	GWLog("themeLoadCallback_less");
	injectSiteNavUIToggle();
	if (!GW.isMobile) {
		injectPostNavUIToggle();
		injectAppearanceAdjustUIToggle();
	}

	registerInitializer('shortenDate', true, () => query(".top-post-meta") != null, function () {
		let dtf = new Intl.DateTimeFormat([], 
			(window.innerWidth < 1100) ? 
				{ month: 'short', day: 'numeric', year: 'numeric' } : 
					{ month: 'long', day: 'numeric', year: 'numeric' });
		let postDate = query(".top-post-meta .date");
		postDate.innerHTML = dtf.format(new Date(+ postDate.dataset.jsDate));
	});

	if (GW.isMobile) {
		query("#content").insertAdjacentHTML("beforeend", "<div id='theme-less-mobile-first-row-placeholder'></div>");
	}

	if (!GW.isMobile) {
		registerInitializer('addSpans', true, () => query(".top-post-meta") != null, function () {
			queryAll(".top-post-meta .date, .top-post-meta .comment-count").forEach(element => {
				element.innerHTML = "<span>" + element.innerHTML + "</span>";
			});
		});

		if (localStorage.getItem("appearance-adjust-ui-toggle-engaged") == null) {
			// If state is not set (user has never clicked on the Less theme's appearance
			// adjustment UI toggle) then show it, but then hide it after a short time.
			registerInitializer('engageAppearanceAdjustUI', true, () => query("#ui-elements-container") != null, function () {
				toggleAppearanceAdjustUI();
				setTimeout(toggleAppearanceAdjustUI, 3000);
			});
		}

		if (fromTheme != "") {
			allUIToggles = queryAll("#ui-elements-container div[id$='-ui-toggle']");
			setTimeout(function () {
				allUIToggles.forEach(toggle => { toggle.addClass("highlighted"); });
			}, 300);
			setTimeout(function () {
				allUIToggles.forEach(toggle => { toggle.removeClass("highlighted"); });
			}, 1800);
		}

		// Unset the height of the #ui-elements-container.
		query("#ui-elements-container").style.height = "";

		// Due to filters vs. fixed elements, we need to be smarter about selecting which elements to filter...
		GW.themeTweaker.filtersExclusionPaths.themeLess = [
			"#content #secondary-bar",
			"#content .post .top-post-meta .date",
			"#content .post .top-post-meta .comment-count",
		];
		applyFilters(GW.currentFilters);
	}

	// We pre-query the relevant elements, so we don't have to run querySelectorAll
	// on every firing of the scroll listener.
	GW.scrollState = {
		"lastScrollTop":					window.pageYOffset || document.documentElement.scrollTop,
		"unbrokenDownScrollDistance":		0,
		"unbrokenUpScrollDistance":			0,
		"siteNavUIToggleButton":			query("#site-nav-ui-toggle button"),
		"siteNavUIElements":				queryAll("#primary-bar, #secondary-bar, .page-toolbar"),
		"appearanceAdjustUIToggleButton":	query("#appearance-adjust-ui-toggle button")
	};
	addScrollListener(updateSiteNavUIState, "updateSiteNavUIStateScrollListener");
}

// Hide the post-nav-ui toggle if none of the elements to be toggled are visible; 
// otherwise, show it.
function updatePostNavUIVisibility() {
	GWLog("updatePostNavUIVisibility");
	var hidePostNavUIToggle = true;
	queryAll("#quick-nav-ui a, #new-comment-nav-ui").forEach(element => {
		if (getComputedStyle(element).visibility == "visible" ||
			element.style.visibility == "visible" ||
			element.style.visibility == "unset")
			hidePostNavUIToggle = false;
	});
	queryAll("#quick-nav-ui, #post-nav-ui-toggle").forEach(element => {
		element.style.visibility = hidePostNavUIToggle ? "hidden" : "";
	});
}

// Hide the site nav and appearance adjust UIs on scroll down; show them on scroll up.
// NOTE: The UIs are re-shown on scroll up ONLY if the user has them set to be 
// engaged; if they're manually disengaged, they are not re-engaged by scroll.
function updateSiteNavUIState(event) {
	GWLog("updateSiteNavUIState");
	let newScrollTop = window.pageYOffset || document.documentElement.scrollTop;
	GW.scrollState.unbrokenDownScrollDistance = (newScrollTop > GW.scrollState.lastScrollTop) ? 
														(GW.scrollState.unbrokenDownScrollDistance + newScrollTop - GW.scrollState.lastScrollTop) : 
													 	0;
	GW.scrollState.unbrokenUpScrollDistance = (newScrollTop < GW.scrollState.lastScrollTop) ?
													 (GW.scrollState.unbrokenUpScrollDistance + GW.scrollState.lastScrollTop - newScrollTop) :
													 0;
	GW.scrollState.lastScrollTop = newScrollTop;

	// Hide site nav UI and appearance adjust UI when scrolling a full page down.
	if (GW.scrollState.unbrokenDownScrollDistance > window.innerHeight) {
		if (GW.scrollState.siteNavUIToggleButton.hasClass("engaged")) toggleSiteNavUI();
		if (GW.scrollState.appearanceAdjustUIToggleButton.hasClass("engaged")) toggleAppearanceAdjustUI();
	}

	// On mobile, make site nav UI translucent on ANY scroll down.
	if (GW.isMobile)
		GW.scrollState.siteNavUIElements.forEach(element => {
			if (GW.scrollState.unbrokenDownScrollDistance > 0) element.addClass("translucent-on-scroll");
			else element.removeClass("translucent-on-scroll");
		});

	// Show site nav UI when scrolling a full page up, or to the top.
	if ((GW.scrollState.unbrokenUpScrollDistance > window.innerHeight || 
		 GW.scrollState.lastScrollTop == 0) &&
		(!GW.scrollState.siteNavUIToggleButton.hasClass("engaged") && 
		 localStorage.getItem("site-nav-ui-toggle-engaged") != "false")) toggleSiteNavUI();

	// On desktop, show appearance adjust UI when scrolling to the top.
	if ((!GW.isMobile) && 
		(GW.scrollState.lastScrollTop == 0) &&
		(!GW.scrollState.appearanceAdjustUIToggleButton.hasClass("engaged")) && 
		(localStorage.getItem("appearance-adjust-ui-toggle-engaged") != "false")) toggleAppearanceAdjustUI();
}

GW.themeUnloadCallback_less = (toTheme = "") => {
	GWLog("themeUnloadCallback_less");
	removeSiteNavUIToggle();
	if (!GW.isMobile) {
		removePostNavUIToggle();
		removeAppearanceAdjustUIToggle();
	}
	window.removeEventListener('resize', updatePostNavUIVisibility);

	document.removeEventListener("scroll", GW["updateSiteNavUIStateScrollListener"]);

	removeElement("#theme-less-mobile-first-row-placeholder");

	if (!GW.isMobile) {
		// Remove spans
		queryAll(".top-post-meta .date, .top-post-meta .comment-count").forEach(element => {
			element.innerHTML = element.firstChild.innerHTML;
		});
	}

	(query(".top-post-meta .date")||{}).innerHTML = (query(".bottom-post-meta .date")||{}).innerHTML;

	// Reset filtered elements selector to default.
	delete GW.themeTweaker.filtersExclusionPaths.themeLess;
	applyFilters(GW.currentFilters);
}

GW.themeLoadCallback_dark = (fromTheme = "") => {
	GWLog("themeLoadCallback_dark");
	query("head").insertAdjacentHTML("beforeend", 
		"<style id='dark-theme-adjustments'>" + 
		`.markdown-reference-link a { color: #d200cf; filter: invert(100%); }` + 
		`#bottom-bar.decorative::before { filter: invert(100%); }` +
		"</style>");
	registerInitializer('makeImagesGlow', true, () => query("#images-overlay") != null, () => {
		queryAll(GW.imageFocus.overlayImagesSelector).forEach(image => {
			image.style.filter = "drop-shadow(0 0 0 #000) drop-shadow(0 0 0.5px #fff) drop-shadow(0 0 1px #fff) drop-shadow(0 0 2px #fff)";
			image.style.width = parseInt(image.style.width) + 12 + "px";
			image.style.height = parseInt(image.style.height) + 12 + "px";
			image.style.top = parseInt(image.style.top) - 6 + "px";
			image.style.left = parseInt(image.style.left) - 6 + "px";
		});
	});
}
GW.themeUnloadCallback_dark = (toTheme = "") => {
	GWLog("themeUnloadCallback_dark");
	removeElement("#dark-theme-adjustments");
}

GW.themeLoadCallback_brutalist = (fromTheme = "") => {
	GWLog("themeLoadCallback_brutalist");
	let bottomBarLinks = queryAll("#bottom-bar a");
	if (!GW.isMobile && bottomBarLinks.length == 5) {
		let newLinkTexts = [ "First", "Previous", "Top", "Next", "Last" ];
		bottomBarLinks.forEach((link, i) => {
			link.dataset.originalText = link.textContent;
			link.textContent = newLinkTexts[i];
		});
	}
}
GW.themeUnloadCallback_brutalist = (toTheme = "") => {
	GWLog("themeUnloadCallback_brutalist");
	let bottomBarLinks = queryAll("#bottom-bar a");
	if (!GW.isMobile && bottomBarLinks.length == 5) {
		bottomBarLinks.forEach(link => {
			link.textContent = link.dataset.originalText;
		});
	}
}

GW.themeLoadCallback_classic = (fromTheme = "") => {
	GWLog("themeLoadCallback_classic");
	queryAll(".comment-item .comment-controls .action-button").forEach(button => {
		button.innerHTML = "";
	});
}
GW.themeUnloadCallback_classic = (toTheme = "") => {
	GWLog("themeUnloadCallback_classic");
	if (GW.isMobile && window.innerWidth <= 900) return;
	queryAll(".comment-item .comment-controls .action-button").forEach(button => {
		button.innerHTML = button.dataset.label;
	});
}

/********************************************/
/* APPEARANCE CUSTOMIZATION (THEME TWEAKER) */
/********************************************/

function injectThemeTweaker() {
	GWLog("injectThemeTweaker");
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
		<div class='clippy'></div>
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

	// Clicking the background overlay closes the theme tweaker.
	themeTweakerUI.addActivateEvent(GW.themeTweaker.UIOverlayClicked = (event) => {
		GWLog("GW.themeTweaker.UIOverlayClicked");
		if (event.type == 'mousedown') {
			themeTweakerUI.style.opacity = "0.01";
		} else {
			toggleThemeTweakerUI();
			themeTweakerUI.style.opacity = "1.0";
			themeTweakReset();
		}
	}, true);

	// Intercept clicks, so they don't "fall through" the background overlay.
	(query("#theme-tweaker-ui > div")||{}).addActivateEvent((event) => { event.stopPropagation(); }, true);

	let sampleTextContainer = query("#theme-tweaker-ui #theme-tweak-section-sample-text .sample-text-container");
	themeTweakerUI.queryAll("input").forEach(field => {
		// All input types in the theme tweaker receive a 'change' event when
		// their value is changed. (Range inputs, in particular, receive this 
		// event when the user lets go of the handle.) This means we should
		// update the filters for the entire page, to match the new setting.
		field.addEventListener("change", GW.themeTweaker.fieldValueChanged = (event) => {
			GWLog("GW.themeTweaker.fieldValueChanged");
			if (event.target.id == 'theme-tweak-control-invert') {
				GW.currentFilters['invert'] = event.target.checked ? '100%' : '0%';
			} else if (event.target.type == 'range') {
				let sliderName = /^theme-tweak-control-(.+)$/.exec(event.target.id)[1];
				query("#theme-tweak-label-" + sliderName).innerText = event.target.value + event.target.dataset["labelSuffix"];
				GW.currentFilters[sliderName] = event.target.value + event.target.dataset["valueSuffix"];
			} else if (event.target.id == 'theme-tweak-control-clippy') {
				query(".clippy-container").style.display = event.target.checked ? "block" : "none";
			}
			// Clear the sample text filters.
			sampleTextContainer.style.filter = "";
			// Apply the new filters globally.
			applyFilters(GW.currentFilters);
		});

		// Range inputs receive an 'input' event while being scrubbed, updating
		// "live" as the handle is moved. We don't want to change the filters 
		// for the actual page while this is happening, but we do want to change
		// the filters for the *sample text*, so the user can see what effects
		// his changes are having, live, without having to let go of the handle.
		if (field.type == "range") field.addEventListener("input", GW.themeTweaker.fieldInputReceived = (event) => {
			GWLog("GW.themeTweaker.fieldInputReceived");
			var sampleTextFilters = GW.currentFilters;

			let sliderName = /^theme-tweak-control-(.+)$/.exec(event.target.id)[1];
			query("#theme-tweak-label-" + sliderName).innerText = event.target.value + event.target.dataset["labelSuffix"];
			sampleTextFilters[sliderName] = event.target.value + event.target.dataset["valueSuffix"];

			sampleTextContainer.style.filter = filterStringFromFilters(sampleTextFilters);
		});
	});

	themeTweakerUI.query(".minimize-button").addActivateEvent(GW.themeTweaker.minimizeButtonClicked = (event) => {
		GWLog("GW.themeTweaker.minimizeButtonClicked");
		let themeTweakerStyle = query("#theme-tweaker-style");

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
	});
	themeTweakerUI.query(".help-button").addActivateEvent(GW.themeTweaker.helpButtonClicked = (event) => {
		GWLog("GW.themeTweaker.helpButtonClicked");
		themeTweakerUI.query("#theme-tweak-control-clippy").checked = JSON.parse(localStorage.getItem("theme-tweaker-settings") || '{ "showClippy": true }')["showClippy"];
		toggleThemeTweakerHelpWindow();
	});
	themeTweakerUI.query(".reset-defaults-button").addActivateEvent(GW.themeTweaker.resetDefaultsButtonClicked = (event) => {
		GWLog("GW.themeTweaker.resetDefaultsButtonClicked");
		themeTweakerUI.query("#theme-tweak-control-invert").checked = false;
		[ "saturate", "brightness", "contrast", "hue-rotate" ].forEach(sliderName => {
			let slider = themeTweakerUI.query("#theme-tweak-control-" + sliderName);
			slider.value = slider.dataset['defaultValue'];
			themeTweakerUI.query("#theme-tweak-label-" + sliderName).innerText = slider.value + slider.dataset['labelSuffix'];
		});
		GW.currentFilters = { };
		applyFilters(GW.currentFilters);

		GW.currentTextZoom = "1.0";
		setTextZoom(GW.currentTextZoom);

		setSelectedTheme("default");
	});
	themeTweakerUI.query(".main-theme-tweaker-window .cancel-button").addActivateEvent(GW.themeTweaker.cancelButtonClicked = (event) => {
		GWLog("GW.themeTweaker.cancelButtonClicked");
		toggleThemeTweakerUI();
		themeTweakReset();
	});
	themeTweakerUI.query(".main-theme-tweaker-window .ok-button").addActivateEvent(GW.themeTweaker.OKButtonClicked = (event) => {
		GWLog("GW.themeTweaker.OKButtonClicked");
		toggleThemeTweakerUI();
		themeTweakSave();
	});
	themeTweakerUI.query(".help-window .cancel-button").addActivateEvent(GW.themeTweaker.helpWindowCancelButtonClicked = (event) => {
		GWLog("GW.themeTweaker.helpWindowCancelButtonClicked");
		toggleThemeTweakerHelpWindow();
		themeTweakerResetSettings();
	});
	themeTweakerUI.query(".help-window .ok-button").addActivateEvent(GW.themeTweaker.helpWindowOKButtonClicked = (event) => {
		GWLog("GW.themeTweaker.helpWindowOKButtonClicked");
		toggleThemeTweakerHelpWindow();
		themeTweakerSaveSettings();
	});

	themeTweakerUI.queryAll(".notch").forEach(notch => {
		notch.addActivateEvent(GW.themeTweaker.sliderNotchClicked = (event) => {
			GWLog("GW.themeTweaker.sliderNotchClicked");
			let slider = event.target.parentElement.query("input[type='range']");
			slider.value = slider.dataset['defaultValue'];
			event.target.parentElement.query(".theme-tweak-control-label").innerText = slider.value + slider.dataset['labelSuffix'];
			GW.currentFilters[/^theme-tweak-control-(.+)$/.exec(slider.id)[1]] = slider.value + slider.dataset['valueSuffix'];
			applyFilters(GW.currentFilters);
		});
	});

	themeTweakerUI.query(".clippy-close-button").addActivateEvent(GW.themeTweaker.clippyCloseButtonClicked = (event) => {
		GWLog("GW.themeTweaker.clippyCloseButtonClicked");
		themeTweakerUI.query(".clippy-container").style.display = "none";
		localStorage.setItem("theme-tweaker-settings", JSON.stringify({ 'showClippy': false }));
		themeTweakerUI.query("#theme-tweak-control-clippy").checked = false;
	});

	query("head").insertAdjacentHTML("beforeend","<style id='theme-tweaker-style'></style>");

	themeTweakerUI.query(".theme-selector").innerHTML = query("#theme-selector").innerHTML;
	themeTweakerUI.queryAll(".theme-selector button").forEach(button => {
		button.addActivateEvent(GW.themeSelectButtonClicked);
	});

	themeTweakerUI.queryAll("#theme-tweak-section-text-size-adjust button").forEach(button => {
		button.addActivateEvent(GW.themeTweaker.textSizeAdjustButtonClicked);
	});

	let themeTweakerToggle = addUIElement(`<div id='theme-tweaker-toggle'><button type='button' tabindex='-1' title="Customize appearance [;]" accesskey=';'>&#xf1de;</button></div>`);
	themeTweakerToggle.query("button").addActivateEvent(GW.themeTweaker.toggleButtonClicked = (event) => {
		GWLog("GW.themeTweaker.toggleButtonClicked");
		GW.themeTweakerStyleSheetAvailable = () => {
			GWLog("GW.themeTweakerStyleSheetAvailable");
			themeTweakerUI.query(".current-theme span").innerText = (readCookie("theme") || "default");

			themeTweakerUI.query("#theme-tweak-control-invert").checked = (GW.currentFilters['invert'] == "100%");
			[ "saturate", "brightness", "contrast", "hue-rotate" ].forEach(sliderName => {
				let slider = themeTweakerUI.query("#theme-tweak-control-" + sliderName);
				slider.value = /^[0-9]+/.exec(GW.currentFilters[sliderName]) || slider.dataset['defaultValue'];
				themeTweakerUI.query("#theme-tweak-label-" + sliderName).innerText = slider.value + slider.dataset['labelSuffix'];
			});

			toggleThemeTweakerUI();
			event.target.disabled = true;
		};

		if (query("link[href^='/css/theme_tweaker.css']")) {
			// Theme tweaker CSS is already loaded.
			GW.themeTweakerStyleSheetAvailable();
		} else {
			// Load the theme tweaker CSS (if not loaded).
			let themeTweakerStyleSheet = document.createElement('link');
			themeTweakerStyleSheet.setAttribute('rel', 'stylesheet');
			themeTweakerStyleSheet.setAttribute('href', '/css/theme_tweaker.css');
			themeTweakerStyleSheet.addEventListener('load', GW.themeTweakerStyleSheetAvailable);
			query("head").appendChild(themeTweakerStyleSheet);
		}
	});
}
function toggleThemeTweakerUI() {
	GWLog("toggleThemeTweakerUI");
	let themeTweakerUI = query("#theme-tweaker-ui");
	themeTweakerUI.style.display = (themeTweakerUI.style.display == "none") ? "block" : "none";
	query("#theme-tweaker-style").innerHTML = (themeTweakerUI.style.display == "none") ? "" : 
		`#content, #ui-elements-container > div:not(#theme-tweaker-ui) {
			pointer-events: none;
		}`;
	if (themeTweakerUI.style.display != "none") {
		// Save selected theme.
		GW.currentTheme = (readCookie("theme") || "default");
		// Focus invert checkbox.
		query("#theme-tweaker-ui #theme-tweak-control-invert").focus();
		// Show sample text in appropriate font.
		updateThemeTweakerSampleText();
		// Disable tab-selection of the search box.
		setSearchBoxTabSelectable(false);
		// Disable scrolling of the page.
		togglePageScrolling(false);
	} else {
		query("#theme-tweaker-toggle button").disabled = false;
		// Re-enable tab-selection of the search box.
		setSearchBoxTabSelectable(true);
		// Re-enable scrolling of the page.
		togglePageScrolling(true);
	}
	// Set theme tweaker assistant visibility.
	query(".clippy-container").style.display = JSON.parse(localStorage.getItem("theme-tweaker-settings") || '{ "showClippy": true }')["showClippy"] ? "block" : "none";
}
function setSearchBoxTabSelectable(selectable) {
	GWLog("setSearchBoxTabSelectable");
	query("input[type='search']").tabIndex = selectable ? "" : "-1";
	query("input[type='search'] + button").tabIndex = selectable ? "" : "-1";
}
function toggleThemeTweakerHelpWindow() {
	GWLog("toggleThemeTweakerHelpWindow");
	let themeTweakerHelpWindow = query("#theme-tweaker-ui .help-window");
	themeTweakerHelpWindow.style.display = (themeTweakerHelpWindow.style.display == "none") ? "block" : "none";
	if (themeTweakerHelpWindow.style.display != "none") {
		// Focus theme tweaker assistant checkbox.
		query("#theme-tweaker-ui #theme-tweak-control-clippy").focus();
		// Disable interaction on main theme tweaker window.
		query("#theme-tweaker-ui").style.pointerEvents = "none";
		query("#theme-tweaker-ui .main-theme-tweaker-window").style.pointerEvents = "none";
	} else {
		// Re-enable interaction on main theme tweaker window.
		query("#theme-tweaker-ui").style.pointerEvents = "auto";
		query("#theme-tweaker-ui .main-theme-tweaker-window").style.pointerEvents = "auto";
	}
}
function themeTweakReset() {
	GWLog("themeTweakReset");
	setSelectedTheme(GW.currentTheme);
	GW.currentFilters = JSON.parse(localStorage.getItem("theme-tweaks") || "{ }");
	applyFilters(GW.currentFilters);
	GW.currentTextZoom = `${parseFloat(localStorage.getItem("text-zoom")) || 1.0}`;
	setTextZoom(GW.currentTextZoom);
}
function themeTweakSave() {
	GWLog("themeTweakSave");
	GW.currentTheme = (readCookie("theme") || "default");
	localStorage.setItem("theme-tweaks", JSON.stringify(GW.currentFilters));
	localStorage.setItem("text-zoom", GW.currentTextZoom);
}

function themeTweakerResetSettings() {
	GWLog("themeTweakerResetSettings");
	query("#theme-tweak-control-clippy").checked = JSON.parse(localStorage.getItem("theme-tweaker-settings") || '{ "showClippy": true }')['showClippy'];
	query(".clippy-container").style.display = query("#theme-tweak-control-clippy").checked ? "block" : "none";
}
function themeTweakerSaveSettings() {
	GWLog("themeTweakerSaveSettings");
	localStorage.setItem("theme-tweaker-settings", JSON.stringify({ 'showClippy': query("#theme-tweak-control-clippy").checked }));
}
function updateThemeTweakerSampleText() {
	GWLog("updateThemeTweakerSampleText");
	let sampleText = query("#theme-tweaker-ui #theme-tweak-section-sample-text .sample-text");

	// This causes the sample text to take on the properties of the body text of a post.
	sampleText.removeClass("post-body");
	let bodyTextElement = query(".post-body") || query(".comment-body");
	sampleText.addClass("post-body");
	sampleText.style.color = bodyTextElement ? 
								getComputedStyle(bodyTextElement).color : 
									getComputedStyle(query("#content")).color;

	// Here we find out what is the actual background color that will be visible behind
	// the body text of posts, and set the sample text’s background to that.
	var backgroundElement = query("#content");
	let searchField = query("#nav-item-search input");
	if (!(getComputedStyle(searchField).backgroundColor == "" || 
		  getComputedStyle(searchField).backgroundColor == "rgba(0, 0, 0, 0)"))
		backgroundElement = searchField;
	else while (getComputedStyle(backgroundElement).backgroundColor == "" || 
				getComputedStyle(backgroundElement).backgroundColor == "rgba(0, 0, 0, 0)")
				backgroundElement = backgroundElement.parentElement;
	sampleText.parentElement.style.backgroundColor = getComputedStyle(backgroundElement).backgroundColor;
}

/*********************/
/* PAGE QUICK-NAV UI */
/*********************/

function injectQuickNavUI() {
	GWLog("injectQuickNavUI");
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
	GWLog("injectNewCommentNavUI");
	let newCommentUIContainer = addUIElement("<div id='new-comment-nav-ui'>" + 
	`<button type='button' class='new-comment-sequential-nav-button new-comment-previous' title='Previous new comment (,)' tabindex='-1'>&#xf0d8;</button>
	<span class='new-comments-count'></span>
	<button type='button' class='new-comment-sequential-nav-button new-comment-next' title='Next new comment (.)' tabindex='-1'>&#xf0d7;</button>`
	+ "</div>");

	newCommentUIContainer.queryAll(".new-comment-sequential-nav-button").forEach(button => {
		button.addActivateEvent(GW.commentQuicknavButtonClicked = (event) => {
			GWLog("GW.commentQuicknavButtonClicked");
			scrollToNewComment(/next/.test(event.target.className));
			event.target.blur();
		});
	});

	document.addEventListener("keyup", GW.commentQuicknavKeyPressed = (event) => { 
		GWLog("GW.commentQuicknavKeyPressed");
		if (event.shiftKey || event.ctrlKey || event.altKey) return;
		if (event.key == ",") scrollToNewComment(false);
		if (event.key == ".") scrollToNewComment(true)
	});

	let hnsDatePicker = addUIElement("<div id='hns-date-picker'>"
	+ `<span>Since:</span>`
	+ `<input type='text' class='hns-date'></input>`
	+ "</div>");

	hnsDatePicker.query("input").addEventListener("input", GW.hnsDatePickerValueChanged = (event) => {
		GWLog("GW.hnsDatePickerValueChanged");
		let hnsDate = time_fromHuman(event.target.value);
		let newCommentsCount = highlightCommentsSince(hnsDate);
		updateNewCommentNavUI(newCommentsCount);
	}, false);

	newCommentUIContainer.query(".new-comments-count").addActivateEvent(GW.newCommentsCountClicked = (event) => {
		GWLog("GW.newCommentsCountClicked");
		let hnsDatePickerVisible = (getComputedStyle(hnsDatePicker).display != "none");
		hnsDatePicker.style.display = hnsDatePickerVisible ? "none" : "block";
	});
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

function updateNewCommentNavUI(newCommentsCount, hnsDate = -1) {
	GWLog("updateNewCommentNavUI");
	// Update the new comments count.
	let newCommentsCountLabel = query("#new-comment-nav-ui .new-comments-count");
	newCommentsCountLabel.innerText = newCommentsCount;
	newCommentsCountLabel.title = `${newCommentsCount} new comments`;

	// Update the date picker field.
	if (hnsDate != -1) {
		query("#hns-date-picker input").value = (new Date(+ hnsDate - (new Date()).getTimezoneOffset() * 60e3)).toISOString().slice(0, 16).replace('T', ' ');
	}
}

/***************************/
/* TEXT SIZE ADJUSTMENT UI */
/***************************/

GW.themeTweaker.textSizeAdjustButtonClicked = (event) => {
	GWLog("GW.themeTweaker.textSizeAdjustButtonClicked");
	var zoomFactor = parseFloat(GW.currentTextZoom) || 1.0;
	if (event.target.hasClass("decrease")) {
		zoomFactor = (zoomFactor - 0.05).toFixed(2);
	} else if (event.target.hasClass("increase")) {
		zoomFactor = (zoomFactor + 0.05).toFixed(2);
	} else {
		zoomFactor = 1.0;
	}
	setTextZoom(zoomFactor);
	GW.currentTextZoom = `${zoomFactor}`;

	if (event.target.parentElement.id == "text-size-adjustment-ui") {
		localStorage.setItem("text-zoom", GW.currentTextZoom);
	}
};

function injectTextSizeAdjustmentUIReal() {
	GWLog("injectTextSizeAdjustmentUIReal");
	let textSizeAdjustmentUIContainer = addUIElement("<div id='text-size-adjustment-ui'>"
	+ `<button type='button' class='text-size-adjust-button decrease' title="Decrease text size [-]" tabindex='-1' accesskey='-'>&#xf068;</button>`
	+ `<button type='button' class='text-size-adjust-button default' title="Reset to default text size [0]" tabindex='-1' accesskey='0'>A</button>`
	+ `<button type='button' class='text-size-adjust-button increase' title="Increase text size [=]" tabindex='-1' accesskey='='>&#xf067;</button>`
	+ "</div>");

	textSizeAdjustmentUIContainer.queryAll("button").forEach(button => {
		button.addActivateEvent(GW.themeTweaker.textSizeAdjustButtonClicked);
	});

	GW.currentTextZoom = `${parseFloat(localStorage.getItem("text-zoom")) || 1.0}`;
}

function injectTextSizeAdjustmentUI() {
	GWLog("injectTextSizeAdjustmentUI");
	if (query("#text-size-adjustment-ui") != null) return;
	if (query("#content.post-page") != null) injectTextSizeAdjustmentUIReal();
	else document.addEventListener("DOMContentLoaded", () => {
		if (!(query(".post-body") == null && query(".comment-body") == null)) injectTextSizeAdjustmentUIReal();
	}, {once: true});
}

/********************************/
/* COMMENTS VIEW MODE SELECTION */
/********************************/

function injectCommentsViewModeSelector() {
	GWLog("injectCommentsViewModeSelector");
	let commentsContainer = query("#comments");
	if (commentsContainer == null) return;

	let currentModeThreaded = (location.href.search("chrono=t") == -1);
	let newHref = "href='" + location.pathname + location.search.replace("chrono=t","") + (currentModeThreaded ? ((location.search == "" ? "?" : "&") + "chrono=t") : "") + location.hash + "' ";

	let commentsViewModeSelector = addUIElement("<div id='comments-view-mode-selector'>"
	+ `<a class="threaded ${currentModeThreaded ? 'selected' : ''}" ${currentModeThreaded ? "" : newHref} ${currentModeThreaded ? "" : "accesskey='x' "} title='Comments threaded view${currentModeThreaded ? "" : " [x]"}'>&#xf038;</a>`
	+ `<a class="chrono ${currentModeThreaded ? '' : 'selected'}" ${currentModeThreaded ? newHref : ""} ${currentModeThreaded ? "accesskey='x' " : ""} title='Comments chronological (flat) view${currentModeThreaded ? " [x]" : ""}'>&#xf017;</a>`
	+ "</div>");

// 	commentsViewModeSelector.queryAll("a").forEach(button => {
// 		button.addActivateEvent(commentsViewModeSelectorButtonClicked);
// 	});

	if (!currentModeThreaded) {
		queryAll(".comment-meta > a.comment-parent-link").forEach(commentParentLink => {
			commentParentLink.textContent = query(commentParentLink.hash).query(".author").textContent;
			commentParentLink.addClass("inline-author");
			commentParentLink.outerHTML = "<div class='comment-parent-link'>in reply to: " + commentParentLink.outerHTML + "</div>";
		});

		queryAll(".comment-child-links a").forEach(commentChildLink => {
			commentChildLink.textContent = commentChildLink.textContent.slice(1);
			commentChildLink.addClasses([ "inline-author", "comment-child-link" ]);
		});

		rectifyChronoModeCommentChildLinks();

		commentsContainer.addClass("chrono");
	} else {
		commentsContainer.addClass("threaded");
	}

	// Remove extraneous top-level comment thread in chrono mode.
	let topLevelCommentThread = query("#comments > .comment-thread");
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
// 		event.target.parentElement.query("." + classes.old).removeClass("selected");
// 
// 		// Update the #comments container.
// 		let commentsContainer = query("#comments");
// 		commentsContainer.removeClass(classes.old);
// 		commentsContainer.addClass(classes.new);
// 
// 		// Update the content.
// 		commentsContainer.outerHTML = newDocument.query("#comments").outerHTML;
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
	GWLog("rectifyChronoModeCommentChildLinks");
	queryAll(".comment-child-links").forEach(commentChildLinksContainer => {
		let children = childrenOfComment(commentChildLinksContainer.closest(".comment-item").id);
		let childLinks = commentChildLinksContainer.queryAll("a");
		childLinks.forEach((link, index) => {
			link.href = "#" + children.find(child => child.query(".author").textContent == link.textContent).id;
		});

		// Sort by date.
		let childLinksArray = Array.from(childLinks)
		childLinksArray.sort((a,b) => query(`${a.hash} .date`).dataset["jsDate"] - query(`${b.hash} .date`).dataset["jsDate"]);
		commentChildLinksContainer.innerHTML = "Replies: " + childLinksArray.map(childLink => childLink.outerHTML).join("");
	});
}
function childrenOfComment(commentID) {
	return Array.from(queryAll(`#${commentID} ~ .comment-item`)).filter(commentItem => {
		let commentParentLink = commentItem.query("a.comment-parent-link");
		return ((commentParentLink||{}).hash == "#" + commentID);
	});
}

/********************************/
/* COMMENTS LIST MODE SELECTION */
/********************************/

function injectCommentsListModeSelector() {
	GWLog("injectCommentsListModeSelector");
	if (query("#content > .comment-thread") == null) return;

	let commentsListModeSelectorHTML = "<div id='comments-list-mode-selector'>"
	+ `<button type='button' class='expanded' title='Expanded comments view' tabindex='-1'></button>`
	+ `<button type='button' class='compact' title='Compact comments view' tabindex='-1'></button>`
	+ "</div>";
	(query("#content.user-page .user-stats") || query(".page-toolbar") || query(".active-bar")).insertAdjacentHTML("afterend", commentsListModeSelectorHTML);
	let commentsListModeSelector = query("#comments-list-mode-selector");

	commentsListModeSelector.queryAll("button").forEach(button => {
		button.addActivateEvent(GW.commentsListModeSelectButtonClicked = (event) => {
			GWLog("GW.commentsListModeSelectButtonClicked");
			event.target.parentElement.queryAll("button").forEach(button => {
				button.removeClass("selected");
				button.disabled = false;
				button.accessKey = '`';
			});
			localStorage.setItem("comments-list-mode", event.target.className);
			event.target.addClass("selected");
			event.target.disabled = true;
			event.target.removeAttribute("accesskey");

			if (event.target.hasClass("expanded")) {
				query("#content").removeClass("compact");
			} else {
				query("#content").addClass("compact");
			}
		});
	});

	let savedMode = (localStorage.getItem("comments-list-mode") == "compact") ? "compact" : "expanded";
	if (savedMode == "compact")
		query("#content").addClass("compact");
	commentsListModeSelector.query(`.${savedMode}`).addClass("selected");
	commentsListModeSelector.query(`.${savedMode}`).disabled = true;
	commentsListModeSelector.query(`.${(savedMode == "compact" ? "expanded" : "compact")}`).accessKey = '`';

	if (GW.isMobile) {
		queryAll("#comments-list-mode-selector ~ .comment-thread").forEach(commentParentLink => {
			commentParentLink.addActivateEvent(function (event) {
				let parentCommentThread = event.target.closest("#content.compact .comment-thread");
				if (parentCommentThread) parentCommentThread.toggleClass("expanded");
			}, false);
		});
	}
}

/**********************/
/* SITE NAV UI TOGGLE */
/**********************/

function injectSiteNavUIToggle() {
	GWLog("injectSiteNavUIToggle");
	let siteNavUIToggle = addUIElement("<div id='site-nav-ui-toggle'><button type='button' tabindex='-1'>&#xf0c9;</button></div>");
	siteNavUIToggle.query("button").addActivateEvent(GW.siteNavUIToggleButtonClicked = (event) => {
		GWLog("GW.siteNavUIToggleButtonClicked");
		toggleSiteNavUI();
		localStorage.setItem("site-nav-ui-toggle-engaged", event.target.hasClass("engaged"));
	});

	if (!GW.isMobile && localStorage.getItem("site-nav-ui-toggle-engaged") == "true") toggleSiteNavUI();
}
function removeSiteNavUIToggle() {
	GWLog("removeSiteNavUIToggle");
	queryAll("#primary-bar, #secondary-bar, .page-toolbar, #site-nav-ui-toggle button").forEach(element => {
		element.removeClass("engaged");
	});
	removeElement("#site-nav-ui-toggle");
}
function toggleSiteNavUI() {
	GWLog("toggleSiteNavUI");
	queryAll("#primary-bar, #secondary-bar, .page-toolbar, #site-nav-ui-toggle button").forEach(element => {
		element.toggleClass("engaged");
		element.removeClass("translucent-on-scroll");
	});
}

/**********************/
/* POST NAV UI TOGGLE */
/**********************/

function injectPostNavUIToggle() {
	GWLog("injectPostNavUIToggle");
	let postNavUIToggle = addUIElement("<div id='post-nav-ui-toggle'><button type='button' tabindex='-1'>&#xf14e;</button></div>");
	postNavUIToggle.query("button").addActivateEvent(GW.postNavUIToggleButtonClicked = (event) => {
		GWLog("GW.postNavUIToggleButtonClicked");
		togglePostNavUI();
		localStorage.setItem("post-nav-ui-toggle-engaged", localStorage.getItem("post-nav-ui-toggle-engaged") != "true");
	});

	if (localStorage.getItem("post-nav-ui-toggle-engaged") == "true") togglePostNavUI();
}
function removePostNavUIToggle() {
	GWLog("removePostNavUIToggle");
	queryAll("#quick-nav-ui, #new-comment-nav-ui, #hns-date-picker, #post-nav-ui-toggle button").forEach(element => {
		element.removeClass("engaged");
	});
	removeElement("#post-nav-ui-toggle");
}
function togglePostNavUI() {
	GWLog("togglePostNavUI");
	queryAll("#quick-nav-ui, #new-comment-nav-ui, #hns-date-picker, #post-nav-ui-toggle button").forEach(element => {
		element.toggleClass("engaged");
	});
}

/*******************************/
/* APPEARANCE ADJUST UI TOGGLE */
/*******************************/

function injectAppearanceAdjustUIToggle() {
	GWLog("injectAppearanceAdjustUIToggle");
	let appearanceAdjustUIToggle = addUIElement("<div id='appearance-adjust-ui-toggle'><button type='button' tabindex='-1'>&#xf013;</button></div>");
	appearanceAdjustUIToggle.query("button").addActivateEvent(GW.appearanceAdjustUIToggleButtonClicked = (event) => {
		GWLog("GW.appearanceAdjustUIToggleButtonClicked");
		toggleAppearanceAdjustUI();
		localStorage.setItem("appearance-adjust-ui-toggle-engaged", event.target.hasClass("engaged"));
	});

	if (GW.isMobile) {
		let themeSelectorCloseButton = appearanceAdjustUIToggle.query("button").cloneNode(true);
		themeSelectorCloseButton.addClass("theme-selector-close-button");
		themeSelectorCloseButton.innerHTML = "&#xf057;";
		query("#theme-selector").appendChild(themeSelectorCloseButton);
		themeSelectorCloseButton.addActivateEvent(GW.appearanceAdjustUIToggleButtonClicked);
	} else {
		if (localStorage.getItem("appearance-adjust-ui-toggle-engaged") == "true") toggleAppearanceAdjustUI();
	}
}
function removeAppearanceAdjustUIToggle() {
	GWLog("removeAppearanceAdjustUIToggle");
	queryAll("#comments-view-mode-selector, #theme-selector, #width-selector, #text-size-adjustment-ui, #theme-tweaker-toggle, #appearance-adjust-ui-toggle button").forEach(element => {
		element.removeClass("engaged");
	});
	removeElement("#appearance-adjust-ui-toggle");
}
function toggleAppearanceAdjustUI() {
	GWLog("toggleAppearanceAdjustUI");
	queryAll("#comments-view-mode-selector, #theme-selector, #width-selector, #text-size-adjustment-ui, #theme-tweaker-toggle, #appearance-adjust-ui-toggle button").forEach(element => {
		element.toggleClass("engaged");
	});
}

/*****************************/
/* MINIMIZED THREAD HANDLING */
/*****************************/

function expandAncestorsOf(comment) {
	GWLog("expandAncestorsOf");
	if (typeof comment == "string") {
		comment = /(?:comment-)?(.+)/.exec(comment)[1];
		comment = query("#comment-" + comment);
	}
	if (!comment) {
		GWLog("Comment with ID " + comment.id + " does not exist, so we can’t expand its ancestors.");
		return;
	}

	// Expand collapsed comment threads.
	let parentOfContainingCollapseCheckbox = (comment.closest("label[for^='expand'] + .comment-thread")||{}).parentElement;
	if (parentOfContainingCollapseCheckbox) parentOfContainingCollapseCheckbox.query("input[id^='expand']").checked = true;

	// Expand collapsed comments.
	let containingTopLevelCommentItem = comment.closest(".comments > ul > li");
	if (containingTopLevelCommentItem) containingTopLevelCommentItem.setCommentThreadMaximized(true, false, true);
}

/**************************/
/* WORD COUNT & READ TIME */
/**************************/

function toggleReadTimeOrWordCount(addWordCountClass) {
	GWLog("toggleReadTimeOrWordCount");
	queryAll(".post-meta .read-time").forEach(element => {
		if (addWordCountClass) element.addClass("word-count");
		else element.removeClass("word-count");

		let titleParts = /(\S+)(.+)$/.exec(element.title);
		[ element.innerHTML, element.title ] = [ `${titleParts[1]}<span>${titleParts[2]}</span>`, element.textContent ];
	});
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
	GWLog("markOriginalPosterComments");
	let postAuthor = query(".post .author");
	if (postAuthor == null) return;

	queryAll(".comment-item .author, .comment-item .inline-author").forEach(author => {
		if (author.dataset.userid == postAuthor.dataset.userid ||
			(author.hash != "" && query(`${author.hash} .author`).dataset.userid == postAuthor.dataset.userid)) {
			author.addClass("original-poster");
			author.title += "Original poster";
		}
	});
}

/********************************/
/* EDIT POST PAGE SUBMIT BUTTON */
/********************************/

function setEditPostPageSubmitButtonText() {
	GWLog("setEditPostPageSubmitButtonText");
	if (!query("#content").hasClass("edit-post-page")) return;

	queryAll("input[type='radio'][name='section'], .question-checkbox").forEach(radio => {
		radio.addEventListener("change", GW.postSectionSelectorValueChanged = (event) => {
			GWLog("GW.postSectionSelectorValueChanged");
			updateEditPostPageSubmitButtonText();
		});
	});

	updateEditPostPageSubmitButtonText();
}
function updateEditPostPageSubmitButtonText() {
	GWLog("updateEditPostPageSubmitButtonText");
	let submitButton = query("input[type='submit']");
	if (query("input#drafts").checked == true) 
		submitButton.value = "Save Draft";
	else if (query(".posting-controls").hasClass("edit-existing-post"))
		submitButton.value = query(".question-checkbox").checked ? "Save Question" : "Save Post";
	else
		submitButton.value = query(".question-checkbox").checked ? "Submit Question" : "Submit Post";
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
	GWLog("injectAntiKibitzer");
	// Inject anti-kibitzer toggle controls.
	let antiKibitzerToggle = addUIElement("<div id='anti-kibitzer-toggle'><button type='button' tabindex='-1' accesskey='g' title='Toggle anti-kibitzer (show/hide authors & karma values) [g]'></button>");
	antiKibitzerToggle.query("button").addActivateEvent(GW.antiKibitzerToggleButtonClicked = (event) => {
		GWLog("GW.antiKibitzerToggleButtonClicked");
		if (query("#anti-kibitzer-toggle").hasClass("engaged") && 
			!event.shiftKey &&
			!confirm("Are you sure you want to turn OFF the anti-kibitzer?\n\n(This will reveal the authors and karma values of all posts and comments!)")) {
			event.target.blur();
			return;
		}

		toggleAntiKibitzerMode();
		event.target.blur();
	});

	// Activate anti-kibitzer mode (if needed).
	if (localStorage.getItem("antikibitzer") == "true")
		toggleAntiKibitzerMode();

	// Remove temporary CSS that hides the authors and karma values.
	removeElement("#antikibitzer-temp");
}

function toggleAntiKibitzerMode() {
	GWLog("toggleAntiKibitzerMode");
	// This will be the URL of the user's own page, if logged in, or the URL of
	// the login page otherwise.
	let userTabTarget = query("#nav-item-login .nav-inner").href;
	let pageHeadingElement = query("h1.page-main-heading");

	let userCount = 0;
	let userFakeName = { };

	let appellation = (query(".comment-thread-page") ? "Commenter" : "User");

	let postAuthor = query(".post-page .post-meta .author");
	if (postAuthor) userFakeName[postAuthor.dataset["userid"]] = "Original Poster";

	let antiKibitzerToggle = query("#anti-kibitzer-toggle");
	if (antiKibitzerToggle.hasClass("engaged")) {
		localStorage.setItem("antikibitzer", "false");

		let redirectTarget = pageHeadingElement && pageHeadingElement.dataset["kibitzerRedirect"];
		if (redirectTarget) {
			window.location = redirectTarget;
			return;
		}

		// Individual comment page title and header
		if (query(".individual-thread-page")) {
			let replacer = (node) => {
				if (!node) return;
				node.firstChild.replaceWith(node.dataset["trueContent"]);
			}
			replacer(query("title:not(.fake-title)"));
			replacer(query("#content > h1"));
		}

		// Author names/links.
		queryAll(".author.redacted, .inline-author.redacted").forEach(author => {
			author.textContent = author.dataset["trueName"];
			if (/\/user/.test(author.href)) author.href = author.dataset["trueLink"];

			author.removeClass("redacted");
		});
		// Post/comment karma values.
		queryAll(".karma-value.redacted").forEach(karmaValue => {
			karmaValue.innerHTML = karmaValue.dataset["trueValue"] + karmaValue.lastChild.outerHTML;
			karmaValue.lastChild.textContent = (parseInt(karmaValue.dataset["trueValue"]) == 1) ? " point" : " points";

			karmaValue.removeClass("redacted");
		});
		// Link post domains.
		queryAll(".link-post-domain.redacted").forEach(linkPostDomain => {
			linkPostDomain.textContent = linkPostDomain.dataset["trueDomain"];

			linkPostDomain.removeClass("redacted");
		});

		antiKibitzerToggle.removeClass("engaged");
	} else {
		localStorage.setItem("antikibitzer", "true");

		let redirectTarget = pageHeadingElement && pageHeadingElement.dataset["antiKibitzerRedirect"];
		if (redirectTarget) {
			window.location = redirectTarget;
			return;
		}

		// Individual comment page title and header
		if (query(".individual-thread-page")) {
			let replacer = (node) => {
				if (!node) return;
				node.dataset["trueContent"] = node.firstChild.wholeText;
				let newText = node.firstChild.wholeText.replace(/^.* comments/, "REDACTED comments");
				node.firstChild.replaceWith(newText);
			}
			replacer(query("title:not(.fake-title)"));
			replacer(query("#content > h1"));
		}

		removeElement("title.fake-title");

		// Author names/links.
		queryAll(".author, .inline-author").forEach(author => {
			// Skip own posts/comments.
			if (author.hasClass("own-user-author"))
				return;

			let userid = author.dataset["userid"] || query(`${author.hash} .author`).dataset["userid"];

			author.dataset["trueName"] = author.textContent;
			author.textContent = userFakeName[userid] || (userFakeName[userid] = appellation + " " + numToAlpha(userCount++));

			if (/\/user/.test(author.href)) {
				author.dataset["trueLink"] = author.pathname;
				author.href = "/user?id=" + author.dataset["userid"];
			}

			author.addClass("redacted");
		});
		// Post/comment karma values.
		queryAll(".karma-value").forEach(karmaValue => {
			// Skip own posts/comments.
			if ((karmaValue.closest(".comment-item") || karmaValue.closest(".post-meta")).query(".author").hasClass("own-user-author"))
				return;

			karmaValue.dataset["trueValue"] = karmaValue.firstChild.textContent;
			karmaValue.innerHTML = "##" + karmaValue.lastChild.outerHTML;
			karmaValue.lastChild.textContent = " points";

			karmaValue.addClass("redacted");
		});
		// Link post domains.
		queryAll(".link-post-domain").forEach(linkPostDomain => {
			// Skip own posts/comments.
			if (userTabTarget == linkPostDomain.closest(".post-meta").query(".author").href)
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
	GWLog("sortComments");
	let commentsContainer = query("#comments");

	commentsContainer.removeClass(/(sorted-\S+)/.exec(commentsContainer.className)[1]);
	commentsContainer.addClass("sorting");

	GW.commentValues = { };
	let clonedCommentsContainer = commentsContainer.cloneNode(true);
	clonedCommentsContainer.queryAll(".comment-thread").forEach(commentThread => {
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
		Array.from(commentThread.childNodes).sort(comparator).forEach(commentItem => { commentThread.appendChild(commentItem); })
	});
	removeElement(commentsContainer.lastChild);
	commentsContainer.appendChild(clonedCommentsContainer.lastChild);
	GW.commentValues = { };

	if (loggedInUserId) {
		// Re-activate vote buttons.
		commentsContainer.queryAll("button.vote").forEach(voteButton => {
			voteButton.addActivateEvent(voteButtonClicked);
		});

		// Re-activate comment action buttons.
		commentsContainer.queryAll(".action-button").forEach(button => {
			button.addActivateEvent(GW.commentActionButtonClicked);
		});
	}

	// Re-activate comment-minimize buttons.
	queryAll(".comment-minimize-button").forEach(button => {
		button.addActivateEvent(GW.commentMinimizeButtonClicked);
	});

	// Re-add comment parent popups.
	addCommentParentPopups();
	
	// Redo new-comments highlighting.
	highlightCommentsSince(time_fromHuman(query("#hns-date-picker input").value));

	requestAnimationFrame(() => {
		commentsContainer.removeClass("sorting");
		commentsContainer.addClass("sorted-" + mode);
	});
}
function commentKarmaValue(commentOrSelector) {
	if (typeof commentOrSelector == "string") commentOrSelector = query(commentOrSelector);
	return GW.commentValues[commentOrSelector.id] || (GW.commentValues[commentOrSelector.id] = parseInt(commentOrSelector.query(".karma-value").firstChild.textContent));
}
function commentDate(commentOrSelector) {
	if (typeof commentOrSelector == "string") commentOrSelector = query(commentOrSelector);
	return GW.commentValues[commentOrSelector.id] || (GW.commentValues[commentOrSelector.id] = parseInt(commentOrSelector.query(".date").dataset.jsDate));
}
function commentVoteCount(commentOrSelector) {
	if (typeof commentOrSelector == "string") commentOrSelector = query(commentOrSelector);
	return GW.commentValues[commentOrSelector.id] || (GW.commentValues[commentOrSelector.id] = parseInt(commentOrSelector.query(".karma-value").title.split(" ")[0]));
}

function injectCommentsSortModeSelector() {
	GWLog("injectCommentsSortModeSelector");
	let topCommentThread = query("#comments > .comment-thread");
	if (topCommentThread == null) return;

	// Do not show sort mode selector if there is no branching in comment tree.
	if (topCommentThread.query(".comment-item + .comment-item") == null) return;

	let commentsSortModeSelectorHTML = "<div id='comments-sort-mode-selector' class='sublevel-nav sort'>" + 
		Object.values(CommentSortMode).map(sortMode => `<button type='button' class='sublevel-item sort-mode-${sortMode}' tabindex='-1' title='Sort by ${sortMode}'>${sortMode}</button>`).join("") +  
		"</div>";
	topCommentThread.insertAdjacentHTML("beforebegin", commentsSortModeSelectorHTML);
	let commentsSortModeSelector = query("#comments-sort-mode-selector");

	commentsSortModeSelector.queryAll("button").forEach(button => {
		button.addActivateEvent(GW.commentsSortModeSelectButtonClicked = (event) => {
			GWLog("GW.commentsSortModeSelectButtonClicked");
			event.target.parentElement.queryAll("button").forEach(button => {
				button.removeClass("selected");
				button.disabled = false;
			});
			event.target.addClass("selected");
			event.target.disabled = true;

			setTimeout(() => { sortComments(/sort-mode-(\S+)/.exec(event.target.className)[1]); });
			setCommentsSortModeSelectButtonsAccesskey();
		});
	});

	// TODO: Make this actually get the current sort mode (if that's saved).
	// TODO: Also change the condition here to properly get chrono/threaded mode,
	// when that is properly done with cookies.
	let currentSortMode = (location.href.search("chrono=t") == -1) ? CommentSortMode.TOP : CommentSortMode.OLD;
	topCommentThread.parentElement.addClass("sorted-" + currentSortMode);
	commentsSortModeSelector.query(".sort-mode-" + currentSortMode).disabled = true;
	commentsSortModeSelector.query(".sort-mode-" + currentSortMode).addClass("selected");
	setCommentsSortModeSelectButtonsAccesskey();
}

function setCommentsSortModeSelectButtonsAccesskey() {
	GWLog("setCommentsSortModeSelectButtonsAccesskey");
	queryAll("#comments-sort-mode-selector button").forEach(button => {
		button.removeAttribute("accesskey");
		button.title = /(.+?)( \[z\])?$/.exec(button.title)[1];
	});
	let selectedButton = query("#comments-sort-mode-selector button.selected");
	let nextButtonInCycle = (selectedButton == selectedButton.parentElement.lastChild) ? selectedButton.parentElement.firstChild : selectedButton.nextSibling;
	nextButtonInCycle.accessKey = "z";
	nextButtonInCycle.title += " [z]";
}

/*************************/
/* COMMENT PARENT POPUPS */
/*************************/

function addCommentParentPopups() {
	GWLog("addCommentParentPopups");
	if (!query("#content").hasClass("comment-thread-page")) return;

	queryAll(".comment-meta a.comment-parent-link, .comment-meta a.comment-child-link").forEach(commentParentLink => {
		commentParentLink.addEventListener("mouseover", GW.commentParentLinkMouseOver = (event) => {
			GWLog("GW.commentParentLinkMouseOver");
			let parentID = commentParentLink.getAttribute("href");
			var parent, popup;
			if (!(parent = (query(parentID)||{}).firstChild)) return;
			var highlightClassName;
			if (parent.getBoundingClientRect().bottom < 10 || parent.getBoundingClientRect().top > window.innerHeight + 10) {
				parentHighlightClassName = "comment-item-highlight-faint";
				popup = parent.cloneNode(true);
				popup.addClasses([ "comment-popup", "comment-item-highlight" ]);
				commentParentLink.addEventListener("mouseout", (event) => {
					removeElement(popup);
				}, {once: true});
				commentParentLink.closest(".comments > .comment-thread").appendChild(popup);
			} else {
				parentHighlightClassName = "comment-item-highlight";
			}
			parent.parentNode.addClass(parentHighlightClassName);
			commentParentLink.addEventListener("mouseout", (event) => {
				parent.parentNode.removeClass(parentHighlightClassName);
			}, {once: true});
		});
	});

	// Due to filters vs. fixed elements, we need to be smarter about selecting which elements to filter...
	GW.themeTweaker.filtersExclusionPaths.commentParentPopups = [
		"#content .comments .comment-thread"
	];
	applyFilters(GW.currentFilters);
}

/***************/
/* IMAGE FOCUS */
/***************/

function imageFocusSetup(imagesOverlayOnly = false) {
	if (typeof GW.imageFocus == "undefined")
		GW.imageFocus = {
			contentImagesSelector:	"#content img",
			overlayImagesSelector:	"#images-overlay img",
			focusedImageSelector:	"#content img.focused, #images-overlay img.focused",
			pageContentSelector:	"#content, #ui-elements-container > *:not(#image-focus-overlay), #images-overlay",
			shrinkRatio:			0.975,
			hideUITimerDuration:	1500,
			hideUITimerExpired:		() => {
				GWLog("GW.imageFocus.hideUITimerExpired");
				let currentTime = new Date();
				let timeSinceLastMouseMove = (new Date()) - GW.imageFocus.mouseLastMovedAt;
				if (timeSinceLastMouseMove < GW.imageFocus.hideUITimerDuration) {
					GW.imageFocus.hideUITimer = setTimeout(GW.imageFocus.hideUITimerExpired, (GW.imageFocus.hideUITimerDuration - timeSinceLastMouseMove));
				} else {
					hideImageFocusUI();
					cancelImageFocusHideUITimer();
				}
			}
		};

	GWLog("imageFocusSetup");
	// Create event listener for clicking on images to focus them.
	GW.imageClickedToFocus = (event) => {
		GWLog("GW.imageClickedToFocus");
		focusImage(event.target);

		if (!GW.isMobile) {
			// Set timer to hide the image focus UI.
			unhideImageFocusUI();
			GW.imageFocus.hideUITimer = setTimeout(GW.imageFocus.hideUITimerExpired, GW.imageFocus.hideUITimerDuration);
		}
	};
	// Add the listener to each image in the overlay (i.e., those in the post).
	queryAll(GW.imageFocus.overlayImagesSelector).forEach(image => {
		image.addActivateEvent(GW.imageClickedToFocus);
	});
	// Accesskey-L starts the slideshow.
	(query(GW.imageFocus.overlayImagesSelector)||{}).accessKey = 'l';
	// Count how many images there are in the post, and set the "… of X" label to that.
	((query("#image-focus-overlay .image-number")||{}).dataset||{}).numberOfImages = queryAll(GW.imageFocus.overlayImagesSelector).length;
	if (imagesOverlayOnly) return;
	// Add the listener to all other content images (including those in comments).
	queryAll(GW.imageFocus.contentImagesSelector).forEach(image => {
		image.addActivateEvent(GW.imageClickedToFocus);
	});

	// Create the image focus overlay.
	let imageFocusOverlay = addUIElement("<div id='image-focus-overlay'>" + 
	`<div class='help-overlay'>
		 <p><strong>Arrow keys:</strong> Next/previous image</p>
		 <p><strong>Escape</strong> or <strong>click</strong>: Hide zoomed image</p>
		 <p><strong>Space bar:</strong> Reset image size & position</p>
		 <p><strong>Scroll</strong> to zoom in/out</p>
		 <p>(When zoomed in, <strong>drag</strong> to pan; <br/><strong>double-click</strong> to close)</p>
	</div>
	<div class='image-number'></div>
	<div class='slideshow-buttons'>
		 <button type='button' class='slideshow-button previous' tabindex='-1' title='Previous image'>&#xf053;</button>
		 <button type='button' class='slideshow-button next' tabindex='-1' title='Next image'>&#xf054;</button>
	</div>
	<div class='caption'></div>` + 
	"</div>");
	imageFocusOverlay.dropShadowFilterForImages = " drop-shadow(10px 10px 10px #000) drop-shadow(0 0 10px #444)";

	imageFocusOverlay.queryAll(".slideshow-button").forEach(button => {
		button.addActivateEvent(GW.imageFocus.slideshowButtonClicked = (event) => {
			GWLog("GW.imageFocus.slideshowButtonClicked");
			focusNextImage(event.target.hasClass("next"));
			event.target.blur();
		});
	});

	// On orientation change, reset the size & position.
	if (typeof(window.msMatchMedia || window.MozMatchMedia || window.WebkitMatchMedia || window.matchMedia) !== 'undefined') {
		window.matchMedia('(orientation: portrait)').addListener(() => { setTimeout(resetFocusedImagePosition, 0); });
	}

	// UI starts out hidden.
	hideImageFocusUI();
}

function focusImage(imageToFocus) {
	GWLog("focusImage");
	// Clear 'last-focused' class of last focused image.
	let lastFocusedImage = query("img.last-focused");
	if (lastFocusedImage) {
		lastFocusedImage.removeClass("last-focused");
		lastFocusedImage.removeAttribute("accesskey");
	}

	// Create the focused version of the image.
	imageToFocus.addClass("focused");
	let imageFocusOverlay = query("#image-focus-overlay");
	let clonedImage = imageToFocus.cloneNode(true);
	clonedImage.style = "";
	clonedImage.removeAttribute("width");
	clonedImage.removeAttribute("height");
	clonedImage.style.filter = imageToFocus.style.filter + imageFocusOverlay.dropShadowFilterForImages;
	imageFocusOverlay.appendChild(clonedImage);
	imageFocusOverlay.addClass("engaged");

	// Set image to default size and position.
	resetFocusedImagePosition();

	// Blur everything else.
	queryAll(GW.imageFocus.pageContentSelector).forEach(element => {
		element.addClass("blurred");
	});

	// Add listener to zoom image with scroll wheel.
	window.addEventListener("wheel", GW.imageFocus.scrollEvent = (event) => {
		GWLog("GW.imageFocus.scrollEvent");
		event.preventDefault();

		let image = query("#image-focus-overlay img");

		// Remove the filter.
		image.savedFilter = image.style.filter;
		image.style.filter = 'none';

		// Locate point under cursor.
		let imageBoundingBox = image.getBoundingClientRect();

		// Calculate resize factor.
		var factor = (image.height > 10 && image.width > 10) || event.deltaY < 0 ?
						1 + Math.sqrt(Math.abs(event.deltaY))/100.0 :
						1;

		// Resize.
		image.style.width = (event.deltaY < 0 ?
							(image.clientWidth * factor) :
							(image.clientWidth / factor))
							+ "px";
		image.style.height = "";

		// Designate zoom origin.
		var zoomOrigin;
		// Zoom from cursor if we're zoomed in to where image exceeds screen, AND
		// the cursor is over the image.
		let imageSizeExceedsWindowBounds = (image.getBoundingClientRect().width > window.innerWidth || image.getBoundingClientRect().height > window.innerHeight);
		let zoomingFromCursor = imageSizeExceedsWindowBounds &&
								(imageBoundingBox.left <= event.clientX &&
								 event.clientX <= imageBoundingBox.right && 
								 imageBoundingBox.top <= event.clientY &&
								 event.clientY <= imageBoundingBox.bottom);
		// Otherwise, if we're zooming OUT, zoom from window center; if we're 
		// zooming IN, zoom from image center.
		let zoomingFromWindowCenter = event.deltaY > 0;
		if (zoomingFromCursor)
			zoomOrigin = { x: event.clientX, 
						   y: event.clientY };
		else if (zoomingFromWindowCenter)
			zoomOrigin = { x: window.innerWidth / 2, 
						   y: window.innerHeight / 2 };
		else
			zoomOrigin = { x: imageBoundingBox.x + imageBoundingBox.width / 2, 
						   y: imageBoundingBox.y + imageBoundingBox.height / 2 };

		// Calculate offset from zoom origin.
		let offsetOfImageFromZoomOrigin = {
			x: imageBoundingBox.x - zoomOrigin.x,
			y: imageBoundingBox.y - zoomOrigin.y
		}
		// Calculate delta from centered zoom.
		let deltaFromCenteredZoom = {
			x: image.getBoundingClientRect().x - (zoomOrigin.x + (event.deltaY < 0 ? offsetOfImageFromZoomOrigin.x * factor : offsetOfImageFromZoomOrigin.x / factor)),
			y: image.getBoundingClientRect().y - (zoomOrigin.y + (event.deltaY < 0 ? offsetOfImageFromZoomOrigin.y * factor : offsetOfImageFromZoomOrigin.y / factor))
		}
		// Adjust image position appropriately.
		image.style.left = parseInt(getComputedStyle(image).left) - deltaFromCenteredZoom.x + "px";
		image.style.top = parseInt(getComputedStyle(image).top) - deltaFromCenteredZoom.y + "px";
		// Gradually re-center image, if it's smaller than the window.
		if (!imageSizeExceedsWindowBounds) {
			let imageCenter = { x: image.getBoundingClientRect().x + image.getBoundingClientRect().width / 2, 
								y: image.getBoundingClientRect().y + image.getBoundingClientRect().height / 2 }
			let windowCenter = { x: window.innerWidth / 2,
								 y: window.innerHeight / 2 }
			let imageOffsetFromCenter = { x: windowCenter.x - imageCenter.x,
										  y: windowCenter.y - imageCenter.y }
			// Divide the offset by 10 because we're nudging the image toward center,
			// not jumping it there.
			image.style.left = parseInt(getComputedStyle(image).left) + imageOffsetFromCenter.x / 10 + "px";
			image.style.top = parseInt(getComputedStyle(image).top) + imageOffsetFromCenter.y / 10 + "px";
		}

		// Put the filter back.
		image.style.filter = image.savedFilter;

		// Set the cursor appropriately.
		setFocusedImageCursor();
	});
	window.addEventListener("MozMousePixelScroll", GW.imageFocus.oldFirefoxCompatibilityScrollEvent = (event) => {
		GWLog("GW.imageFocus.oldFirefoxCompatibilityScrollEvent");
		event.preventDefault();
	});

	// If image is bigger than viewport, it's draggable. Otherwise, click unfocuses.
	window.addEventListener("mouseup", GW.imageFocus.mouseUp = (event) => {
		GWLog("GW.imageFocus.mouseUp");
		window.onmousemove = '';

		// We only want to do anything on left-clicks.
		if (event.button != 0) return;

		// Don't unfocus if click was on a slideshow next/prev button!
		if (event.target.hasClass("slideshow-button")) return;

		// We also don't want to do anything if clicked on the help overlay.
		if (event.target.classList.contains("help-overlay") ||
			event.target.closest(".help-overlay"))
			return;

		let focusedImage = query("#image-focus-overlay img");
		if (event.target == focusedImage && 
			(focusedImage.height >= window.innerHeight || focusedImage.width >= window.innerWidth)) {
			// If the mouseup event was the end of a pan of an overside image,
			// put the filter back; do not unfocus.
			focusedImage.style.filter = focusedImage.savedFilter;
		} else {
			unfocusImageOverlay();
			return;
		}
	});
	window.addEventListener("mousedown", GW.imageFocus.mouseDown = (event) => {
		GWLog("GW.imageFocus.mouseDown");
		event.preventDefault();

		let focusedImage = query("#image-focus-overlay img");
		if (focusedImage.height >= window.innerHeight || focusedImage.width >= window.innerWidth) {
			let mouseCoordX = event.clientX;
			let mouseCoordY = event.clientY;

			let imageCoordX = parseInt(getComputedStyle(focusedImage).left);
			let imageCoordY = parseInt(getComputedStyle(focusedImage).top);

			// Save the filter.
			focusedImage.savedFilter = focusedImage.style.filter;

			window.onmousemove = (event) => {
				// Remove the filter.
				focusedImage.style.filter = "none";
				focusedImage.style.left = imageCoordX + event.clientX - mouseCoordX + 'px';
				focusedImage.style.top = imageCoordY + event.clientY - mouseCoordY + 'px';
			};
			return false;
		}
	});

	// Double-click on the image unfocuses.
	clonedImage.addEventListener('dblclick', GW.imageFocus.doubleClick = (event) => {
		GWLog("GW.imageFocus.doubleClick");
		if (event.target.hasClass("slideshow-button")) return;

		unfocusImageOverlay();
	});

	// Escape key unfocuses, spacebar resets.
	document.addEventListener("keyup", GW.imageFocus.keyUp = (event) => {
		GWLog("GW.imageFocus.keyUp");
		let allowedKeys = [ " ", "Spacebar", "Escape", "Esc", "ArrowUp", "ArrowDown", "ArrowLeft", "ArrowRight", "Up", "Down", "Left", "Right" ];
		if (!allowedKeys.contains(event.key) || 
			getComputedStyle(query("#image-focus-overlay")).display == "none") return;

		event.preventDefault();

		switch (event.key) {
		case "Escape": 
		case "Esc":
			unfocusImageOverlay();
			break;
		case " ":
		case "Spacebar":
			resetFocusedImagePosition();
			break;
		case "ArrowDown":
		case "Down":
		case "ArrowRight":
		case "Right":
			if (query("#images-overlay img.focused")) focusNextImage(true);
			break;
		case "ArrowUp":
		case "Up":
		case "ArrowLeft":
		case "Left":
			if (query("#images-overlay img.focused")) focusNextImage(false);
			break;
		}
	});

	// Prevent spacebar or arrow keys from scrolling page when image focused.
	togglePageScrolling(false);

	// If the image comes from the images overlay, for the main post...
	if (imageToFocus.closest("#images-overlay")) {
		// Mark the overlay as being in slide show mode (to show buttons/count).
		imageFocusOverlay.addClass("slideshow");

		// Set state of next/previous buttons.
		let images = queryAll(GW.imageFocus.overlayImagesSelector);
		var indexOfFocusedImage = getIndexOfFocusedImage();
		imageFocusOverlay.query(".slideshow-button.previous").disabled = (indexOfFocusedImage == 0);
		imageFocusOverlay.query(".slideshow-button.next").disabled = (indexOfFocusedImage == images.length - 1);

		// Set the image number.
		query("#image-focus-overlay .image-number").textContent = (indexOfFocusedImage + 1);

		// Replace the hash.
		history.replaceState(null, null, "#if_slide_" + (indexOfFocusedImage + 1));
	} else {
		imageFocusOverlay.removeClass("slideshow");
	}

	// Set the caption.
	setImageFocusCaption();

	// Moving mouse unhides image focus UI.
	window.addEventListener("mousemove", GW.imageFocus.mouseMoved = (event) => {
		GWLog("GW.imageFocus.mouseMoved");
		let currentDateTime = new Date();
		if (!(event.target.tagName == "IMG" || event.target.id == "image-focus-overlay")) {
			cancelImageFocusHideUITimer();
		} else {
			if (!GW.imageFocus.hideUITimer) {
				unhideImageFocusUI();
				GW.imageFocus.hideUITimer = setTimeout(GW.imageFocus.hideUITimerExpired, GW.imageFocus.hideUITimerDuration);
			}
			GW.imageFocus.mouseLastMovedAt = currentDateTime;
		}
	});
}

function resetFocusedImagePosition() {
	GWLog("resetFocusedImagePosition");
	let focusedImage = query("#image-focus-overlay img");
	if (!focusedImage) return;

	let sourceImage = query(GW.imageFocus.focusedImageSelector);

	// Make sure that initially, the image fits into the viewport.
	let constrainedWidth = Math.min(sourceImage.naturalWidth, window.innerWidth * GW.imageFocus.shrinkRatio);
	let widthShrinkRatio = constrainedWidth / sourceImage.naturalWidth;
	var constrainedHeight = Math.min(sourceImage.naturalHeight, window.innerHeight * GW.imageFocus.shrinkRatio);
	let heightShrinkRatio = constrainedHeight / sourceImage.naturalHeight;
	let shrinkRatio = Math.min(widthShrinkRatio, heightShrinkRatio);
	focusedImage.style.width = (sourceImage.naturalWidth * shrinkRatio) + "px";
	focusedImage.style.height = (sourceImage.naturalHeight * shrinkRatio) + "px";

	// Remove modifications to position.
	focusedImage.style.left = "";
	focusedImage.style.top = "";

	// Set the cursor appropriately.
	setFocusedImageCursor();
}
function setFocusedImageCursor() {
	let focusedImage = query("#image-focus-overlay img");
	if (!focusedImage) return;
	focusedImage.style.cursor = (focusedImage.height >= window.innerHeight || focusedImage.width >= window.innerWidth) ? 
						 		'move' : '';
}

function unfocusImageOverlay() {
	GWLog("unfocusImageOverlay");

	// Remove event listeners.
	window.removeEventListener("wheel", GW.imageFocus.scrollEvent);
	window.removeEventListener("MozMousePixelScroll", GW.imageFocus.oldFirefoxCompatibilityScrollEvent);
	// NOTE: The double-click listener does not need to be removed manually,
	// because the focused (cloned) image will be removed anyway.
	document.removeEventListener("keyup", GW.imageFocus.keyUp);
	document.removeEventListener("keydown", GW.imageFocus.keyDown);
	window.removeEventListener("mousemove", GW.imageFocus.mouseMoved);
	window.removeEventListener("mousedown", GW.imageFocus.mouseDown);
	window.removeEventListener("mouseup", GW.imageFocus.mouseUp);

	// Set accesskey of currently focused image (if it's in the images overlay).
	let currentlyFocusedImage = query("#images-overlay img.focused");
	if (currentlyFocusedImage) {
		currentlyFocusedImage.addClass("last-focused");
		currentlyFocusedImage.accessKey = 'l';
	}

	// Remove focused image and hide overlay.
	let imageFocusOverlay = query("#image-focus-overlay");
	imageFocusOverlay.removeClass("engaged");
	removeElement(imageFocusOverlay.query("img"));

	// Un-blur content/etc.
	queryAll(GW.imageFocus.pageContentSelector).forEach(element => {
		element.removeClass("blurred");
	});

	// Unset "focused" class of focused image.
	query(GW.imageFocus.focusedImageSelector).removeClass("focused");

	// Re-enable page scrolling.
	togglePageScrolling(true);

	// Reset the hash, if needed.
	if (location.hash.hasPrefix("#if_slide_"))
		history.replaceState(null, null, "#");
}

function getIndexOfFocusedImage() {
	let images = queryAll(GW.imageFocus.overlayImagesSelector);
	var indexOfFocusedImage = -1;
	for (i = 0; i < images.length; i++) {
		if (images[i].hasClass("focused")) {
			indexOfFocusedImage = i;
			break;
		}
	}
	return indexOfFocusedImage;
}

function focusNextImage(next = true) {
	GWLog("focusNextImage");
	let images = queryAll(GW.imageFocus.overlayImagesSelector);
	var indexOfFocusedImage = getIndexOfFocusedImage();

	if (next ? (++indexOfFocusedImage == images.length) : (--indexOfFocusedImage == -1)) return;

	// Remove existing image.
	removeElement("#image-focus-overlay img");
	// Unset "focused" class of just-removed image.
	query(GW.imageFocus.focusedImageSelector).removeClass("focused");

	// Create the focused version of the image.
	images[indexOfFocusedImage].addClass("focused");
	let imageFocusOverlay = query("#image-focus-overlay");
	let clonedImage = images[indexOfFocusedImage].cloneNode(true);
	clonedImage.style = "";
	clonedImage.removeAttribute("width");
	clonedImage.removeAttribute("height");
	clonedImage.style.filter = images[indexOfFocusedImage].style.filter + imageFocusOverlay.dropShadowFilterForImages;
	imageFocusOverlay.appendChild(clonedImage);
	imageFocusOverlay.addClass("engaged");
	// Set image to default size and position.
	resetFocusedImagePosition();
	// Set state of next/previous buttons.
	imageFocusOverlay.query(".slideshow-button.previous").disabled = (indexOfFocusedImage == 0);
	imageFocusOverlay.query(".slideshow-button.next").disabled = (indexOfFocusedImage == images.length - 1);
	// Set the image number display.
	query("#image-focus-overlay .image-number").textContent = (indexOfFocusedImage + 1);
	// Set the caption.
	setImageFocusCaption();
	// Replace the hash.
	history.replaceState(null, null, "#if_slide_" + (indexOfFocusedImage + 1));
}

function setImageFocusCaption() {
	GWLog("setImageFocusCaption");
	var T = { }; // Temporary storage.

	// Clear existing caption, if any.
	let captionContainer = query("#image-focus-overlay .caption");
	Array.from(captionContainer.children).forEach(child => { child.remove(); });

	// Determine caption.
	let currentlyFocusedImage = query(GW.imageFocus.focusedImageSelector);
	var captionHTML;
	if ((T.enclosingFigure = currentlyFocusedImage.closest("figure")) && 
		(T.figcaption = T.enclosingFigure.query("figcaption"))) {
		captionHTML = (T.figcaption.query("p")) ? 
					  T.figcaption.innerHTML : 
					  "<p>" + T.figcaption.innerHTML + "</p>"; 
	} else if (currentlyFocusedImage.title != "") {
		captionHTML = `<p>${currentlyFocusedImage.title}</p>`;
	}
	// Insert the caption, if any.
	if (captionHTML) captionContainer.insertAdjacentHTML("beforeend", captionHTML);
}

function hideImageFocusUI() {
	GWLog("hideImageFocusUI");
	let imageFocusOverlay = query("#image-focus-overlay");
	imageFocusOverlay.queryAll(".slideshow-button, .help-overlay, .image-number, .caption").forEach(element => {
		element.addClass("hidden");
	});
}

function unhideImageFocusUI() {
	GWLog("unhideImageFocusUI");
	let imageFocusOverlay = query("#image-focus-overlay");
	imageFocusOverlay.queryAll(".slideshow-button, .help-overlay, .image-number, .caption").forEach(element => {
		element.removeClass("hidden");
	});
}

function cancelImageFocusHideUITimer() {
	clearTimeout(GW.imageFocus.hideUITimer);
	GW.imageFocus.hideUITimer = null;
}

/*****************/
/* KEYBOARD HELP */
/*****************/

function keyboardHelpSetup() {
	let keyboardHelpOverlay = addUIElement("<div id='keyboard-help-overlay'>" + `
		<div class='keyboard-help-container'>
			<button type='button' title='Close keyboard shortcuts' class='close-keyboard-help'>&#xf00d;</button>
			<h1>Keyboard shortcuts</h1>
			<p class='note'>Keys shown in yellow (e.g., <code class='ak'>]</code>) are <a href='https://en.wikipedia.org/wiki/Access_key#Access_in_different_browsers' target='_blank'>accesskeys</a>, and require a browser-specific modifier key (or keys).</p>
			<p class='note'>Keys shown in grey (e.g., <code>?</code>) do not require any modifier keys.</p>
			<div class='keyboard-shortcuts-lists'>` + [ [
				"General",
				[ [ '?' ], "Show keyboard shortcuts" ],
				[ [ 'Esc' ], "Hide keyboard shortcuts" ]
			], [
				"Site navigation",
				[ [ 'ak-h' ], "Go to Home (a.k.a. “Frontpage”) view" ],
				[ [ 'ak-f' ], "Go to Featured (a.k.a. “Curated”) view" ],
				[ [ 'ak-a' ], "Go to All (a.k.a. “Community”) view" ],
				[ [ 'ak-m' ], "Go to Meta view" ],
				[ [ 'ak-c' ], "Go to Recent Comments view" ],
				[ [ 'ak-r' ], "Go to Archive view" ],
				[ [ 'ak-q' ], "Go to Sequences view" ],
				[ [ 'ak-t' ], "Go to About page" ],
				[ [ 'ak-u' ], "Go to User or Login page" ],
				[ [ 'ak-o' ], "Go to Inbox page" ]
			], [
				"Page navigation",
				[ [ 'ak-,' ], "Jump up to top of page" ],
				[ [ 'ak-.' ], "Jump down to bottom of page" ],
				[ [ 'ak-/' ], "Jump to top of comments section" ],
				[ [ 'ak-s' ], "Search" ],
			], [
				"Page actions",
				[ [ 'ak-n' ], "New post or comment" ],
				[ [ 'ak-e' ], "Edit current post" ]
			], [
				"Post/comment list views",
				[ [ '.' ], "Focus next entry in list" ],
				[ [ ',' ], "Focus previous entry in list" ],
				[ [ ';' ], "Cycle between links in focused entry" ],
				[ [ 'Enter' ], "Go to currently focused entry" ],
				[ [ 'Esc' ], "Unfocus currently focused entry" ],
				[ [ 'ak-]' ], "Go to next page" ],
				[ [ 'ak-[' ], "Go to previous page" ],
				[ [ 'ak-\\' ], "Go to first page" ],
				[ [ 'ak-e' ], "Edit currently focused post" ]
			], [
				"Editor",
				[ [ 'ak-k' ], "Bold text" ],
				[ [ 'ak-i' ], "Italic text" ],
				[ [ 'ak-l' ], "Insert hyperlink" ],
				[ [ 'ak-q' ], "Blockquote text" ]
			], [				
				"Appearance",
				[ [ 'ak-=' ], "Increase text size" ],
				[ [ 'ak--' ], "Decrease text size" ],
				[ [ 'ak-0' ], "Reset to default text size" ],
				[ [ 'ak-′' ], "Cycle through content width settings" ],
				[ [ 'ak-1' ], "Switch to default theme [A]" ],
				[ [ 'ak-2' ], "Switch to dark theme [B]" ],
				[ [ 'ak-3' ], "Switch to grey theme [C]" ],
				[ [ 'ak-4' ], "Switch to ultramodern theme [D]" ],
				[ [ 'ak-5' ], "Switch to simple theme [E]" ],
				[ [ 'ak-6' ], "Switch to brutalist theme [F]" ],
				[ [ 'ak-7' ], "Switch to ReadTheSequences theme [G]" ],
				[ [ 'ak-8' ], "Switch to classic Less Wrong theme [H]" ],
				[ [ 'ak-9' ], "Switch to modern Less Wrong theme [I]" ],
				[ [ 'ak-;' ], "Open theme tweaker" ],
				[ [ 'Enter' ], "Save changes and close theme tweaker "],
				[ [ 'Esc' ], "Close theme tweaker (without saving)" ]
			], [
				"Slide shows",
				[ [ 'ak-l' ], "Start/resume slideshow" ],
				[ [ 'Esc' ], "Exit slideshow" ],
				[ [ '&#x2192;', '&#x2193;' ], "Next slide" ],
				[ [ '&#x2190;', '&#x2191;' ], "Previous slide" ],
				[ [ 'Space' ], "Reset slide zoom" ]
			], [
				"Miscellaneous",
				[ [ 'ak-x' ], "Switch to next view on user page" ],
				[ [ 'ak-z' ], "Switch to previous view on user page" ],
				[ [ 'ak-`&nbsp;' ], "Toggle compact comment list view" ]
			] ].map(section => 
			`<ul><li class='section'>${section[0]}</li>` + section.slice(1).map(entry =>
				`<li>
					<span class='keys'>` + 
					entry[0].map(key =>
						(key.hasPrefix("ak-")) ? `<code class='ak'>${key.substring(3)}</code>` : `<code>${key}</code>`
					).join("") + 
					`</span>
					<span class='action'>${entry[1]}</span>
				</li>`
			).join("\n") + `</ul>`).join("\n") + `
			</ul></div>		
		</div>
	` + "</div>");

	// Add listener to show the keyboard help overlay.
	document.addEventListener("keypress", GW.keyboardHelpShowKeyPressed = (event) => {
		GWLog("GW.keyboardHelpShowKeyPressed");
		if (event.key == '?')
			toggleKeyboardHelpOverlay(true);
	});

	// Clicking the background overlay closes the keyboard help overlay.
	keyboardHelpOverlay.addActivateEvent(GW.keyboardHelpOverlayClicked = (event) => {
		GWLog("GW.keyboardHelpOverlayClicked");
		if (event.type == 'mousedown') {
			keyboardHelpOverlay.style.opacity = "0.01";
		} else {
			toggleKeyboardHelpOverlay(false);
			keyboardHelpOverlay.style.opacity = "1.0";
		}
	}, true);

	// Intercept clicks, so they don't "fall through" the background overlay.
	(query("#keyboard-help-overlay .keyboard-help-container")||{}).addActivateEvent((event) => { event.stopPropagation(); }, true);

	// Clicking the close button closes the keyboard help overlay.
	keyboardHelpOverlay.query("button.close-keyboard-help").addActivateEvent(GW.closeKeyboardHelpButtonClicked = (event) => {
		toggleKeyboardHelpOverlay(false);
	});

	// Add button to open keyboard help.
	query("#nav-item-about").insertAdjacentHTML("beforeend", "<button type='button' tabindex='-1' class='open-keyboard-help' title='Keyboard shortcuts'>&#xf11c;</button>");
	query("#nav-item-about button.open-keyboard-help").addActivateEvent(GW.openKeyboardHelpButtonClicked = (event) => {
		GWLog("GW.openKeyboardHelpButtonClicked");
		toggleKeyboardHelpOverlay(true);
		event.target.blur();
	});
}

function toggleKeyboardHelpOverlay(show) {
	console.log("toggleKeyboardHelpOverlay");

	let keyboardHelpOverlay = query("#keyboard-help-overlay");
	show = (typeof show != "undefined") ? show : (getComputedStyle(keyboardHelpOverlay) == "hidden");
	keyboardHelpOverlay.style.visibility = show ? "visible" : "hidden";

	// Prevent scrolling the document when the overlay is visible.
	togglePageScrolling(!show);

	// Focus the close button as soon as we open.
	keyboardHelpOverlay.query("button.close-keyboard-help").focus();

	if (show) {
		// Add listener to show the keyboard help overlay.
		document.addEventListener("keyup", GW.keyboardHelpHideKeyPressed = (event) => {
			GWLog("GW.keyboardHelpHideKeyPressed");
			if (event.key == 'Escape')
				toggleKeyboardHelpOverlay(false);
		});
	} else {
		document.removeEventListener("keyup", GW.keyboardHelpHideKeyPressed);
	}

	// Disable / enable tab-selection of the search box.
	setSearchBoxTabSelectable(!show);
}

/*******************************/
/* HTML TO MARKDOWN CONVERSION */
/*******************************/

function MarkdownFromHTML(text) {
	GWLog("MarkdownFromHTML");
	// Wrapper tags, paragraphs, bold, italic, code blocks.
	text = text.replace(/<(.+?)(?:\s(.+?))?>/g, (match, tag, attributes, offset, string) => {
		switch(tag) {
		case "html":
		case "/html":
		case "head":
		case "/head":
		case "body":
		case "/body":
		case "p":
			return "";
		case "/p":
			return "\n";
		case "strong":
		case "/strong":
			return "**";
		case "em":
		case "/em":
			return "*";
		case "code":
		case "/code":
			return "`";
		default:
			return match;
		}
	});

	// Unordered lists.
	text = text.replace(/<ul>\s+?((?:.|\n)+?)\s+?<\/ul>/g, (match, listItems, offset, string) => {
		return listItems.replace(/<li>((?:.|\n)+?)<\/li>/g, (match, listItem, offset, string) => {
			return `* ${listItem}\n`;
		});
	});

	// Ordered lists.
	text = text.replace(/<ol(?:\sstart=["']([0-9]+)["'])?>\s+?((?:.|\n)+?)\s+?<\/ol>/g, (match, start, listItems, offset, string) => {
		var countedItemValue = 0;
		return listItems.replace(/<li(?:\svalue=["']([0-9]+)["'])?>((?:.|\n)+?)<\/li>/g, (match, specifiedItemValue, listItem, offset, string) => {
			var itemValue;
			if (typeof specifiedItemValue != "undefined") {
				specifiedItemValue = parseInt(specifiedItemValue);
				countedItemValue = itemValue = specifiedItemValue;
			} else {
				itemValue = (start ? parseInt(start) - 1 : 0) + ++countedItemValue;
			}
			return `${itemValue}. ${listItem}\n`;
		});
	});

	// Headings.
	text = text.replace(/<h([1-9])>(.+?)<\/h[1-9]>/g, (match, level, headingText, offset, string) => {
		return { "1":"#", "2":"##", "3":"###" }[level] + " " + headingText + "\n";
	});

	// Blockquotes.
	text = text.replace(/<blockquote>((?:.|\n)+?)<\/blockquote>/g, (match, quotedText, offset, string) => {
		return `> ${quotedText.trim().split("\n").join("\n> ")}\n`;
	});

	// Links.
	text = text.replace(/<a href="(.+?)">(.+?)<\/a>/g, (match, href, text, offset, string) => {
		return `[${text}](${href})`;
	}).trim();

	// Horizontal rules.
	text = text.replace(/<hr(.+?)\/?>/g, (match, offset, string) => {
		return "\n---\n";
	});

	return text;
}

/******************/
/* INITIALIZATION */
/******************/

registerInitializer('earlyInitialize', true, () => query("#content") != null, function () {
	GWLog("INITIALIZER earlyInitialize");
	// Check to see whether we're on a mobile device (which we define as a touchscreen)
// 	GW.isMobile = /Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini/i.test(navigator.userAgent);
	GW.isMobile = ('ontouchstart' in document.documentElement);
	GW.isFirefox = navigator.userAgent.toLowerCase().indexOf('firefox') > -1;

	if (GW.isMobile)
		query("head").addClass("touch-ui");

	// Backward compatibility
	let storedTheme = localStorage.getItem('selected-theme');
	if (storedTheme) {
		setTheme(storedTheme);
		localStorage.removeItem('selected-theme');
	}

	// Animate width & theme adjustments?
	GW.adjustmentTransitions = false;

	// Add the content width selector.
	injectContentWidthSelector();
	// Add the text size adjustment widget.
	injectTextSizeAdjustmentUI();
	// Add the theme selector.
	injectThemeSelector();
	// Add the theme tweaker.
	injectThemeTweaker();
	// Add the quick-nav UI.
	injectQuickNavUI();

	setTimeout(() => { updateInbox(); }, 0);
});

registerInitializer('initialize', false, () => document.readyState != 'loading', function () {
	GWLog("INITIALIZER initialize");
	forceInitializer('earlyInitialize');

	// This is for "qualified hyperlinking", i.e. "link without comments" and/or
	// "link without nav bars".
	if (getQueryVariable("comments") == "false")
		query("#content").addClass("no-comments");
	if (getQueryVariable("hide-nav-bars") == "true") {
		query("#content").addClass("no-nav-bars");
		let auxAboutLink = addUIElement("<div id='aux-about-link'><a href='/about' accesskey='t' target='_new'>&#xf129;</a></div>");
	}

	// If the page cannot have comments, remove the accesskey from the #comments
	// quick-nav button; and if the page can have comments, but does not, simply 
	// disable the #comments quick nav button.
	let content = query("#content");
	if (content.query("#comments") == null) {
		query("#quick-nav-ui a[href='#comments']").accessKey = '';
	} else if (content.query("#comments .comment-thread") == null) {
		query("#quick-nav-ui a[href='#comments']").addClass("no-comments");
	}

	// Links to comments generated by LW have a hash that consists of just the 
	// comment ID, which can start with a number. Prefix it with "comment-".
	if (location.hash.length == 18) {
		location.hash = "#comment-" + location.hash.substring(1);
	}

	// If the viewport is wide enough to fit the desktop-size content column,
	// use a long date format; otherwise, a short one.
	let useLongDate = window.innerWidth > 900;
	let dtf = new Intl.DateTimeFormat([], 
		( useLongDate ? 
			{ month: 'short', day: 'numeric', year: 'numeric', hour: 'numeric', minute: 'numeric' }
				: { month: 'numeric', day: 'numeric', year: '2-digit', hour: 'numeric', minute: 'numeric' } ));
	queryAll(".date").forEach(date => {
		let d = date.dataset.jsDate;
		if (d) { date.innerHTML = dtf.format(new Date(+ d)); }
	});

	GW.needHashRealignment = false;

	// On edit post pages and conversation pages, add GUIEdit buttons to the 
	// textarea, expand it, and markdownify the existing text, if any (this is
	// needed if a post was last edited on LW).
	queryAll(".with-markdown-editor textarea").forEach(textarea => {
		textarea.addTextareaFeatures();
		expandTextarea(textarea);
		textarea.value = MarkdownFromHTML(textarea.value);
	});
	// Focus the textarea.
	queryAll(((getQueryVariable("post-id")) ? "#edit-post-form textarea" : "#edit-post-form input[name='title']") + (GW.isMobile ? "" : ", .conversation-page textarea")).forEach(field => { field.focus(); });

	// If this is a post page...
	let postMeta = query(".post .post-meta");
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
		clonedPostMeta.query("input[type='checkbox']").id += "-bottom";
		clonedPostMeta.query("label").htmlFor += "-bottom";
		query(".post").appendChild(clonedPostMeta);
	}

	// If client is logged in...
	if (loggedInUserId) {
		// Add upvote/downvote buttons.
		if (typeof postVote != 'undefined') {
			queryAll(".post-meta .karma-value").forEach(karmaValue => {
				addVoteButtons(karmaValue, postVote, 'Posts');
				karmaValue.parentElement.addClass("active-controls");
			});
		}
		if (typeof commentVotes != 'undefined') {
			queryAll(".comment-meta .karma-value").forEach(karmaValue => {
				let commentID = karmaValue.getCommentId();
				addVoteButtons(karmaValue, commentVotes[commentID], 'Comments');
				karmaValue.parentElement.addClass("active-controls");
			});
		}

		// Color the upvote/downvote buttons with an embedded style sheet.
		query("head").insertAdjacentHTML("beforeend","<style id='vote-buttons'>" + 
		`.upvote:hover,
		.upvote:focus,
		.upvote.selected {
			color: #00d800;
		}
		.downvote:hover,
		.downvote:focus,
		.downvote.selected {
			color: #eb4c2a;
		}` +
		"</style>");

		// Activate the vote buttons.
		queryAll("button.vote").forEach(voteButton => {
			voteButton.addActivateEvent(voteButtonClicked);
		});

		// For all comment containers...
		queryAll(".comments").forEach((commentsContainer) => {
			// Add reply buttons.
			commentsContainer.queryAll(".comment").forEach(comment => {
				comment.insertAdjacentHTML("afterend", "<div class='comment-controls posting-controls'></div>");
				comment.parentElement.query(".comment-controls").constructCommentControls();
			});

			// Add top-level new comment form.
			if (!query(".individual-thread-page")) {
				commentsContainer.insertAdjacentHTML("afterbegin", "<div class='comment-controls posting-controls'></div>");
				commentsContainer.query(".comment-controls").constructCommentControls();
			}
		});

		// Hash realignment is needed because adding the above elements almost
		// certainly caused the page to reflow, and now client is no longer
		// scrolled to the place indicated by the hash.
		GW.needHashRealignment = true;
	}

	// Clean up ToC
	queryAll(".contents-list li a").forEach(tocLink => {
		tocLink.innerText = tocLink.innerText.replace(/^[0-9]+\. /, '');
		tocLink.innerText = tocLink.innerText.replace(/^[0-9]+: /, '');
		tocLink.innerText = tocLink.innerText.replace(/^M{0,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})\. /i, '');
		tocLink.innerText = tocLink.innerText.replace(/^[A-Z]\. /, '');
	});

	// If we're on a comment thread page...
	if (query(".comments") != null) {
		// Add comment-minimize buttons to every comment.
		queryAll(".comment-meta").forEach(commentMeta => {
			if (!commentMeta.lastChild.hasClass("comment-minimize-button"))
				commentMeta.insertAdjacentHTML("beforeend", "<div class='comment-minimize-button maximized'>&#xf146;</div>");
		});
		if (query("#content.comment-thread-page") && !query("#content").hasClass("individual-thread-page")) {
			// Format and activate comment-minimize buttons.
			queryAll(".comment-minimize-button").forEach(button => {
				button.closest(".comment-item").setCommentThreadMaximized(false);
				button.addActivateEvent(GW.commentMinimizeButtonClicked = (event) => {
					event.target.closest(".comment-item").setCommentThreadMaximized(true);
				});
			});
		}
	}
	if (getQueryVariable("chrono") == "t") {
		query("head").insertAdjacentHTML("beforeend", "<style>.comment-minimize-button::after { display: none; }</style>");
	}
	let urlParts = document.URL.split('#comment-');
	if (urlParts.length > 1) {
		expandAncestorsOf(urlParts[1]);
		GW.needHashRealignment = true;
	}

	// On mobile, replace the labels for the checkboxes on the edit post form
	// with icons, to save space.
	if (GW.isMobile && query(".edit-post-page")) {
		query("label[for='link-post']").innerHTML = "&#xf0c1";
		query("label[for='question']").innerHTML = "&#xf128";
	}

	// Add error message (as placeholder) if user tries to click Search with
	// an empty search field.
	query("#nav-item-search form").addEventListener("submit", GW.siteSearchFormSubmitted = (event) => {
		let searchField = event.target.query("input");
		if (searchField.value == "") {
			event.preventDefault();
			event.target.blur();
			searchField.placeholder = "Enter a search string!";
			searchField.focus();
		}
	});
	// Remove the placeholder / error on any input.
	query("#nav-item-search input").addEventListener("input", GW.siteSearchFieldValueChanged = (event) => {
		event.target.placeholder = "";
	});

	// Prevent conflict between various single-hotkey listeners and text fields
	queryAll("input[type='text'], input[type='search'], input[type='password']").forEach(inputField => {
		inputField.addEventListener("keyup", (event) => { event.stopPropagation(); });
		inputField.addEventListener("keypress", (event) => { event.stopPropagation(); });
	});

	if (content.hasClass("post-page")) {
		// Read and update last-visited-date.
		let lastVisitedDate = getLastVisitedDate();
		setLastVisitedDate(Date.now());

		// Save the number of comments this post has when it's visited.
		updateSavedCommentCount();

		if (content.query(".comments .comment-thread") != null) {
			// Add the new comments count & navigator.
			injectNewCommentNavUI();

			// Get the highlight-new-since date (as specified by URL parameter, if 
			// present, or otherwise the date of the last visit).
			let hnsDate = parseInt(getQueryVariable("hns")) || lastVisitedDate;

			// Highlight new comments since the specified date.			 
			let newCommentsCount = highlightCommentsSince(hnsDate);

			// Update the comment count display.
			updateNewCommentNavUI(newCommentsCount, hnsDate);
		}
	} else {
		// On listing pages, make comment counts more informative.
		badgePostsWithNewComments();
	}

	// Add the comments list mode selector widget (expanded vs. compact).
	injectCommentsListModeSelector();

	// Add the comments view selector widget (threaded vs. chrono).
// 	injectCommentsViewModeSelector();

	// Add the comments sort mode selector (top, hot, new, old).
	injectCommentsSortModeSelector();

	// Add the toggle for the post nav UI elements on mobile.
	if (GW.isMobile) injectPostNavUIToggle();

	// Add the toggle for the appearance adjustment UI elements on mobile.
	if (GW.isMobile) injectAppearanceAdjustUIToggle();

	// Add the antikibitzer.
	injectAntiKibitzer();

	// Add comment parent popups.
	addCommentParentPopups();

	// Mark original poster's comments with a special class.
	markOriginalPosterComments();
	
	// On the All view, mark posts with non-positive karma with a special class.
	if (query("#content").hasClass("all-index-page")) {
		queryAll("#content.index-page h1.listing + .post-meta .karma-value").forEach(karmaValue => {
			if (parseInt(karmaValue.textContent.replace("−", "-")) > 0) return;

			karmaValue.closest(".post-meta").previousSibling.addClass("spam");
		});
	}

	// Set the "submit" button on the edit post page to something more helpful.
	setEditPostPageSubmitButtonText();

	// Compute the text of the pagination UI tooltip text.
	queryAll("#top-nav-bar a:not(.disabled), #bottom-bar a").forEach(link => {
		link.dataset.targetPage = parseInt((/=([0-9]+)/.exec(link.href)||{})[1]||0)/20 + 1;
	});

	// Add event listeners for Escape and Enter, for the theme tweaker.
	let themeTweakerHelpWindow = query("#theme-tweaker-ui .help-window");
	let themeTweakerUI = query("#theme-tweaker-ui");
	document.addEventListener("keyup", GW.themeTweaker.keyPressed = (event) => {
		if (event.key == "Escape") {
			if (themeTweakerHelpWindow.style.display != "none") {
				toggleThemeTweakerHelpWindow();
				themeTweakerResetSettings();
			} else if (themeTweakerUI.style.display != "none") {
				toggleThemeTweakerUI();
				themeTweakReset();
			}
		} else if (event.key == "Enter") {
			if (themeTweakerHelpWindow.style.display != "none") {
				toggleThemeTweakerHelpWindow();
				themeTweakerSaveSettings();
			} else if (themeTweakerUI.style.display != "none") {
				toggleThemeTweakerUI();
				themeTweakSave();
			}
		}
	});

	// Add event listener for . , ; (for navigating listings pages).
	let listings = queryAll("h1.listing a[href^='/posts'], #content > .comment-thread .comment-meta a.date");
	if (listings.length > 0) {
		document.addEventListener("keyup", GW.postListingsNavKeyPressed = (event) => { 
			if (event.ctrlKey || event.shiftKey || event.altKey || !(event.key == "," || event.key == "." || event.key == ';' || event.key == "Escape")) return;

			if (event.key == "Escape") {
				if (document.activeElement.parentElement.hasClass("listing"))
					document.activeElement.blur();
				return;
			}

			if (event.key == ';') {
				if (document.activeElement.parentElement.hasClass("link-post-listing")) {
					let links = document.activeElement.parentElement.queryAll("a");
					links[document.activeElement == links[0] ? 1 : 0].focus();
				} else if (document.activeElement.parentElement.hasClass("comment-meta")) {
					let links = document.activeElement.parentElement.queryAll("a.date, a.permalink");
					links[document.activeElement == links[0] ? 1 : 0].focus();
					document.activeElement.closest(".comment-item").addClass("comment-item-highlight");
				}
				return;
			}

			var indexOfActiveListing = -1;
			for (i = 0; i < listings.length; i++) {
				if (document.activeElement.parentElement.hasClass("listing") && 
					listings[i] === document.activeElement.parentElement.query("a[href^='/posts']")) {
					indexOfActiveListing = i;
					break;
				} else if (document.activeElement.parentElement.hasClass("comment-meta") && 
					listings[i] === document.activeElement.parentElement.query("a.date")) {
					indexOfActiveListing = i;
					break;
				}
			}
			// Remove edit accesskey from currently highlighted post by active user, if applicable.
			if (indexOfActiveListing > -1) {
				delete (listings[indexOfActiveListing].parentElement.query(".edit-post-link")||{}).accessKey;
			}
			let indexOfNextListing = (event.key == "." ? ++indexOfActiveListing : (--indexOfActiveListing + listings.length + 1)) % (listings.length + 1);
			if (indexOfNextListing < listings.length) {
				listings[indexOfNextListing].focus();

				if (listings[indexOfNextListing].closest(".comment-item")) {
					listings[indexOfNextListing].closest(".comment-item").addClasses([ "expanded", "comment-item-highlight" ]);
					listings[indexOfNextListing].closest(".comment-item").scrollIntoView();
				}
			} else {
				document.activeElement.blur();
			}
			// Add edit accesskey to newly highlighted post by active user, if applicable.
			(listings[indexOfActiveListing].parentElement.query(".edit-post-link")||{}).accessKey = 'e';
		});
		queryAll("#content > .comment-thread .comment-meta a.date, #content > .comment-thread .comment-meta a.permalink").forEach(link => {
			link.addEventListener("blur", GW.commentListingsHyperlinkUnfocused = (event) => {
				event.target.closest(".comment-item").removeClasses([ "expanded", "comment-item-highlight" ]);
			});
		});
	}
	// Add event listener for ; (to focus the link on link posts).
	if (query("#content").hasClass("post-page") && 
		query(".post").hasClass("link-post")) {
		document.addEventListener("keyup", GW.linkPostLinkFocusKeyPressed = (event) => {
			if (event.key == ';') query("a.link-post-link").focus();
		});
	}

	// Add accesskeys to user page view selector.
	let viewSelector = query("#content.user-page > .sublevel-nav");
	if (viewSelector) {
		let currentView = viewSelector.query("span");
		(currentView.nextSibling || viewSelector.firstChild).accessKey = 'x';
		(currentView.previousSibling || viewSelector.lastChild).accessKey = 'z';
	}

	// Add accesskey to index page sort selector.
	(query("#content.index-page > .sublevel-nav.sort a")||{}).accessKey = 'z';

	// Move MathJax style tags to <head>.
	var aggregatedStyles = "";
	queryAll("#content style").forEach(styleTag => {
		aggregatedStyles += styleTag.innerHTML;
		removeElement("style", styleTag.parentElement);
	});
	if (aggregatedStyles != "") {
		query("head").insertAdjacentHTML("beforeend", "<style id='mathjax-styles'>" + aggregatedStyles + "</style>");
	}

	// Add listeners to switch between word count and read time.
	if (localStorage.getItem("display-word-count")) toggleReadTimeOrWordCount(true);
	queryAll(".post-meta .read-time").forEach(element => {
		element.addActivateEvent(GW.readTimeOrWordCountClicked = (event) => {
			let displayWordCount = localStorage.getItem("display-word-count");
			toggleReadTimeOrWordCount(!displayWordCount);
			if (displayWordCount) localStorage.removeItem("display-word-count");
			else localStorage.setItem("display-word-count", true);
		});
	});

	// Add copy listener to strip soft hyphens (inserted by server-side hyphenator).
	query("#content").addEventListener("copy", GW.textCopied = (event) => {
		event.preventDefault();
		const selectedHTML = getSelectionHTML();
		const selectedText = getSelection().toString();
		event.clipboardData.setData("text/plain", selectedText.replace(/\u00AD|\u200b/g, ""));
		event.clipboardData.setData("text/html", selectedHTML.replace(/\u00AD|\u200b/g, ""));
	});

	// Set up Image Focus feature.
	imageFocusSetup();

	// Set up keyboard shortcuts guide overlay.
	keyboardHelpSetup();
});

/*************************/
/* POST-LOAD ADJUSTMENTS */
/*************************/

registerInitializer('pageLayoutFinished', false, () => document.readyState == "complete", function () {
	GWLog("INITIALIZER pageLayoutFinished");
	forceInitializer('initialize');

	realignHashIfNeeded();

	postSetThemeHousekeeping();

	focusImageSpecifiedByURL();

	// FOR TESTING ONLY, COMMENT WHEN DEPLOYING.
// 	query("input[type='search']").value = GW.isMobile;
// 	query("head").insertAdjacentHTML("beforeend", "<style>" + 
// 		`@media only screen and (hover:none) { #nav-item-search input { background-color: red; }}` + 
// 		`@media only screen and (hover:hover) { #nav-item-search input { background-color: LightGreen; }}` + 
// 		"</style>");
});

function generateImagesOverlay() {
	GWLog("generateImagesOverlay");
	// Don't do this on the about page.
	if (query(".about-page") != null) return;

	// Remove existing, if any.
	removeElement("#images-overlay");

	// Create new.
	query("body").insertAdjacentHTML("afterbegin", "<div id='images-overlay'></div>");
	let imagesOverlay = query("#images-overlay");
	let imagesOverlayLeftOffset = imagesOverlay.getBoundingClientRect().left;
	queryAll(".post-body img").forEach(image => {
		image.style = "";
		image.className = "";

		let clonedImageContainer = document.createElement("div");

		let clonedImage = image.cloneNode(true);
		clonedImage.style.borderStyle = getComputedStyle(image).borderStyle;
		clonedImage.style.borderColor = getComputedStyle(image).borderColor;
		clonedImage.style.borderWidth = Math.round(parseFloat(getComputedStyle(image).borderWidth)) + "px";
		clonedImageContainer.appendChild(clonedImage);

		let zoomLevel = parseFloat(GW.currentTextZoom);

		clonedImageContainer.style.top = image.getBoundingClientRect().top * zoomLevel - parseFloat(getComputedStyle(image).marginTop) + window.scrollY + "px";
		clonedImageContainer.style.left = image.getBoundingClientRect().left * zoomLevel - parseFloat(getComputedStyle(image).marginLeft) - imagesOverlayLeftOffset + "px";
		clonedImageContainer.style.width = image.getBoundingClientRect().width * zoomLevel + "px";
		clonedImageContainer.style.height = image.getBoundingClientRect().height * zoomLevel + "px";

		imagesOverlay.appendChild(clonedImageContainer);
	});

	// Add the event listeners to focus each image.
	imageFocusSetup(true);
}

function adjustUIForWindowSize() {
	GWLog("adjustUIForWindowSize");
	var bottomBarOffset;

	// Adjust bottom bar state.
	let bottomBar = query("#bottom-bar");
	bottomBarOffset = bottomBar.hasClass("decorative") ? 16 : 30;
	if (query("#content").clientHeight > window.innerHeight + bottomBarOffset) {
		bottomBar.removeClass("decorative");

		bottomBar.query("#nav-item-top").style.display = "";
	} else if (bottomBar) {
		if (bottomBar.childElementCount > 1) bottomBar.removeClass("decorative");
		else bottomBar.addClass("decorative");

		bottomBar.query("#nav-item-top").style.display = "none";
	}

	// Show quick-nav UI up/down buttons if content is taller than window.
	bottomBarOffset = bottomBar.hasClass("decorative") ? 16 : 30;
	queryAll("#quick-nav-ui a[href='#top'], #quick-nav-ui a[href='#bottom-bar']").forEach(element => {
		element.style.visibility = (query("#content").clientHeight > window.innerHeight + bottomBarOffset) ? "unset" : "hidden";
	});

	// Move anti-kibitzer toggle if content is very short.
	if (query("#content").clientHeight < 400) (query("#anti-kibitzer-toggle")||{}).style.bottom = "125px";

	// Update the visibility of the post nav UI.
	updatePostNavUIVisibility();
}

function recomputeUIElementsContainerHeight(force = false) {
	GWLog("recomputeUIElementsContainerHeight");
	if (!GW.isMobile &&
		(force || query("#ui-elements-container").style.height != "")) {
		let bottomBarOffset = query("#bottom-bar").hasClass("decorative") ? 16 : 30;
		query("#ui-elements-container").style.height = (query("#content").clientHeight <= window.innerHeight + bottomBarOffset) ? 
														query("#content").clientHeight + "px" :
														"100vh";
	}
}

function realignHashIfNeeded() {
	if (GW.needHashRealignment)
		realignHash();
}
function realignHash() {
	GWLog("realignHash");
	if (!location.hash) return;

	let targetElement = query(location.hash);
	if (targetElement) targetElement.scrollIntoView(true);
	GW.needHashRealignment = false;
}

function focusImageSpecifiedByURL() {
	GWLog("focusImageSpecifiedByURL");
	if (location.hash.hasPrefix("#if_slide_")) {
		registerInitializer('focusImageSpecifiedByURL', true, () => query("#images-overlay") != null, () => {
			let images = queryAll(GW.imageFocus.overlayImagesSelector);
			let imageToFocus = (/#if_slide_([0-9]+)/.exec(location.hash)||{})[1];
			if (imageToFocus > 0 && imageToFocus <= images.length) {
				focusImage(images[imageToFocus - 1]);

				// Set timer to hide the image focus UI.
				unhideImageFocusUI();
				GW.imageFocus.hideUITimer = setTimeout(GW.imageFocus.hideUITimerExpired, GW.imageFocus.hideUITimerDuration);
			}
		});
	}
}

/***********/
/* GUIEDIT */
/***********/

function insertMarkup(event) {
	var mopen = '', mclose = '', mtext = '', func = false;
	if (typeof arguments[1] == 'function') {
		func = arguments[1];
	} else {
		mopen = arguments[1];
		mclose = arguments[2];
		mtext = arguments[3];
	}

	var textarea = event.target.closest("form").query("textarea");
	textarea.focus();
	var p0 = textarea.selectionStart;
	var p1 = textarea.selectionEnd;
	var cur0 = cur1 = p0;

	var str = (p0 == p1) ? mtext : textarea.value.substring(p0, p1);
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
	// The document.execCommand API is broken in Firefox 
	// ( https://bugzilla.mozilla.org/show_bug.cgi?id=1220696 ), but using it
	// allows native undo/redo to work; so we enable it in other browsers.
	if (GW.isFirefox) {
		textarea.value = textarea.value.substring(0, p0) + str + textarea.value.substring(p1);
	} else {
		document.execCommand("insertText", false, str);
	}
	// Expand textarea, if needed.
	expandTextarea(textarea);

	// Set selection.
	textarea.selectionStart = cur0;
	textarea.selectionEnd = cur1;

	return;
}

GW.guiEditButtons = [
	[ 'strong', 'Strong (bold)', 'k', '**', '**', 'Bold text', '&#xf032;' ],
	[ 'em', 'Emphasized (italic)', 'i', '*', '*', 'Italicized text', '&#xf033;' ],
	[ 'link', 'Hyperlink', 'l', hyperlink, '', '', '&#xf0c1;' ],
	[ 'image', 'Image', '', '![', '](image url)', 'Image alt-text', '&#xf03e;' ],
	[ 'heading1', 'Heading level 1', '', '\\n# ', '', 'Heading', '&#xf1dc;<sup>1</sup>' ],
	[ 'heading2', 'Heading level 2', '', '\\n## ', '', 'Heading', '&#xf1dc;<sup>2</sup>' ],
	[ 'heading3', 'Heading level 3', '', '\\n### ', '', 'Heading', '&#xf1dc;<sup>3</sup>' ],
	[ 'blockquote', 'Blockquote', 'q', blockquote, '', '', '&#xf10e;' ],
	[ 'bulleted-list', 'Bulleted list', '', '\\n* ', '', 'List item', '&#xf0ca;' ],
	[ 'numbered-list', 'Numbered list', '', '\\n1. ', '', 'List item', '&#xf0cb;' ],
	[ 'horizontal-rule', 'Horizontal rule', '', '\\n\\n---\\n\\n', '', '', '&#xf068;' ],
	[ 'inline-code', 'Inline code', '', '`', '`', 'Code', '&#xf121;' ],
	[ 'code-block', 'Code block', '', '```\\n', '\\n```', 'Code', '&#xf1c9;' ],
	[ 'formula', 'LaTeX', '', '$', '$', 'LaTeX formula', '&#xf155;' ],
	[ 'spoiler', 'Spoiler block', '', '::: spoiler\\n', '\\n:::', 'Spoiler text', '&#xf2fc;' ]
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
