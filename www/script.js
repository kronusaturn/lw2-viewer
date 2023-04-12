/***************************/
/* INITIALIZATION REGISTRY */
/***************************/

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
	document.cookie = name + "=" + (value || "")  + expires + "; path=/; SameSite=Lax" + (GW.secureCookies ? "; Secure" : "");
}

/*******************************/
/* EVENT LISTENER MANIPULATION */
/*******************************/

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

// Workaround for Firefox bug https://bugzilla.mozilla.org/show_bug.cgi?id=325942
Element.prototype.scrollIntoView = function(realSIV) {
	return function(bottom) {
		realSIV.call(this, bottom);
		if(fixTarget = this.closest("input[id^='expand'] ~ .comment-thread")) {
			window.scrollBy(0, fixTarget.scrollTop);
			fixTarget.scrollTop = 0;
		}
	}
}(Element.prototype.scrollIntoView);

/*	If top of element is not at or above the top of the screen, scroll it into
	view. */
Element.prototype.scrollIntoViewIfNeeded = function() {
	GWLog("scrollIntoViewIfNeeded");
	if (this.getBoundingClientRect().bottom > window.innerHeight && 
		this.getBoundingClientRect().top > 0) {
		this.scrollIntoView(false);
	}
}

function urlEncodeQuery(params) {
	return params.keys().map((x) => {return "" + x + "=" + encodeURIComponent(params[x])}).join("&");
}

function handleAjaxError(event) {
	if(event.target.getResponseHeader("Content-Type") === "application/json") console.log("doAjax error: " + JSON.parse(event.target.responseText)["error"]);
	else console.log("doAjax error: Something bad happened :(");
}

function doAjax(params) {
	let req = new XMLHttpRequest();
	let requestMethod = params["method"] || "GET";
	req.addEventListener("load", (event) => {
		if(event.target.status < 400) {
			if(params["onSuccess"]) params.onSuccess(event);
		} else {
			if(params["onFailure"]) params.onFailure(event);
			else handleAjaxError(event);
		}
		if(params["onFinish"]) params.onFinish(event);
	});
	req.open(requestMethod, (params.location || document.location) + ((requestMethod == "GET" && params.params) ? "?" + urlEncodeQuery(params.params) : ""));
	if(requestMethod == "POST") {
		req.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");
		params["params"]["csrf-token"] = GW.csrfToken;
		req.send(urlEncodeQuery(params.params));
	} else {
		req.send();
	}
}

function activateReadyStateTriggers() {
	if(document.readyState == 'interactive') {
		activateTrigger('DOMReady');
	} else if(document.readyState == 'complete') {
		activateTrigger('DOMReady');
		activateTrigger('DOMComplete');
	}
}

document.addEventListener('readystatechange', activateReadyStateTriggers);
activateReadyStateTriggers();

function callWithServerData(fname, uri) {
	doAjax({
		location: uri,
		onSuccess: (event) => {
			let response = JSON.parse(event.target.responseText);
			window[fname](response);
		}
	});
}

deferredCalls.forEach((x) => callWithServerData.apply(null, x));
deferredCalls = null;

/*	Return the currently selected text, as HTML (rather than unstyled text).
	*/
function getSelectionHTML() {
	let container = newElement("DIV");
	container.appendChild(window.getSelection().getRangeAt(0).cloneContents());
	return container.innerHTML;
}

/*	Given an HTML string, creates an element from that HTML, adds it to 
	#ui-elements-container (creating the latter if it does not exist), and 
	returns the created element.
	*/
function addUIElement(element_html) {
	let ui_elements_container = query("#ui-elements-container");
	if (ui_elements_container == null)
		ui_elements_container = document.body.appendChild(newElement("NAV", { "id": "ui-elements-container" }));

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
	if (!enable) {
		GW.scrollPositionBeforeScrollingDisabled = window.scrollY;
		document.body.addClass("no-scroll");
		document.body.style.top = `-${GW.scrollPositionBeforeScrollingDisabled}px`;
	} else {
		document.body.removeClass("no-scroll");
		document.body.removeAttribute("style");
		window.scrollTo(0, GW.scrollPositionBeforeScrollingDisabled);
	}
}

DOMRectReadOnly.prototype.isInside = function (x, y) {
	return (this.left <= x && this.right >= x && this.top <= y && this.bottom >= y);
}

/*	Simple mutex mechanism.
 */
function doIfAllowed(f, passHolder, passName, releaseImmediately = false) {
	if (passHolder[passName] == false)
		return;

	passHolder[passName] = false;

	f();

	if (releaseImmediately) {
		passHolder[passName] = true;
	} else {
		requestAnimationFrame(() => {
			passHolder[passName] = true;
		});
	}
}

/*******************/
/* COPY PROCESSORS */
/*******************/

/*********************************************************************/
/*  Workaround for Firefox weirdness, based on more Firefox weirdness.
 */
DocumentFragment.prototype.getSelection = function () {
	return document.getSelection();
}

/******************************************************************************/
/*  Returns true if the node contains only whitespace and/or other empty nodes.
 */
function isNodeEmpty(node) {
	if (node.nodeType == Node.TEXT_NODE)
		return (node.textContent.match(/\S/) == null);

	if (   node.nodeType == Node.ELEMENT_NODE
		&& [ "IMG", "VIDEO", "AUDIO", "IFRAME", "OBJECT" ].includes(node.tagName))
		return false;

	if (node.childNodes.length == 0)
		return true;

	for (childNode of node.childNodes)
		if (isNodeEmpty(childNode) == false)
			return false;

	return true;
}

/***************************************************************/
/*  Returns a DocumentFragment containing the current selection.
 */
function getSelectionAsDocument(doc = document) {
	let docFrag = doc.getSelection().getRangeAt(0).cloneContents();

	//	Strip whitespace (remove top-level empty nodes).
	let nodesToRemove = [ ];
	docFrag.childNodes.forEach(node => {
		if (isNodeEmpty(node))
			nodesToRemove.push(node);
	});
	nodesToRemove.forEach(node => {
		docFrag.removeChild(node);
	});

	return docFrag;
}

/*****************************************************************************/
/*  Adds the given copy processor, appending it to the existing array thereof.

    Each copy processor should take two arguments: the copy event, and the
    DocumentFragment which holds the selection as it is being processed by each
    successive copy processor.

    A copy processor should return true if processing should continue after it’s
    done, false otherwise (e.g. if it has entirely replaced the contents of the
    selection object with what the final clipboard contents should be).
 */
function addCopyProcessor(processor) {
	if (GW.copyProcessors == null)
		GW.copyProcessors = [ ];

	GW.copyProcessors.push(processor);
}

/******************************************************************************/
/*  Set up the copy processor system by registering a ‘copy’ event handler to
    call copy processors. (Must be set up for the main document, and separately
    for any shadow roots.)
 */
function registerCopyProcessorsForDocument(doc) {
	GWLog("registerCopyProcessorsForDocument", "rewrite.js", 1);

	doc.addEventListener("copy", (event) => {
		if (   GW.copyProcessors == null
			|| GW.copyProcessors.length == 0)
			return;

		event.preventDefault();
		event.stopPropagation();

		let selection = getSelectionAsDocument(doc);

		let i = 0;
		while (	  i < GW.copyProcessors.length
			  && GW.copyProcessors[i++](event, selection));

		// This is necessary for .innerText to work properly.
		let wrapper = newElement("DIV");
		wrapper.appendChild(selection);
		document.body.appendChild(wrapper);

		let makeLinksAbsolute = (node) => {
			if(node['attributes']) {
				for(attr of ['src', 'href']) {
					if(node[attr])
						node[attr] = node[attr];
				}
			}
			node.childNodes.forEach(makeLinksAbsolute);
		}
		makeLinksAbsolute(wrapper);

		event.clipboardData.setData("text/plain", wrapper.innerText);
		event.clipboardData.setData("text/html", wrapper.innerHTML);

		document.body.removeChild(wrapper);
	});
}

/*******************************************/
/*  Set up copy processors in main document.
 */
registerCopyProcessorsForDocument(document);

/*****************************************************************************/
/*  Makes it so that copying a rendered equation or other math element copies
    the LaTeX source, instead of the useless gibberish that is the contents of
    the text nodes of the HTML representation of the equation.
 */
addCopyProcessor((event, selection) => {
	if (event.target.closest(".mjx-math")) {
		selection.replaceChildren(event.target.closest(".mjx-math").getAttribute("aria-label"));

		return false;
	}

	selection.querySelectorAll(".mjx-chtml").forEach(mathBlock => {
		mathBlock.innerHTML = " " + mathBlock.querySelector(".mjx-math").getAttribute("aria-label") + " ";
	});

	return true;
});

/************************************************************************/
/*  Remove soft hyphens and other extraneous characters from copied text.
 */
addCopyProcessor((event, selection) => {
	let replaceText = (node) => {
		if(node.nodeType == Node.TEXT_NODE) {
			node.nodeValue = node.nodeValue.replace(/\u00AD|\u200b/g, "");
		}

		node.childNodes.forEach(replaceText);
	}
	replaceText(selection);

	return true;
});


/********************/
/* DEBUGGING OUTPUT */
/********************/

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

function processUserStatus(userStatus) {
	window.userStatus = userStatus;
	if(userStatus) {
		if(userStatus.notifications) {
			let element = query('#inbox-indicator');
			element.className = 'new-messages';
			element.title = 'New messages [o]';
		}
	} else {
		location.reload();
	}
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
		// User mentions autocomplete
		if(!userAutocomplete &&
		   textarea.value.charAt(textarea.selectionStart - 1) === "@" &&
		   (textarea.selectionStart === 1 ||
		    !textarea.value.charAt(textarea.selectionStart - 2).match(/[a-zA-Z0-9]/))) {
			beginAutocompletion(textarea, textarea.selectionStart);
		}
	}, false);
	textarea.addEventListener("click", (event) => {
		if(!userAutocomplete) {
			let start = textarea.selectionStart, end = textarea.selectionEnd;
			let value = textarea.value;
			if (start <= 1) return;
			for (; value.charAt(start - 1) != "@"; start--) {
				if (start <= 1) return;
				if (value.charAt(start - 1) == " ") return;
			}
			for(; end < value.length && value.charAt(end) != " "; end++) { true }
			beginAutocompletion(textarea, start, end);
		}
	});

	textarea.addEventListener("paste", (event) => {
		let html = event.clipboardData.getData("text/html");
		if(html) {
			html = html.replace(/\n|\r/gm, "");
			let isQuoted = textarea.selectionStart >= 2 &&
			    textarea.value.substring(textarea.selectionStart - 2, textarea.selectionStart) == "> ";
			document.execCommand("insertText", false, MarkdownFromHTML(html, (isQuoted ? "> " : null)));
			event.preventDefault();
		}
	});

	textarea.addEventListener("keyup", (event) => { event.stopPropagation(); });
	textarea.addEventListener("keypress", (event) => { event.stopPropagation(); });
	textarea.addEventListener("keydown", (event) => {
		// Special case for alt+4
		// Generalize this before adding more.
		if(event.altKey && event.key === '4') {
			insertMarkup(event, "$", "$", "LaTeX formula");
			event.stopPropagation();
			event.preventDefault();
		}
	});

	let form = textarea.closest("form");

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
	// to get around the “children of elements with a filter applied cannot be
	// fixed” issue.
	if (GW.isMobile && window.innerWidth <= 520) {
		let fixedEditorElements = textareaContainer.queryAll("textarea, .guiedit-buttons-container, .guiedit-mobile-auxiliary-button, #markdown-hints");
		textarea.addEventListener("focus", GW.textareaFocusedMobile = (event) => {
			GWLog("GW.textareaFocusedMobile");
			Appearance.savedFilters = Appearance.currentFilters;
			Appearance.applyFilters(Appearance.noFilters);
			fixedEditorElements.forEach(element => {
				element.style.filter = Appearance.filterStringFromFilters(Appearance.savedFilters);
			});
		});
		textarea.addEventListener("blur", GW.textareaBlurredMobile = (event) => {
			GWLog("GW.textareaBlurredMobile");
			requestAnimationFrame(() => {
				Appearance.applyFilters(Appearance.savedFilters);
				Appearance.savedFilters = null;
				fixedEditorElements.forEach(element => {
					element.style.filter = Appearance.filterStringFromFilters(Appearance.savedFilters);
				});
			});
		});
	}
}

Element.prototype.injectReplyForm = function(editMarkdownSource) {
	GWLog("injectReplyForm");
	let commentControls = this;
	let editCommentId = (editMarkdownSource ? commentControls.getCommentId() : false);
	let postId = commentControls.parentElement.dataset["postId"];
	let tagId = commentControls.parentElement.dataset["tagId"];
	let withparent = (!editMarkdownSource && commentControls.getCommentId());
	let answer = commentControls.parentElement.id == "answers";
	let parentAnswer = commentControls.closest("#answers > .comment-thread > .comment-item");
	let withParentAnswer = (!editMarkdownSource && parentAnswer && parentAnswer.getCommentId());
	let parentCommentItem = commentControls.closest(".comment-item");
	let alignmentForum = userStatus.alignmentForumAllowed && alignmentForumPost &&
	    (!parentCommentItem || parentCommentItem.firstChild.querySelector(".comment-meta .alignment-forum"));
	commentControls.innerHTML = "<button class='cancel-comment-button' tabindex='-1'>Cancel</button>" +
		"<form method='post'>" + 
		"<div class='textarea-container'>" + 
		"<textarea name='text' oninput='enableBeforeUnload();'></textarea>" +
		(withparent ? "<input type='hidden' name='parent-comment-id' value='" + commentControls.getCommentId() + "'>" : "") +
		(withParentAnswer ? "<input type='hidden' name='parent-answer-id' value='" + withParentAnswer + "'>" : "") +
		(editCommentId ? "<input type='hidden' name='edit-comment-id' value='" + editCommentId + "'>" : "") +
		(postId ? "<input type='hidden' name='post-id' value='" + postId + "'>" : "") +
		(tagId ? "<input type='hidden' name='tag-id' value='" + tagId + "'>" : "") +
		(answer ? "<input type='hidden' name='answer' value='t'>" : "") +
		(commentControls.parentElement.id == "nominations" ? "<input type='hidden' name='nomination' value='t'>" : "") +
		(commentControls.parentElement.id == "reviews" ? "<input type='hidden' name='nomination-review' value='t'>" : "") +
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
	textarea.addTextareaFeatures();
	textarea.focus();
}

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

function findVoteControls(targetType, targetId, voteAxis) {
	var voteAxisQuery = (voteAxis ? "."+voteAxis : "");

	if(targetType == "Post") {
		return queryAll(".post-meta .voting-controls"+voteAxisQuery);
	} else if(targetType == "Comment") {
		return queryAll("#comment-"+targetId+" > .comment > .comment-meta .voting-controls"+voteAxisQuery+", #comment-"+targetId+" > .comment > .comment-controls .voting-controls"+voteAxisQuery);
	}
}

function votesEqual(vote1, vote2) {
	var allKeys = Object.assign({}, vote1);
	Object.assign(allKeys, vote2);

	for(k of allKeys.keys()) {
		if((vote1[k] || "neutral") !== (vote2[k] || "neutral")) return false;
	}
	return true;
}

function addVoteButtons(element, vote, targetType) {
	GWLog("addVoteButtons");
	vote = vote || {};
	let voteAxis = element.parentElement.dataset.voteAxis || "karma";
	let voteType = parseVoteType(vote[voteAxis]);
	let voteClass = makeVoteClass(voteType);

	element.parentElement.queryAll("button").forEach((button) => {
		button.disabled = false;
		if (voteType) {
			if (button.dataset["voteType"] === (voteType.up ? "upvote" : "downvote"))
				button.addClass(voteClass);
		}
		updateVoteButtonVisualState(button);
		button.addActivateEvent(voteButtonClicked);
	});
}

function updateVoteButtonVisualState(button) {
	GWLog("updateVoteButtonVisualState");

	button.removeClasses([ "none", "one", "two-temp", "two" ]);

	if (button.disabled)
		button.addClass("none");
	else if (button.hasClass("big-vote"))
		button.addClass("two");
	else if (button.hasClass("selected"))
		button.addClass("one");
	else
		button.addClass("none");
}

function changeVoteButtonVisualState(button) {
	GWLog("changeVoteButtonVisualState");

	/*	Interaction states are:

		0  0·    (neutral; +1 click)
		1  1·    (small vote; +1 click)
		2  2·    (big vote; +1 click)

		Visual states are (with their state classes in [brackets]) are:

		01    (no vote) [none]
		02    (small vote active) [one]
		12    (small vote active, temporary indicator of big vote) [two-temp]
		22    (big vote active) [two]

		The following are the 9 possible interaction state transitions (and
		the visual state transitions associated with them):

		                VIS.    VIS.
		FROM    TO      FROM    TO      NOTES
		====    ====    ====    ====    =====
		0       0·      01      12      first click
		0·      1       12      02      one click without second
		0·      2       12      22      second click

		1       1·      02      12      first click
		1·      0       12      01      one click without second
		1·      2       12      22      second click

		2       2·      22      12      first click
		2·      1       12      02      one click without second
		2·      0       12      01      second click
	*/
	let transitions = [
		[ "big-vote two-temp clicked-twice", "none"     ], // 2· => 0
		[ "big-vote two-temp clicked-once",  "one"      ], // 2· => 1
		[ "big-vote clicked-once",           "two-temp" ], // 2  => 2·

		[ "selected two-temp clicked-twice", "two"      ], // 1· => 2
		[ "selected two-temp clicked-once",  "none"     ], // 1· => 0
		[ "selected clicked-once",           "two-temp" ], // 1  => 1·

		[ "two-temp clicked-twice",          "two"      ], // 0· => 2
		[ "two-temp clicked-once",           "one"      ], // 0· => 1
		[ "clicked-once",                    "two-temp" ], // 0  => 0·
	];
	for (let [ interactionClasses, visualStateClass ] of transitions) {
		if (button.hasClasses(interactionClasses.split(" "))) {
			button.removeClasses([ "none", "one", "two-temp", "two" ]);
			button.addClass(visualStateClass);
			break;
		}
	}
}

function voteCompleteEvent(targetType, targetId, response) {
	GWLog("voteCompleteEvent");

	var currentVote = voteData[targetType][targetId] || {};
	var desiredVote = voteDesired[targetType][targetId];

	var controls = findVoteControls(targetType, targetId);
	var controlsByAxis = new Object;

	controls.forEach(control => {
		const voteAxis = (control.dataset.voteAxis || "karma");

		if (!desiredVote || (currentVote[voteAxis] || "neutral") === (desiredVote[voteAxis] || "neutral")) {
			control.removeClass("waiting");
			control.querySelectorAll("button").forEach(button => button.removeClass("waiting"));
		}

		if(!controlsByAxis[voteAxis]) controlsByAxis[voteAxis] = new Array;
		controlsByAxis[voteAxis].push(control);

		const voteType = currentVote[voteAxis];
		const vote = parseVoteType(voteType);
		const voteUpDown = (vote.up ? 'upvote' : (vote.down ? 'downvote' : ''));
		const voteClass = makeVoteClass(vote);

		if (response && response[voteAxis]) {
			const [voteType, displayText, titleText] = response[voteAxis];

			const displayTarget = control.query(".karma-value");
			if (displayTarget.hasClass("redacted")) {
				displayTarget.dataset["trueValue"] = displayText;
			} else {
				displayTarget.innerHTML = displayText;
			}
			displayTarget.setAttribute("title", titleText);
		}

		control.queryAll("button.vote").forEach(button => {
			updateVoteButton(button, voteUpDown, voteClass);
		});
	});
}

function updateVoteButton(button, voteUpDown, voteClass) {
	button.removeClasses([ "clicked-once", "clicked-twice", "selected", "big-vote" ]);
	if (button.dataset.voteType == voteUpDown)
		button.addClass(voteClass);
	updateVoteButtonVisualState(button);
}

function makeVoteRequestCompleteEvent(targetType, targetId) {
	return (event) => {
		var currentVote = {};
		var response = null;

		if (event.target.status == 200) {
			response = JSON.parse(event.target.responseText);
			for (const voteAxis of response.keys()) {
				currentVote[voteAxis] = response[voteAxis][0];
			}
			voteData[targetType][targetId] = currentVote;
		} else {
			delete voteDesired[targetType][targetId];
			currentVote = voteData[targetType][targetId];
		}

		var desiredVote = voteDesired[targetType][targetId];

		if (desiredVote && !votesEqual(currentVote, desiredVote)) {
			sendVoteRequest(targetType, targetId);
		} else {
			delete voteDesired[targetType][targetId];
			voteCompleteEvent(targetType, targetId, response);
		}
	}
}

function sendVoteRequest(targetType, targetId) {
	GWLog("sendVoteRequest");

	doAjax({
		method: "POST",
		location: "/karma-vote",
		params: { "target": targetId,
			  "target-type": targetType,
			  "vote": JSON.stringify(voteDesired[targetType][targetId]) },
		onFinish: makeVoteRequestCompleteEvent(targetType, targetId)
	});
}

function voteButtonClicked(event) {
	GWLog("voteButtonClicked");
	let voteButton = event.target;

	// 500 ms (0.5 s) double-click timeout.
	let doubleClickTimeout = 500;

	if (!voteButton.clickedOnce) {
		voteButton.clickedOnce = true;
		voteButton.addClass("clicked-once");
		changeVoteButtonVisualState(voteButton);

		setTimeout(GW.vbDoubleClickTimeoutCallback = (voteButton) => {
			if (!voteButton.clickedOnce) return;

			// Do single-click code.
			voteButton.clickedOnce = false;
			voteEvent(voteButton, 1);
		}, doubleClickTimeout, voteButton);
	} else {
		voteButton.clickedOnce = false;

		// Do double-click code.
		voteButton.removeClass("clicked-once");
		voteButton.addClass("clicked-twice");
		voteEvent(voteButton, 2);
	}
}

function voteEvent(voteButton, numClicks) {
	GWLog("voteEvent");
	voteButton.blur();

	let voteControl = voteButton.parentNode;

	let targetType = voteButton.dataset.targetType;
	let targetId = ((targetType == 'Comment') ? voteButton.getCommentId() : voteButton.parentNode.dataset.postId);
	let voteAxis = voteControl.dataset.voteAxis || "karma";
	let voteUpDown = voteButton.dataset.voteType;

	let voteType;
	if (   (numClicks == 2 && voteButton.hasClass("big-vote"))
		|| (numClicks == 1 && voteButton.hasClass("selected") && !voteButton.hasClass("big-vote"))) {
		voteType = "neutral";
	} else {
		let vote = parseVoteType(voteUpDown);
		vote.big = (numClicks == 2);
		voteType = makeVoteType(vote);
	}

	let voteControls = findVoteControls(targetType, targetId, voteAxis);
	for (const voteControl of voteControls) {
		voteControl.addClass("waiting");
		voteControl.queryAll(".vote").forEach(button => {
			button.addClass("waiting");
			updateVoteButton(button, voteUpDown, makeVoteClass(parseVoteType(voteType)));
		});
	}

	let voteRequestPending = voteDesired[targetType][targetId];
	let voteObject = Object.assign({}, voteRequestPending || voteData[targetType][targetId] || {});
	voteObject[voteAxis] = voteType;
	voteDesired[targetType][targetId] = voteObject;

	if (!voteRequestPending) sendVoteRequest(targetType, targetId);
}

function initializeVoteButtons() {
	// Color the upvote/downvote buttons with an embedded style sheet.
	insertHeadHTML(`<style id="vote-buttons">
		:root {
			--GW-upvote-button-color: #00d800;
			--GW-downvote-button-color: #eb4c2a;
		}
	</style>`);
}

function processVoteData(voteData) {
	window.voteData = voteData;

	window.voteDesired = new Object;
	for(key of voteData.keys()) {
		voteDesired[key] = new Object;
	}

	initializeVoteButtons();
	
	addTriggerListener("postLoaded", {priority: 3000, fn: () => {
		queryAll(".post .post-meta .karma-value").forEach(karmaValue => {
			let postID = karmaValue.parentNode.dataset.postId;
			addVoteButtons(karmaValue, voteData.Post[postId], 'Post');
			karmaValue.parentElement.addClass("active-controls");
		});
	}});

	addTriggerListener("DOMReady", {priority: 3000, fn: () => {
		queryAll(".comment-meta .karma-value, .comment-controls .karma-value").forEach(karmaValue => {
			let commentID = karmaValue.getCommentId();
			addVoteButtons(karmaValue, voteData.Comment[commentID], 'Comment');
			karmaValue.parentElement.addClass("active-controls");
		});
	}});
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
	let bottomBar = query("#bottom-bar");
	let bottomOffset = (bottomBar ? bottomBar.getBoundingClientRect().top : document.body.getBoundingClientRect().bottom);
	let atbottom =  bottomOffset <= window.innerHeight;
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
		commentItem.nextNewComment = null;
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
		history.replaceState(window.history.state, null, "#comment-" + targetCommentID);
		targetComment.scrollIntoView();
	}

	GW.newCommentScrollListener();
}

function getPostHash() {
	let postHash = /^\/posts\/([^\/]+)/.exec(location.pathname);
	return (postHash ? postHash[1] : false);
}
function setHistoryLastVisitedDate(date) {
	window.history.replaceState({ lastVisited: date }, null);
}
function getLastVisitedDate() {
	// Get the last visited date (or, if posting a comment, the previous last visited date).
	if(window.history.state) return (window.history.state||{})['lastVisited'];
	let aCommentHasJustBeenPosted = (query(".just-posted-comment") != null);
	let storageName = (aCommentHasJustBeenPosted ? "previous-last-visited-date_" : "last-visited-date_") + getPostHash();
	let currentVisited = localStorage.getItem(storageName);
	setHistoryLastVisitedDate(currentVisited);
	return currentVisited;
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

		let savedCommentCount = parseInt(localStorage.getItem("comment-count_" + postHash), 10) || 0;
		let commentCountDisplay = postLink.parentElement.nextSibling.query(".comment-count");
		let currentCommentCount = parseInt(/([0-9]+)/.exec(commentCountDisplay.textContent)[1], 10) || 0;

		if (currentCommentCount > savedCommentCount)
			commentCountDisplay.addClass("new-comments");
		else
			commentCountDisplay.removeClass("new-comments");
		commentCountDisplay.title = `${currentCommentCount} comments (${currentCommentCount - savedCommentCount} new)`;
	});
}


/*****************/
/* MEDIA QUERIES */
/*****************/

GW.mediaQueries = {
    systemDarkModeActive:  matchMedia("(prefers-color-scheme: dark)")
};


/************************/
/* ACTIVE MEDIA QUERIES */
/************************/

/*  This function provides two slightly different versions of its functionality,
    depending on how many arguments it gets.

    If one function is given (in addition to the media query and its name), it
    is called whenever the media query changes (in either direction).

    If two functions are given (in addition to the media query and its name),
    then the first function is called whenever the media query starts matching,
    and the second function is called whenever the media query stops matching.

    If you want to call a function for a change in one direction only, pass an
    empty closure (NOT null!) as one of the function arguments.

    There is also an optional fifth argument. This should be a function to be
    called when the active media query is canceled.
 */
function doWhenMatchMedia(mediaQuery, name, ifMatchesOrAlwaysDo, otherwiseDo = null, whenCanceledDo = null) {
    if (typeof GW.mediaQueryResponders == "undefined")
        GW.mediaQueryResponders = { };

    let mediaQueryResponder = (event, canceling = false) => {
        if (canceling) {
            GWLog(`Canceling media query “${name}”`, "media queries", 1);

            if (whenCanceledDo != null)
                whenCanceledDo(mediaQuery);
        } else {
            let matches = (typeof event == "undefined") ? mediaQuery.matches : event.matches;

            GWLog(`Media query “${name}” triggered (matches: ${matches ? "YES" : "NO"})`, "media queries", 1);

            if ((otherwiseDo == null) || matches)
            	ifMatchesOrAlwaysDo(mediaQuery);
            else
            	otherwiseDo(mediaQuery);
        }
    };
    mediaQueryResponder();
    mediaQuery.addListener(mediaQueryResponder);

    GW.mediaQueryResponders[name] = mediaQueryResponder;
}

/*  Deactivates and discards an active media query, after calling the function
    that was passed as the whenCanceledDo parameter when the media query was
    added.
 */
function cancelDoWhenMatchMedia(name) {
    GW.mediaQueryResponders[name](null, true);

    for ([ key, mediaQuery ] of Object.entries(GW.mediaQueries))
        mediaQuery.removeListener(GW.mediaQueryResponders[name]);

    GW.mediaQueryResponders[name] = null;
}


/******************************/
/* DARK/LIGHT MODE ADJUSTMENT */
/******************************/

DarkMode = {
	/*****************/
	/*	Configuration.
	 */
	modeOptions: [
		[ "auto", "&#xf042;", "Set light or dark mode automatically, according to system-wide setting (Win: Start → Personalization → Colors; Mac: Apple → System-Preferences → General → Appearance; iOS: Settings → Display-and-Brightness; Android: Settings → Display)" ],
		[ "light", "&#xe28f;", "Light mode at all times (black-on-white)" ],
		[ "dark", "&#xf186;", "Dark mode at all times (inverted: white-on-black)" ]
	],

	selectedModeOptionNote: " [This option is currently selected.]",

	/******************/
	/*	Infrastructure.
	 */

	modeSelector: null,
	modeSelectorInteractable: true,

	/******************/
	/*	Mode selection.
	 */

    /*  Returns current (saved) mode (light, dark, or auto).
     */
    getSavedMode: () => {
        return (localStorage.getItem("dark-mode-setting") || "auto");
    },

	/*	Saves specified mode (light, dark, or auto).
	 */
	saveMode: (mode) => {
		GWLog("DarkMode.setMode");

		if (mode == "auto")
			localStorage.removeItem("dark-mode-setting");
		else
			localStorage.setItem("dark-mode-setting", mode);
	},

	/*  Set specified color mode (light, dark, or auto).
	 */
	setMode: (selectedMode = DarkMode.getSavedMode()) => {
		GWLog("DarkMode.setMode");

		//	The style block should be inlined (and already loaded).
		let darkModeStyles = document.querySelector("#inlined-dark-mode-styles");
		if (darkModeStyles) {
			//	Set `media` attribute of style block to match requested mode.
			if (selectedMode == "auto") {
				darkModeStyles.media = "all and (prefers-color-scheme: dark)";
			} else if (selectedMode == "dark") {
				darkModeStyles.media = "all";
			} else {
				darkModeStyles.media = "not all";
			}
		}

		//	Update state.
		DarkMode.updateModeSelectorState(DarkMode.modeSelector);
	},

	modeSelectorHTML: (inline = false) => {
		let selectorTagName = (inline ? "span" : "div");
		let selectorId = (inline ? `` : ` id="dark-mode-selector"`);
		let selectorClass = (` class="dark-mode-selector mode-selector` + (inline ? ` mode-selector-inline` : ``) + `"`);

		//	Get saved mode setting (or default).
		let currentMode = DarkMode.getSavedMode();

		return `<${selectorTagName}${selectorId}${selectorClass}>`
			+ DarkMode.modeOptions.map(modeOption => {
				let [ name, label, desc ] = modeOption;
				let selected = (name == currentMode ? " selected" : "");
				let disabled = (name == currentMode ? " disabled" : "");
				let active = ((   currentMode == "auto"
							   && name == (GW.mediaQueries.systemDarkModeActive.matches ? "dark" : "light"))
							  ? " active"
							  : "");
				if (name == currentMode)
					desc += DarkMode.selectedModeOptionNote;
				return `<button
							type="button"
							class="select-mode-${name}${selected}${active}"
							${disabled}
							tabindex="-1"
							data-name="${name}"
							title="${desc}"
								>${label}</button>`;
			  }).join("")
			+ `</${selectorTagName}>`;
	},

	injectModeSelector: (replacedElement = null) => {
		GWLog("DarkMode.injectModeSelector", "dark-mode.js", 1);

		//	Inject the mode selector widget.
		let modeSelector;
		if (replacedElement) {
			replacedElement.innerHTML = DarkMode.modeSelectorHTML(true);
			modeSelector = replacedElement.firstElementChild;
			unwrap(replacedElement);
		} else {
			if (GW.isMobile) {
				if (Appearance.themeSelector == null)
					return;

				Appearance.themeSelectorAuxiliaryControlsContainer.insertAdjacentHTML("beforeend", DarkMode.modeSelectorHTML());
			} else {
				addUIElement(DarkMode.modeSelectorHTML());
			}

			modeSelector = DarkMode.modeSelector = query("#dark-mode-selector");
		}

		//  Add event listeners and update state.
		requestAnimationFrame(() => {
			//	Activate mode selector widget buttons.
			modeSelector.querySelectorAll("button").forEach(button => {
				button.addActivateEvent(DarkMode.modeSelectButtonClicked);
			});
		});

		/*	Add active media query to update mode selector state when system dark
			mode setting changes. (This is relevant only for the ‘auto’ setting.)
		 */
		doWhenMatchMedia(GW.mediaQueries.systemDarkModeActive, "DarkMode.updateModeSelectorStateForSystemDarkMode", () => { 
			DarkMode.updateModeSelectorState(modeSelector);
		});
	},

	modeSelectButtonClicked: (event) => {
		GWLog("DarkMode.modeSelectButtonClicked");

		/*	We don’t want clicks to go through if the transition 
			between modes has not completed yet, so we disable the 
			button temporarily while we’re transitioning between 
			modes.
		 */
		doIfAllowed(() => {
			// Determine which setting was chosen (ie. which button was clicked).
			let selectedMode = event.target.dataset.name;

			// Save the new setting.
			DarkMode.saveMode(selectedMode);

			// Actually change the mode.
			DarkMode.setMode(selectedMode);
		}, DarkMode, "modeSelectorInteractable");

		event.target.blur();
	},

	updateModeSelectorState: (modeSelector = DarkMode.modeSelector) => {
		GWLog("DarkMode.updateModeSelectorState");

		/*	If the mode selector has not yet been injected, then do nothing.
		 */
		if (modeSelector == null)
			return;

		//	Get saved mode setting (or default).
		let currentMode = DarkMode.getSavedMode();

		//	Clear current buttons state.
		modeSelector.querySelectorAll("button").forEach(button => {
			button.classList.remove("active", "selected");
			button.disabled = false;
			if (button.title.endsWith(DarkMode.selectedModeOptionNote))
				button.title = button.title.slice(0, (-1 * DarkMode.selectedModeOptionNote.length));
		});

		//	Set the correct button to be selected.
		modeSelector.querySelectorAll(`.select-mode-${currentMode}`).forEach(button => {
			button.classList.add("selected");
			button.disabled = true;
			button.title += DarkMode.selectedModeOptionNote;
		});

		/*	Ensure the right button (light or dark) has the “currently active” 
			indicator, if the current mode is ‘auto’.
		 */
		if (currentMode == "auto")
			modeSelector.querySelector(`.select-mode-${(GW.mediaQueries.systemDarkModeActive.matches ? "dark" : "light")}`).classList.add("active");
	}
};


/****************************/
/* APPEARANCE CUSTOMIZATION */
/****************************/

Appearance = { ...Appearance,
	/**************************************************************************/
	/* INFRASTRUCTURE
	 */

	noFilters: { },

	themeSelector: null,
	themeSelectorAuxiliaryControlsContainer: null,
	themeSelectorInteractionBlockerOverlay: null,
	themeSelectorInteractableTimer: null,

	themeTweakerToggle: null,

	themeTweakerStyleBlock: null,

	themeTweakerUI: null,
	themeTweakerUIMainWindow: null,
	themeTweakerUIHelpWindow: null,
	themeTweakerUISampleTextContainer: null,
	themeTweakerUIClippyContainer: null,
	themeTweakerUIClippyControl: null,

	widthSelector: null,

	textSizeAdjustmentWidget: null,

	appearanceAdjustUIToggle: null,

	/**************************************************************************/
	/* FUNCTIONALITY
	 */

	/*	Return a new <link> element linking a style sheet (.css file) for the
		given theme name and color scheme preference (i.e., value for the 
		‘media’ attribute; may be “light”, “dark”, or “” [empty string]).
	 */
	makeNewStyle: (newThemeName, colorSchemePreference) => {
		let styleSheetNameSuffix = newThemeName == Appearance.defaultTheme
								   ? "" 
								   : ("-" + newThemeName);
		let currentStyleSheetNameComponents = /style[^\.]*(\..+)$/.exec(query("head link[href*='.css']").href);

		let newStyle = newElement("LINK", {
			"class": "theme",
			"rel": "stylesheet",
			"href": ("/css/style" + styleSheetNameSuffix + currentStyleSheetNameComponents[1]),
			"media": (colorSchemePreference ? ("(prefers-color-scheme: " + colorSchemePreference + ")") : "")
		});
		return newStyle;
	},

	setTheme: (newThemeName, save = true) => {
		GWLog("Appearance.setTheme");

		let oldThemeName = "";
		if (typeof(newThemeName) == "undefined") {
			/*	If no theme name to set is given, that means we’re setting the 
				theme initially, on page load. The .currentTheme value will have
				been set by .setup().
			 */
			newThemeName = Appearance.currentTheme;

			/*	If the selected (saved) theme is the default theme, then there’s
				nothing to do.
			 */
			if (newThemeName == Appearance.defaultTheme)
				return;
		} else {
			oldThemeName = Appearance.currentTheme;

			/*	When the unload callback runs, the .currentTheme value is still 
				that of the old theme.
			 */
			let themeUnloadCallback = Appearance.themeUnloadCallbacks[oldThemeName];
			if (themeUnloadCallback != null)
				themeUnloadCallback(newThemeName);

			/*	The old .currentTheme value is saved in oldThemeName.
			 */
			Appearance.currentTheme = newThemeName;

			/*	The ‘save’ parameter might be false if this function is called 
				from the theme tweaker, in which case we want to switch only 
				temporarily, and preserve the saved setting until the user 
				clicks “OK”.
			 */
			if (save)
				Appearance.saveCurrentTheme();
		}

		let newMainStyle, newStyles;
		if (newThemeName === Appearance.defaultTheme) {
			newStyles = [ Appearance.makeNewStyle("dark", "dark"), Appearance.makeNewStyle(Appearance.defaultTheme, "light") ];
			newMainStyle = (window.matchMedia("prefers-color-scheme: dark").matches ? newStyles[0] : newStyles[1]);
		} else {
			newStyles = [ Appearance.makeNewStyle(newThemeName) ];
			newMainStyle = newStyles[0];
		}

		let oldStyles = queryAll("head link.theme");
		newMainStyle.addEventListener("load", (event) => { oldStyles.forEach(x => removeElement(x)); });
		newMainStyle.addEventListener("load", (event) => { Appearance.postSetThemeHousekeeping(oldThemeName, newThemeName); });

		if (Appearance.adjustmentTransitions) {
			pageFadeTransition(false);
			setTimeout(() => {
				newStyles.forEach(newStyle => document.head.insertBefore(newStyle, oldStyles[0].nextSibling));
			}, 500);
		} else {
			newStyles.forEach(newStyle => document.head.insertBefore(newStyle, oldStyles[0].nextSibling));
		}

		//	Update UI state of all theme selectors.
		Appearance.updateThemeSelectorsState();
	},

	postSetThemeHousekeeping: (oldThemeName = "", newThemeName = null) => {
		GWLog("Appearance.postSetThemeHousekeeping");

		if (newThemeName == null)
			newThemeName = Appearance.getSavedTheme();

		document.body.className = document.body.className.replace(new RegExp("(^|\\s+)theme-\\w+(\\s+|$)"), "$1").trim();
		document.body.addClass("theme-" + newThemeName);

		recomputeUIElementsContainerHeight(true);

		let themeLoadCallback = Appearance.themeLoadCallbacks[newThemeName];
		if (themeLoadCallback != null)
			themeLoadCallback(oldThemeName);

		recomputeUIElementsContainerHeight();
		adjustUIForWindowSize();
		window.addEventListener("resize", GW.windowResized = (event) => {
			GWLog("GW.windowResized");
			adjustUIForWindowSize();
			recomputeUIElementsContainerHeight();
		});

		generateImagesOverlay();

		if (Appearance.adjustmentTransitions)
			pageFadeTransition(true);
		Appearance.updateThemeTweakerSampleText();

		if (typeof(window.msMatchMedia || window.MozMatchMedia || window.WebkitMatchMedia || window.matchMedia) !== "undefined") {
			window.matchMedia("(orientation: portrait)").addListener(generateImagesOverlay);
		}
	},

	themeLoadCallbacks: {
		brutalist: (fromTheme = "") => {
			GWLog("Appearance.themeLoadCallbacks.brutalist");

			let bottomBarLinks = queryAll("#bottom-bar a");
			if (!GW.isMobile && bottomBarLinks.length == 5) {
				let newLinkTexts = [ "First", "Previous", "Top", "Next", "Last" ];
				bottomBarLinks.forEach((link, i) => {
					link.dataset.originalText = link.textContent;
					link.textContent = newLinkTexts[i];
				});
			}
		},

		classic: (fromTheme = "") => {
			GWLog("Appearance.themeLoadCallbacks.classic");

			queryAll(".comment-item .comment-controls .action-button").forEach(button => {
				button.innerHTML = "";
			});
		},

		dark: (fromTheme = "") => {
			GWLog("Appearance.themeLoadCallbacks.dark");

			insertHeadHTML(`<style id="dark-theme-adjustments">
				.markdown-reference-link a { color: #d200cf; filter: invert(100%); }
				#bottom-bar.decorative::before { filter: invert(100%); }
			</style>`);
			registerInitializer("makeImagesGlow", true, () => query("#images-overlay") != null, () => {
				queryAll(GW.imageFocus.overlayImagesSelector).forEach(image => {
					image.style.filter = "drop-shadow(0 0 0 #000) drop-shadow(0 0 0.5px #fff) drop-shadow(0 0 1px #fff) drop-shadow(0 0 2px #fff)";
					image.style.width = parseInt(image.style.width) + 12 + "px";
					image.style.height = parseInt(image.style.height) + 12 + "px";
					image.style.top = parseInt(image.style.top) - 6 + "px";
					image.style.left = parseInt(image.style.left) - 6 + "px";
				});
			});
		},

		less: (fromTheme = "") => {
			GWLog("Appearance.themeLoadCallbacks.less");

			injectSiteNavUIToggle();
			if (!GW.isMobile) {
				injectPostNavUIToggle();
				Appearance.injectAppearanceAdjustUIToggle();
			}

			registerInitializer("shortenDate", true, () => query(".top-post-meta") != null, function () {
				let dtf = new Intl.DateTimeFormat([], 
					(window.innerWidth < 1100) ? 
						{ month: "short", day: "numeric", year: "numeric" } : 
							{ month: "long", day: "numeric", year: "numeric" });
				let postDate = query(".top-post-meta .date");
				postDate.innerHTML = dtf.format(new Date(+ postDate.dataset.jsDate));
			});

			if (GW.isMobile) {
				query("#content").insertAdjacentHTML("beforeend", `<div id="theme-less-mobile-first-row-placeholder"></div>`);
			}

			if (!GW.isMobile) {
				registerInitializer("addSpans", true, () => query(".top-post-meta") != null, function () {
					queryAll(".top-post-meta .date, .top-post-meta .comment-count").forEach(element => {
						element.innerHTML = "<span>" + element.innerHTML + "</span>";
					});
				});

				if (localStorage.getItem("appearance-adjust-ui-toggle-engaged") == null) {
					// If state is not set (user has never clicked on the Less theme’s appearance
					// adjustment UI toggle) then show it, but then hide it after a short time.
					registerInitializer("engageAppearanceAdjustUI", true, () => query("#ui-elements-container") != null, function () {
						Appearance.toggleAppearanceAdjustUI();
						setTimeout(Appearance.toggleAppearanceAdjustUI, 3000);
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
				Appearance.filtersExclusionPaths.themeLess = [
					"#content #secondary-bar",
					"#content .post .top-post-meta .date",
					"#content .post .top-post-meta .comment-count",
				];
				Appearance.applyFilters();
			}

			// We pre-query the relevant elements, so we don’t have to run querySelectorAll
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
	},

	themeUnloadCallbacks: {
		brutalist: (toTheme = "") => {
			GWLog("Appearance.themeUnloadCallbacks.brutalist");

			let bottomBarLinks = queryAll("#bottom-bar a");
			if (!GW.isMobile && bottomBarLinks.length == 5) {
				bottomBarLinks.forEach(link => {
					link.textContent = link.dataset.originalText;
				});
			}
		},

		classic: (toTheme = "") => {
			GWLog("Appearance.themeUnloadCallbacks.classic");

			if (GW.isMobile && window.innerWidth <= 900)
				return;

			queryAll(".comment-item .comment-controls .action-button").forEach(button => {
				button.innerHTML = button.dataset.label;
			});
		},

		dark: (toTheme = "") => {
			GWLog("Appearance.themeUnloadCallbacks.dark");

			removeElement("#dark-theme-adjustments");
		},

		less: (toTheme = "") => {
			GWLog("Appearance.themeUnloadCallbacks.less");

			removeSiteNavUIToggle();
			if (!GW.isMobile) {
				removePostNavUIToggle();
				Appearance.removeAppearanceAdjustUIToggle();
			}

			window.removeEventListener("resize", updatePostNavUIVisibility);

			document.removeEventListener("scroll", GW["updateSiteNavUIStateScrollListener"]);

			removeElement("#theme-less-mobile-first-row-placeholder");

			if (!GW.isMobile) {
				// Remove spans
				queryAll(".top-post-meta .date, .top-post-meta .comment-count").forEach(element => {
					element.innerHTML = element.firstChild.innerHTML;
				});
			}

			(query(".top-post-meta .date")||{}).innerHTML = (query(".bottom-post-meta .date")||{}).innerHTML;

			//	Reset filtered elements selector to default.
			delete Appearance.filtersExclusionPaths.themeLess;
			Appearance.applyFilters();
		}
	},

	pageFadeTransition: (fadeIn) => {
		if (fadeIn) {
			document.body.removeClass("transparent");
		} else {
			document.body.addClass("transparent");
		}
	},

	/*	Set the saved theme setting to the currently active theme.
	 */
	saveCurrentTheme: () => {
		GWLog("Appearance.saveCurrentTheme");

		if (Appearance.currentTheme == Appearance.defaultTheme)
			setCookie("theme", "");
		else
			setCookie("theme", Appearance.currentTheme);
	},

	/*	Reset theme, theme tweak filters, and text zoom to their saved settings.
	 */
	themeTweakReset: () => {
		GWLog("Appearance.themeTweakReset");

		Appearance.setTheme(Appearance.getSavedTheme());
		Appearance.applyFilters(Appearance.getSavedFilters());
		Appearance.setTextZoom(Appearance.getSavedTextZoom());
	},

	/*	Set the saved theme, theme tweak filter, and text zoom settings to their
		currently active values.
	 */
	themeTweakSave: () => {
		GWLog("Appearance.themeTweakSave");

		Appearance.saveCurrentTheme();
		Appearance.saveCurrentFilters();
		Appearance.saveCurrentTextZoom();
	},

	/*	Reset theme, theme tweak filters, and text zoom to their default levels.
		(Do not save the new settings, however.)
	 */
	themeTweakResetDefaults: () => {
		GWLog("Appearance.themeTweakResetDefaults");

		Appearance.setTheme(Appearance.defaultTheme, false);
		Appearance.applyFilters(Appearance.defaultFilters);
		Appearance.setTextZoom(Appearance.defaultTextZoom, false);
	},

	themeTweakerResetSettings: () => {
		GWLog("Appearance.themeTweakerResetSettings");

		Appearance.themeTweakerUIClippyControl.checked = Appearance.getSavedThemeTweakerClippyState();
		Appearance.themeTweakerUIClippyContainer.style.display = Appearance.themeTweakerUIClippyControl.checked 
																 ? "block" 
																 : "none";
	},

	themeTweakerSaveSettings: () => {
		GWLog("Appearance.themeTweakerSaveSettings");

		Appearance.saveThemeTweakerClippyState();
	},

	getSavedThemeTweakerClippyState: () => {
		return (JSON.parse(localStorage.getItem("theme-tweaker-settings") || `{ "showClippy": ${Appearance.defaultThemeTweakerClippyState} }` )["showClippy"]);
	},

	saveThemeTweakerClippyState: () => {
		GWLog("Appearance.saveThemeTweakerClippyState");

		localStorage.setItem("theme-tweaker-settings", JSON.stringify({ "showClippy": Appearance.themeTweakerUIClippyControl.checked }));
	},

	getSavedAppearanceAdjustUIToggleState: () => {
		return ((localStorage.getItem("appearance-adjust-ui-toggle-engaged") == "true") || Appearance.defaultAppearanceAdjustUIToggleState);
	},

	saveAppearanceAdjustUIToggleState: () => {
		GWLog("Appearance.saveAppearanceAdjustUIToggleState");

		localStorage.setItem("appearance-adjust-ui-toggle-engaged", Appearance.appearanceAdjustUIToggle.query("button").hasClass("engaged"));
	},

	/**************************************************************************/
	/* UI CONSTRUCTION & MANIPULATION
	 */

	contentWidthSelectorHTML: () => {
		return ("<div id='width-selector'>"
			+ String.prototype.concat.apply("", Appearance.widthOptions.map(widthOption => {
				let [name, desc, abbr] = widthOption;
				let selected = (name == Appearance.currentWidth ? " selected" : "");
				let disabled = (name == Appearance.currentWidth ? " disabled" : "");
				return `<button type="button" class="select-width-${name}${selected}"${disabled} title="${desc}" tabindex="-1" data-name="${name}">${abbr}</button>`
			}))
		+ "</div>");
	},

	injectContentWidthSelector: () => {
		GWLog("Appearance.injectContentWidthSelector");

		//	Inject the content width selector widget and activate buttons.
		Appearance.widthSelector = addUIElement(Appearance.contentWidthSelectorHTML());
		Appearance.widthSelector.queryAll("button").forEach(button => {
			button.addActivateEvent(Appearance.widthAdjustButtonClicked);
		});

		//	Make sure the accesskey (to cycle to the next width) is on the right button.
		Appearance.setWidthAdjustButtonsAccesskey();

		//	Inject transitions CSS, if animating changes is enabled.
		if (Appearance.adjustmentTransitions) {
			insertHeadHTML(
				`<style id="width-transition">
					#content,
					#ui-elements-container,
					#images-overlay {
						transition:
							max-width 0.3s ease;
					}
				</style>`);
		}
	},

	setWidthAdjustButtonsAccesskey: () => {
		GWLog("Appearance.setWidthAdjustButtonsAccesskey");

		Appearance.widthSelector.queryAll("button").forEach(button => {
			button.removeAttribute("accesskey");
			button.title = /(.+?)( \['\])?$/.exec(button.title)[1];
		});
		let selectedButton = Appearance.widthSelector.query("button.selected");
		let nextButtonInCycle = selectedButton == selectedButton.parentElement.lastChild
												  ? selectedButton.parentElement.firstChild 
												  : selectedButton.nextSibling;
		nextButtonInCycle.accessKey = "'";
		nextButtonInCycle.title += ` [\']`;
	},

	injectTextSizeAdjustmentUI: () => {
		GWLog("Appearance.injectTextSizeAdjustmentUI");

		if (Appearance.textSizeAdjustmentWidget != null)
			return;

		let inject = () => {
			GWLog("Appearance.injectTextSizeAdjustmentUI [INJECTING]");

			Appearance.textSizeAdjustmentWidget = addUIElement("<div id='text-size-adjustment-ui'>"
				+ `<button type='button' class='text-size-adjust-button decrease' title="Decrease text size [-]" tabindex='-1' accesskey='-'>&#xf068;</button>`
				+ `<button type='button' class='text-size-adjust-button default' title="Reset to default text size [0]" tabindex='-1' accesskey='0'>A</button>`
				+ `<button type='button' class='text-size-adjust-button increase' title="Increase text size [=]" tabindex='-1' accesskey='='>&#xf067;</button>`
			+ "</div>");

			Appearance.textSizeAdjustmentWidget.queryAll("button").forEach(button => {
				button.addActivateEvent(Appearance.textSizeAdjustButtonClicked);
			});
		};

		if (query("#content.post-page") != null) {
			inject();
		} else {
			document.addEventListener("DOMContentLoaded", () => {
				if (!(   query(".post-body") == null 
					  && query(".comment-body") == null))
					inject();
			}, { once: true });
		}
	},

	themeSelectorHTML: () => {
		return ("<div id='theme-selector' class='theme-selector'>"
			+ String.prototype.concat.apply("", Appearance.themeOptions.map(themeOption => {
				let [name, desc, letter] = themeOption;
				let selected = (name == Appearance.currentTheme ? ' selected' : '');
				let disabled = (name == Appearance.currentTheme ? ' disabled' : '');
				let accesskey = letter.charCodeAt(0) - 'A'.charCodeAt(0) + 1;
				return `<button type='button' class='select-theme select-theme-${name}${selected}'${disabled} title="${desc} [${accesskey}]" data-theme-name="${name}" data-theme-description="${desc}" accesskey='${accesskey}' tabindex='-1'>${letter}</button>`;
			}))
		+ "</div>");
	},

	injectThemeSelector: () => {
		GWLog("Appearance.injectThemeSelector");

		Appearance.themeSelector = addUIElement(Appearance.themeSelectorHTML());
		Appearance.themeSelector.queryAll("button").forEach(button => {
			button.addActivateEvent(Appearance.themeSelectButtonClicked);
		});

		if (GW.isMobile) {
			//	Add close button.
			let themeSelectorCloseButton = newElement("BUTTON", { "class": "theme-selector-close-button" }, { "innerHTML": "&#xf057;" });
			themeSelectorCloseButton.addActivateEvent(Appearance.themeSelectorCloseButtonClicked);
			Appearance.themeSelector.appendChild(themeSelectorCloseButton);

			//	Inject auxiliary controls container.
			Appearance.themeSelectorAuxiliaryControlsContainer = newElement("DIV", { "class": "auxiliary-controls-container" });
			Appearance.themeSelector.appendChild(Appearance.themeSelectorAuxiliaryControlsContainer);

			//	Inject mobile versions of various UI elements.
			Appearance.injectThemeTweakerToggle();
			injectAntiKibitzerToggle();
			DarkMode.injectModeSelector();

			//	Inject interaction blocker overlay.
			Appearance.themeSelectorInteractionBlockerOverlay = Appearance.themeSelector.appendChild(newElement("DIV", { "class": "interaction-blocker-overlay" }));
			Appearance.themeSelectorInteractionBlockerOverlay.addActivateEvent(event => { event.stopPropagation(); });
		}

		//	Inject transitions CSS, if animating changes is enabled.
		if (Appearance.adjustmentTransitions) {
			insertHeadHTML(`<style id="theme-fade-transition">
				body {
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
				}
			</style>`);
		}
	},

	updateThemeSelectorsState: () => {
		GWLog("Appearance.updateThemeSelectorsState");

		queryAll(".theme-selector button.select-theme").forEach(button => {
			button.removeClass("selected");
			button.disabled = false;
		});
		queryAll(".theme-selector button.select-theme-" + Appearance.currentTheme).forEach(button => {
			button.addClass("selected");
			button.disabled = true;
		});

		Appearance.themeTweakerUI.query(".current-theme span").innerText = Appearance.currentTheme;
	},

	setThemeSelectorInteractable: (interactable) => {
		GWLog("Appearance.setThemeSelectorInteractable");

		Appearance.themeSelectorInteractionBlockerOverlay.classList.toggle("enabled", (interactable == false));
	},

	themeTweakerUIHTML: () => {
		return (`<div id="theme-tweaker-ui" style="display: none;">\n` 
			+ `<div class="theme-tweaker-window main-window">
				<div class="theme-tweaker-window-title-bar">
					<div class="theme-tweaker-window-title">
						<h1>Customize appearance</h1>
					</div>
					<div class="theme-tweaker-window-title-bar-buttons-container">
						<button type="button" class="help-button" tabindex="-1"></button>
						<button type="button" class="minimize-button minimize" tabindex="-1"></button>
						<button type="button" class="close-button" tabindex="-1"></button>
					</div>
				</div>
				<div class="theme-tweaker-window-content-view">
					<div class="theme-select">
						<p class="current-theme">Current theme:
							<span>${Appearance.getSavedTheme()}</span>
						</p>
						<div class="theme-selector"></div>
					</div>
					<div class="controls-container">
						<div id="theme-tweak-section-sample-text" class="section" data-label="Sample text">
							<div class="sample-text-container"><span class="sample-text">
								<p>Less Wrong (text)</p>
								<p><a href="#">Less Wrong (link)</a></p>
							</span></div>
						</div>
						<div id="theme-tweak-section-text-size-adjust" class="section" data-label="Text size">
							<button type="button" class="text-size-adjust-button decrease" title="Decrease text size"></button>
							<button type="button" class="text-size-adjust-button default" title="Reset to default text size"></button>
							<button type="button" class="text-size-adjust-button increase" title="Increase text size"></button>
						</div>
						<div id="theme-tweak-section-invert" class="section" data-label="Invert (photo-negative)">
							<input type="checkbox" id="theme-tweak-control-invert"></input>
							<label for="theme-tweak-control-invert">Invert colors</label>
						</div>
						<div id="theme-tweak-section-saturate" class="section" data-label="Saturation">
							<input type="range" id="theme-tweak-control-saturate" min="0" max="300" data-default-value="100" data-value-suffix="%" data-label-suffix="%">
							<p class="theme-tweak-control-label" id="theme-tweak-label-saturate"></p>
							<div class="notch theme-tweak-slider-notch-saturate" title="Reset saturation to default value (100%)"></div>
						</div>
						<div id="theme-tweak-section-brightness" class="section" data-label="Brightness">
							<input type="range" id="theme-tweak-control-brightness" min="0" max="300" data-default-value="100" data-value-suffix="%" data-label-suffix="%">
							<p class="theme-tweak-control-label" id="theme-tweak-label-brightness"></p>
							<div class="notch theme-tweak-slider-notch-brightness" title="Reset brightness to default value (100%)"></div>
						</div>
						<div id="theme-tweak-section-contrast" class="section" data-label="Contrast">
							<input type="range" id="theme-tweak-control-contrast" min="0" max="300" data-default-value="100" data-value-suffix="%" data-label-suffix="%">
							<p class="theme-tweak-control-label" id="theme-tweak-label-contrast"></p>
							<div class="notch theme-tweak-slider-notch-contrast" title="Reset contrast to default value (100%)"></div>
						</div>
						<div id="theme-tweak-section-hue-rotate" class="section" data-label="Hue rotation">
							<input type="range" id="theme-tweak-control-hue-rotate" min="0" max="360" data-default-value="0" data-value-suffix="deg" data-label-suffix="°">
							<p class="theme-tweak-control-label" id="theme-tweak-label-hue-rotate"></p>
							<div class="notch theme-tweak-slider-notch-hue-rotate" title="Reset hue to default (0° away from standard colors for theme)"></div>
						</div>
					</div>
					<div class="buttons-container">
						<button type="button" class="reset-defaults-button">Reset to defaults</button>
						<button type="button" class="ok-button default-button">OK</button>
						<button type="button" class="cancel-button">Cancel</button>
					</div>
				</div>
			</div>
			<div class="clippy-container">
				<span class="hint">Hi, I’m Bobby the Basilisk! Click on the minimize button (<img src="data:image/gif;base64,R0lGODlhFgAUAPIAMQAAAAMDA394f7+4v9/Y3//4/2JX/38AACwAAAAAFgAUAAADRki63B6kyEkrFbCMzbvnWPSNXqiRqImm2Uqq7gfH3Uxv9p3TNuD/wFqLAywChCKi8Yc83XDD52AXCwmu2KxWG+h6v+BwNwEAOw==" />) to minimize the theme tweaker window, so that you can see what the page looks like with the current tweaked values. (But remember, <span>the changes won’t be saved until you click “OK”!</span>)
				<div class="clippy"></div>
				<button type="button" class="clippy-close-button" tabindex="-1" title="Hide theme tweaker assistant (you can bring him back by clicking the ? button in the title bar)"></button>
			</div>
			<div class="theme-tweaker-window help-window" style="display: none;">
				<div class="theme-tweaker-window-title-bar">
					<div class="theme-tweaker-window-title">
						<h1>Theme tweaker help</h1>
					</div>
				</div>
				<div class="theme-tweaker-window-content-view">
					<div id="theme-tweak-section-clippy" class="section" data-label="Theme Tweaker Assistant">
						<input type="checkbox" id="theme-tweak-control-clippy" checked="checked"></input>
						<label for="theme-tweak-control-clippy">Show Bobby the Basilisk</label>
					</div>
					<div class="buttons-container">
						<button type="button" class="ok-button default-button">OK</button>
						<button type="button" class="cancel-button">Cancel</button>
					</div>
				</div>
			</div>
		` + `\n</div>`);
	},

	injectThemeTweaker: () => {
		GWLog("Appearance.injectThemeTweaker");

		Appearance.themeTweakerUI = addUIElement(Appearance.themeTweakerUIHTML());
		Appearance.themeTweakerUIMainWindow = Appearance.themeTweakerUI.firstElementChild;
		Appearance.themeTweakerUIHelpWindow = Appearance.themeTweakerUI.query(".help-window");
		Appearance.themeTweakerUISampleTextContainer = Appearance.themeTweakerUI.query("#theme-tweak-section-sample-text .sample-text-container");
		Appearance.themeTweakerUIClippyContainer = Appearance.themeTweakerUI.query(".clippy-container");
		Appearance.themeTweakerUIClippyControl = Appearance.themeTweakerUI.query("#theme-tweak-control-clippy");

		//	Clicking the background overlay closes the theme tweaker.
		Appearance.themeTweakerUI.addActivateEvent(Appearance.themeTweakerUIOverlayClicked, true);

		//	Intercept clicks, so they don’t “fall through” the background overlay.
		Array.from(Appearance.themeTweakerUI.children).forEach(themeTweakerUIWindow => {
			themeTweakerUIWindow.addActivateEvent((event) => {
				event.stopPropagation();
			}, true);
		});

		Appearance.themeTweakerUI.queryAll("input").forEach(field => {
			/*	All input types in the theme tweaker receive a ‘change’ event 
				when their value is changed. (Range inputs, in particular, 
				receive this event when the user lets go of the handle.) This 
				means we should update the filters for the entire page, to match 
				the new setting.
			 */
			field.addEventListener("change", Appearance.themeTweakerUIFieldValueChanged);

			/*	Range inputs receive an ‘input’ event while being scrubbed, 
				updating “live” as the handle is moved. We don’t want to change 
				the filters for the actual page while this is happening, but we 
				do want to change the filters for the *sample text*, so the user
				can see what effects his changes are having, live, without 
				having to let go of the handle.
			 */
			if (field.type == "range")
				field.addEventListener("input", Appearance.themeTweakerUIFieldInputReceived);
		});

		Appearance.themeTweakerUI.query(".help-button").addActivateEvent(Appearance.themeTweakerUIHelpButtonClicked);
		Appearance.themeTweakerUI.query(".minimize-button").addActivateEvent(Appearance.themeTweakerUIMinimizeButtonClicked);
		Appearance.themeTweakerUI.query(".close-button").addActivateEvent(Appearance.themeTweakerUICloseButtonClicked);
		Appearance.themeTweakerUI.query(".reset-defaults-button").addActivateEvent(Appearance.themeTweakerUIResetDefaultsButtonClicked);
		Appearance.themeTweakerUI.query(".main-window .cancel-button").addActivateEvent(Appearance.themeTweakerUICancelButtonClicked);
		Appearance.themeTweakerUI.query(".main-window .ok-button").addActivateEvent(Appearance.themeTweakerUIOKButtonClicked);
		Appearance.themeTweakerUI.query(".help-window .cancel-button").addActivateEvent(Appearance.themeTweakerUIHelpWindowCancelButtonClicked);
		Appearance.themeTweakerUI.query(".help-window .ok-button").addActivateEvent(Appearance.themeTweakerUIHelpWindowOKButtonClicked);

		Appearance.themeTweakerUI.queryAll(".notch").forEach(notch => {
			notch.addActivateEvent(Appearance.themeTweakerUISliderNotchClicked);
		});

		Appearance.themeTweakerUI.query(".clippy-close-button").addActivateEvent(Appearance.themeTweakerUIClippyCloseButtonClicked);

		insertHeadHTML(`<style id="theme-tweaker-style"></style>`);
		Appearance.themeTweakerStyleBlock = document.head.query("#theme-tweaker-style");

		Appearance.themeTweakerUI.query(".theme-selector").innerHTML = query("#theme-selector").innerHTML;
		Appearance.themeTweakerUI.queryAll(".theme-selector > *:not(.select-theme)").forEach(element => {
			element.remove();
		});
		Appearance.themeTweakerUI.queryAll(".theme-selector button").forEach(button => {
			button.addActivateEvent(Appearance.themeSelectButtonClicked);
		});

		Appearance.themeTweakerUI.queryAll("#theme-tweak-section-text-size-adjust button").forEach(button => {
			button.addActivateEvent(Appearance.textSizeAdjustButtonClicked);
		});

		if (GW.isMobile == false)
			Appearance.injectThemeTweakerToggle();
	},

	themeTweakerToggleHTML: () => {
		return (`<div id="theme-tweaker-toggle">`
					+ `<button 
							type="button" 
							tabindex="-1" 
							title="Customize appearance [;]" 
							accesskey=";"
								>&#xf1de;</button>`
				+ `</div>`);
	},

	injectThemeTweakerToggle: () => {
		GWLog("Appearance.injectThemeTweakerToggle");

		if (GW.isMobile) {
			if (Appearance.themeSelector == null)
				return;

			Appearance.themeSelectorAuxiliaryControlsContainer.insertAdjacentHTML("beforeend", Appearance.themeTweakerToggleHTML());
			Appearance.themeTweakerToggle = Appearance.themeSelector.query("#theme-tweaker-toggle");
		} else {
			Appearance.themeTweakerToggle = addUIElement(Appearance.themeTweakerToggleHTML());	
		}

		Appearance.themeTweakerToggle.query("button").addActivateEvent(Appearance.themeTweakerToggleClicked);
	},

	showThemeTweakerUI: () => {
		GWLog("Appearance.showThemeTweakerUI");

		if (query("link[href^='/css/theme_tweaker.css']") == null) {
			//	Theme tweaker CSS needs to be loaded.

			let themeTweakerStyleSheet = newElement("LINK", {
				"rel": "stylesheet",
				"href": "/css/theme_tweaker.css"
			});

			themeTweakerStyleSheet.addEventListener("load", (event) => {
				requestAnimationFrame(() => {
					themeTweakerStyleSheet.disabled = false;
				});
				Appearance.showThemeTweakerUI();
			}, { once: true });

			document.head.appendChild(themeTweakerStyleSheet);

			return;
		}

		Appearance.themeTweakerUI.query(".current-theme span").innerText = Appearance.getSavedTheme();

		Appearance.themeTweakerUI.query("#theme-tweak-control-invert").checked = (Appearance.currentFilters["invert"] == "100%");
		[ "saturate", "brightness", "contrast", "hue-rotate" ].forEach(sliderName => {
			let slider = Appearance.themeTweakerUI.query("#theme-tweak-control-" + sliderName);
			slider.value = /^[0-9]+/.exec(Appearance.currentFilters[sliderName]) || slider.dataset["defaultValue"];
			Appearance.themeTweakerUI.query("#theme-tweak-label-" + sliderName).innerText = slider.value + slider.dataset["labelSuffix"];
		});

		Appearance.toggleThemeTweakerUI();
	},

	toggleThemeTweakerUI: () => {
		GWLog("Appearance.toggleThemeTweakerUI");

		let show = (Appearance.themeTweakerUI.style.display == "none");

		Appearance.themeTweakerUI.style.display = show ? "block" : "none";
		Appearance.setThemeTweakerWindowMinimized(false);
		Appearance.themeTweakerStyleBlock.innerHTML = show ? `#content, #ui-elements-container > div:not(#theme-tweaker-ui) { pointer-events: none; user-select: none; }` : "";

		if (show) {
			// Disable button.
			Appearance.themeTweakerToggle.query("button").disabled = true;
			// Focus invert checkbox.
			Appearance.themeTweakerUI.query("#theme-tweaker-ui #theme-tweak-control-invert").focus();
			// Show sample text in appropriate font.
			Appearance.updateThemeTweakerSampleText();
			// Disable tab-selection of the search box.
			setSearchBoxTabSelectable(false);
			// Disable scrolling of the page.
			togglePageScrolling(false);
		} else {
			// Re-enable button.
			Appearance.themeTweakerToggle.query("button").disabled = false;
			// Re-enable tab-selection of the search box.
			setSearchBoxTabSelectable(true);
			// Re-enable scrolling of the page.
			togglePageScrolling(true);
		}

		// Set theme tweaker assistant visibility.
		Appearance.themeTweakerUIClippyContainer.style.display = (Appearance.getSavedThemeTweakerClippyState() == true) ? "block" : "none";
	},

	setThemeTweakerWindowMinimized: (minimize) => {
		GWLog("Appearance.setThemeTweakerWindowMinimized");

		Appearance.themeTweakerUIMainWindow.query(".minimize-button").swapClasses([ "minimize", "maximize" ], (minimize ? 1 : 0));
		Appearance.themeTweakerUIMainWindow.classList.toggle("minimized", minimize);
		Appearance.themeTweakerUI.classList.toggle("main-window-minimized", minimize);
	},

	toggleThemeTweakerHelpWindow: () => {
		GWLog("Appearance.toggleThemeTweakerHelpWindow");

		Appearance.themeTweakerUIHelpWindow.style.display = Appearance.themeTweakerUIHelpWindow.style.display == "none" 
														? "block" 
														: "none";
		if (Appearance.themeTweakerUIHelpWindow.style.display != "none") {
			// Focus theme tweaker assistant checkbox.
			Appearance.themeTweakerUI.query("#theme-tweak-control-clippy").focus();
			// Disable interaction on main theme tweaker window.
			Appearance.themeTweakerUI.style.pointerEvents = "none";
			Appearance.themeTweakerUIMainWindow.style.pointerEvents = "none";
		} else {
			// Re-enable interaction on main theme tweaker window.
			Appearance.themeTweakerUI.style.pointerEvents = "auto";
			Appearance.themeTweakerUIMainWindow.style.pointerEvents = "auto";
		}
	},

	resetThemeTweakerUIDefaultState: () => {
		GWLog("Appearance.resetThemeTweakerUIDefaultState");

		Appearance.themeTweakerUI.query("#theme-tweak-control-invert").checked = false;

		[ "saturate", "brightness", "contrast", "hue-rotate" ].forEach(sliderName => {
			let slider = Appearance.themeTweakerUI.query("#theme-tweak-control-" + sliderName);
			slider.value = slider.dataset["defaultValue"];
			Appearance.themeTweakerUI.query("#theme-tweak-label-" + sliderName).innerText = slider.value + slider.dataset["labelSuffix"];
		});
	},

	updateThemeTweakerSampleText: () => {
		GWLog("Appearance.updateThemeTweakerSampleText");

		let sampleText = Appearance.themeTweakerUISampleTextContainer.query("#theme-tweak-section-sample-text .sample-text");

		// This causes the sample text to take on the properties of the body text of a post.
		sampleText.removeClass("body-text");
		let bodyTextElement = query(".post-body") || query(".comment-body");
		sampleText.addClass("body-text");
		sampleText.style.color = bodyTextElement ? 
			getComputedStyle(bodyTextElement).color : 
			getComputedStyle(query("#content")).color;

		// Here we find out what is the actual background color that will be visible behind
		// the body text of posts, and set the sample text’s background to that.
		let findStyleBackground = (selector) => {
			let x;
			Array.from(query("link[rel=stylesheet]").sheet.cssRules).forEach(rule => {
				if (rule.selectorText == selector)
					x = rule;
			});
			return x.style.backgroundColor;
		};

		sampleText.parentElement.style.backgroundColor = findStyleBackground("#content::before") || findStyleBackground("body") || "#fff";
	},

	injectAppearanceAdjustUIToggle: () => {
		GWLog("Appearance.injectAppearanceAdjustUIToggle");

		Appearance.appearanceAdjustUIToggle = addUIElement(`<div id="appearance-adjust-ui-toggle"><button type="button" tabindex="-1">&#xf013;</button></div>`);
		Appearance.appearanceAdjustUIToggle.query("button").addActivateEvent(Appearance.appearanceAdjustUIToggleButtonClicked);

		if (  !GW.isMobile 
			&& Appearance.getSavedAppearanceAdjustUIToggleState() == true) {
			Appearance.toggleAppearanceAdjustUI();
		}
	},

	removeAppearanceAdjustUIToggle: () => {
		GWLog("Appearance.removeAppearanceAdjustUIToggle");

		queryAll(Appearance.themeLessAppearanceAdjustUIElementsSelector).forEach(element => {
			element.removeClass("engaged");
		});
		removeElement("#appearance-adjust-ui-toggle");
	},

	toggleAppearanceAdjustUI: () => {
		GWLog("Appearance.toggleAppearanceAdjustUI");

		queryAll(Appearance.themeLessAppearanceAdjustUIElementsSelector).forEach(element => {
			element.toggleClass("engaged");
		});

		if (GW.isMobile) {
			clearTimeout(Appearance.themeSelectorInteractableTimer);
			Appearance.setThemeSelectorInteractable(false);
			Appearance.themeSelectorInteractableTimer = setTimeout(() => {
				Appearance.setThemeSelectorInteractable(true);
			}, 200);
		}
	},

	/**************************************************************************/
	/* EVENTS
	 */

	/*	Theme selector close button (on mobile version of theme selector).
	 */
	themeSelectorCloseButtonClicked: (event) => {
		GWLog("Appearance.themeSelectorCloseButtonClicked");

		Appearance.toggleAppearanceAdjustUI();
		Appearance.saveAppearanceAdjustUIToggleState();
	},

	/*	“Cog” button (to toggle the appearance adjust UI widgets in “less” 
		theme, or theme selector UI on mobile).
	 */
	appearanceAdjustUIToggleButtonClicked: (event) => {
		GWLog("Appearance.appearanceAdjustUIToggleButtonClicked");

		Appearance.toggleAppearanceAdjustUI();
		Appearance.saveAppearanceAdjustUIToggleState();
	},

	/*	Width adjust buttons (“normal”, “wide”, “fluid”).
	 */
	widthAdjustButtonClicked: (event) => {
		GWLog("Appearance.widthAdjustButtonClicked");

		// Determine which setting was chosen (i.e., which button was clicked).
		let selectedWidth = event.target.dataset.name;

		//	Switch width.
		Appearance.currentWidth = selectedWidth;

		// Save the new setting.
		Appearance.saveCurrentWidth();

		// Save current visible comment
		let visibleComment = getCurrentVisibleComment();

		// Actually change the content width.
		Appearance.setContentWidth(selectedWidth);
		event.target.parentElement.childNodes.forEach(button => {
			button.removeClass("selected");
			button.disabled = false;
		});
		event.target.addClass("selected");
		event.target.disabled = true;

		// Make sure the accesskey (to cycle to the next width) is on the right button.
		Appearance.setWidthAdjustButtonsAccesskey();

		// Regenerate images overlay.
		generateImagesOverlay();

		if (visibleComment)
			visibleComment.scrollIntoView();
	},

	/*	Theme selector buttons (“A” through “I”).
	 */
	themeSelectButtonClicked: (event) => {
		GWLog("Appearance.themeSelectButtonClicked");

		let themeName = /select-theme-([^\s]+)/.exec(event.target.className)[1];
		let save = (Appearance.themeTweakerUI.contains(event.target) == false);
		Appearance.setTheme(themeName, save);
		if (GW.isMobile)
			Appearance.toggleAppearanceAdjustUI();
	},

	/*	The text size adjust (“-”, “A”, “+”) buttons.
	 */
	textSizeAdjustButtonClicked: (event) => {
		GWLog("Appearance.textSizeAdjustButtonClicked");

		var zoomFactor = Appearance.currentTextZoom;
		if (event.target.hasClass("decrease")) {
			zoomFactor -= 0.05;
		} else if (event.target.hasClass("increase")) {
			zoomFactor += 0.05;
		} else {
			zoomFactor = Appearance.defaultTextZoom;
		}

		let save = (   Appearance.textSizeAdjustmentWidget != null 
					&& Appearance.textSizeAdjustmentWidget.contains(event.target));
		Appearance.setTextZoom(zoomFactor, save);
	},

	/*	Theme tweaker toggle button.
	 */
	themeTweakerToggleClicked: (event) => {
		GWLog("Appearance.themeTweakerToggleClicked");

		Appearance.showThemeTweakerUI();
	},

	/***************************/
	/*	Theme tweaker UI events.
	 */

	/*	Key pressed while theme tweaker is open.
	 */
	themeTweakerUIKeyPressed: (event) => {
		GWLog("Appearance.themeTweakerUIKeyPressed");

		if (event.key == "Escape") {
			if (Appearance.themeTweakerUIHelpWindow.style.display != "none") {
				Appearance.toggleThemeTweakerHelpWindow();
				Appearance.themeTweakerResetSettings();
			} else if (Appearance.themeTweakerUI.style.display != "none") {
				Appearance.toggleThemeTweakerUI();
				Appearance.themeTweakReset();
			}
		} else if (event.key == "Enter") {
			if (Appearance.themeTweakerUIHelpWindow.style.display != "none") {
				Appearance.toggleThemeTweakerHelpWindow();
				Appearance.themeTweakerSaveSettings();
			} else if (Appearance.themeTweakerUI.style.display != "none") {
				Appearance.toggleThemeTweakerUI();
				Appearance.themeTweakSave();
			}
		}
	},

	/*	Theme tweaker overlay clicked.
	 */
	themeTweakerUIOverlayClicked: (event) => {
		GWLog("Appearance.themeTweakerUIOverlayClicked");

		if (event.type == "mousedown") {
			Appearance.themeTweakerUI.style.opacity = "0.01";
		} else {
			Appearance.toggleThemeTweakerUI();
			Appearance.themeTweakerUI.style.opacity = "1.0";
			Appearance.themeTweakReset();
		}
	},

	/*	In the theme tweaker, a slider clicked, or released after drag; or a
		checkbox clicked (either in the main theme tweaker UI, or in the help
		window).
	 */
	themeTweakerUIFieldValueChanged: (event) => {
		GWLog("Appearance.themeTweakerUIFieldValueChanged");

		if (event.target.id == "theme-tweak-control-invert") {
			Appearance.currentFilters["invert"] = event.target.checked ? "100%" : "0%";
		} else if (event.target.type == "range") {
			let sliderName = /^theme-tweak-control-(.+)$/.exec(event.target.id)[1];
			Appearance.themeTweakerUI.query("#theme-tweak-label-" + sliderName).innerText = event.target.value + event.target.dataset["labelSuffix"];
			Appearance.currentFilters[sliderName] = event.target.value + event.target.dataset["valueSuffix"];
		} else if (event.target.id == "theme-tweak-control-clippy") {
			Appearance.themeTweakerUIClippyContainer.style.display = event.target.checked ? "block" : "none";
		}

		// Clear the sample text filters.
		Appearance.themeTweakerUISampleTextContainer.style.filter = "";

		// Apply the new filters globally.
		Appearance.applyFilters();
	},

	/*	Theme tweaker slider dragged (live-update event).
	 */
	themeTweakerUIFieldInputReceived: (event) => {
		GWLog("Appearance.themeTweakerUIFieldInputReceived");

		let sampleTextFilters = Appearance.currentFilters;
		let sliderName = /^theme-tweak-control-(.+)$/.exec(event.target.id)[1];
		Appearance.themeTweakerUI.query("#theme-tweak-label-" + sliderName).innerText = event.target.value + event.target.dataset["labelSuffix"];
		sampleTextFilters[sliderName] = event.target.value + event.target.dataset["valueSuffix"];

		Appearance.themeTweakerUISampleTextContainer.style.filter = Appearance.filterStringFromFilters(sampleTextFilters);
	},

	/*	Close button in main theme tweaker UI (title bar).
	 */
	themeTweakerUICloseButtonClicked: (event) => {
		GWLog("Appearance.themeTweakerUICloseButtonClicked");

		Appearance.toggleThemeTweakerUI();
		Appearance.themeTweakReset();
	},

	/*	Minimize button in main theme tweaker UI (title bar).
	 */
	themeTweakerUIMinimizeButtonClicked: (event) => {
		GWLog("Appearance.themeTweakerUIMinimizeButtonClicked");

		Appearance.setThemeTweakerWindowMinimized(event.target.hasClass("minimize"));
	},

	/*	Help (“?”) button in main theme tweaker UI (title bar).
	 */
	themeTweakerUIHelpButtonClicked: (event) => {
		GWLog("Appearance.themeTweakerUIHelpButtonClicked");

		Appearance.themeTweakerUIClippyControl.checked = Appearance.getSavedThemeTweakerClippyState();
		Appearance.toggleThemeTweakerHelpWindow();
	},

	/*	“Reset Defaults” button in main theme tweaker UI.
	 */
	themeTweakerUIResetDefaultsButtonClicked: (event) => {
		GWLog("Appearance.themeTweakerUIResetDefaultsButtonClicked");

		Appearance.themeTweakResetDefaults();
		Appearance.resetThemeTweakerUIDefaultState();
	},

	/*	“Cancel” button in main theme tweaker UI.
	 */
	themeTweakerUICancelButtonClicked: (event) => {
		GWLog("Appearance.themeTweakerUICancelButtonClicked");

		Appearance.toggleThemeTweakerUI();
		Appearance.themeTweakReset();
	},

	/*	“OK” button in main theme tweaker UI.
	 */
	themeTweakerUIOKButtonClicked: (event) => {
		GWLog("Appearance.themeTweakerUIOKButtonClicked");

		Appearance.toggleThemeTweakerUI();
		Appearance.themeTweakSave();
	},

	/*	“Cancel” button in theme tweaker help window.
	 */
	themeTweakerUIHelpWindowCancelButtonClicked: (event) => {
		GWLog("Appearance.themeTweakerUIHelpWindowCancelButtonClicked");

		Appearance.toggleThemeTweakerHelpWindow();
		Appearance.themeTweakerResetSettings();
	},

	/*	“OK” button in theme tweaker help window.
	 */
	themeTweakerUIHelpWindowOKButtonClicked: (event) => {
		GWLog("Appearance.themeTweakerUIHelpWindowOKButtonClicked");

		Appearance.toggleThemeTweakerHelpWindow();
		Appearance.themeTweakerSaveSettings();
	},

	/*	The notch in the theme tweaker sliders (to reset the slider to its
		default value).
	 */
	themeTweakerUISliderNotchClicked: (event) => {
		GWLog("Appearance.themeTweakerUISliderNotchClicked");

		let slider = event.target.parentElement.query("input[type='range']");
		slider.value = slider.dataset["defaultValue"];
		event.target.parentElement.query(".theme-tweak-control-label").innerText = slider.value + slider.dataset["labelSuffix"];
		Appearance.currentFilters[/^theme-tweak-control-(.+)$/.exec(slider.id)[1]] = slider.value + slider.dataset["valueSuffix"];
		Appearance.applyFilters();
	},

	/*	The close button in the “Bobby the Basilisk” help message.
	 */
	themeTweakerUIClippyCloseButtonClicked: (event) => {
		GWLog("Appearance.themeTweakerUIClippyCloseButtonClicked");

		Appearance.themeTweakerUIClippyContainer.style.display = "none";
		Appearance.themeTweakerUIClippyControl.checked = false;
		Appearance.saveThemeTweakerClippyState();
	}
};

function setSearchBoxTabSelectable(selectable) {
	GWLog("setSearchBoxTabSelectable");
	query("input[type='search']").tabIndex = selectable ? "" : "-1";
	query("input[type='search'] + button").tabIndex = selectable ? "" : "-1";
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
		if (GW.scrollState.appearanceAdjustUIToggleButton.hasClass("engaged")) 
			Appearance.toggleAppearanceAdjustUI();
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
		(localStorage.getItem("appearance-adjust-ui-toggle-engaged") != "false")) 
			Appearance.toggleAppearanceAdjustUI();
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
		if(hnsDate) {
			setHistoryLastVisitedDate(hnsDate);
			let newCommentsCount = highlightCommentsSince(hnsDate);
			updateNewCommentNavUI(newCommentsCount);
		}
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
//     let template = newElement("TEMPLATE", { }, { "innerHTML": html.trim() });
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

	if (query(".sublevel-nav") || query("#top-nav-bar")) {
		(query(".sublevel-nav") || query("#top-nav-bar")).insertAdjacentHTML("beforebegin", commentsListModeSelectorHTML);
	} else {
		(query(".page-toolbar") || query(".active-bar")).insertAdjacentHTML("afterend", commentsListModeSelectorHTML);
	}
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
			(author.tagName == "A" && author.hash != "" && query(`${author.hash} .author`).dataset.userid == postAuthor.dataset.userid)) {
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

function activateAntiKibitzer() {
	GWLog("activateAntiKibitzer");

	//	Activate anti-kibitzer mode (if needed).
	if (localStorage.getItem("antikibitzer") == "true")
		toggleAntiKibitzerMode();

	//	Remove temporary CSS that hides the authors and karma values.
	removeElement("#antikibitzer-temp");

	//	Inject controls (if desktop).
	if (GW.isMobile == false)
		injectAntiKibitzerToggle();
}

function injectAntiKibitzerToggle() {
	GWLog("injectAntiKibitzerToggle");

	let antiKibitzerHTML = `<div id="anti-kibitzer-toggle">
		<button type="button" tabindex="-1" accesskey="g" title="Toggle anti-kibitzer (show/hide authors & karma values) [g]"></button>
	</div>`;

	if (GW.isMobile) {
		if (Appearance.themeSelector == null)
			return;

		Appearance.themeSelectorAuxiliaryControlsContainer.insertAdjacentHTML("beforeend", antiKibitzerHTML);
	} else {
		addUIElement(antiKibitzerHTML);	
	}

	//	Activate anti-kibitzer toggle button.
	query("#anti-kibitzer-toggle button").addActivateEvent(GW.antiKibitzerToggleButtonClicked = (event) => {
		GWLog("GW.antiKibitzerToggleButtonClicked");
		if (   query("#anti-kibitzer-toggle").hasClass("engaged")
			&& !event.shiftKey 
			&& !confirm("Are you sure you want to turn OFF the anti-kibitzer?\n\n(This will reveal the authors and karma values of all posts and comments!)")) {
			event.target.blur();
			return;
		}

		toggleAntiKibitzerMode();
		event.target.blur();
	});
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
			karmaValue.innerHTML = karmaValue.dataset["trueValue"];

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

			let userid = author.dataset["userid"] || author.hash && query(`${author.hash} .author`).dataset["userid"];

			if(!userid) return;

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

			karmaValue.dataset["trueValue"] = karmaValue.innerHTML;
			karmaValue.innerHTML = "##<span> points</span>";

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
	try {
		return GW.commentValues[commentOrSelector.id] || (GW.commentValues[commentOrSelector.id] = parseInt(commentOrSelector.query(".karma-value").firstChild.textContent));
	} catch(e) {return null};
}
function commentDate(commentOrSelector) {
	if (typeof commentOrSelector == "string") commentOrSelector = query(commentOrSelector);
	try {
		return GW.commentValues[commentOrSelector.id] || (GW.commentValues[commentOrSelector.id] = parseInt(commentOrSelector.query(".date").dataset.jsDate));
	} catch(e) {return null};
}
function commentVoteCount(commentOrSelector) {
	if (typeof commentOrSelector == "string") commentOrSelector = query(commentOrSelector);
	try {
		return GW.commentValues[commentOrSelector.id] || (GW.commentValues[commentOrSelector.id] = parseInt(commentOrSelector.query(".karma-value").title.split(" ")[0]));
	} catch(e) {return null};
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

function previewPopupsEnabled() {
	let isDisabled = localStorage.getItem("preview-popups-disabled");
	return (typeof(isDisabled) == "string" ? !JSON.parse(isDisabled) : !GW.isMobile);
}

function setPreviewPopupsEnabled(state) {
	localStorage.setItem("preview-popups-disabled", !state);
	updatePreviewPopupToggle();
}

function updatePreviewPopupToggle() {
	let style = (previewPopupsEnabled() ? "--display-slash: none" : "");
	query("#preview-popup-toggle").setAttribute("style", style);
}

function injectPreviewPopupToggle() {
	GWLog("injectPreviewPopupToggle");

	let toggle = addUIElement("<div id='preview-popup-toggle' title='Toggle link preview popups'><svg width=40 height=50 id='popup-svg'></svg>");
	// This is required because Chrome can't use filters on an externally used SVG element.
	fetch(GW.assets["popup.svg"]).then(response => response.text().then(text => { query("#popup-svg").outerHTML = text }))
	updatePreviewPopupToggle();
	toggle.addActivateEvent(event => setPreviewPopupsEnabled(!previewPopupsEnabled()))
}

var currentPreviewPopup = { };

function removePreviewPopup(previewPopup) {
	if(previewPopup.element)
		removeElement(previewPopup.element);

	if(previewPopup.timeout)
		clearTimeout(previewPopup.timeout);

	if(currentPreviewPopup.pointerListener)
		window.removeEventListener("pointermove", previewPopup.pointerListener);

	if(currentPreviewPopup.mouseoutListener)
		document.body.removeEventListener("mouseout", currentPreviewPopup.mouseoutListener);

	if(currentPreviewPopup.scrollListener)
		window.removeEventListener("scroll", previewPopup.scrollListener);

	currentPreviewPopup = { };
}

function addCommentParentPopups() {
	GWLog("addCommentParentPopups");
	//if (!query("#content").hasClass("comment-thread-page")) return;

	queryAll("a[href]").forEach(linkTag => {
		let linkHref = linkTag.getAttribute("href");

		let url;
		try { url = new URL(linkHref, window.location.href); }
		catch(e) { }
		if(!url) return;

		if(GW.sites[url.host]) {
			let linkCommentId = (/\/(?:comment|answer)\/([^\/#]+)$/.exec(url.pathname)||[])[1] || (/#comment-(.+)/.exec(url.hash)||[])[1];
			
			if(url.hash && linkTag.hasClass("comment-parent-link") || linkTag.hasClass("comment-child-link")) {
				linkTag.addEventListener("pointerover", GW.commentParentLinkMouseOver = (event) => {
					if(event.pointerType == "touch") return;
					GWLog("GW.commentParentLinkMouseOver");
					removePreviewPopup(currentPreviewPopup);
					let parentID = linkHref;
					var parent, popup;
					if (!(parent = (query(parentID)||{}).firstChild)) return;
					var highlightClassName;
					if (parent.getBoundingClientRect().bottom < 10 || parent.getBoundingClientRect().top > window.innerHeight + 10) {
						parentHighlightClassName = "comment-item-highlight-faint";
						popup = parent.cloneNode(true);
						popup.addClasses([ "comment-popup", "comment-item-highlight" ]);
						linkTag.addEventListener("mouseout", (event) => {
							removeElement(popup);
						}, {once: true});
						linkTag.closest(".comments > .comment-thread").appendChild(popup);
					} else {
						parentHighlightClassName = "comment-item-highlight";
					}
					parent.parentNode.addClass(parentHighlightClassName);
					linkTag.addEventListener("mouseout", (event) => {
						parent.parentNode.removeClass(parentHighlightClassName);
					}, {once: true});
				});
			}
			else if(url.pathname.match(/^\/(users|posts|events|tag|s|p|explore)\//)
				&& !(url.pathname.match(/^\/(p|explore)\//) && url.hash.match(/^#comment-/)) // Arbital comment links not supported yet.
				&& !(url.searchParams.get('format'))
				&& !linkTag.closest("nav:not(.post-nav-links)")
				&& (!url.hash || linkCommentId)
				&& (!linkCommentId || linkTag.getCommentId() !== linkCommentId)) {
				linkTag.addEventListener("pointerover", event => {
					if(event.buttons != 0 || event.pointerType == "touch" || !previewPopupsEnabled()) return;
					if(currentPreviewPopup.linkTag) return;
					linkTag.createPreviewPopup();
				});
				linkTag.createPreviewPopup = function() {
					removePreviewPopup(currentPreviewPopup);

					currentPreviewPopup = {linkTag: linkTag};
					
					let popup = newElement("IFRAME");
					currentPreviewPopup.element = popup;

					let popupTarget = linkHref;
					if(popupTarget.match(/#comment-/)) {
						popupTarget = popupTarget.replace(/#comment-/, "/comment/");
					}
					// 'theme' attribute is required for proper caching
					popup.setAttribute("src", popupTarget + (popupTarget.match(/\?/) ? '&' : '?') + "format=preview&theme=" + (readCookie('theme') || 'default'));
					popup.addClass("preview-popup");
					
					let linkRect = linkTag.getBoundingClientRect();

					if(linkRect.right + 710 < window.innerWidth)
						popup.style.left = linkRect.right + 10 + "px";
					else
						popup.style.right = "10px";

					popup.style.width = "700px";
					popup.style.height = "500px";
					popup.style.visibility = "hidden";
					popup.style.transition = "none";

					let recenter = function() {
						let popupHeight = 500;
						if(popup.contentDocument && popup.contentDocument.readyState !== "loading") {
							let popupContent = popup.contentDocument.querySelector("#content");
							if(popupContent) {
								popupHeight = popupContent.clientHeight + 2;
								if(popupHeight > (window.innerHeight * 0.875)) popupHeight = window.innerHeight * 0.875;
								popup.style.height = popupHeight + "px";
							}
						}
						popup.style.top = (window.innerHeight - popupHeight) * (linkRect.top / (window.innerHeight - linkRect.height)) + 'px';
					}

					recenter();

					query('#content').insertAdjacentElement("beforeend", popup);

					let clickListener = event => {
						if(!event.target.closest("a, input, label")
						   && !event.target.closest("popup-hide-button")) {
							window.location = linkHref;
						}
					};

					popup.addEventListener("load", () => {
						let hideButton = newElement("DIV", {
							"class": "popup-hide-button"
						}, {
							"innerHTML": "&#xf070;"
						});
						hideButton.onclick = (event) => {
							removePreviewPopup(currentPreviewPopup);
							setPreviewPopupsEnabled(false);
							event.stopPropagation();
						}
						popup.contentDocument.body.appendChild(hideButton);
						
						let popupBody = popup.contentDocument.body;
						popupBody.addEventListener("click", clickListener);
						popupBody.style.cursor = "pointer";

						recenter();
					});

					popup.contentDocument.body.addEventListener("click", clickListener);
					
					currentPreviewPopup.timeout = setTimeout(() => {
						recenter();

						requestIdleCallback(() => {
							if(currentPreviewPopup.element === popup) {
								popup.scrolling = "";
								popup.style.visibility = "unset";
								popup.style.transition = null;

								popup.animate([
									{ opacity: 0, transform: "translateY(10%)" },
									{ opacity: 1, transform: "none" }
								], { duration: 150, easing: "ease-out" });
							}
						});
					}, 1000);

					let pointerX, pointerY, mousePauseTimeout = null;

					currentPreviewPopup.pointerListener = (event) => {
						pointerX = event.clientX;
						pointerY = event.clientY;

						if(mousePauseTimeout) clearTimeout(mousePauseTimeout);
						mousePauseTimeout = null;

						let overElement = document.elementFromPoint(pointerX, pointerY);
						let mouseIsOverLink = linkRect.isInside(pointerX, pointerY);

						if(mouseIsOverLink || overElement === popup
						   || (pointerX < popup.getBoundingClientRect().left
						       && event.movementX >= 0)) {
							if(!mouseIsOverLink && overElement !== popup) {
								if(overElement['createPreviewPopup']) {
									mousePauseTimeout = setTimeout(overElement.createPreviewPopup, 150);
								} else {
									mousePauseTimeout = setTimeout(() => removePreviewPopup(currentPreviewPopup), 500);
								}
							}
						} else {
							removePreviewPopup(currentPreviewPopup);
							if(overElement['createPreviewPopup']) overElement.createPreviewPopup();
						}
					};
					window.addEventListener("pointermove", currentPreviewPopup.pointerListener);

					currentPreviewPopup.mouseoutListener = (event) => {
						clearTimeout(mousePauseTimeout);
						mousePauseTimeout = null;
					}
					document.body.addEventListener("mouseout", currentPreviewPopup.mouseoutListener);

					currentPreviewPopup.scrollListener = (event) => {
						let overElement = document.elementFromPoint(pointerX, pointerY);
						linkRect = linkTag.getBoundingClientRect();
						if(linkRect.isInside(pointerX, pointerY) || overElement === popup) return;
						removePreviewPopup(currentPreviewPopup);
					};
					window.addEventListener("scroll", currentPreviewPopup.scrollListener, {passive: true});
				};
			}
		}
	});
	queryAll(".comment-meta a.comment-parent-link, .comment-meta a.comment-child-link").forEach(commentParentLink => {
		
	});

	// Due to filters vs. fixed elements, we need to be smarter about selecting which elements to filter...
	Appearance.filtersExclusionPaths.commentParentPopups = [
		"#content .comments .comment-thread"
	];
	Appearance.applyFilters();
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
		history.replaceState(window.history.state, null, "#if_slide_" + (indexOfFocusedImage + 1));
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
		history.replaceState(window.history.state, null, "#");
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
	history.replaceState(window.history.state, null, "#if_slide_" + (indexOfFocusedImage + 1));
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
	let keyboardHelpOverlay = addUIElement("<nav id='keyboard-help-overlay'>" + `
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
				[ [ 'ak-v' ], "Go to Tags view"],
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
				[ [ 'ak-`&nbsp;' ], "Toggle compact comment list view" ],
				[ [ 'ak-g' ], "Toggle anti-kibitzer" ]
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
	` + "</nav>");

	// Add listener to show the keyboard help overlay.
	document.addEventListener("keypress", GW.keyboardHelpShowKeyPressed = (event) => {
		GWLog("GW.keyboardHelpShowKeyPressed");
		if (event.key == '?')
			toggleKeyboardHelpOverlay(true);
	});

	// Clicking the background overlay closes the keyboard help overlay.
	keyboardHelpOverlay.addActivateEvent(GW.keyboardHelpOverlayClicked = (event) => {
		GWLog("GW.keyboardHelpOverlayClicked");
		if (event.type == "mousedown") {
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

/**********************/
/* PUSH NOTIFICATIONS */
/**********************/

function pushNotificationsSetup() {
	let pushNotificationsButton = query("#enable-push-notifications");
	if(pushNotificationsButton && (pushNotificationsButton.dataset.enabled || (navigator.serviceWorker && window.Notification && window.PushManager))) {
		pushNotificationsButton.onclick = pushNotificationsButtonClicked;
		pushNotificationsButton.style.display = 'unset';
	}
}

function urlBase64ToUint8Array(base64String) {
	const padding = '='.repeat((4 - base64String.length % 4) % 4);
	const base64 = (base64String + padding)
	      .replace(/-/g, '+')
	      .replace(/_/g, '/');
	
	const rawData = window.atob(base64);
	const outputArray = new Uint8Array(rawData.length);
	
	for (let i = 0; i < rawData.length; ++i) {
		outputArray[i] = rawData.charCodeAt(i);
	}
	return outputArray;
}

function pushNotificationsButtonClicked(event) {
	event.target.style.opacity = 0.33;
	event.target.style.pointerEvents = "none";

	let reEnable = (message) => {
		if(message) alert(message);
		event.target.style.opacity = 1;
		event.target.style.pointerEvents = "unset";
	}

	if(event.target.dataset.enabled) {
		fetch('/push/register', {
			method: 'post',
			headers: { 'Content-type': 'application/json' },
			body: JSON.stringify({
				cancel: true
			}),
		}).then(() => {
			event.target.innerHTML = "Enable push notifications";
			event.target.dataset.enabled = "";
			reEnable();
		}).catch((err) => reEnable(err.message));
	} else {
		Notification.requestPermission().then((permission) => {
			navigator.serviceWorker.ready
				.then((registration) => {
					return registration.pushManager.getSubscription()
						.then(async function(subscription) {
							if (subscription) {
								return subscription;
							}
							return registration.pushManager.subscribe({
								userVisibleOnly: true,
								applicationServerKey: urlBase64ToUint8Array(applicationServerKey)
							});
						})
						.catch((err) => reEnable(err.message));
				})
				.then((subscription) => {
					fetch('/push/register', {
						method: 'post',
						headers: {
							'Content-type': 'application/json'
						},
						body: JSON.stringify({
							subscription: subscription
						}),
					});
				})
				.then(() => {
					event.target.innerHTML = "Disable push notifications";
					event.target.dataset.enabled = "true";
					reEnable();
				})
				.catch(function(err){ reEnable(err.message) });
			
		});
	}
}

/*******************************/
/* HTML TO MARKDOWN CONVERSION */
/*******************************/

function MarkdownFromHTML(text, linePrefix) {
	GWLog("MarkdownFromHTML");
	console.log(text);

	let docFrag = document.createRange().createContextualFragment(text);
	let output = "";
	let owedLines = -1;
	linePrefix = linePrefix || "";

	let out = text => {
		if(owedLines > 0) {
			output += ("\n" + linePrefix).repeat(owedLines);
		}
		output += text;
		owedLines = 0;
	}
	let forceLine = n => {
		n = n || 1;
		out(("\n" + linePrefix).repeat(n));
	}
	let newLine = (n) => {
		n = n || 1;
		if(owedLines >= 0 && owedLines < n) {
			owedLines = n;
		}
	};
	let newParagraph = () => {
		newLine(2);
	};
	let withPrefix = (prefix, fn) => {
		let oldPrefix = linePrefix;
		linePrefix += prefix;
		owedLines = -1;
		fn();
		owedLines = 0;
		linePrefix = oldPrefix;
	};

	let doConversion = (node) => {
		if(node.nodeType == Node.TEXT_NODE) {
			let lines = node.nodeValue.split(/\r|\n/m);
			for(text of lines.slice(0, -1)) {
				out(text);
				newLine();
			}
			out(lines.at(-1));
		} else if(node.nodeType == Node.ELEMENT_NODE) {
			switch(node.tagName) {
			case "P":
			case "DIV":
			case "UL":
			case "OL":
				newParagraph();
				node.childNodes.forEach(doConversion);
				newParagraph();
				break;
			case "BR":
				forceLine();
				break;
			case "HR":
				newLine();
				out("---");
				newLine();
				break;
			case "B":
			case "STRONG":
				out("**");
				node.childNodes.forEach(doConversion);
				out("**");
				break;
			case "I":
			case "EM":
				out("*");
				node.childNodes.forEach(doConversion);
				out("*");
				break;
			case "LI":
				newLine();
				let listPrefix;
				if(node.parentElement.tagName == "OL") {
					let i = 1;
					for(let e = node; e = e.previousElementSibling;) { i++ }
					listPrefix = "" + i + ". ";
				} else {
					listPrefix = "* ";
				}
				out(listPrefix);
				owedLines = -1;
				withPrefix(" ".repeat(listPrefix.length), () => node.childNodes.forEach(doConversion));
				newLine();
				break;
			case "H1":
			case "H2":
			case "H3":
			case "H4":
			case "H5":
			case "H6":
				newParagraph();
				out("#".repeat(node.tagName.charAt(1)) + " ");
				node.childNodes.forEach(doConversion);
				newParagraph();
				break;
			case "A":
				let href = node.href;
				out('[');
				node.childNodes.forEach(doConversion);
				out(`](${href})`);
				break;
			case "IMG":
				let src = node.src;
				let alt = node.alt || "";
				out(`![${alt}](${src})`);
				break;
			case "BLOCKQUOTE":
				newParagraph();
				out("> ");
				withPrefix("> ", () => node.childNodes.forEach(doConversion));
				newParagraph();
				break;
			case "PRE":
				newParagraph();
				out('```');
				forceLine();
				out(node.innerText);
				forceLine();
				out('```');
				newParagraph();
				break;
			case "CODE":
				out('`');
				node.childNodes.forEach(doConversion);
				out('`');
				break;
			default:
				node.childNodes.forEach(doConversion);
			}
		} else {
			node.childNodes.forEach(doConversion);
		}
	}
	doConversion(docFrag);

	return output;
}

/************************************/
/* ANCHOR LINK SCROLLING WORKAROUND */
/************************************/

addTriggerListener('navBarLoaded', {priority: -1, fn: () => {
	let hash = location.hash;
	if(hash && hash !== "#top" && !document.query(hash)) {
		let content = document.query("#content");
		content.style.display = "none";
		addTriggerListener("DOMReady", {priority: -1, fn: () => {
			content.style.visibility = "hidden";
			content.style.display = null;
			requestIdleCallback(() => {content.style.visibility = null}, {timeout: 500});
		}});
	}
}});

/******************/
/* INITIALIZATION */
/******************/

addTriggerListener('navBarLoaded', {priority: 3000, fn: function () {
	GWLog("INITIALIZER earlyInitialize");
	// Check to see whether we're on a mobile device (which we define as a narrow screen)
	GW.isMobile = (window.innerWidth <= 1160);
	GW.isFirefox = navigator.userAgent.toLowerCase().indexOf('firefox') > -1;

	// Backward compatibility
	let storedTheme = localStorage.getItem("selected-theme");
	if (storedTheme) {
		Appearance.setTheme(storedTheme);
		localStorage.removeItem("selected-theme");
	}

	// Animate width & theme adjustments?
	Appearance.adjustmentTransitions = false;
	// Add the content width selector.
	Appearance.injectContentWidthSelector();
	// Add the text size adjustment widget.
	Appearance.injectTextSizeAdjustmentUI();
	// Add the theme selector.
	Appearance.injectThemeSelector();
	// Add the theme tweaker.
	Appearance.injectThemeTweaker();

	// Add the dark mode selector (if desktop).
	if (GW.isMobile == false)
		DarkMode.injectModeSelector();

	// Add the quick-nav UI.
	injectQuickNavUI();

	// Finish initializing when ready.
	addTriggerListener('DOMReady', {priority: 100, fn: mainInitializer});
}});

function mainInitializer() {
	GWLog("INITIALIZER initialize");

	// This is for "qualified hyperlinking", i.e. "link without comments" and/or
	// "link without nav bars".
	if (getQueryVariable("hide-nav-bars") == "true") {
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

	// On edit post pages and conversation pages, add GUIEdit buttons to the 
	// textarea and expand it.
	queryAll(".with-markdown-editor textarea").forEach(textarea => {
		textarea.addTextareaFeatures();
		expandTextarea(textarea);
	});
	// Focus the textarea.
	queryAll(((getQueryVariable("post-id")) ? "#edit-post-form textarea" : "#edit-post-form input[name='title']") + (GW.isMobile ? "" : ", .conversation-page textarea")).forEach(field => { field.focus(); });

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
		insertHeadHTML(`<style> .comment-minimize-button::after { display: none; } </style>`);
	}

	// On mobile, replace the labels for the checkboxes on the edit post form
	// with icons, to save space.
	if (GW.isMobile && query(".edit-post-page")) {
		query("label[for='link-post']").innerHTML = "&#xf0c1";
		query("label[for='question']").innerHTML = "&#xf128";
	}

	// Add error message (as placeholder) if user tries to click Search with
	// an empty search field.
	searchForm: {
		let searchForm = query("#nav-item-search form");
		if(!searchForm) break searchForm;
		searchForm.addEventListener("submit", GW.siteSearchFormSubmitted = (event) => {
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
	}

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
	if (GW.useFancyFeatures) injectCommentsSortModeSelector();

	// Add the toggle for the post nav UI elements on mobile.
	if (GW.isMobile) injectPostNavUIToggle();

	// Add the toggle for the appearance adjustment UI elements on mobile.
	if (GW.isMobile)
		Appearance.injectAppearanceAdjustUIToggle();

	// Activate the antikibitzer.
	if (GW.useFancyFeatures)
		activateAntiKibitzer();

	// Add comment parent popups.
	injectPreviewPopupToggle();
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
	document.addEventListener("keyup", Appearance.themeTweakerUIKeyPressed);

	// Add event listener for . , ; (for navigating listings pages).
	let listings = queryAll("h1.listing a[href^='/posts'], #content > .comment-thread .comment-meta a.date");
	if (!query(".comments") && listings.length > 0) {
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
		insertHeadHTML(`<style id="mathjax-styles"> ${aggregatedStyles} </style>`);
	}

	/*  Makes double-clicking on a math element select the entire math element.
		(This actually makes no difference to the behavior of the copy listener
		 which copies the entire LaTeX source of the full equation no matter how 
		 much of said equation is selected when the copy command is sent; 
		 however, it ensures that the UI communicates the actual behavior in a 
		 more accurate and understandable way.)
	 */
	query("#content").querySelectorAll(".mjpage").forEach(mathBlock => {
		mathBlock.addEventListener("dblclick", (event) => {
			document.getSelection().selectAllChildren(mathBlock.querySelector(".mjx-chtml"));
		});
		mathBlock.title = mathBlock.classList.contains("mjpage__block")
						  ? "Double-click to select equation, then copy, to get LaTeX source"
						  : "Double-click to select equation; copy to get LaTeX source";
	});

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

	// Set up Image Focus feature.
	imageFocusSetup();

	// Set up keyboard shortcuts guide overlay.
	keyboardHelpSetup();

	// Show push notifications button if supported
	pushNotificationsSetup();

	// Show elements now that javascript is ready.
	removeElement("#hide-until-init");

	activateTrigger("pageLayoutFinished");
}

/*************************/
/* POST-LOAD ADJUSTMENTS */
/*************************/

window.addEventListener("pageshow", badgePostsWithNewComments);

addTriggerListener('pageLayoutFinished', {priority: 100, fn: function () {
	GWLog("INITIALIZER pageLayoutFinished");

	Appearance.postSetThemeHousekeeping();

	focusImageSpecifiedByURL();

	// FOR TESTING ONLY, COMMENT WHEN DEPLOYING.
// 	query("input[type='search']").value = GW.isMobile;
// 	insertHeadHTML(`<style>
// 		@media only screen and (hover:none) { #nav-item-search input { background-color: red; }}
// 		@media only screen and (hover:hover) { #nav-item-search input { background-color: LightGreen; }}
// 	</style>`);
}});

function generateImagesOverlay() {
	GWLog("generateImagesOverlay");
	// Don’t do this on the about page.
	if (query(".about-page") != null) return;
	return;

	// Remove existing, if any.
	removeElement("#images-overlay");

	// Create new.
	document.body.insertAdjacentHTML("afterbegin", "<div id='images-overlay'></div>");
	let imagesOverlay = query("#images-overlay");
	let imagesOverlayLeftOffset = imagesOverlay.getBoundingClientRect().left;
	queryAll(".post-body img").forEach(image => {
		let clonedImageContainer = newElement("DIV");

		let clonedImage = image.cloneNode(true);
		clonedImage.style.borderStyle = getComputedStyle(image).borderStyle;
		clonedImage.style.borderColor = getComputedStyle(image).borderColor;
		clonedImage.style.borderWidth = Math.round(parseFloat(getComputedStyle(image).borderWidth)) + "px";
		clonedImageContainer.appendChild(clonedImage);

		let zoomLevel = Appearance.currentTextZoom;

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
	document.execCommand("insertText", false, str);

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
	[ 'formula', 'LaTeX [alt+4]', '', '$', '$', 'LaTeX formula', '&#xf155;' ],
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

/******************/
/* SERVICE WORKER */
/******************/

if(navigator.serviceWorker) {
	navigator.serviceWorker.register('/service-worker.js');
	setCookie("push", "t");
}

/*********************/
/* USER AUTOCOMPLETE */
/*********************/

function zLowerUIElements() {
	let uiElementsContainer = query("#ui-elements-container");
	if (uiElementsContainer)
		uiElementsContainer.style.zIndex = "1";
}

function zRaiseUIElements() {
	let uiElementsContainer = query("#ui-elements-container");
	if (uiElementsContainer)
		uiElementsContainer.style.zIndex = "";
}

var userAutocomplete = null;

function abbreviatedInterval(date) {
	let seconds = Math.floor((new Date() - date) / 1000);
	let days = Math.floor(seconds / (60 * 60 * 24));
	let years = Math.floor(days / 365);
	if (years)
		return years + "y";
	else if (days)
		return days + "d";
	else
		return "today";
}

function beginAutocompletion(control, startIndex, endIndex) {
	if(userAutocomplete) abortAutocompletion(userAutocomplete);

	let complete = { control: control,
			 abortController: new AbortController(),
			 fetchAbortController: new AbortController(),
			 container: document.createElement("div") };

	endIndex = endIndex || control.selectionEnd;
	let valueLength = control.value.length;

	complete.container.className = "autocomplete-container "
								 + "right "
								 + (window.innerWidth > 1280
								 	? "outside"
								 	: "inside");
	control.insertAdjacentElement("afterend", complete.container);
	zLowerUIElements();

	let makeReplacer = (userSlug, displayName) => {
		return () => {
			let replacement = '[@' + displayName + '](/users/' + userSlug + '?mention=user)';
			control.value = control.value.substring(0, startIndex - 1) +
				replacement +
				control.value.substring(endIndex);
			abortAutocompletion(complete);
			complete.control.selectionStart = complete.control.selectionEnd = startIndex + -1 + replacement.length;
			complete.control.focus();
		};
	};

	let switchHighlight = (newHighlight) => {
		if (!newHighlight)
			return;

		complete.highlighted.removeClass("highlighted");
		newHighlight.addClass("highlighted");
		complete.highlighted = newHighlight;

		//	Scroll newly highlighted item into view, if need be.
		if (  complete.highlighted.offsetTop + complete.highlighted.offsetHeight 
			> complete.container.scrollTop + complete.container.clientHeight) {
			complete.container.scrollTo(0, complete.highlighted.offsetTop + complete.highlighted.offsetHeight - complete.container.clientHeight);
		} else if (complete.highlighted.offsetTop < complete.container.scrollTop) {
			complete.container.scrollTo(0, complete.highlighted.offsetTop);
		}
	};
	let highlightNext = () => {
		switchHighlight(complete.highlighted.nextElementSibling ?? complete.container.firstElementChild);
	};
	let highlightPrev = () => {
		switchHighlight(complete.highlighted.previousElementSibling ?? complete.container.lastElementChild);
	};

	let updateCompletions = () => {
		let fragment = control.value.substring(startIndex, endIndex);

		fetch("/-user-autocomplete?" + urlEncodeQuery({q: fragment}),
		      {signal: complete.fetchAbortController.signal})
			.then((res) => res.json())
			.then((res) => {
				if(res.error) return;
				if(res.length == 0) return abortAutocompletion(complete);

				complete.container.innerHTML = "";
				res.forEach(entry => {
					let entryContainer = document.createElement("div");
					[ [ entry.displayName, "name" ],
					  [ abbreviatedInterval(Date.parse(entry.createdAt)), "age" ],
					  [ (entry.karma || 0) + " karma", "karma" ]
					].forEach(x => {
						let e = document.createElement("span");
						e.append(x[0]);
						e.className = x[1];
						entryContainer.append(e);
					});
					entryContainer.onclick = makeReplacer(entry.slug, entry.displayName);
					complete.container.append(entryContainer);
				});
				complete.highlighted = complete.container.children[0];
				complete.highlighted.classList.add("highlighted");
				complete.container.scrollTo(0, 0);
				})
			.catch((e) => {});
	};

	document.body.addEventListener("click", (event) => {
		if (!complete.container.contains(event.target)) {
			abortAutocompletion(complete);
			event.preventDefault();
			event.stopPropagation();
		}
	}, {signal: complete.abortController.signal,
	    capture: true});
	
	control.addEventListener("keydown", (event) => {
		switch (event.key) {
		case "Escape":
			abortAutocompletion(complete);
			event.preventDefault();
			return;
		case "ArrowUp":
			highlightPrev();
			event.preventDefault();
			return;
		case "ArrowDown":
			highlightNext();
			event.preventDefault();
			return;
		case "Tab":
			if (event.shiftKey)
				highlightPrev();
			else
				highlightNext();
			event.preventDefault();
			return;
		case "Enter":
			complete.highlighted.onclick();
			event.preventDefault();
			return;
		}
	}, {signal: complete.abortController.signal});

	control.addEventListener("selectionchange", (event) => {
		if (control.selectionStart < startIndex ||
		    control.selectionEnd > endIndex) {
			abortAutocompletion(complete);
		}
	}, {signal: complete.abortController.signal});
	
	control.addEventListener("input", (event) => {
		complete.fetchAbortController.abort();
		complete.fetchAbortController = new AbortController();

		endIndex += control.value.length - valueLength;
		valueLength = control.value.length;

		if (endIndex < startIndex) {
			abortAutocompletion(complete);
			return;
		}
		
		updateCompletions();
	}, {signal: complete.abortController.signal});

	userAutocomplete = complete;

	if(startIndex != endIndex) updateCompletions();
}

function abortAutocompletion(complete) {
	complete.fetchAbortController.abort();
	complete.abortController.abort();
	complete.container.remove();
	userAutocomplete = null;
	zRaiseUIElements();
}
