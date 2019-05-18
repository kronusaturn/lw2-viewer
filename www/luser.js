/****************/
/* MORE HELPERS */
/****************/

function doAjax(params) {
	let req = new XMLHttpRequest();
	req.addEventListener("load", (event) => {
		if (event.target.status < 400) {
			if (params["onSuccess"]) params.onSuccess();
		} else {
			if (params["onFailure"]) params.onFailure();
		}
	});
	req.open((params["method"] || "GET"), (params.location || document.location));
	if (params["method"] == "POST") {
		req.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");
		params["params"]["csrf-token"] = GW.csrfToken;
		req.send(params.params.keys().map((x) => { return "" + x + "=" + encodeURIComponent(params.params[x]); }).join("&"));
	} else {
		req.send();
	}
}

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

	let checkbox = query("#markdown-hints-checkbox");
	checkbox.checked = !checkbox.checked;
}
function hideMarkdownHintsBox() {
	GWLog("hideMarkdownHintsBox");

	let checkbox = query("#markdown-hints-checkbox");
	checkbox.checked = false;
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

		if (GW.mediaQueries.mobileNarrow.matches) {
			// Remove markdown hints.
			hideMarkdownHintsBox();
			query(".guiedit-mobile-help-button").removeClass("active");
		}

		// Expand textarea if needed.
		expandTextarea(textarea);
	}, false);
	textarea.addEventListener("keyup", (event) => { event.stopPropagation(); });
	textarea.addEventListener("keypress", (event) => { event.stopPropagation(); });

	textarea.insertAdjacentHTML("beforebegin", "<div class='guiedit-buttons-container'></div>");
	let textareaContainer = textarea.closest(".textarea-container");
	var buttons_container = textareaContainer.query(".guiedit-buttons-container");
	for (var buttonSpec of GW.guiEditButtons) {
		// Get button specification.
		let [ name, desc, accesskey, m_before_or_func, m_after, placeholder, icon ] = buttonSpec;

		// Construct button.
		buttons_container.insertAdjacentHTML("beforeend", 
			`<button type='button' class='guiedit guiedit-${name}' tabindex='-1' title='${desc}' data-tooltip='${desc}'>` + 
				`<div>${icon}</div>` + 
			`</button>`
		);
		let button = buttons_container.lastElementChild;

		// Set accesskey (if specified).
		if (accesskey != "") {
			button.accessKey = accesskey;
			button.title += ` [${accesskey}]`;
			button.dataset.tooltip += ` [${accesskey}]`;
		}

		// Add click event handler.
		button.addActivateEvent(GW.GUIEditButtonClicked = (event) => {
			insertMarkup(event, m_before_or_func, m_after, placeholder);
		});
	}

	// Inject markdown hints box (hidden unless user clicks to show).
	var markdown_hints = 
	`<input type='checkbox' id='markdown-hints-checkbox' tabindex='-1'>
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

	// Hide markdown hints box when transitioning to/from narrow-mobile.
	doWhenMatchMedia(GW.mediaQueries.mobileNarrow, "hideMarkdownHintsBox", () => {
		hideMarkdownHintsBox();
		query(".guiedit-mobile-help-button").removeClass("active");
	});

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

	/*	On smartphone (narrow mobile) screens, when a textarea is focused (and
		automatically fullscreened), remove all the filters from the page, and 
		then apply them *just* to the fixed editor UI elements. This is in order
		to get around the “children of elements with a filter applied cannot be
		fixed” issue.
		*/
	let fixedEditorElements = textareaContainer.queryAll("#content textarea, .guiedit-buttons-container, .guiedit-mobile-auxiliary-button, #markdown-hints");
	doWhenMatchMedia(GW.mediaQueries.mobileNarrow, "mobileEditorFilterBugFix", () => {
		textarea.addEventListener("focus", GW.textareaFocusedMobile = (event) => {
			GWLog("GW.textareaFocusedMobile");

			if (!textarea.hasClass("full-screen")) {
				GW.savedFilters = GW.currentFilters;
				GW.currentFilters = { };
				applyFilters(GW.currentFilters);
				fixedEditorElements.forEach(element => {
					element.style.filter = filterStringFromFilters(GW.savedFilters);
				});

				textarea.addClass("full-screen");
			}
		});
		query(".guiedit-mobile-exit-button").addEventListener("blur", GW.textareaClosedMobile = (event) => {
			GWLog("GW.textareaClosedMobile");

			GW.currentFilters = GW.savedFilters;
			GW.savedFilters = { };
			requestAnimationFrame(() => {
				applyFilters(GW.currentFilters);
				fixedEditorElements.forEach(element => {
					element.style.filter = filterStringFromFilters(GW.savedFilters);
				});
			});

			textarea.removeClass("full-screen");
		})
	}, () => {
		textarea.removeClass("full-screen");
		textarea.removeEventListener("focus", GW.textareaFocusedMobile);
		query(".guiedit-mobile-exit-button").removeEventListener("blur", GW.textareaClosedMobile);
		requestAnimationFrame(() => {
			if (GW.savedFilters) GW.currentFilters = GW.savedFilters;
			applyFilters(GW.currentFilters);
			fixedEditorElements.forEach(element => {
				element.style.filter = "";
			});
		});
	});
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

	commentControls.addClass("active");

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

	Æ(commentControls.closest(".comment-item")).addClass("open-for-editing");
}

Element.prototype.updateCommentControlButton = function() {
	let retractFn = () => {
		if (this.closest(".comment-item").firstChild.hasClass("retracted"))
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
		replyButton.setAttribute("accesskey", (commentType == "comment" ? "n" : "w"));
		replyButton.setAttribute("title", "Post new " + commentType + (commentType == "comment" ? " [n]" : ""));
	} else {
		if (commentControls.parentElement.query(".comment-body").hasAttribute("data-markdown-source")) {
			let buttonsList = [];
			if (!commentControls.parentElement.query(".comment-thread"))
				buttonsList.push("delete-button");
			buttonsList.push("retract-button", "edit-button");
			buttonsList.forEach(buttonClass => {
				let button = commentControls.appendChild(document.createElement("button"));
				button.addClass(buttonClass);
				button.updateCommentControlButton();
			});
		}
		replyButton.className = "reply-button action-button";
		replyButton.innerHTML = commentControls.parentElement.hasClass("answer-item") ? "Add comment" : "Reply";
		replyButton.dataset.label = commentControls.parentElement.hasClass("answer-item") ? "Add comment" : "Reply";
	}
	commentControls.appendChild(replyButton);
	replyButton.tabIndex = '-1';

	// On mobile, hide labels for all but the Reply button.
	doWhenMatchMedia(GW.mediaQueries.mobileNarrow, "mobileActionButtonLabels", (mediaQuery) => {
		commentControls.queryAll(".delete-button, .retract-button, .unretract-button, .edit-button").forEach(button => {
			button.innerHTML = mediaQuery.matches ? "" : button.dataset.label;
		});
	});

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
		queryAll("#content textarea").forEach(textarea => {
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
	Æ(Æ(commentControls.closest(".comment-item")).query(".comment-body")).style.display = "";

	let enteredText = commentControls.query("textarea").value;
	if (enteredText) commentControls.dataset.enteredText = enteredText;

	disableBeforeUnload();
	commentControls.constructCommentControls();

	commentControls.removeClass("active");

	Æ(commentControls.closest(".comment-item")).removeClass("open-for-editing");
}

function expandTextarea(textarea) {
	GWLog("expandTextarea");

	if (GW.mediaQueries.mobileNarrow.matches) {
		textarea.style.height = "";
		return;
	}

	let totalBorderHeight = 30;
	if (textarea.clientHeight == textarea.scrollHeight + totalBorderHeight) return;

	requestAnimationFrame(() => {
		textarea.style.height = 'auto';
		textarea.style.height = textarea.scrollHeight + totalBorderHeight + 'px';
		textarea.closest("form").scrollIntoViewIfNeeded();
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
			if (fn) fn();
			if (action != "delete")
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
			let commentItem = target.closest(".comment-item");
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

/**************************/
/* PROMPT TO SAVE CHANGES */
/**************************/

function enableBeforeUnload() {
	window.onbeforeunload = function () { return true; };
}
function disableBeforeUnload() {
	window.onbeforeunload = null;
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
		default:
			return match;
		}
	});

	// <div> and <span>.
	text = text.replace(/<div.+?>(.+?)<\/div>/g, (match, text, offset, string) => {
		return `${text}\n`;
	}).replace(/<span.+?>(.+?)<\/span>/g, (match, text, offset, string) => {
		return `${text}\n`;
	});

	// Unordered lists.
	text = text.replace(/<ul>\s+?((?:.|\n)+?)\s+?<\/ul>/g, (match, listItems, offset, string) => {
		return listItems.replace(/<li>((?:.|\n)+?)<\/li>/g, (match, listItem, offset, string) => {
			return `* ${listItem}\n`;
		});
	});

	// Ordered lists.
	text = text.replace(/<ol.+?(?:\sstart=["']([0-9]+)["'])?.+?>\s+?((?:.|\n)+?)\s+?<\/ol>/g, (match, start, listItems, offset, string) => {
		var countedItemValue = 0;
		return listItems.replace(/<li(?:\svalue=["']([0-9]+)["'])?>((?:.|\n)+?)<\/li>/g, (match, specifiedItemValue, listItem, offset, string) => {
			var itemValue;
			if (typeof specifiedItemValue != "undefined") {
				specifiedItemValue = parseInt(specifiedItemValue);
				countedItemValue = itemValue = specifiedItemValue;
			} else {
				itemValue = (start ? parseInt(start) - 1 : 0) + ++countedItemValue;
			}
			return `${itemValue}. ${listItem.trim()}\n`;
		});
	});

	// Headings.
	text = text.replace(/<h([1-9]).+?>(.+?)<\/h[1-9]>/g, (match, level, headingText, offset, string) => {
		return { "1":"#", "2":"##", "3":"###" }[level] + " " + headingText + "\n";
	});

	// Blockquotes.
	text = text.replace(/<blockquote>((?:.|\n)+?)<\/blockquote>/g, (match, quotedText, offset, string) => {
		return `> ${quotedText.trim().split("\n").join("\n> ")}\n`;
	});

	// Links.
	text = text.replace(/<a.+?href="(.+?)">(.+?)<\/a>/g, (match, href, text, offset, string) => {
		return `[${text}](${href})`;
	}).trim();

	// Images.
	text = text.replace(/<img.+?src="(.+?)".+?\/>/g, (match, src, offset, string) => {
		return `![](${src})`;
	});

	// Horizontal rules.
	text = text.replace(/<hr(.+?)\/?>/g, (match, offset, string) => {
		return "\n---\n";
	});

	// Line breaks.
	text = text.replace(/<br\s?\/?>/g, (match, offset, string) => {
		return "\\\n";
	});

	// Preformatted text (possibly with a code block inside).
	text = text.replace(/<pre>(?:\s*<code>)?((?:.|\n)+?)(?:<\/code>\s*)?<\/pre>/g, (match, text, offset, string) => {
		return "```\n" + text + "\n```";
	});

	// Code blocks.
	text = text.replace(/<code>(.+?)<\/code>/g, (match, text, offset, string) => {
		return "`" + text + "`";
	});

	// HTML entities.
	text = text.replace(/&(.+?);/g, (match, entity, offset, string) => {
		switch(entity) {
		case "gt":
			return ">";
		case "lt":
			return "<";
		case "amp":
			return "&";
		case "apos":
			return "'";
		case "quot":
			return "\"";
		default:
			return match;
		}
	});

	return text;
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
	str = func ? func(str, p0, textarea.value) : (mopen + str + mclose);

	// Determine selection.
	var opt = { };
	if (!func) {
		cur0 += (p0 == p1) ? mopen.length : str.length;
		cur1 = (p0 == p1) ? (cur0 + mtext.length) : cur0;
	} else {
		cur0 = str[1];
		cur1 = str[2];
		if (str.length > 3) opt = str[3];
		str = str[0];
	}

	// Update textarea contents.
	insertText(str, textarea, p0, p1);
	if (opt && opt.where && opt.text) {
		switch (opt.where) {
		case "start":
			insertText(opt.text, textarea, 0, 0);
			break;
		case "end":
			insertText(opt.text, textarea, textarea.value.length, textarea.value.length);
			break;
		default:
			break;
		}
		if (opt.cur_pos) {
			switch (opt.cur_pos) {
			case "text-start":
				cur0 = cur1 = 0;
				break;
			case "text-end":
				cur0 = cur1 = textarea.value.length;
				break;
			default:
				cur0 = ((opt.where == "start") ? 0 : (textarea.value.length - opt.text.length)) + opt.cur_pos[0];
				cur1 = cur0 + opt.cur_pos[1];
				break;
			}
		}
	}
	// Expand textarea, if needed.
	expandTextarea(textarea);

	// Set selection.
	textarea.selectionStart = cur0;
	textarea.selectionEnd = cur1;

	return;
}

function insertText(text, textarea, p0, p1) {
	// The document.execCommand API is broken in Firefox 
	// ( https://bugzilla.mozilla.org/show_bug.cgi?id=1220696 ), but using it
	// allows native undo/redo to work; so we enable it in other browsers.
	if (GW.isFirefox) {
		textarea.value = textarea.value.substring(0, p0) + text + textarea.value.substring(p1);
	} else {
		textarea.selectionStart = p0;
		textarea.selectionEnd = p1;
		document.execCommand("insertText", false, text);
	}
}

GW.guiEditButtons = [
	[ 'strong', 'Strong (bold)', 'k', '**', '**', 'Bold text', '&#xf032;' ],
	[ 'em', 'Emphasized (italic)', 'i', '*', '*', 'Italicized text', '&#xf033;' ],
	[ 'strikethrough', 'Strike-through', '', '~~', '~~', 'Struck-out text', '&#xf0cc;' ],
	[ 'link', 'Hyperlink', 'l', GUIEdit_hyperlink, '', '', '&#xf0c1;' ],
	[ 'image', 'Image', '', GUIEdit_image, '', 'Image alt-text', '&#xf03e;' ],
	[ 'heading1', 'Heading level 1', '', '\n# ', '', 'Heading', '&#xf1dc;<sup>1</sup>' ],
	[ 'heading2', 'Heading level 2', '', '\n## ', '', 'Heading', '&#xf1dc;<sup>2</sup>' ],
	[ 'heading3', 'Heading level 3', '', '\n### ', '', 'Heading', '&#xf1dc;<sup>3</sup>' ],
	[ 'blockquote', 'Blockquote', 'q', GUIEdit_blockquote, '', '', '&#xf10e;' ],
	[ 'bulleted-list', 'Bulleted list', '', '\n* ', '', 'List item', '&#xf0ca;' ],
	[ 'numbered-list', 'Numbered list', '', '\n1. ', '', 'List item', '&#xf0cb;' ],
	[ 'horizontal-rule', 'Horizontal rule', '', '\n\n---\n\n', '', '', '&#xf068;' ],
	[ 'inline-code', 'Inline code', '', '`', '`', 'Code', '&#xf121;' ],
	[ 'code-block', 'Code block', '', '```\n', '\n```', 'Code', '&#xf1c9;' ],
	[ 'formula', 'LaTeX', '', '$', '$', 'LaTeX formula', '&#xf155;' ],
	[ 'spoiler', 'Spoiler block', '', '::: spoiler\n', '\n:::', 'Spoiler text', '&#xf2fc;' ],
	[ 'footnote', 'Footnote', 'n', GUIEdit_footnote, '', '', '&#xf7e9;' ]
];

function GUIEdit_blockquote(text, startpos) {
	if (text == '') {
		text = "> Quoted text";
		return [ text, startpos + 2, startpos + text.length ];
	} else {
		text = "> " + text.split("\n").join("\n> ") + "\n";
		return [ text, startpos + text.length, startpos + text.length ];
	}
}

function GUIEdit_hyperlink(text, startpos) {
	var url = '', link_text = '', endpos = startpos;
	if (text.search(/^https?/) != -1) {
		url = text;
	} else {
		link_text = text;
		url = prompt("Link address (URL):");
		if (!url) {
			endpos = startpos + text.length;
			return [ text, startpos, endpos ];
		}
	}
	if (!link_text) {
		link_text = "link text";
		startpos = startpos + 1;
		endpos = startpos + link_text.length;
	} else {
		startpos = startpos + text.length + url.length + 4;
		endpos = startpos;
	}

	return [ "[" + link_text + "](" + url + ")", startpos, endpos ];
}

function GUIEdit_image(text, startpos) {
	var src = '', alt_text = '', title_text = '', endpos = startpos;
	if (text.search(/^https?/) != -1) {
		src = text;
	} else {
		alt_text = text;
		src = prompt("Image URL:");
		if (!src) {
			endpos = startpos + text.length;
			return [ text, startpos, endpos ];
		}
	}
	if (!alt_text) {
		alt_text = "image alt text";
		title_text = "image title text";
		startpos = startpos + 2;
		endpos = startpos + alt_text.length;
	} else {
		title_text = alt_text;
		startpos = startpos + text.length + src.length + 5;
		endpos = startpos;
	}

	return [ "![" + alt_text + "](" + src + " \"" + title_text + "\")", startpos, endpos ];
}

function GUIEdit_footnote(text, startpos, fulltext) {
	var footnotes = [...new Set(fulltext.match(/\[\^.+?\]/g))];
	var fnref = `[^${footnotes.length + 1}]`;

	startpos = startpos + fnref.length;
	var endpos = startpos;
	var fntext = text || "Footnote text."
	var options = { 
		where:		"end",
		text:		`\n\n${fnref}: ${fntext}`
	};
	if (!text) {
		options.cur_pos = [ fnref.length + 4, fnref.length + 4 + fntext.length ]
	}

	return [ fnref, startpos, endpos, options ];
}
