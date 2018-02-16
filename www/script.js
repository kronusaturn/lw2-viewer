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
Element.prototype.hasClass = function(className) {
	return this.className.match(new RegExp("(^|\\s)" + className + "(\\s|$)"));
}

Element.prototype.addActivateEvent = function(func, includeMouseDown) {
	let ael = this.activateEventListener = function(e) { if(e.button === 0 || e.key === ' ') func(e) };
	if(includeMouseDown) this.addEventListener("mousedown", ael);
	this.addEventListener("mouseup", ael);
	this.addEventListener("keyup", ael);
}

Element.prototype.removeActivateEvent = function() {
	let ael = this.activateEventListener;
	this.removeEventListener("mousedown", ael);
	this.removeEventListener("mouseup", ael);
	this.removeEventListener("keyup", ael);
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

	textarea.addEventListener("focus", function(e) { e.target.parentElement.parentElement.scrollIntoViewIfNeeded(); });
	textarea.addEventListener("input", OnInputExpandTextarea, false);
	textarea.addEventListener("keyup", function(e) { e.stopPropagation(); });
	
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
	if ((this.offsetHeight - 30) < this.scrollHeight)
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
		buttonTarget.parentNode.style.opacity = "";
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
	e.target.parentNode.style.opacity = "0.5";
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
Element.prototype.setCommentThreadMaximized = function(toggle, userOriginated = true, force) {
	let ci = this;
	let storageName = "thread-minimized-" + ci.getCommentId();
	let minimize_button = ci.querySelector(".comment-minimize-button");
	let maximize = force || (toggle ? /minimized/.test(minimize_button.className) : !window.localStorage.getItem(storageName));
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
	let targetComment = null;
	let tcid = null;
	if (ci) {
		targetComment = (next ? ci.nextNewComment : ci.prevNewComment);
		if (targetComment) {
			tcid = targetComment.getCommentId();
		}
	} else {
		if (window.newComments[0]) {
			tcid = window.newComments[0];
			targetComment = document.querySelector("#comment-" + tcid);
		}
	}
	if(targetComment) {
		expandAncestorsOf(tcid);
		history.replaceState(null, null, "#comment-" + tcid);
		targetComment.scrollIntoView();
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
	let widthOptions = [
		['normal', 'Narrow (fixed-width) content column', 'N', '900px'],
		['wide', 'Wide (fixed-width) content column', 'W', '1150px'],
		['fluid', 'Full-width (fluid) content column', 'F', '(100vw - 300px)']
	];
	let currentWidth = window.localStorage.getItem("selected-width") || '900px';
	let widthSelector = addUIElement(
		"<div id='width-selector'>" +
		String.prototype.concat.apply("", widthOptions.map(function (wo) {
			let [name, desc, abbr, width] = wo;
			let selected = (width == currentWidth ? ' selected' : '');
			let disabled = (width == currentWidth ? ' disabled' : '');
			return `<button type='button' class='select-width-${name}${selected}'${disabled} title='${desc}' tabindex='-1' data-width='${width}'>${abbr}</button>`})) +
		"</div>");
	widthSelector.querySelectorAll("button").forEach(function (button) {
		button.addActivateEvent(widthAdjustButtonClicked);
	});
}
function widthAdjustButtonClicked(event) {
	let selectedWidth = event.target.getAttribute("data-width");
	if(selectedWidth == "900px") window.localStorage.removeItem("selected-width"); else window.localStorage.setItem("selected-width", selectedWidth);
	setContentWidth(selectedWidth);
	event.target.parentElement.childNodes.forEach(function (button) {
		button.removeClass("selected");
		button.disabled = false;
	});
	event.target.addClass("selected");
	event.target.disabled = true;
}

function injectThemeSelector() {
	document.querySelector("head").insertAdjacentHTML("beforeend", "<style id='theme-select-buttons'>" + 
		`#theme-selector button.select-theme-default,
		#theme-selector button.select-theme-default:hover {
			color: #000;
			background-color: #fff;
		}
		#theme-selector button.select-theme-dark,
		#theme-selector button.select-theme-dark:hover {
			color: #fff;
			background-color: #000;
		}
		#theme-selector button.select-theme-grey,
		#theme-selector button.select-theme-grey:hover {
			color: #f60;
			background-color: #eee;
			font-weight: 600;
		}
		#theme-selector button.select-theme-ultramodern,
		#theme-selector button.select-theme-ultramodern:hover {
			color: #f60;
			background-color: #888;
			font-weight: 400;
		}
		#theme-selector button.select-theme-zero,
		#theme-selector button.select-theme-zero:hover {
			color: #00e;
			background-color: #ccc;
			font-weight: 400;
		}
		#theme-selector button.select-theme-brutalist,
		#theme-selector button.select-theme-brutalist:hover {
			color: #000;
			background-color: #fff;
			font-weight: 400;
		}` + "</style>");

	let currentTheme = window.localStorage.getItem("selected-theme") || "default";
	let themeOptions = [
		['default', 'Default theme (dark text on light background)'],
		['dark', 'Dark theme (light text on dark background)'],
		['grey', 'Grey theme (more subdued than default theme)'],
		['ultramodern', 'Ultramodern theme (very hip)'],
		['zero', 'Simple theme with no custom fonts'],
		['brutalist', 'Brutalist theme (the Motherland calls!)']
	];
	let themeSelector = addUIElement(
		"<div id='theme-selector'>" +
		String.prototype.concat.apply("", themeOptions.map(function (to) {
			let [name, desc] = to;
			let selected = (name == currentTheme ? ' selected' : '');
			let disabled = (name == currentTheme ? ' disabled' : '');
			return `<button type='button' class='select-theme-${name}${selected}'${disabled} title='${desc}' tabindex='-1'>A</button>`;})) +
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

function injectThemeTweaker() {
	let themeTweakerToggle = addUIElement("<div id='theme-tweaker-toggle'><button type='button' tabindex='-1' title='Customize appearance'>&#xf1de;</button></div>");
	themeTweakerToggle.querySelector("button").addActivateEvent(themeTweakerToggleButtonClicked);
	
	let themeTweakerUI = addUIElement("<div id='theme-tweaker-ui' style='display: none;'>" + 
	`<div class='main-theme-tweaker-window'>
		<h1>Customize appearance</h1>
		<button type='button' class='minimize-button minimize' tabindex='-1'></button>
		<button type='button' class='help-button' tabindex='-1'></button>
		<p class='current-theme'>Current theme: <span>` + 
		(window.localStorage.getItem("selected-theme") || "default") + 
		`</span></p>
		<div class='controls-container'>
			<div id='theme-tweak-section-invert' class='section' data-label='Invert (photo-negative)'>
				<input type='checkbox' id='theme-tweak-control-invert'></input>
				<label for='theme-tweak-control-invert'>Invert colors</label>
			</div>
			<div id='theme-tweak-section-saturate' class='section' data-label='Saturation'>
				<input type="range" id="theme-tweak-control-saturate" min="0" max="300" data-default-value="100" data-value-suffix="%" data-label-suffix="%">
				<p class="theme-tweak-control-label" id="theme-tweak-label-saturate"></p>
			</div>
			<div id='theme-tweak-section-brightness' class='section' data-label='Brightness'>
				<input type="range" id="theme-tweak-control-brightness" min="0" max="300" data-default-value="100" data-value-suffix="%" data-label-suffix="%">
				<p class="theme-tweak-control-label" id="theme-tweak-label-brightness"></p>
			</div>
			<div id='theme-tweak-section-contrast' class='section' data-label='Contrast'>
				<input type="range" id="theme-tweak-control-contrast" min="0" max="300" data-default-value="100" data-value-suffix="%" data-label-suffix="%">
				<p class="theme-tweak-control-label" id="theme-tweak-label-contrast"></p>
			</div>
			<div id='theme-tweak-section-hue-rotate' class='section' data-label='Hue rotation'>
				<input type="range" id="theme-tweak-control-hue-rotate" min="0" max="360" data-default-value="0" data-value-suffix="deg" data-label-suffix="°">
				<p class="theme-tweak-control-label" id="theme-tweak-label-hue-rotate"></p>
			</div>
		</div>
		<div class='buttons-container'>
			<button type="button" class="reset-defaults-button">Reset to defaults</button>
			<button type='button' class='ok-button default-button'>OK</button>
			<button type='button' class='cancel-button'>Cancel</button>
		</div>
	</div>
	<div class="clippy-container">
        <span class="hint">Hi, I'm Bobby the Basilisk! Click on the minimize button (<img src='/minimize_button_icon.gif' />) to minimize the theme tweaker window, so that you can see what the page looks like with the current tweaked values. (But remember, <span>the changes won't be saved until you click "OK"!</span>)
        <img class='clippy' src='/basilisk.png' />
    </div>
	<div class='help-window'>
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
	
	document.querySelectorAll("#theme-tweaker-ui > div").forEach(function (themeTweakerUIWindow) {
		themeTweakerUIWindow.addActivateEvent(clickInterceptor, true);
	});
	
	themeTweakerUI.querySelectorAll("input").forEach(function (field) {
		field.addEventListener((field.type == "checkbox" ? "change" : "input"), themeTweakerFieldInputReceived);
	});
	
	themeTweakerUI.querySelector(".minimize-button").addActivateEvent(themeTweakerMinimizeButtonClicked);
	themeTweakerUI.querySelector(".help-button").addActivateEvent(themeTweakerHelpButtonClicked);
	themeTweakerUI.querySelector(".reset-defaults-button").addActivateEvent(themeTweakerResetDefaultsButtonClicked);
	themeTweakerUI.querySelector(".main-theme-tweaker-window .cancel-button").addActivateEvent(themeTweakerCancelButtonClicked);
	themeTweakerUI.querySelector(".main-theme-tweaker-window .ok-button").addActivateEvent(themeTweakerOKButtonClicked);
	themeTweakerUI.querySelector(".help-window .cancel-button").addActivateEvent(themeTweakerHelpWindowCancelButtonClicked);
	themeTweakerUI.querySelector(".help-window .ok-button").addActivateEvent(themeTweakerHelpWindowOKButtonClicked);
	
	document.querySelector("head").insertAdjacentHTML("beforeend","<style id='theme-tweaker-style'></style>");
}
function toggleThemeTweakerUI() {
	let themeTweakerUI = document.querySelector("#theme-tweaker-ui");
	themeTweakerUI.style.display = (themeTweakerUI.style.display == "none") ? "block" : "none";
	document.querySelector("#theme-tweaker-style").innerHTML = (themeTweakerUI.style.display == "none") ? "" : 
		`#content, #ui-elements-container > div:not(#theme-tweaker-ui) {
			pointer-events: none;
		}`;
	// Focus invert checkbox.
	if (themeTweakerUI.style.display != "none")
		document.querySelector("#theme-tweaker-ui #theme-tweak-control-invert").focus();
	// Set theme tweaker assistant visibility.
	document.querySelector(".clippy-container").style.display = JSON.parse(window.localStorage.getItem("theme-tweaker-settings") || '{ "showClippy": true }')["showClippy"] ? "block" : "none";
}
function themeTweakerToggleButtonClicked(event) {
	document.querySelector("#theme-tweaker-ui .current-theme span").innerText = (window.localStorage.getItem("selected-theme") || "default");
	
	document.querySelector("#theme-tweak-control-invert").checked = (window.currentFilters['invert'] == "100%");	
	[ "saturate", "brightness", "contrast", "hue-rotate" ].forEach(function (sliderName) {
		let slider = document.querySelector("#theme-tweak-control-" + sliderName);
		slider.value = /^[0-9]+/.exec(window.currentFilters[sliderName]) || slider.dataset['defaultValue'];
		document.querySelector("#theme-tweak-label-" + sliderName).innerText = slider.value + slider.dataset['labelSuffix'];
	});

	toggleThemeTweakerUI();
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
	[ "saturate", "brightness", "contrast", "hue-rotate" ].forEach(function (sliderName) {
		let slider = document.querySelector("#theme-tweak-control-" + sliderName);
		slider.value = slider.dataset['defaultValue'];
		document.querySelector("#theme-tweak-label-" + sliderName).innerText = slider.value + slider.dataset['labelSuffix'];
	});
	window.currentFilters = { };
	applyFilters(window.currentFilters);
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
	window.currentFilters = JSON.parse(window.localStorage.getItem("theme-tweaks") || "{ }");
	applyFilters(window.currentFilters);
}
function themeTweakSave() {
	window.localStorage.setItem("theme-tweaks", JSON.stringify(window.currentFilters));
}
function clickInterceptor(event) {
	event.stopPropagation();
}
function themeTweakerFieldInputReceived(event) {
	if (event.target.id == 'theme-tweak-control-invert') {
		window.currentFilters['invert'] = event.target.checked ? '100%' : '0%';
	} else if (event.target.type == 'range') {
		let sliderName = /^theme-tweak-control-(.+)$/.exec(event.target.id)[1];
		document.querySelector("#theme-tweak-label-" + sliderName).innerText = event.target.value + event.target.dataset["labelSuffix"];
		window.currentFilters[sliderName] = event.target.value + event.target.dataset["valueSuffix"];
	} else if (event.target.id == 'theme-tweak-control-clippy') {
		document.querySelector(".clippy-container").style.display = event.target.checked ? "block" : "none";
	}
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
}
function themeTweakerSaveSettings() {
	window.localStorage.setItem("theme-tweaker-settings", JSON.stringify({ 'showClippy': document.querySelector("#theme-tweak-control-clippy").checked }));
}

function expandAncestorsOf(commentId) {
	try { document.querySelector('#comment-'+commentId).closest("label[for^='expand'] + .comment-thread").parentElement.querySelector("input[id^='expand']").checked = true; }
	catch (e) { }
	try { document.querySelector('#comment-'+commentId).closest("#comments > ul > li").setCommentThreadMaximized(true, false, true); }
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

var initializeDone = false;
function initialize() {
	if(initializeDone || (document.readyState == "loading")) return;
	initializeDone = true;

	window.requestAnimationFrame(function() {
		if(location.hash.length == 18) {
			location.hash = "#comment-" + location.hash.substring(1);
		}
		var content = document.querySelector("#content");
		if (content.clientHeight <= window.innerHeight + 30 || 
			(content.querySelector("#comments") && content.querySelector("#comments").childNodes.length == 0)) {
			try { document.querySelector(".post .post-meta .comment-count").addClass("no-comments"); }
			catch (e) { }
		}

		try {
			let dtf = new Intl.DateTimeFormat([], (window.innerWidth > 520 ? {month: 'short', day: 'numeric', year: 'numeric', hour: 'numeric', minute: 'numeric'}
										       : {month: 'numeric', day: 'numeric', year: '2-digit', hour: 'numeric', minute: 'numeric'}));
			document.querySelectorAll(".date").forEach(function (e) {
				let d = e.getAttribute("data-js-date");
				if(d) { e.innerHTML = dtf.format(new Date(+ d)); }
			});
		}
		catch(e) { }

		window.needHashRealignment = false;

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
			document.querySelector("head").insertAdjacentHTML("beforeend","<style id='vote-buttons'>" + 
			`.upvote:hover,
			.upvote.selected {
				color: #00d800;
			}
			.downvote:hover,
			.downvote.selected {
				color: #eb4c2a;
			}` + "</style>");
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
		
		// Format and activate comment-minimize buttons.
		document.querySelectorAll(".comment-minimize-button").forEach(function (b) {
			b.closest(".comment-item").setCommentThreadMaximized(false);
			b.addActivateEvent(commentMinimizeButtonClicked);
		});
		let urlParts = document.URL.split('#comment-');
		if (urlParts.length > 1) {
			expandAncestorsOf(urlParts[1]);
			window.needHashRealignment = true;
		}
		
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
		// Add the theme selector.
		injectThemeSelector();
		// Add the theme tweaker.
		injectThemeTweaker();

		// Call pageLayoutFinished() once all activity that can affect the page layout has finished.
		document.addEventListener("readystatechange", pageLayoutFinished);
		window.setTimeout(pageLayoutFinished);
	})
}

var pageLayoutFinishedDone = false;
function pageLayoutFinished() {
	if(pageLayoutFinishedDone || (document.readyState != "complete")) return;
	pageLayoutFinishedDone = true;

	window.requestAnimationFrame(function () {
		if (window.needHashRealignment)
			realignHash();

		let content = document.querySelector("#content");
		if (content.clientHeight <= window.innerHeight + 30) {
			removeElement(".post .post-meta a[href='#bottom-bar']", content);
		} else {
			removeElement("#hide-bottom-bar", document.querySelector("head"));
		}
	});
}
function realignHash() {
	let h = location.hash;
	if (h)
		document.querySelectorAll(h).forEach(function (e) { e.scrollIntoView(true); });
}

function removeElement(selector, ancestor = document) {
	var element = ancestor.querySelector(selector);
	if (element) element.parentElement.removeChild(element);
}

document.addEventListener("DOMContentLoaded", initialize, {once: true});
window.setTimeout(initialize);
