function injectCommentsViewModeSelector() {
	GWLog("injectCommentsViewModeSelector");

	let commentsContainer = query("#comments");
	if (commentsContainer == null) return;

	let currentModeThreaded = (location.href.search("chrono=t") == -1);
	let newHref = "href='" + location.pathname + location.search.replace("chrono=t","") + (currentModeThreaded ? ((location.search == "" ? "?" : "&") + "chrono=t") : "") + location.hash + "' ";

	let commentsViewModeSelector = addUIElement({{{parts/comments_view_mode_selector}}});

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
