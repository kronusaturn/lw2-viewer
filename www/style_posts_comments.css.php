/******************/
/* LISTINGS PAGES */
/******************/

.listings,
#content.sequence-page article {
	padding: 0 var(--GW-current-page-content-right-side-padding) 0 var(--GW-current-page-content-left-side-padding);
}

/*=----------------=*/
/*= Sequence pages =*/
/*=----------------=*/

#content.sequence-page section {
	margin: 3em 0 0 0;
}
#content.sequence-page section section {
	margin: 2em 0 0 0;
}

/************/
/* LISTINGS */
/************/

h1.listing {
	line-height: 1.15;
	position: relative;
	display: flex;

	font-family: var(--GW-post-listings-font), var(--GW-Font-Awesome);
	font-weight: var(--GW-post-listings-font-weight);
}

h1.listing a {
	position: relative;
}

/*	Link-post links
	*/
h1.listing a[href^="http"] {
	transform: scale(0.75) scaleX(-1) translateX(12%);
	text-align: right;
}

/*=----------------------=*/
/*= Listing hover reveal =*/
/*=----------------------=*/
/*	(On desktops, hover over a multi-line listing to reveal all of it) */

@media only screen and (hover: hover) and (pointer: fine) and (min-width: 961px) {
	h1.listing {
		max-height: 1.15em;
	}
	h1.listing a {
		z-index: 1;
		padding: 0 0 1px 1px;
	}
	h1.listing a[href^='/posts/'],
	h1.listing a[href^='/s/'],
	h1.listing a[href^='/conversation?id'] {
		white-space: nowrap;
		text-overflow: ellipsis;
		overflow: hidden;
		border-bottom: 1px solid transparent;
		-webkit-hyphens: auto;
		-moz-hyphens: auto;
		-ms-hyphens: auto;
		hyphens: auto;
		height: 100%;
	}
}
@media only screen and (hover: hover) and (pointer: fine) {
	h1.listing a[href^='/posts/']:hover,
	h1.listing a[href^='/posts/']:focus,
	h1.listing a[href^='/s/']:hover,
	h1.listing a[href^='/s/']:focus,
	h1.listing a[href^='/conversation?id']:hover,
	h1.listing a[href^='/conversation?id']:focus {
		text-decoration: dotted underline;
		white-space: initial;
		overflow: visible;
		z-index: 2;
	}
	h1.listing a[href^="http"]:hover {
		text-decoration: none;
	}
	h1.listing:focus-within::before {
		content: "\F105";
		font-family: var(--GW-Font-Awesome);
		display: block;
		position: absolute;
		left: -0.75em;
	}

/*	Adds hysteresis to the hover area (i.e., prevents oscillation due to small
	mouse movements).
	*/
	h1.listing a[href^='/posts/']:hover::before,
	h1.listing a[href^='/s/']:hover::before,
	h1.listing a[href^='/conversation?id']:hover::before {
		content: "";
		position: absolute;
		top: -10px;
		right: -10px;
		bottom: -10px;
		left: -10px;
		z-index: -1;
	}
}

h1.listing a[href^='http'] {
	-webkit-user-select: none;
	-moz-user-select: none;
	user-select: none;
}

/*=-----------------------=*/
/*= In-listing edit links =*/
/*=-----------------------=*/

h1.listing .edit-post-link {
	position: absolute;
	margin: 0;
	padding: 5px 3px 0 10px;
	top: 0;
	right: 0;
	height: calc(100% + 0.5em);
}
h1.listing .edit-post-link:hover {
	text-decoration: none;
}

/*=------------------=*/
/*= Post type prefix =*/
/*=------------------=*/

h1.listing .post-type-prefix {
	display: inline-block;
}

/*=---------------------------------=*/
/*= Error messages on listing pages =*/
/*=---------------------------------=*/

.listing-message {
	width: 100%;
	text-align: center;
	padding: 1.25em 0 1.25em 0;
	font-size: 1.375em;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 960px) {
	h1.listing:focus-within::before {
		display: none;
	}
	h1.listing a[href^="http"] {
		align-self: flex-start;
		position: relative;
		top: 1px;
	}
}

/*********************/
/* LISTING POST-META */
/*********************/

h1.listing + .post-meta {
	position: relative;
	justify-content: flex-start;
	margin: 0 0 1rem 0;

	font-family: var(--GW-listings-post-meta-font);
	font-weight: var(--GW-listings-post-meta-font-weight);
}

h1.listing + .post-meta .karma-value {
	cursor: default;
}
h1.listing + .post-meta .post-section {
	width: 0;
	margin: 0;
	overflow: hidden;
}
h1.listing + .post-meta .post-section::before {
	position: absolute;
	left: -1.75em;
	width: 1.5em;
	text-align: center;
}

h1.listing + .post-meta .read-time {
	cursor: pointer;
}

/********************/
/* POSTS & COMMENTS */
/********************/

.post-meta > *,
.comment-meta > * {
	display: inline-block;
	font-size: 1.0625em;
	white-space: nowrap;
}

.body-text {
	overflow-wrap: break-word;
	text-align: justify;

	font-family: var(--GW-body-text-font);
	font-weight: var(--GW-body-text-font-weight);
	color: var(--GW-body-text-color);

	text-shadow: 0 0 1px var(--GW-content-background-color);
}
.body-text p {
	margin: 1em 0;
}
.retracted .body-text {
	text-decoration: line-through;
}

.bare-url {
	word-break: break-all;
	hyphens: auto;
}

/*************/
/* POST-META */
/*************/

.post-meta {
	display: flex;
	flex-flow: row wrap;
	justify-content: center;
}

.post-meta .lw2-link,
.post-meta .post-section,
.post-meta .qualified-linking {
	-webkit-user-select: none;
	-moz-user-select: none;
	user-select: none;
}

.post-meta a:visited {
	color: var(--GW-hyperlink-color);
}
.post-meta a:hover,
.post-meta a:focus {
	color: var(--GW-hyperlink-hover-color);
}
.post-meta a:active {
	color: var(--GW-hyperlink-active-color);
}

.post-meta .lw2-link {
	opacity: 0.5;
	order: 1;
}
.post-meta .post-section {
	order: -1;
}
.post-meta .post-section::before {
	font-family: var(--GW-Font-Awesome);
	font-weight: 900;
	padding: 0 1px;
}
.post-section.frontpage::before {
	content: "\F015";
}
.post-section.featured::before {
	content: "\F005";
}
.post-section.meta::before {
	content: "\F077";
}
.post-section.personal::before {
	content: "\F007";
}
.post-section.draft::before {
	content: "\F15B";
}
.post-section.alignment-forum::before,
.comment-meta .alignment-forum {
	content: "AF";
	font-family: var(--GW-Alignment-Forum-logotype-font);
	font-weight: bold;
}

/*= Karma controls hover tooltips =*/

@media only screen and (hover: hover) and (pointer: fine) {
	.post .karma,
	.comment-item .karma {
		position: relative;
	}
	.post .karma.active-controls::after,
	.comment-item .karma.active-controls::after {
		content: "Double-click for strong vote";
		position: absolute;
		left: 0;
		right: 0;
		margin: auto;
		pointer-events: none;
		min-width: 108px;
		line-height: 1.15;
		white-space: normal;
		text-align: center;
		font-size: 0.875rem;
		opacity: 0;
		transition: opacity 0.2s ease;
	}
<?php fit_content(".post .karma.active-controls::after, \n\t\t.comment-item .karma.active-controls::after", "width", "\t"); ?>
	.post .karma.active-controls:hover::after,
	.comment-item .karma.active-controls:hover::after {
		opacity: 1.0;
	}
	.comment-item .karma.active-controls:hover::after {
		z-index: 10000;
	}

	.post .karma .karma-value::after,
	.comment-item .karma .karma-value::after {
		content: attr(title);
		position: absolute;
		left: 0;
		right: 0;
		margin: auto;
		pointer-events: none;
		white-space: nowrap;
		text-align: center;
		font-size: 0.875rem;
		opacity: 0;
		transition: opacity 0.2s ease;
	}
<?php fit_content(".post .karma .karma-value::after, \n\t\t.comment-item .karma .karma-value::after", "width", "\t"); ?>
	.post .karma .karma-value:hover::after,
	.comment-item .karma .karma-value:hover::after {
		opacity: 1.0;
	}
	.comment-item .karma .karma-value:hover::after {
		z-index: 5001;
	}
}

/*= Author hover tooltip =*/

@media only screen and (hover: hover) and (pointer: fine) {
	.author {
		position: relative;
	}
	.author:not(.redacted)[data-full-name]::before {
		content: attr(data-full-name);
		position: absolute;
		pointer-events: none;
		display: block;
		left: 50%;
		bottom: 2em;
		transform: translateX(-50%);
		white-space: nowrap;
		text-align: center;
		font-size: 0.75em;
		font-weight: var(--GW-UI-font-weight-light);
		opacity: 0;
		transition: opacity 0.2s ease;
		z-index: 5001;
	}
	.author:hover::before {
		opacity: 1.0;
	}
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 720px) {
	.post-meta .karma-value span,
	.post-meta .comment-count span {
		display: none;
	}
	.post-meta .comment-count::before {
		content: "\F086\2004";
		font-family: var(--GW-Font-Awesome);
		font-size: 0.875em;
		font-weight: 400;
	}
	.post .karma.active-controls::after,
	.comment-item .karma.active-controls::after,
	.post .karma .karma-value::after,
	.comment-item .karma .karma-value::after {
		display: none;
	}
}

/*********/
/* POSTS */
/*********/

.post {
	grid-row: 3;
	max-width: 100%;
	padding: 
		0 
		calc(var(--GW-current-page-content-right-side-padding) + var(--GW-post-right-side-padding))
		0 
		calc(var(--GW-current-page-content-left-side-padding) + var(--GW-post-left-side-padding));
}

h1.post-title {
	margin: 1.1em 0 0.35em 0;
	text-align: center;
	font-size: 2.5em;
	line-height: 1;

	font-family: var(--GW-post-title-font);
	font-weight: var(--GW-post-title-font-weight);
}

h1.post-title .post-type-prefix {
	display: block;
	font-size: 0.75em;
	margin: -0.5em 0 0.125em 0;
}

.post .post-meta {
	position: relative;
	z-index: 2;
}
.post .bottom-post-meta {
	padding: 22px 0 20px 0;
}

.bottom-post-meta {
	border-style: solid;
	border-width: 1px 0;
}
#content:not(.in-sequence):not(.no-comments) .bottom-post-meta,
#content:not(.in-sequence).question-post-page.no-comments .bottom-post-meta {
	border-bottom: none;
}

.post-body {
	min-height: 8em;
	line-height: 1.5;
	font-size: 1.3rem;
	overflow: auto;

	/*	Corrects for protruding italics, etc.
		*/
	margin: 1.5em -2px 0 -2px;
	padding: 0 2px;
}
.post-body > *:first-child:not(.contents),
.post-body .contents + * {
	margin-top: 0;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 520px) {
	.post-body {
		font-size: 1.2rem;
		line-height: 1.45;
	}
	h1.post-title {
		font-size: 2em;
	}
	.post .post-meta {
		line-height: 1.9;
	}
}

/**************/
/* LINK POSTS */
/**************/

.post.link-post .post-body > p:first-child {
	text-align: center;
	font-size: 1.25em;
}
.post.link-post .post-body > p:only-child {
	font-size: 1.5em;
	margin: 0 0 1em 0;
}
.post.link-post a.link-post-link {
	font-family: var(--GW-link-post-link-font);
	font-weight: var(--GW-link-post-link-font-weight);
}
.post.link-post a.link-post-link::before {
	content: "\F0C1";
	font-family: var(--GW-Font-Awesome);
	font-weight: 900;
	display: inline-block;
	transform: scale(0.75) scaleX(-1) translateX(0.125em);
}

/************/
/* COMMENTS */
/************/

.comments {
	max-width: 100%;
	margin: 0 var(--GW-current-page-content-right-side-padding) 1px var(--GW-current-page-content-right-side-padding);
	position: relative;
}
.comments::before {
	content: "";
	position: absolute;
	display: block;
	top: 0;
	left: 0;
	width: 100%;
	height: 100%;
	pointer-events: none;
}
ul.comment-thread {
	list-style-type: none;
	padding: 0;
	max-width: 100%;
}

.comment-item {
	position: relative;
	margin: 2em 0 0 0;
	background-color: var(--GW-comment-background-color);
}
#content.search-results-page .comment-item {
	margin: 1em 0 0 0;
}
.comment-item .comment-item {
	margin: 1em 8px 8px 16px;
}
.comment-item .comment-item + .comment-item {
	margin: 2em 8px 8px 16px;
}

.comment-body {
	line-height: 1.45;
	font-size: 1.2rem;
	padding: 10px;
}
.comment-body ul {
	list-style-type: circle;
}
.comment-body > *:first-child {
	margin-top: 0;
}
.comment-body > *:last-child {
	margin-bottom: 0;
}

.comments-empty-message {
	width: 100%;
	text-align: center;
	padding: 0.75em 0 0.9em 0;
	font-size: 1.5em;
	font-weight: var(--GW-UI-font-weight-light);
}

/*=---------------------------=*/
/*= Comment highlight overlay =*/
/*=---------------------------=*/

.comment-item::before {
	content: "";
	position: absolute;
	width: 100%;
	height: 100%;
	z-index: 5000;
	left: 0;
	top: 0;
	pointer-events: none;
	display: none;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	.comment-item .comment-item {
		margin: 0.75em 2px 4px 6px;
	}
	.comment-item .comment-item + .comment-item {
		margin: 1.5em 2px 4px 6px;
	}
	.comment-body ul {
		padding-left: 30px;
	}
	.comment-body ol {
		padding-left: 30px;
	}
}

/***********/
/* ANSWERS */
/***********/

.answer-item {
	margin-top: 3.5em;
}
.answer-item::after {
	content: "Answer";
	position: absolute;
	text-align: center;
	padding: 0.125em 0.375em 0 0.375em;
	border-style: solid;
	bottom: 100%;
	font-size: 1.25rem;

	background-color: inherit;
	border-width: inherit;
	border-bottom-width: 0;
	border-color: inherit;

	z-index: 5000;
}

/*****************************/
/* HIGHLIGHTING NEW COMMENTS */
/*****************************/

.comment-item.new-comment::before {
	display: initial;
}

/********************/
/* COMMENT LISTINGS */
/********************/

.listings .comment-thread:first-child > .comment-item {
	margin-top: 0;
}
.listings .comment-thread .comment-meta a.date:focus,
.listings .comment-thread .comment-meta a.permalink:focus {
	position: relative;
}

.listings .comment-item-focused .comment {
	background-color: var(--GW-comment-item-focused-background-color);
	outline: var(--GW-comment-item-focused-outline);
}

/**********************************/
/* DEEP COMMENT THREAD COLLAPSING */
/**********************************/

.comment-item input[id^="expand"] {
	display: none;
}
.comment-item input[id^="expand"] + label {
	display: block;
	visibility: hidden;
	position: relative;
	margin: 8px 9px;
}
.comment-item input[id^="expand"] + label::after {
	content: "(Expand " attr(data-child-count) " below)";
	visibility: visible;
	position: absolute;
	left: 0;
	white-space: nowrap;
	cursor: pointer;

	color: var(--GW-hyperlink-color);
	font-weight: var(--GW-UI-font-weight-heavy);
}
.comment-item input[id^="expand"] + label:hover::after,
.comment-item input[id^="expand"] + label:focus::after {
	color: var(--GW-hyperlink-hover-color);
}
.comment-item input[id^="expand"] + label:active::after {
	color: var(--GW-hyperlink-active-color);
}
.comment-item input[id^="expand"]:checked + label::after {
	content: "(Collapse " attr(data-child-count) "	below)";
}
.comment-item input[id^="expand"] ~ .comment-thread {
	max-height: 34px;
	overflow: hidden;
}
.comment-item input[id^="expand"] ~ .comment-thread > li:first-child {
	margin-top: 0;
}
.comment-item input[id^="expand"]:checked ~ .comment-thread {
	max-height: 1000000px;
}

.comment-item input[id^="expand"]:checked ~ .comment-thread .comment-thread .comment-item {
	margin: 0;
}
.comment-item input[id^="expand"]:checked ~ .comment-thread .comment-thread .comment-item a.comment-parent-link:hover::after {
	display: none;
}

/****************/
/* COMMENT-META */
/****************/

.comment-meta {
	padding: 2px 24px 2px 10px;
	margin: 0 -1px;
	border: none;
	display: flex;
	flex-flow: row wrap;
	align-items: baseline;
}
.user-page .comment-meta,
.conversation-page .comment-meta {
	padding-right: 10px;
}

.comment-meta > * {
	margin-right: 1em;
}

.comment-meta .comment-post-title {
	flex-basis: 100%;
	overflow: hidden;
	text-overflow: ellipsis;
	line-height: 1.3;
}
#content.conversation-page .comment-meta .comment-post-title {
	display: none;
}
.comment-item .author:not(.redacted).original-poster::after {
	content: "\2004(OP)";
	font-size: 0.75em;
}

.comment-meta .author {
	font-weight: var(--GW-comment-meta-author-font-weight);
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	.comment-meta {
		padding: 2px 40px 2px 10px;
	}
}
@media only screen and (max-width: 720px) {
	.comment-meta > * {
		margin-bottom: 0.125em;
	}
	.comment-meta .karma-value span {
		display: none;
	}
}
@media only screen and (max-width: 520px) {
	.comment-meta {
		padding: 2px 10px;
		position: relative;
	}
	.comment-meta .author-wrapper {
		flex-basis: 100%;
	}
	.comment-post-title2 {
		display: block;
		text-overflow: ellipsis;
		overflow: hidden;
	}
	.comment-meta .lw2-link {
		display: none;
	}
}

/**********************/
/* ANTI-KIBITZER MODE */
/**********************/

.author.redacted,
.inline-author.redacted {
	font-weight: var(--GW-UI-font-weight-light);
}

/*****************************/
/* COMMENT THREAD NAVIGATION */
/*****************************/

.comment-item.depth-odd {
	--GW-comment-background-color: var(--GW-comment-background-color-odd);
	--GW-comment-parent-background-color: var(--GW-comment-background-color-even);
}
.comment-item.depth-even {
	--GW-comment-background-color: var(--GW-comment-background-color-even);
	--GW-comment-parent-background-color: var(--GW-comment-background-color-odd);
}

.comment-item:target {
	--GW-comment-background-color: var(--GW-comment-background-color-target);
}
.comment-item:target > .comment-thread > .comment-item {
	--GW-comment-parent-background-color: var(--GW-comment-background-color-target);
}

div.comment-parent-link {
	font-weight: var(--GW-UI-font-weight-heavy);
}

a.comment-parent-link {
	font-weight: var(--GW-UI-font-weight-light);
}

/*	Hover highlighting.
	*/
@media only screen and (hover: hover) and (pointer: fine) {
	a.comment-parent-link::before {
		content: "\F062";
		font-family: var(--GW-Font-Awesome);
		font-weight: 900;
		font-size: 0.75rem;
		line-height: 1;
		position: absolute;
		z-index: 1;
		display: block;
		padding: 3px 3px 0 3px;
		width: 17px;
		height: calc(100% + 2px);
		top: -1px;
		left: -18px;
		border-left: 1px solid transparent;
	}
	a.comment-parent-link::after {
		content: "";
		position: absolute;
		z-index: 0;
		display: block;
		width: calc(100% + 26px);
		height: calc(100% + 38px);
		top: -29px;
		left: -17px;
		pointer-events: none;
		overflow: hidden;
		visibility: hidden;
	}
	a.comment-parent-link:hover::after {
		visibility: visible;
	}
}

/*=--------------=*/
/*= Highlighting =*/
/*=--------------=*/

.comment-item.focused::before,
.comment-item.new-comment::before,
.comment-item.highlight::before,
.comment-item.highlight-faint::before,
.comment-popup {
	display: initial;
	outline: 3px solid var(--GW-comment-item-outline-color);
}

.comment-item.answer-item.focused::after,
.comment-item.answer-item.new-comment::after,
.comment-item.answer-item.highlight::after,
.comment-item.answer-item.highlight-faint::after {
	border-width: 3px 3px 0 3px;
	left: -3px;
	border-color: var(--GW-comment-item-outline-color);
}

.comment-item.highlight,
.comment-item.highlight-faint,
.comment-popup {
	filter: drop-shadow(0 0 5px var(--GW-comment-item-outline-color));
}

.comment-item.new-comment {
	--GW-comment-item-outline-color: var(--GW-comment-item-new-comment-outline-color);
}
.comment-item.focused {
	--GW-comment-item-outline-color: var(--GW-comment-item-focused-outline-color);
}
#content.compact .comment-item {
	--GW-comment-item-outline-color: var(--GW-comment-item-expanded-outline-color);
}
.comment-item.highlight,
.comment-popup {
	--GW-comment-item-outline-color: var(--GW-comment-item-higlight-color);
}
.comment-item.highlight-faint {
	--GW-comment-item-outline-color: var(--GW-comment-item-highlight-faint-color);
}

/*=----------------=*/
/*=	Comment popups =*/
/*=----------------=*/

@media only screen and (hover: hover) and (pointer: fine) {
	.comment-popup {
		position: fixed;
		top: 10%;
		right: 10%;
		max-width: 700px;
		z-index: 10001;
		font-size: 1rem;
		white-space: unset;
		pointer-events: none;
		background-color: var(--GW-comment-popup-background-color);
		border-width: 1px;
		border-style: solid;
		border-color: transparent;
	}
	.comment-popup .comment-parent-link,
	.comment-popup .comment-minimize-button {
		display: none;
	}
	.comment-popup .comment-body {
		font-size: 1.0625rem;
	}
}

/*=---------------------=*/
/*= Comment child links =*/
/*=---------------------=*/

.comment-child-links {
	flex-basis: 100%;

	font-weight: var(--GW-UI-font-weight-heavy);
}
a.comment-child-link {
	margin: 0 0.25em;
	display: inline-block;

	font-weight: var(--GW-UI-font-weight-light);
}
a.comment-child-link::before {
	content: ">";
	display: inline-block;
	margin: 0 2px 0 0;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	a.comment-parent-link::before,
	a.comment-parent-link::after {
		display: none;
	}
}

/**********************/
/* COMMENT PERMALINKS */
/**********************/
/********************/
/* COMMENT LW LINKS */
/********************/

.comment-meta .permalink::before,
.comment-meta .lw2-link::before,
.comment-meta .comment-parent-link span::before,
.post .post-meta .lw2-link::before {
	content: "";
	display: inline-block;
	width: 1rem;
	height: 1rem;
	border-radius: 3px;
	box-shadow: 
		0 0 0 1px #fff,
		0 0 0 2px #00e,
		0 0 0 3px transparent;
	background-size: auto 16px;
	position: relative;
	top: 2px;
	opacity: 0.5;
}

.comment-meta .permalink::before,
.comment-meta .lw2-link::before,
.post .post-meta .lw2-link::before,
.comment-meta .comment-parent-link span::before {
	background-image: var(--GW-comment-meta-icons-normal-sprites);
}
.comment-meta .lw2-link::before,
.post .post-meta .lw2-link::before {
	background-position: -16px;
}
.comment-meta .comment-parent-link span::before {
	left: unset;
	background-position: -32px;
}

.comment-meta .permalink:hover::before,
.comment-meta .lw2-link:hover::before,
.comment-meta .comment-parent-link:hover span::before,
.post .post-meta .lw2-link:hover::before {
	background-color: #00e;
	background-image: var(--GW-comment-meta-icons-hover-sprites);
	box-shadow: 
		0 0 0 2px #00e,
		0 0 0 3px transparent;
	opacity: 1.0;
	filter: unset;
}
.comment-meta .permalink:active::before,
.comment-meta .lw2-link:active::before,
.comment-meta .comment-parent-link span:active::before,
.post .post-meta .lw2-link:active::before {
	transform: scale(0.9);
}

.comment-meta .permalink,
.comment-meta .lw2-link,
.comment-meta .comment-parent-link span,
.post .post-meta .lw2-link {
	position: relative;
	opacity: 1.0;
}
.comment-meta .permalink::after,
.comment-meta .lw2-link::after,
.comment-meta .comment-parent-link span::after,
.post .post-meta .lw2-link::after {
	content: "";
	width: 30px;
	height: 30px;
	display: block;
	position: absolute;
	top: -2px;
	left: -7px;
	box-shadow: none;
	pointer-events: auto;
	visibility: visible;
}
.comment-meta .comment-parent-link span::after {
	top: -5px;
}
.post .post-meta .lw2-link {
	padding: 0 3px;
}
.post .post-meta .lw2-link::after {
	left: -4px;
}

/*************************/
/* COMMENTS COMPACT VIEW */
/*************************/

#content.compact .comment-thread {
	font-size: 0.9375rem;
}
#content.compact .comment-thread .comment-body {
	font-size: 1.0625rem;
}
#content.compact .comment-item {
	max-height: var(--GW-comment-compact-height);
	margin-top: 1em;
	overflow: hidden;
	position: relative;
	pointer-events: none;
}
#content.conversation-page.compact .comment-item {
	max-height: calc(var(--GW-comment-minimized-height) - 2px);
}

.comment-item.minimized .karma.active-controls::after,
.comment-item.minimized .karma .karma-value::after {
	display: none;
}

/*=-----------------------------=*/
/*= Ellipsis (“...”) hover area =*/
/*=-----------------------------=*/

#content.compact .comment-item::after {
	content: "…";
	position: absolute;
	right: 0;
	bottom: 0;
	height: 100%;
	width: 2em;
	display: flex;
	justify-content: center;
	align-items: center;
	font-size: 2rem;
	line-height: 1;
	pointer-events: auto;
	color: var(--GW-hyperlink-color);
}
#content.conversation-page.compact .comment-item::after {
	padding: 0 0 0.25em 0;
}

#content.compact .comment-meta {
	white-space: nowrap;
	overflow: hidden;
	text-overflow: ellipsis;
	padding: 2px 4rem 2px 10px;
}
#content.compact .comment-meta a {
	pointer-events: auto;
}

/*=---------------------------=*/
/*= Expand on hover/focus/tap =*/
/*=---------------------------=*/

@media only screen and (hover: hover) and (pointer: fine) {
	#content.compact .comment-item:hover {
		z-index: 11;
	}
	#content.compact .comment-item.expanded {
		z-index: 10;
	}
	#content.compact .comment-item:hover,
	#content.compact .comment-item.expanded {
		overflow: visible;
		pointer-events: auto;
	}

	#content.compact .comment-thread:last-child .comment-item.expanded {
		max-height: unset;
	}

	#content.compact .comment-item:hover .comment-meta {
		white-space: unset;
	}

	#content.compact .comment-item:hover .comment,
	#content.compact .comment-item.expanded .comment {
		position: relative;
		z-index: 1;
	}
	#content.compact .comment-item:hover .comment::before,
	#content.compact .comment-item.expanded .comment::before {
		content: "";
		position: absolute;
		width: calc(100% + 20px);
		height: calc(100% + 20px);
		z-index: -1;
		top: -10px;
		left: -10px;
	}
}
@media not screen and (hover: hover) and (pointer: fine) {
	#content.compact .comment-thread.expanded .comment-item {
		overflow: visible;
		pointer-events: auto;
		z-index: 10;
	}

	#content.compact .comment-thread.expanded:last-child .comment-item {
		max-height: unset;
	}
	#content.compact .comment-thread.expanded .comment {
		position: relative;
		z-index: 1;
		margin-bottom: 2em;
	}
	#content.compact .comment-thread.expanded .comment::before {
		content: "";
		position: absolute;
		display: block;
		width: calc(100% + 20px);
		height: calc(100% + 20px);
		z-index: -1;
		top: -10px;
		left: -10px;
	}
	#content.compact .comment-thread.expanded:last-child .comment {
		margin: 0;
	}
	#content.compact .comment-thread.expanded .comment::after {
		content: "";
		display: block;
		position: fixed;
		top: 0;
		left: 0;
		width: 100%;
		height: 100%;
		z-index: -2;
		background-color: rgba(0, 0, 0, 0.5);
	}
}

/*=--------------=*/
/*= Highlighting =*/
/*=--------------=*/

@media only screen and (hover: hover) and (pointer: fine) {
	#content.compact .comment-item:hover .comment {
		outline: 3px solid var(--GW-comment-item-outline-color);
	}
	#content.compact .comment-item:hover .comment::before,
	#content.compact .comment-item.expanded .comment::before {
		background-color: var(--GW-comment-item-expanded-background-color);
		box-shadow: var(--GW-comment-item-expanded-box-shadow);
	}
}
@media not screen and (hover: hover) and (pointer: fine) {
	#content.compact .comment-thread.expanded .comment {
		outline: 3px solid var(--GW-comment-item-outline-color);
	}
	#content.compact .comment-thread.expanded .comment::before {
		background-color: var(--GW-comment-item-expanded-background-color);
		box-shadow: var(--GW-comment-item-expanded-box-shadow);
	}
}

/*	Hide the overlay pseudo-element.
	*/
#content.compact .comment-item::before {
	display: none;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 520px) {
	#content.compact .comment-item {
		max-height: var(--GW-comment-compact-height-mobile);
	}
	#content.conversation-page.compact .comment-item {
		max-height: var(--GW-comment-minimized-height-mobile);
	}
}

/***********************************/
/* COMMENT THREAD MINIMIZE BUTTONS */
/***********************************/

.comment-minimize-button {
	font-family: var(--GW-UI-font), var(--GW-Font-Awesome);
	font-weight: 900;
	font-size: 1.25rem;
	line-height: 1;
	position: absolute;
	right: 1px;
	width: 18px;
	margin: 0;
	cursor: pointer;
	-webkit-user-select: none;
	-moz-user-select: none;
	user-select: none;
}
.comment-minimize-button:active {
	transform: scale(0.9);
}
.comment-minimize-button::after {
	content: attr(data-child-count);
	font-size: 0.8125rem;
	position: absolute;
	left: 0;
	width: 100%;
	text-align: center;
	top: 21px;

	font-family: var(--GW-UI-font);
	font-weight: var(--GW-UI-font-weight-light);
}
#content.individual-thread-page .comment-minimize-button {
	display: none;
}

/*=--------------------=*/
/*= Minimized comments =*/
/*=--------------------=*/

.comment-item.minimized {
	height: var(--GW-comment-minimized-height);
	overflow: hidden;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 520px) {
	.comment-item.minimized {
		height: var(--GW-comment-minimized-height-mobile);
	}
	.comment-minimize-button {
		right: 3px;
	}
}

/*****************/
/* IGNORE SYSTEM */
/*****************/

.comment-item.ignored {
	overflow: hidden;
}
#content.comment-thread-page .comment-item.ignored:not(.maximized) {
	height: var(--GW-comment-minimized-height);
}

/*	Ignored comment author.
	*/
.comment-item.ignored .author {
	text-decoration: line-through;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 520px) {
	#content.comment-thread-page .comment-item.ignored:not(.maximized) {
		height: var(--GW-comment-compact-minimized-height);
	}
}

/***********************************/
/* INDIVIDUAL COMMENT THREAD PAGES */
/***********************************/

.individual-thread-page h1.post-title {
	margin: 0.75em var(--GW-current-page-content-right-side-padding) 3px var(--GW-current-page-content-right-side-padding);
	text-align: left;
	font-size: 2.25em;
}

.comments > .comment-thread > .comment-item > .comment .comment-parent-link::before,
.comments > .comment-thread > .comment-item > .comment .comment-parent-link::after {
	display: none;
}

/****************/
/* VOTE BUTTONS */
/****************/

.vote {
	margin: 0;
	font-family: var(--GW-Font-Awesome);
	font-weight: 900;
	border: none;
	color: var(--GW-vote-button-color);
}

.upvote.selected,
.upvote:hover,
.upvote:focus {
	color: var(--GW-upvote-button-color);
}
.downvote.selected,
.downvote:hover,
.downvote:focus {
	color: var(--GW-downvote-button-color);
}

.karma.waiting {
	opacity: 0.5;
}
.karma.waiting button {
	pointer-events: none;
}

/* Replicated karma controls at bottom of comments. */
.comment-controls .karma {
	float: left;
	margin-left: -14px;
	position: relative;
	bottom: 1px;
}

/**************************/
/* QUALIFIED HYPERLINKING */
/**************************/

#content.no-comments #comments, 
#content.no-comments .post-meta .comment-count,
#content.no-comments .post-meta .karma,
#content.no-comments #answers .comment-controls,
#content.no-comments .answer-item .comment-thread,
#content.no-comments .answer-item .karma,
#content.no-comments + #ui-elements-container #new-comment-nav-ui,
#content.no-comments + #ui-elements-container #hns-date-picker {
	display: none;
}

#content.no-nav-bars #primary-bar,
#content.no-nav-bars #secondary-bar {
	display: none;
}
#content.no-nav-bars .post,
#content.no-nav-bars #bottom-bar {
	grid-row: unset;
}
#content.no-nav-bars {
	margin: 8px auto;
}
#content.no-nav-bars + #ui-elements-container {
	top: 8px;
	height: calc(100vh - 8px);
}

/*	Viewport width adjustments.
	*/
@media only screen and (max-width: 900px) {
	#content.no-nav-bars {
		margin: 0;
	}
}

/*=---------------------------------------------------------=*/
/*= Auxiliary “About” link in qualified link page view mode =*/
/*=---------------------------------------------------------=*/

#aux-about-link {
	position: absolute;
	width: 1.5em;
	top: 0;
	right: 100%;
	margin: 8px 52px;
}
#aux-about-link a {
	font-family: var(--GW-Font-Awesome);
	font-weight: 900;
	font-size: 1.25rem;
	opacity: 0.4;
	display: flex;
	justify-content: center;
}

/*=--------------------------=*/
/*= Qualified linking button =*/
/*=--------------------------=*/

.post-meta .qualified-linking {
	position: relative;
}
.post-meta .qualified-linking input[type='checkbox'] {
	visibility: hidden;
	width: 0;
	height: 0;
	margin: 0;
}
.post-meta .qualified-linking label {
	font-family: var(--GW-Font-Awesome);
	font-weight: 900;
	color: var(--GW-hyperlink-color);
	padding: 0 0.5em;
	display: inline-block;
	user-select: none;
}
.post-meta .qualified-linking label:hover {
	cursor: pointer;
}
.post-meta .qualified-linking label:active span {
	display: inline-block;
	transform: scale(0.9);
}

.post-meta .qualified-linking label::before {
	display: none;
}

.post-meta .qualified-linking label::after {
	content: "";
	width: 100vw;
	height: 0;
	left: 0;
	top: 0;
	position: fixed;
	z-index: 1;
	cursor: default;
}
.post-meta .qualified-linking input[type='checkbox']:checked + label::after {
	height: 100vh;
	background-color: var(--GW-body-background-color);
	opacity: 0.8;
}

/*=---------------------------=*/
/*= Qualified linking toolbar =*/
/*=---------------------------=*/

.post-meta .qualified-linking-toolbar {
	position: absolute;
	right: 0.25em;
	top: 110%;
	z-index: 1;
	grid-template-columns: 1fr auto;
	grid-gap: 4px;
	padding: 4px;
}
.post-meta .qualified-linking input[type='checkbox'] ~ .qualified-linking-toolbar {
	display: none;
}
.post-meta .qualified-linking input[type='checkbox']:checked ~ .qualified-linking-toolbar {
	display: grid;
}
#qualified-linking-toolbar-toggle-checkbox-bottom ~ .qualified-linking-toolbar {
	top: unset;
	bottom: 125%;
}

.post-meta .qualified-linking-toolbar a {
	padding: 1px 6px;
	text-align: center;
	user-select: none;
	grid-column: 1;
}
.qualified-linking-toolbar button {
	font-family: var(--GW-Font-Awesome);
	grid-column: 2;
	padding: 2px 8px 0 8px;
	font-size: 0.875em;
	font-weight: 400;
}
