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

/* Links to link-posts (not the link-post links themselves; that’s below) */
h1.listing a[href^='http'] + a {
	margin-left: 0.125em;
}
/* Link-post links */
h1.listing a[href^="http"] {
	transform: scale(0.75);
	margin: 0 0 0 -0.125em;
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

/*=-----------------------=*/
/*= In-listing edit links =*/
/*=-----------------------=*/

h1.listing .edit-post-link {
	position: absolute;
	margin: 0;
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

/*********************/
/* LISTING POST-META */
/*********************/

h1.listing + .post-meta {
	position: relative;
	justify-content: flex-start;

	font-family: var(--GW-listings-post-meta-font);
	font-weight: var(--GW-listings-post-meta-font-weight);
}

h1.listing + .post-meta .post-section {
	width: 0;
	margin: 0;
	overflow: hidden;
}
h1.listing + .post-meta .post-section::before {
	position: absolute;
	left: -1.625em;
}

h1.listing + .post-meta .read-time {
	cursor: default;
}

/********************/
/* POSTS & COMMENTS */
/********************/

.post-meta > *,
.comment-meta > * {
	display: inline-block;
	margin-right: 1em;
	font-size: 1.0625em;
	white-space: nowrap;
}

.body-text {
	overflow-wrap: break-word;
	text-align: justify;

	font-family: var(--GW-body-text-font);
	font-weight: var(--GW-body-text-font-weight);
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
		pointer-events: none;
		display: block;
		left: 6px;
		max-width: calc(100% - 12px);
		line-height: 1.15;
		white-space: normal;
		text-align: center;
		font-size: 0.875rem;
		opacity: 0;
		transition: opacity 0.2s ease;
	}
	.post .karma.active-controls:hover::after,
	.comment-item .karma.active-controls:hover::after {
		opacity: 1.0;
	}

	.post .karma .karma-value::after,
	.comment-item .karma .karma-value::after {
		content: attr(title);
		position: absolute;
		pointer-events: none;
		display: block;
		left: 50%;
		transform: translateX(-50%);
		white-space: nowrap;
		text-align: center;
		font-size: 0.875rem;
		color: #bbb;
		opacity: 0;
		transition: opacity 0.2s ease;
	}
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
	.post-meta .comment-count span,
	.post-meta .lw2-link span {
		display: none;
	}
	.post-meta .comment-count::before {
		content: "\F086\2004";
		font-family: var(--GW-Font-Awesome);
		font-size: 0.875em;
		font-weight: 400;
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
.post .post-meta {
	position: relative;
	z-index: 2;
}
.post .bottom-post-meta {
	padding: 20px 0 22px 0;
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

.post .post-meta .lw2-link {
	width: 0;
	visibility: hidden;
}
.post .post-meta .lw2-link::before {
	visibility: visible;
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
	font-size: 1.125em;
	margin: 0.5em 0 0 0;
}
.post.link-post .post-body > p:only-child {
	font-size: 1.5em;
	margin: 1em 0;
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
	transform: scale(0.75) translateX(-0.1em);
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
.comments .comment-thread > li {
	position: relative;
}
.listings > .comment-thread:first-child > .comment-item {
	margin-top: 0;
}

.comment-item {
	margin: 2em 0 0 0;
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
	font-size: 1.375em;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	.comment-body ul {
		padding-left: 30px;
	}
	.comment-body ol {
		padding-left: 30px;
	}
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
	content: "(Expand " attr(data-child-count) "	below)";
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
		width: 16px;
		height: calc(100% + 2px);
		top: -1px;
		left: -17px;
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

/*	Comment popups.
	*/
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
	}
	.comment-popup .comment-parent-link {
		display: none;
	}
	.comment-popup .comment-body {
		font-size: 1.0625rem;
	}
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
.comment-meta a.comment-parent-link span::before,
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
	padding: 0 0 0 2px;
	background-size: 100%;
	position: relative;
	top: 2px;
	opacity: 0.5;
}
.comment-meta .permalink::before {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/anchor-white-on-blue.gif")) ?>');
}
.comment-meta .lw2-link::before,
.post .post-meta .lw2-link::before {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/lw-white-on-blue.gif")) ?>');
}
.comment-meta a.comment-parent-link span::before {
	left: unset;
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/up-arrow-white-on-blue.gif")) ?>');
}
.comment-meta .permalink:hover::before {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/anchor-blue-on-white.gif")) ?>');
}
.comment-meta .lw2-link:hover::before,
.post .post-meta .lw2-link:hover::before {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/lw-blue-on-white.gif")) ?>');
}
.comment-meta a.comment-parent-link span:hover::before {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/up-arrow-blue-on-white.gif")) ?>');
}
.comment-meta .permalink:hover::before,
.comment-meta .lw2-link:hover::before,
.comment-meta a.comment-parent-link span:hover::before,
.post .post-meta .lw2-link:hover::before {
	box-shadow: 
		0 0 0 2px #00e,
		0 0 0 3px transparent;
	opacity: 1.0;
	filter: unset;
}
.comment-meta .permalink:active::before,
.comment-meta .lw2-link:active::before,
.comment-meta a.comment-parent-link span:active::before,
.post .post-meta .lw2-link:active::before {
	transform: scale(0.9);
}

.comment-meta .permalink,
.comment-meta .lw2-link,
.comment-meta a.comment-parent-link span,
.post .post-meta .lw2-link {
	position: relative;
	opacity: 1.0;
}
.comment-meta .permalink::after,
.comment-meta .lw2-link::after,
.comment-meta a.comment-parent-link span::after,
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
	max-height: 36px;
}

/*=-----------------------------=*/
/*= Ellipsis (“...”) hover area =*/
/*=-----------------------------=*/

#content.compact .comment-item::after {
	content: "…";
	position: absolute;
	right: 0;
	bottom: 0;
	font-size: 2rem;
	line-height: 1;
	padding: 0 16px 10px 64px;
	pointer-events: auto;

	color: var(--GW-hyperlink-color);
}

#content.compact .comment-meta {
	white-space: nowrap;
	overflow: hidden;
	text-overflow: ellipsis;
	padding: 2px 10px;
}
#content.compact .comment-meta a {
	pointer-events: auto;
}

/*=---------------------------=*/
/*= Expand on hover/focus/tap =*/
/*=---------------------------=*/

@media only screen and (hover: hover) and (pointer: fine) {
	#content.compact .comment-item::after {
		padding: 0 16px 10px 64px;
	}
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
	#content.compact .comment-item::after {
		height: 100%;
		padding: 18px 16px 10px 24px;
	}
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
		background-color: rgba(0,0,0,0.5);
	}
}

/*****************************/
/* HIGHLIGHTING NEW COMMENTS */
/*****************************/

.new-comment::before {
	content: "";
	position: absolute;
	width: 100%;
	height: 100%;
	z-index: 5000;
	pointer-events: none;
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
	.comment-minimize-button{
		right: 3px;
	}
}

/*****************/
/* IGNORE SYSTEM */
/*****************/

.comment-item.ignored {
	overflow: hidden;
}
#content.index-page .comment-item.ignored,
#content.inbox-user-page .comment-item.ignored {
	max-height: var(--GW-comment-compact-height);
}
#content.comment-thread-page .comment-item.ignored:not(.maximized) {
	height: var(--GW-comment-compact-height);
}

/*	Ignored comment author.
	*/
.comment-item.ignored .author {
	text-decoration: line-through;
}

/***********************************/
/* INDIVIDUAL COMMENT THREAD PAGES */
/***********************************/

.individual-thread-page h1.post-title {
	margin: 0.75em var(--GW-current-page-content-right-side-padding) 3px var(--GW-current-page-content-right-side-padding);
	text-align: left;
	font-size: 2.25em;
}

/****************/
/* VOTE BUTTONS */
/****************/

.vote {
	margin: 0;
}
.vote {
	font-family: var(--GW-Font-Awesome);
	font-weight: 900;
	border: none;
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
	font-size: 0.9375em;
}

/**************************/
/* QUALIFIED HYPERLINKING */
/**************************/

#content.no-comments .comments, 
#content.no-comments .post-meta .comment-count,
#content.no-comments .post-meta .karma,
#content.no-comments + #ui-elements-container #new-comment-nav-ui,
#content.no-comments + #ui-elements-container #hns-date-picker,
#content.no-comments + #ui-elements-container #quick-nav-ui {
	display: none;
}

#content.no-nav-bars #primary-bar,
#content.no-nav-bars #secondary-bar {
	display: none;
}
#content.no-nav-bars {
	margin: 8px auto;
}
#content.no-nav-bars + #ui-elements-container > * {
	padding-top: 8px;
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
	top: 40px;
	left: -70px;
	width: 1.5em;
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
}
.post-meta .qualified-linking input[type='checkbox'] ~ .qualified-linking-toolbar {
	display: none;
}
.post-meta .qualified-linking input[type='checkbox']:checked ~ .qualified-linking-toolbar {
	display: block;
}
#qualified-linking-toolbar-toggle-checkbox-bottom ~ .qualified-linking-toolbar {
	top: unset;
	bottom: 125%;
}

.post-meta .qualified-linking-toolbar a {
	display: block;
	padding: 0 6px;
	margin: 4px;
	text-align: center;
	user-select: none;
}