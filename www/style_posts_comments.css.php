/******************/
/* LISTINGS PAGES */
/******************/

.listings {
	padding: 15px var(--GW-content-side-padding) 0 var(--GW-content-side-padding);
}
#content.user-page .listings {
	padding: 0 var(--GW-user-page-content-side-padding);
}
#content.recent-comments-page .listings {
	padding: 0 var(--GW-recent-comments-page-content-side-padding);
}
#content.conversation-page .listings {
	padding: 0 var(--GW-conversation-page-content-side-padding) 0 var(--GW-conversation-page-content-side-padding);
}

/*=----------------=*/
/*= Sequence pages =*/
/*=----------------=*/

#content.sequence-page article {
	padding: 0 var(--GW-sequence-page-content-side-padding) 15px var(--GW-sequence-page-content-side-padding);
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

/* Adds hysteresis to the hover area (i.e., prevents oscillation due to small
   mouse movements) */

	h1.listing a[href^='/posts/']:hover::before,
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

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

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
.post-meta .lw2-link {
	opacity: 0.5;
	order: 1;
}
.post-meta .post-section {
	order: -1;
	margin: 0;
	visibility: hidden;
}
.post-meta .post-section::before,
.comment-meta .alignment-forum {
	visibility: visible;
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
	.author:not(.redacted)::before {
		content: attr(data-full-name);
		position: absolute;
		pointer-events: none;
		display: block;
		padding: 0 1em;
		left: 50%;
		bottom: 2em;
		transform: translateX(-50%);
		white-space: nowrap;
		text-align: center;
		font-size: 0.875rem;
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
	.post-meta .lw2-link span,
	.post-meta .karma-value span,
	.post-meta .comment-count span {
		display: none;
	}
	.post-meta .comment-count::before {
		content: "\F086";
		font-family: var(--GW-Font-Awesome);
		font-size: 0.875em;
		margin: 0 0.25em 0 0;
		font-weight: 400;
	}
}

/*********/
/* POSTS */
/*********/

.post {
	max-width: 100%;
}

.post-body {
	min-height: 8em;
	line-height: 1.5;
	font-size: 1.3rem;
	overflow: auto;
	margin: 0.5em 30px 0 30px;
}
h1.post-title {
	margin: 1.1em 30px 0.35em 30px;
	text-align: center;
	font-size: 2.5em;
	line-height: 1;

	font-family: var(--GW-post-title-font);
	font-weight: var(--GW-post-title-font-weight);
}
.post .post-meta {
	text-align: center;
	position: relative;
	z-index: 2;
}
.post .top-post-meta:last-child {
	margin-bottom: 40px;
}
.post .bottom-post-meta {
	margin: 0;
	padding: 20px 0 22px 0;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	.post-body,
	h1.post-title {
		padding: 0 6px;
	}
}
@media only screen and (max-width: 520px) {
	.post-body {
		font-size: 1.2rem;
		line-height: 1.45;
	}
	h1.post-title {
		font-size: 2em;
	}
}

/**************/
/* LINK POSTS */
/**************/

.post.link-post > .post-body > p:first-child {
	text-align: center;
	font-size: 1.125em;
	margin: 0.5em 0 0 0;
}
.post.link-post > .post-body > p:only-child {
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
	font-size: 0.75em;
	position: relative;
	top: -2px;
	margin-right: 0.25em;
}

/************/
/* COMMENTS */
/************/

.comments {
	max-width: 100%;
	padding: 0 0 1px 0;
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
.conversation-page .comment-meta .comment-post-title {
	margin: 0;
	flex-basis: unset;
	flex: 1 0 auto;
	text-align: right;
	display: none;	/* Not sure if we need to display this... */
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
	.comment-meta .comment-parent-link {
		opacity: 1.0;
	}
}
@media only screen and (max-width: 520px) {
	.comment-meta {
		padding: 2px 10px;
		position: relative;
	}
	.comment-meta .author {
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

a.comment-parent-link:not(.inline-author),
a.comment-parent-link.inline-author::before {
	opacity: 0.5;
}

/*	Hover highlighting.
	*/
@media only screen and (hover: hover) and (pointer: fine) {
	a.comment-parent-link:hover {
		opacity: 1.0;
	}
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
	a.comment-parent-link {
		width: 0;
		visibility: hidden;
		position: relative;
	}
	a.comment-parent-link::before {
		padding: 0;
		font-size: 1em;
		left: 0;
		top: 0;
		line-height: inherit;
		visibility: visible;
		content: "\F3BF";
		transform: scaleX(-1);
		width: 2em;
		text-align: center;
	}
}
@media only screen and (max-width: 520px) {
	a.comment-parent-link {
		position: static;
	}
	a.comment-parent-link::before {
		padding: 6px;
		left: unset;
		right: 0;
		top: unset;
		bottom: 0;
		height: 2em;
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
.individual-thread-page a.comment-parent-link:empty::before {
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
.comment-meta .lw2-link::before {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/lw-white-on-blue.gif")) ?>');
}
.individual-thread-page a.comment-parent-link:empty::before {
	left: unset;
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/up-arrow-white-on-blue.gif")) ?>');
}
.comment-meta .permalink:hover::before {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/anchor-blue-on-white.gif")) ?>');
}
.comment-meta .lw2-link:hover::before {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/lw-blue-on-white.gif")) ?>');
}
.individual-thread-page a.comment-parent-link:empty:hover::before {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/up-arrow-blue-on-white.gif")) ?>');
}
.comment-meta .permalink:hover::before,
.comment-meta .lw2-link:hover::before,
.individual-thread-page a.comment-parent-link:empty:hover::before {
	box-shadow: 
		0 0 0 2px #00e,
		0 0 0 3px transparent;
	opacity: 1.0;
	filter: unset;
}
.comment-meta .permalink:active::before,
.comment-meta .lw2-link:active::before,
.individual-thread-page a.comment-parent-link:empty:active::before {
	transform: scale(0.9);
}

.comment-meta .permalink,
.comment-meta .lw2-link,
.individual-thread-page .comment-parent-link:empty {
	position: relative;
	opacity: 1.0;
}
.comment-meta .permalink::after,
.comment-meta .lw2-link::after,
.individual-thread-page .comment-parent-link:empty::after {
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
#content.compact .comment-thread:hover {
	z-index: 1;
}
#content.compact .comment-thread .comment-body {
	font-size: 1.0625rem;
}
#content.compact .comment-thread .comment-item,
#content.index-page .comment-item.ignored,
#content.inbox-user-page .comment-item.ignored {
	max-height: 61px;
	margin-top: 1em;
	overflow: hidden;
	position: relative;
}
#content.compact .comment-thread .comment-item {
	pointer-events: none;
}
#content.compact .comment-thread .comment-item::after {
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
@media only screen and (hover: hover) and (pointer: fine) {
	#content.compact:not(:focus-within) .comment-thread .comment-item:hover,
	#content.compact .comment-thread .comment-item.expanded {
		overflow: visible;
		pointer-events: auto;
		z-index: 10;
	}
}
@media not screen and (hover: hover) and (pointer: fine) {
	#content.compact .comment-thread.expanded .comment-item {
		overflow: visible;
		pointer-events: auto;
		z-index: 10;
	}
}
#content.compact .comment-thread .comment-item .comment-meta {
	white-space: nowrap;
	overflow: hidden;
	text-overflow: ellipsis;
	padding: 2px 10px;
}
#content.compact .comment-thread .comment-item:hover .comment-meta {
	white-space: unset;
}
#content.compact .comment-thread .comment-item .comment-meta a {
	pointer-events: auto;
}
#content.compact .comment-thread .comment-item .comment-meta .comment-post-title {
	display: inline;
}
#content.compact .comment-thread .comment-item .comment-meta .karma + .comment-post-title {
	margin-left: 0.75em;
}
@media only screen and (hover: hover) and (pointer: fine) {
	#content.compact .comment-thread:last-of-type .comment-item:hover,
	#content.compact .comment-thread:last-of-type .comment-item.expanded {
		max-height: unset;
	}
	#content.compact .comment-thread .comment-item:hover .comment,
	#content.compact .comment-thread .comment-item.expanded .comment {
		position: relative;
		z-index: 1;
		margin-bottom: 2em;
		bottom: 0;
	}
	#content.compact .comment-thread .comment-item:hover .comment::before,
	#content.compact .comment-thread .comment-item.expanded .comment::before{
		content: "";
		position: absolute;
		display: block;
		width: calc(100% + 20px);
		height: calc(100% + 20px);
		z-index: -1;
		top: -10px;
		left: -10px;
	}
	#content.compact .comment-thread:last-of-type .comment-item:hover .comment,
	#content.compact .comment-thread:last-of-type .comment-item.expanded .comment{
		margin: 0;
	}
}
@media not screen and (hover: hover) and (pointer: fine) {
	#content.compact .comment-thread.expanded:last-of-type .comment-item {
		max-height: unset;
	}
	#content.compact .comment-thread.expanded .comment-item .comment {
		position: relative;
		z-index: 1;
		margin-bottom: 2em;
		bottom: 0;
	}
	#content.compact .comment-thread.expanded .comment-item .comment::before {
		content: "";
		position: absolute;
		display: block;
		width: calc(100% + 14px);
		height: calc(100% + 20px);
		z-index: -1;
		top: -10px;
		left: -10px;
	}
	#content.compact .comment-thread.expanded:last-of-type .comment-item .comment {
		margin: 0;
	}
	#content.compact .comment-thread.expanded .comment-item .comment::after {
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
	top: 1px;
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

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 520px) {
	.comment-minimize-button{
		right: 2px;
	}
}

/*****************/
/* IGNORE SYSTEM */
/*****************/

#content.comment-thread-page .comment-item.ignored {
	height: 38px;
	overflow: hidden;
}
.comment-item.ignored > .comment > .comment-meta > .author {
	text-decoration: line-through;
}

/***********************************/
/* INDIVIDUAL COMMENT THREAD PAGES */
/***********************************/

.individual-thread-page > h1 {
	line-height: 1;
	margin: 0.75em 0 3px 0;
}
.individual-thread-page .comments {
	border: none;
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

/*****************************/
/* COMMENTING AND POSTING UI */
/*****************************/

.comment-controls {
	text-align: right;
	margin: 0 8px 8px 16px;
	position: relative;
	z-index: 9999;
}
.comment-thread .comment-controls + .comment-thread > li:first-child {
	margin-top: 8px;
}
.comments > .comment-controls {
	margin: 8px 0 0 0;
}
.comments > .comment-controls:last-child {
	margin: 8px 0 16px 0;
}

.posting-controls input[type='submit'],
.comment-controls .cancel-comment-button,
.new-comment-button {
	font-weight: var(--GW-UI-font-weight-heavy);
}

.posting-controls input[type='submit'] {
	margin: 6px;
	padding: 4px 10px;
	font-size: 1.125rem;
}

.comment-controls .cancel-comment-button {
	position: absolute;
	right: 0;
	margin: 0;
	height: 27px;
	font-size: inherit;
	padding: 4px 8px 2px 4px;
	z-index: 1;
}
.comment-controls .cancel-comment-button::before {
	font-family: var(--GW-Font-Awesome);
	margin-right: 3px;
	content: '\F00D';
	font-weight: 900;
	font-size: 0.9em;
	opacity: 0.7;
}

.comment + .comment-controls .action-button {
	font-weight: var(--GW-UI-font-weight-light);
	font-size: 1.0625em;
	padding: 1px 6px;
}
.comment-controls .action-button::before {
	font-family: var(--GW-Font-Awesome);
	margin-right: 3px;
}
.new-comment-button {
	font-size: 1.5rem;
	margin: 0 0.25em;
}
.comment-controls .reply-button::before {
	content: '\F3E5';
	font-weight: 900;
	font-size: 0.9em;
	opacity: 0.6;
}

.post-controls {
	text-align: right;
	margin: 0.75em 0 0 0;
	grid-row: 3;
	align-self: start;
	justify-self: end;
}
.post {
	grid-row: 3;
}
.edit-post-link {
	display: inline-block;
	margin-bottom: 0.25em;
	font-size: 1.125rem;
}
.edit-post-link::before {
	margin-right: 0.3em;
}
.comment-controls .edit-button::before,
.edit-post-link::before {
	content: '\F303';
	font-family: var(--GW-Font-Awesome);
	font-weight: 900;
	font-size: 0.75em;
	position: relative;
	top: -1px;
}

.comment-controls .delete-button {
	margin-right: 0.25em;
}
.comment-controls .edit-button,
.comment-controls .retract-button,
.comment-controls .unretract-button {
	margin-right: 1em;
}
.comment-controls .retract-button::before {
	content: '\F4B3';
	opacity: 0.6;
}
.comment-controls .unretract-button::before {
	content: '\F075';
	opacity: 0.9;
}
.comment-controls .delete-button::before {
	content: '\F05E';
	opacity: 0.7;
}
.comment-controls .retract-button::before,
.comment-controls .unretract-button::before,
.comment-controls .delete-button::before {
	font-weight: 900;
	font-size: 0.9em;
}

.comment-controls form {
	position: relative;
}
.textarea-container {
	position: relative;
}
.posting-controls textarea {
	display: block;
	width: 100%;
	height: 15em;
	min-height: 15em;
	max-height: calc(100vh - 6em);
	margin: 2px 0 0 0;
	padding: 4px 5px;
	font-size: 1.2rem;
	border-style: solid;
	border-width: 29px 1px 1px 1px;
	resize: none;

	font-family: var(--GW-editor-font);
	font-weight: var(--GW-editor-font-weight);
}

/* GUIEdit buttons */

.guiedit-buttons-container {
	position: absolute;
	left: 1px;
	top: 1px;
	width: calc(100% - 2px);
	height: 28px;
	text-align: left;
	padding: 1px 4px 0 4px;
	overflow: hidden;
}
.comment-thread-page .guiedit-buttons-container {
	padding-right: 60px;
}
.guiedit-buttons-container button {
	height: 26px;
	padding: 0 7px;
	font-weight: 900;
	font-size: 0.875rem;
	line-height: 1;
	position: static;

	font-family: var(--GW-Font-Awesome), var(--GW-body-text-font);
}
.guiedit-buttons-container button:active {
	transform: none;
}
.guiedit-buttons-container button:active div {
	transform: scale(0.9);
}
.guiedit-buttons-container button sup {
	font-weight: var(--GW-UI-font-weight-heavy);
}
button.guiedit {
	font-family: var(--GW-UI-font), var(--GW-Font-Awesome);
}
.guiedit::after {
	content: attr(data-tooltip);
	position: absolute;
	font-size: 1rem;
	top: 2px;
	left: 464px;
	height: 25px;
	padding: 4px 0;
	white-space: nowrap;
	visibility: hidden;

	font-family: var(--GW-UI-font);
	font-weight: var(--GW-UI-font-weight-light);
}
.guiedit:hover::after {
	visibility: visible;
}

/* Markdown hints */

.posting-controls .markdown-reference-link {
	float: left;
	padding: 1px 0 0 6px;
}
.posting-controls .markdown-reference-link a {
	padding-right: 1.5em;
	margin-right: 0.15em;
	background-image: url('data:image/png;base64,<?php echo base64_encode(file_get_contents("assets/markdown.png")); ?>');
	background-size: 1.25em;
	background-repeat: no-repeat;
	background-position: right center;
}

#markdown-hints-checkbox + label {
	float: left;
	margin: 2px 0 0 1em;
	line-height: 1.3;
	cursor: pointer;

	color: var(--GW-hyperlink-color);
}
#markdown-hints-checkbox + label:hover,
#markdown-hints-checkbox + label:focus {
	color: var(--GW-hyperlink-hover-color);
}
#markdown-hints-checkbox + label:active {
	color: var(--GW-hyperlink-active-color);
}
#edit-post-form #markdown-hints-checkbox + label {
	padding: 0;
}
#markdown-hints-checkbox {
	visibility: hidden;
	float: left;
}
#markdown-hints-checkbox + label::after {
	content: "(Show Markdown help)";
}
#markdown-hints-checkbox:checked + label::after {
	content: "(Hide Markdown help)";
}
#markdown-hints-checkbox + label::before {
	content: '\F059';
	font-family: var(--GW-Font-Awesome);
	font-weight: 900;
	margin-right: 3px;
}
#markdown-hints-checkbox:checked + label::before {
	font-weight: 400;
}
#markdown-hints {
	margin: 4px 0 0 4px;
	padding: 4px 8px;
	position: absolute;
	text-align: left;
	top: calc(100% - 1em);
	z-index: 1;
	display: none;
}
.comment-controls #markdown-hints {
	top: calc(100% + 1.75em);
}
#markdown-hints-checkbox:checked ~ #markdown-hints {
	display: table;
}
.markdown-hints-row {
	display: table-row;
}
#markdown-hints .markdown-hints-row span,
#markdown-hints .markdown-hints-row code {
	float: none;
	display: table-cell;
	border: none;
	background-color: inherit;
	padding: 0 12px 0 0;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	.comment-controls .cancel-comment-button {
		max-width: 1.3em;
		overflow: hidden;
		margin-right: 0.125em;
	}
	.comment-controls .edit-button::before {
		font-size: 0.9375em;
	}
	.comments > .comment-controls .cancel-comment-button {
		right: 8px;	
	}
	.comment-controls .cancel-comment-button::before {
		font-size: 1.25rem;		
	}
}
@media only screen and (max-width: 520px) {
	.comment-controls {
		position: static;
	}
	.comment-controls:focus-within {
		z-index: 10001;
	}
	.comment-controls .cancel-comment-button {
		right: 10px;
	}
	.textarea-container:focus-within textarea {
		position: fixed;
		top: 0;
		left: 2px;
		width: calc(100vw - 4px);
		height: calc(100% - 100px);
		min-height: unset;
		max-height: unset;
		border-width: 1px;
		z-index: 11001;
	}
	#content.conversation-page .textarea-container:focus-within textarea {
		height: calc(100% - 54px);
	}
	#content.conversation-page .textarea-container:focus-within::after {
		content: "";
		display: block;
		width: 100%;
		height: 50px;
		position: fixed;
		left: 0;
		bottom: 0;
		z-index: 11000;
	}
	.textarea-container:focus-within .guiedit-buttons-container {
		position: fixed;
		z-index: 11002;
		left: 0;
		width: 100vw;
		height: auto;
		background-image: none;
		padding: 3px 4px 4px 4px;
		margin: 0;
		text-align: center;
		top: auto;
		bottom: 0;
	}
	.textarea-container:focus-within button.guiedit {
		font-size: 0.9375rem;
		line-height: 1.5;
		height: auto;
		width: calc((100% / 10) - 2px);
		padding: 10px 1px 8px 0;
		position: relative;
		margin: 1px;
	}
	.textarea-container:focus-within .guiedit-mobile-auxiliary-button {
		z-index: 11011;
		position: fixed;
		bottom: 7px;
		width: calc(((100% - 16px) / 10) * 2.5 - 7px);
		font-size: 1.25rem;
		padding: 5px 5px 6px 5px;
		display: block;

		font-weight: var(--GW-UI-font-weight-heavy);
	}
	.textarea-container:focus-within button.guiedit sup {
		position: absolute;
		left: calc(50% + 0.65em);
		top: calc(50% - 1.3em);
	}
	.textarea-container:focus-within .guiedit-mobile-help-button {
		left: 8px;
	}
	.textarea-container:focus-within .guiedit-mobile-exit-button {
		right: 8px;
	}
	.guiedit::after {
		display: none;
	}

	#markdown-hints,
	#edit-post-form #markdown-hints {
		z-index: 11111;
		position: fixed;
		top: 40px;
		left: 0;
		right: 0;
		margin: auto;
		padding: 4px 0 4px 8px;
		width: 310px;
		border-width: 3px;
		border-style: double;
		pointer-events: none;
	}
	#markdown-hints::after {
		content: "(Type to hide this help box.)";
		color: #090;
		display: block;
		margin: 12px 18px 13px 10px;
		padding: 5px;
		font-size: 0.9em;
		text-align: center;
	}
}

/******************/
/* EDIT POST FORM */
/******************/

#edit-post-form {
	padding: 1em 1em 4em 1em;
}
#edit-post-form .post-meta-fields {
	display: grid;
	grid-template-columns: 5em auto auto auto 1fr auto;
	margin-bottom: 0.625em;
}

#edit-post-form label[for='title'],
#edit-post-form label[for='url'],
#edit-post-form label[for='section'] {
	grid-column: 1;
}
#edit-post-form input[type='text'] {
	padding: 0.25em;
	grid-column: 2 / span 4;
	margin-bottom: 0.5em;
}

#edit-post-form .link-post-checkbox,
#edit-post-form .link-post-checkbox + label {
	grid-row: 1;
	grid-column: 6;
}
#edit-post-form .question-checkbox,
#edit-post-form .question-checkbox + label {
	grid-row: 3;
	grid-column: 5;
	justify-self: start;
	margin-left: 1.5em;
}

#edit-post-form .post-meta-fields input[type='checkbox'] {
	height: 0;
	opacity: 0;
	pointer-events: none;
}
#edit-post-form .post-meta-fields input[type='checkbox'] + label {
	white-space: nowrap;
	position: relative;
	cursor: pointer;
	padding: 0.25em 0.5em 0.25em calc(20px + 0.25em + 0.3725em);
	align-self: start;
}
#edit-post-form .post-meta-fields input[type='checkbox'] + label::before {
	content: "";
	font-family: var(--GW-Font-Awesome);
	font-size: 1.375rem;
	line-height: 0.7;
	text-indent: 1px;
	font-weight: 900;
	position: absolute;
	width: 20px;
	height: 20px;
	left: 5px;
}
#edit-post-form label[for='url'],
#edit-post-form input[name='url'] {
	display: none;
}
#edit-post-form .link-post-checkbox:checked ~ label[for='url'],
#edit-post-form .link-post-checkbox:checked ~ input[name='url'] {
	display: initial;
}
#edit-post-form label {
	line-height: normal;
	border: 1px solid transparent;
	text-align: right;
    padding: 0.25em 0.5em;
    white-space: nowrap;
}
#edit-post-form input[type='radio'] {
	width: 0;
	margin: 0;
	opacity: 0;
	pointer-events: none;

	font-weight: var(--GW-UI-font-weight-heavy);
}
#edit-post-form input[type='radio'] + label {
	padding: 4px 12px;
	text-align: center;
	border-style: solid;
	border-width: 1px 1px 1px 0;
	cursor: pointer;
}
#edit-post-form input[type='radio']:checked + label {
	cursor: default;
}

#edit-post-form label[for='section'] {
	grid-row: 3;
}
#edit-post-form input[type='radio'] + label {
	grid-row: 3;
}
<?php fit_content("#edit-post-form input[type='radio'] + label"); ?>

#edit-post-form textarea {
	min-height: 24em;
}

#edit-post-form input[type='submit'] {
	padding: 6px 12px;
	float: right;
}
#edit-post-form #markdown-hints {
	top: calc(100% + 2em);
}

#edit-post-form button.guiedit div {
	overflow: visible;
}
.guiedit-mobile-auxiliary-button {
	display: none;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 520px) {
	#edit-post-form {
		padding-bottom: 0;
	}
	#edit-post-form .post-meta-fields {
		grid-template-columns: 4.5em auto auto auto 1fr auto;
	}
	#edit-post-form label[for='url'], 
	#edit-post-form label[for='section'],
	#edit-post-form label[for='title'] {
		padding-left: 0;
	}
	#edit-post-form .post-meta-fields input[type='checkbox'] + label {
		white-space: normal;
		line-height: 0.9;
		top: -1px;
		font-family: var(--GW-Font-Awesome);
		font-weight: 900;
		justify-self: start;
	}
	#edit-post-form .post-meta-fields .question-checkbox,
	#edit-post-form .post-meta-fields .question-checkbox + label {
		grid-column: 6;
		margin-left: unset;
	}
	#edit-post-form .post-meta-fields input[type='radio'] + label {
		align-self: start;
	}
	#edit-post-form .textarea-container:focus-within textarea {
		height: calc(100% - 101px);
		min-height: unset;
	}

	#markdown-hints-checkbox,
	#markdown-hints-checkbox + label {
		display: none;
	}

	#edit-post-form div:last-child {
		clear: both;
		overflow: auto;
	}
	#edit-post-form input[type='submit'] {
		float: none;
		display: block;
		font-size: 1.5rem;
		margin: 1rem auto 1.5rem auto;
		padding: 6px 12px 8px 12px;
	}
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

#aux-about-link {
	position: fixed;
	top: 40px;
	left: calc((100% - 900px) / 2 - 69px);
	width: 1.5em;
	height: 1.5em;
	text-align: center;
	display: table;
}
#aux-about-link a {
	display: table-cell;
	width: 100%;
	vertical-align: middle;
	font-family: var(--GW-Font-Awesome);
	font-weight: 900;
	font-size: 1.25rem;
	opacity: 0.4;
	z-index: 1;
}

.qualified-linking {
	margin: 0;
	position: relative;
}
.qualified-linking input[type='checkbox'] {
	visibility: hidden;
	width: 0;
	height: 0;
	margin: 0;
}
.qualified-linking label {
	font-family: var(--GW-Font-Awesome);
	font-weight: 900;
	font-size: 1rem;
	padding: 0 0.5em;
	display: inline-block;
	margin-left: 0.25em;
}
.qualified-linking label:hover {
	cursor: pointer;
}
.qualified-linking label:active span {
	display: inline-block;
	transform: scale(0.9);
}
.qualified-linking label::selection {
	background-color: transparent;
}

.qualified-linking label::after {
	content: "";
	width: 100vw;
	height: 0;
	left: 0;
	top: 0;
	position: fixed;
	z-index: 1;
	cursor: default;
}
.qualified-linking input[type='checkbox']:checked + label::after {
	height: 100vh;
}

.qualified-linking-toolbar {
	position: absolute;
	right: 0.25em;
	top: 110%;
	z-index: 1;
}
.qualified-linking input[type='checkbox'] ~ .qualified-linking-toolbar {
	display: none;
}
.qualified-linking input[type='checkbox']:checked ~ .qualified-linking-toolbar {
	display: block;
}
#qualified-linking-toolbar-toggle-checkbox-bottom ~ .qualified-linking-toolbar {
	top: unset;
	bottom: 125%;
}

.qualified-linking-toolbar a {
	display: block;
	padding: 0 6px;
	margin: 4px;
}
.qualified-linking-toolbar a::selection {
	background-color: transparent;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 520px) {
	.qualified-linking-toolbar {
		right: -5em;
	}
}
