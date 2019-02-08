<?php
	$UI_font = "'Anonymous Pro', monospace";
	$text_font = "'Input Sans', sans-serif"
?>

/*******************/
/* BRUTALIST THEME */
/*******************/

body {
	color: #000;
	background-color: #fff;
	font-family: <?php echo $UI_font; ?>;
}
#content {
	line-height: 1.55;
}
#content::before {
	background-color: #fff;
	box-shadow: 
		-1px 0 0 #000 inset,
		0 2px 0 #000;
	border-style: solid;
	border-color: #000;
	border-width: 0 1px 0 2px;
}
#content.no-nav-bars::before {
	border-width: 2px;
	box-shadow: none;
}

/*=========*/
/* NAV BAR */
/*=========*/

.nav-bar {
	justify-content: flex-start;
}
.nav-bar:nth-of-type(2) {
	border-bottom: 2px solid #000;
}
.nav-inner {
	font-size: 1.375em;
	font-weight: 600;
	border-style: solid;
	border-color: transparent;
	border-width: 0 2px;
}
#secondary-bar .nav-inner {
	font-size: 1em;
}

.nav-current:not(#nav-item-search) .nav-inner,
.nav-bar a.nav-inner:hover {
	box-shadow: 
		0 0 0 4px #fff inset,
		0 0 0 6px #000 inset;
}
.nav-bar a.nav-inner:active {
	box-shadow: 
		0 0 0 8px #fff inset,
		0 0 0 10px #000 inset;
}

#bottom-bar.decorative::before,
#bottom-bar.decorative::after {
	content: "GW";
	display: block;
	text-align: center;
	padding: 0.25em 0 1em 0;
}
#bottom-bar.decorative::before {
	width: 100%;
	color: transparent;
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/one_pixel_DDD.gif")) ?>');
	background-repeat: repeat-x;
	background-position: center 35%;
	margin: 0 30px;
	filter: contrast(90%);
}
#bottom-bar.decorative::after {
	color: #d8d8d8;
	position: absolute;
	left: 0;
	right: 0;
	margin: auto;
	background-color: #fff;
	padding-right: 5px;
	padding-left: 6px;
}
<?php fit_content("#bottom-bar.decorative::after"); ?>

/* Accesskey hints */

.nav-inner::after {
	display: block;
	position: absolute;
	left: 8px;
	top: 3px;
	font-weight: 400;
	font-size: 0.7em;
	color: #d8d8d8;
}
.inactive-bar .nav-inner::after {
	color: #ccc;
}
.nav-inner:hover::after {
	color: #bbb;
}

/* Search tab */

#nav-item-search button {
	border: none;
	font-weight: inherit;
}
#nav-item-search input::placeholder {
	color: #d00;
	font-weight: normal;
}

/* Inbox indicator */

#inbox-indicator {
	text-shadow: 1px 1px 0 #fff, -1px -1px 0 #fff;
}

/*==============*/
/* PAGE TOOLBAR */
/*==============*/

.page-toolbar button,
.page-toolbar button:hover,
.page-toolbar button:active,
.page-toolbar button:focus {
	border: none;
	box-shadow: none;
}
.page-toolbar a:hover,
.page-toolbar button:hover {
	text-decoration: dotted underline;
}
.page-toolbar a:active,
.page-toolbar button:active {
	transform: scale(0.9);
}

.rss::before {
	filter: grayscale(100%);
}

/*==============*/
/* SUBLEVEL NAV */
/*==============*/

.sublevel-nav .sublevel-item {
	border-color: #000;
	border-style: solid;
	border-width: 1px 1px 1px 0;
}
.sublevel-nav .sublevel-item:first-child {
	border-width: 1px;
}
.sublevel-nav .sublevel-item:hover {
	box-shadow: 
		0 0 0 2px #fff inset,
		0 0 0 3px #000 inset;
}
.sublevel-nav .sublevel-item:active {
	box-shadow: 
		0 0 0 4px #fff inset,
		0 0 0 5px #000 inset;
	font-weight: normal;
}
.sublevel-nav .sublevel-item:disabled,
.sublevel-nav span.sublevel-item {
	box-shadow: 
		0 0 0 2px #fff inset,
		0 0 0 3px #000 inset;
	font-weight: bold;
	color: inherit;
}

/*=====================*/
/* SORT ORDER SELECTOR */
/*=====================*/

.sublevel-nav.sort {
	padding: 18px 0 0 0;
	border: 2px solid #000;
}
.sublevel-nav.sort::before {
	text-transform: uppercase;
	font-weight: 600;
}
.sublevel-nav.sort .sublevel-item {
	padding: 7px 8px 6px 9px;
	text-transform: uppercase;
	border: none;
}

/*================*/
/* WIDTH SELECTOR */
/*================*/

#width-selector button {
	box-shadow:
		0 0 0 4px #fff inset,
		0 0 0 5px #000 inset;
}
#width-selector button:hover,
#width-selector button.selected {
	box-shadow:
		0 0 0 1px #fff inset,
		0 0 0 2px #000 inset,
		0 0 0 4px #fff inset,
		0 0 0 5px #000 inset;
}
#width-selector button::after {
	color: #000;
	font-size: 0.9em;
}

/*================*/
/* THEME SELECTOR */
/*================*/

#theme-selector button {
	box-shadow:
		0 0 0 5px #fff inset;
}
#theme-selector button:hover,
#theme-selector button.selected {
	box-shadow:
		0 0 0 2px #fff inset,
		0 0 0 3px #000 inset,
		0 0 0 5px #fff inset;
}

#theme-selector button::before {
	font-size: 0.9375em;
	padding: 6px;
	width: 7em;
	color: #aaa;
	background-color: #fff;
}
#theme-selector button:hover::before,
#theme-selector button.selected::before {
	color: #000;
}

/*======================*/
/* ANTI-KIBITZER TOGGLE */
/*======================*/

#anti-kibitzer-toggle button::before,
#anti-kibitzer-toggle button::after {
	background-color: #999;
	-webkit-background-clip: text;
	color: transparent;
	text-shadow: rgba(255,255,255,0.5) 0px 1px 1px;
}
#anti-kibitzer-toggle button:hover {
	box-shadow: none;
}
#anti-kibitzer-toggle button:hover::before,
#anti-kibitzer-toggle button:hover::after {
	background-color: #000;
	text-shadow: rgba(255,255,255,0.2) 0px 1px 1px;
}

/*=========================*/
/* TEXT SIZE ADJUSTMENT UI */
/*=========================*/

#text-size-adjustment-ui button:hover,
#text-size-adjustment-ui button:active,
#text-size-adjustment-ui button:focus {
	box-shadow: none;
	color: #777;
}
#text-size-adjustment-ui::after {
	color: #000;
}

/*======================*/
/* THEME TWEAKER TOGGLE */
/*======================*/

#theme-tweaker-toggle button:hover,
#theme-tweaker-toggle button:active {
	box-shadow: none;
}

/*=================*/
/* QUICKNAV WIDGET */
/*=================*/

#quick-nav-ui a {
	color: #000;
	box-shadow: 
		0 0 0 1px #fff,
		0 0 0 3px #000;
}
#quick-nav-ui a[href='#bottom-bar'] {
	line-height: 1.8;
}
@media only screen and (hover: hover), not screen and (-moz-touch-enabled) {
	#quick-nav-ui a:hover {
		box-shadow: 
			0 0 0 1px #fff inset,
			0 0 0 3px #000 inset,
			0 0 0 1px #fff,
			0 0 0 3px #000;
	}
}
#quick-nav-ui a:active {
	box-shadow: 
		0 0 0 3px #fff inset,
		0 0 0 5px #000 inset,
		0 0 0 1px #fff,
		0 0 0 3px #000;
}
#quick-nav-ui a[href='#comments'].no-comments {
	opacity: 0.4;
	color: #bbb;
}

/*======================*/
/* NEW COMMENT QUICKNAV */
/*======================*/

#new-comment-nav-ui .new-comments-count {
	left: 2px;
}
#new-comment-nav-ui .new-comments-count::after {
	position: relative;
	right: 1px;
}
#new-comment-nav-ui .new-comment-sequential-nav-button:disabled {
	color: #bbb;
}

/*=================*/
/* HNS DATE PICKER */
/*=================*/

#hns-date-picker {
	bottom: 75px;
	text-indent: -16px;
}
#hns-date-picker span {
	color: #777;
	font-weight: 600;
	text-indent: 0px;
}
#hns-date-picker input {
	color: #666;
	width: 160px;
}
#hns-date-picker input:focus {
	color: #000;
}

/*======================*/
/* TEXT SIZE ADJUSTMENT */
/*======================*/

/*=============================*/
/* COMMENTS VIEW MODE SELECTOR */
/*=============================*/

/*==========*/
/* ARCHIVES */
/*==========*/

.archive-nav {
	border: 2px solid #000;
}
.archive-nav span[class^='archive-nav-item'],
.archive-nav a:hover {
	font-weight: bold;
	box-shadow: 
		0 0 0 1px #fff inset,
		0 0 0 3px #000 inset;
}
.archive-nav a:active {
	transform: none;
	box-shadow: 
		0 0 0 3px #fff inset,
		0 0 0 5px #000 inset;
}

/*==========*/
/* LISTINGS */
/*==========*/

h1.listing {
	margin: 0.7em 20px 0.1em 20px;
	max-width: calc(100% - 40px);
	font-family: <?php echo $UI_font; ?>, 'Font Awesome';
	font-size: 1.625rem;
}

h1.listing a[href^="http"] {
	font-size: 0.7em;
	top: 6px;
	color: #fff;
	text-shadow: 
		 0.5px 0.5px 0 #000, 
		-0.5px -0.5px 0 #000,
		 0 0 2px #000;
}

@media only screen and (hover: hover), not screen and (-moz-touch-enabled) {
	h1.listing a:hover,
	h1.listing a:focus {
		color: #777;
		background-color: rgba(255,255,255,0.85);
	}	
	h1.listing:focus-within::before {
		color: #000;
		left: -0.625em;
	}
	h1.listing a[href^="http"]:hover {
		color: #fff;
		text-shadow: 
			 0.5px 0.5px 0 #000, 
			-0.5px -0.5px 0 #000,
			 0 0 2px #000,
			 0 0 3px #000;
	}
}

h1.listing .edit-post-link {
	padding: 5px 3px 36px 0.5em;
	top: 0;
	right: 0;
}
h1.listing .edit-post-link:hover {
	text-decoration: none;
}
#content.user-page h1.listing .edit-post-link {
	background-color: #fff;
}

/*===================*/
/* TOP PAGINATION UI */
/*===================*/

#top-nav-bar a:hover {
	color: #777;
}
#bottom-bar #nav-item-last a::before {
	margin-left: -2.3em;
	left: 3.9em;
}

/*= Top pagination UI hover tooltips =*/

#top-nav-bar a::after,
#bottom-bar a::after {
	color: #000;
}
#bottom-bar a:not([href='#top'])::after {
	background-color: #fff;
	left: 0;
	right: 0;
	margin: auto;
	padding: 1px 6px;
	bottom: 2px;
}
<?php fit_content("#bottom-bar a:not([href='#top'])::after"); ?>

/*======*/
/* SPAM */
/*======*/

h1.listing.spam {
	opacity: 0.1;
}
h1.listing.spam + .post-meta {
	opacity: 0.2;
}
h1.listing.spam:hover,
h1.listing.spam + .post-meta:hover,
h1.listing.spam:hover + .post-meta {
	opacity: 1.0;
}

/*===================*/
/* LISTING POST-META */
/*===================*/

h1.listing + .post-meta > * {
	line-height: 1.5;
}
h1.listing + .post-meta .read-time {
	opacity: 0.5;
}

/*============*/
/* USER PAGES */
/*============*/

#content.user-page h1.page-main-heading {
	border-bottom: 1px dotted #000;
}

#content.user-page h1.listing,
#content.user-page h1.listing + .post-meta {
	border-style: solid;
	border-color: #000;
}
#content.user-page h1.listing {
	padding: 0 6px;
	padding-top: 0.25em;
	border-width: 2px 2px 0 2px;
	margin: 1rem 0 0 0;
	max-width: 100%;
}
#content.own-user-page h1.listing,
h1.listing.own-post-listing {
	padding-right: 36px;
}
@media only screen and (hover: hover), not screen and (-moz-touch-enabled) {
	#content.user-page h1.listing:focus-within::before {
		left: -0.625em;
	}
}
#content.user-page h1.listing + .post-meta {
	padding: 0.75em 6px 0.5em 33px;
	border-width: 0 2px 2px 2px;
	margin: 0 0 1rem 0;
}
#content.user-page h1.listing + .post-meta .post-section::before {
	left: -2px;
	top: unset;
}

#content.conversations-user-page h1.listing {
	padding: 8px 6px 8px 10px;
	font-size: 1.5rem;
}
#content.conversations-user-page h1.listing + .post-meta {
	padding: 10px 4px 6px 4px;
	margin: 0;
}

.user-stats .karma-total {
	font-weight: bold;
}

/*===============*/
/* CONVERSATIONS */
/*===============*/

#content.conversation-page textarea {
	border-top-width: 2px;
}

/*============*/
/* LOGIN PAGE */
/*============*/

.login-container form input[type='submit'] {
	font-weight: bold;
}

/* “Log in” form */
#login-form {
    grid-template-columns: 5.75em 1fr;
}

#login-form a:hover {
	text-decoration: dotted underline;
}
#login-form a:active {
	transform: scale(0.9);
}

/* “Create account” form */

#signup-form {
    grid-template-columns: 10.5em 1fr;
	border: 2px solid #000;
}

/* Log in tip */

.login-container .login-tip {
	border: 1px solid #000;
}

/* Message box */

.error-box {
	border: 1px solid red;
	background-color: #faa;
}
.success-box {
	border: 1px solid green;
	background-color: #afa;
}

/*=====================*/
/* PASSWORD RESET PAGE */
/*=====================*/

.reset-password-container label {
	width: 12em;
}
.reset-password-container input[type='submit'] {
	font-weight: bold;
}

/*===================*/
/* TABLE OF CONTENTS */
/*===================*/

.contents {
	font-family: <?php echo $UI_font; ?>;
	border: 2px solid #000;
	background-color: #fff;
}
.contents-head {
	font-weight: bold;
	font-size: 1.125em;
}
.post-body .contents ul {
	font-size: 0.9375em;
}
.contents ul a {
	border-bottom: 2px dotted #999;
}
.post-body .contents li::before {
	color: #999;
	font-feature-settings: "tnum";
}

/*==================*/
/* POSTS & COMMENTS */
/*==================*/

.post-body,
.comment-body {
	font-family: <?php echo $text_font; ?>;
	font-weight: 200;
}
.post-body a,
.comment-body a {
	border-bottom: 2px dotted #000;
}
.post-body a:hover,
.comment-body a:hover {
	color: #999;
}
.post-meta a:hover,
.comment-meta a:hover {
	text-decoration: dotted underline;
}

/*===========*/
/* POST-META */
/*===========*/

.post-meta .post-section::before {
	color: #fff;
	top: -1px;
	text-shadow: 
		1px 1px 0 #777, 
		0 1px 0 #777, 
		0 0 5px #777;
}
a.post-section:hover {
	text-decoration: none;
}
a.post-section:hover::before {
	color: #eee;
}
.post-meta .date {
	color: #888;
}
.bottom-post-meta {
	border-top: 1px dotted #000;
}

/*============*/
/* LINK POSTS */
/*============*/

.post.link-post a.link-post-link {
	font-family: <?php echo $UI_font; ?>;
	font-weight: 600;
	font-size: 1.25em;
	border: none;
}
.post.link-post a.link-post-link::before {
	color: #fff;
	text-shadow: 
		 0.5px 0.5px 0 #000, 
		-0.5px -0.5px 0 #000,
		 0 0 2px #000;
}
.post.link-post a.link-post-link:hover {
	color: inherit;
	border-bottom: 2px dotted #000;
}
.post.link-post a.link-post-link:hover::before {
	text-shadow: 
		 0.5px 0.5px 0 #000, 
		-0.5px -0.5px 0 #000,
		 0 0 2px #000,
		 0 0 3px #000;
}
.post.link-post a.link-post-link:focus {
	color: #999;
	border-bottom: 2px dotted #999;
}

/*=======*/
/* POSTS */
/*=======*/

.post-body {
	font-size: 1.125rem;
	margin-top: 1em;
}
h1.post-title {
	font-size: 2.25rem;
	margin-top: 1.375em;
}

/*==========*/
/* COMMENTS */
/*==========*/

#comments::before {
	border-top: 2px solid #000;
}
#content > .comment-thread .comment-meta a.date:focus,
#content > .comment-thread .comment-meta a.permalink:focus {
	color: #999;
	outline: 2px dotted #999;
	position: relative;
	background-color: #fff;
	padding: 0 5px;
	left: -5px;
}
#content > .comment-thread .comment-meta a.date:focus + *,
#content > .comment-thread .comment-meta a.permalink:focus + *:not(.comment-post-title) {
	margin-left: -10px;
}
.comment-item {
	border: 2px solid #000;
}

.comment-body {
	font-size: 1rem;
	line-height: 1.5;
}

/*================================*/
/* DEEP COMMENT THREAD COLLAPSING */
/*================================*/

.comment-item input[id^="expand"] + label::after {
	font-weight: 600;
}
.comment-item input[id^="expand"] + label:hover::after {
	color: #c00;
}
.comment-item input[id^="expand"] + label:active::after,
.comment-item input[id^="expand"] + label:focus::after{
	color: #c00;
}
.comment-item input[id^="expand"]:checked ~ .comment-thread .comment-thread .comment-item {
	border-width: 1px 0 0 0;
}

/*==============*/
/* COMMENT-META */
/*==============*/

.comment-meta .author {
	font-weight: bold;
	font-size: 1.25em;
}
.comment-item .author:not(.redacted).original-poster::after {
	opacity: 0.6;
}

.comment-item .karma.active-controls::after,
.comment-item .karma .karma-value::after,
.post .karma.active-controls::after,
.post .karma .karma-value::after {
	background-color: #fff;
	color: #000;
	box-shadow: 0 0 0 1px #000 inset;
}
.comment-item .karma.active-controls::after,
.post .karma.active-controls::after {
	padding: 6px;
	bottom: -46px;
	max-width: unset;
	transform: translateX(-2px);
}
.comment-item .karma .karma-value::after,
.post .karma .karma-value::after {
	padding: 2px 8px;
	top: -26px;
	min-width: 80px;
}

/*====================*/
/* ANTI-KIBITZER MODE */
/*====================*/

.author.redacted,
.inline-author.redacted {
	opacity: 0.5;
	font-weight: 400;
}

.karma-value.redacted {
	opacity: 0.5;
}

.link-post-domain.redacted {
	opacity: 0.3;
}

/*===========================*/
/* COMMENT THREAD NAVIGATION */
/*===========================*/

div.comment-parent-link {
	font-weight: 600;
}
a.comment-parent-link {
	font-weight: 400;
}
a.comment-parent-link::after {
	display: none;
}
a.comment-parent-link::before {
	color: #bbb;
	padding: 4px 3px 0 2px;
}
a.comment-parent-link:hover::before {
	color: #999;
}

div.comment-child-links {
	font-weight: 600;
}
div.comment-child-links a {
	font-weight: 400;
}
.comment-child-link::before {
	color: #aaa;
}

.comment-item-highlight,
.comment-item-highlight-faint {
	border: 2px solid #ccc;
}

.comment-popup {
	border: 2px solid #000;
	background-color: #fff;
}
.comment-popup .comment-body {
	font-size: 0.9375rem;
}

/*====================*/
/* COMMENT PERMALINKS */
/*====================*/

.comment-meta .permalink,
.comment-meta .lw2-link,
.individual-thread-page .comment-parent-link:empty {
	filter: grayscale(100%);
}

/*=======================*/
/* COMMENTS COMPACT VIEW */
/*=======================*/

#comments-list-mode-selector button {
	box-shadow:
		0 0 0 4px #fff inset,
		0 0 0 5px #000 inset;
	border: none;
}
#comments-list-mode-selector button:hover,
#comments-list-mode-selector button.selected {
	box-shadow:
		0 0 0 1px #fff inset,
		0 0 0 2px #000 inset,
		0 0 0 4px #fff inset,
		0 0 0 5px #000 inset;
}
#content.compact > .comment-thread .comment-item::after {
	background: linear-gradient(to right, transparent 0%, #fff 50%, #fff 100%);
}

@media only screen and (hover: hover), not screen and (-moz-touch-enabled) {
	#content.compact > .comment-thread .comment-item:hover .comment,
	#content.compact > .comment-thread .comment-item.expanded .comment {
		background-color: #fff;
		outline: 3px solid #000;
	}
	#content.compact > .comment-thread .comment-item:hover .comment::before,
	#content.compact > .comment-thread .comment-item.expanded .comment::before {
		background-color: #fff;
		box-shadow: 
			0 0  3px #fff,
			0 0  5px #fff,
			0 0  7px #fff,
			0 0 10px #fff,
			0 0 20px #fff,
			0 0 30px #fff,
			0 0 40px #fff;
	}
}
@media only screen and (hover: none), only screen and (-moz-touch-enabled) {
	#content.compact > .comment-thread.expanded .comment-item .comment {
		background-color: #fff;
		outline: 3px solid #000;
	}
	#content.compact > .comment-thread.expanded .comment-item .comment::before {
		background-color: #fff;
		box-shadow: 
			0 0  3px #fff,
			0 0  5px #fff,
			0 0  7px #fff,
			0 0 10px #fff,
			0 0 20px #fff,
			0 0 30px #fff,
			0 0 40px #fff;
	}
}

#content.user-page.compact > h1.listing {
	margin-top: 0.5rem;
}
#content.user-page.compact > h1.listing + .post-meta {
	margin-bottom: 0.5rem;
}

/*===========================*/
/* HIGHLIGHTING NEW COMMENTS */
/*===========================*/

.new-comment {
	border: 2px dotted #000;
}
.new-comment::before {
	display: none;
}

/*=================================*/
/* COMMENT THREAD MINIMIZE BUTTONS */
/*=================================*/

.comment-minimize-button {
	color: #ccc;
	font-family: Font Awesome, <?php echo $UI_font; ?>;
}
.comment-minimize-button:hover {
	color: #aaa;
}
.comment-minimize-button::after {
	font-size: 0.8125rem;
	top: 20px;
	left: 0.5px;
	color: #777;
}
.comment-minimize-button.maximized::after {
	color: #ccc;
}

/*=================================*/
/* INDIVIDUAL COMMENT THREAD PAGES */
/*=================================*/

.individual-thread-page > h1 a:hover {
	color: #777;
	text-decoration: dotted underline;
}

.individual-thread-page > .comments {
	border-top: 2px solid #000;
}

/*==============*/
/* VOTE BUTTONS */
/*==============*/

.vote::before {
	content: "";
	border-radius: 2px;
	background-size: 17px 17px;
	width: 17px;
	height: 17px;
	display: inline-block;
	position: relative;
	top: 2.5px;
}
.vote:active {
	transform: none;
}
.vote:hover,
.vote:active,
.vote:focus {
	box-shadow: none;
}
.vote:hover::before,
.vote.selected::before,
.vote.clicked-once::before,
.vote.clicked-twice::before {
	filter: drop-shadow(0 0 1px #fff);
}
.vote::before,
.waiting .vote.big-vote.clicked-twice::before {
	filter: contrast(5%) brightness(182%);
}
.vote.clicked-once::before,
.vote.big-vote.clicked-once::before {
	box-shadow:
		0 0 0 3px #ccc,
		0 0 0 4px transparent;
}
.vote.big-vote.clicked-twice::before,
.waiting .vote.big-vote:not(.clicked-twice)::before,
.waiting .vote:not(.big-vote).clicked-once::before {
	box-shadow: none;
}
.vote.clicked-twice::before,
.vote.big-vote::before {
	box-shadow:
		0 0 0 3px #000,
		0 0 0 4px transparent;
}

.upvote::before {
	background-image: url('data:image/svg+xml;base64,<?php echo base64_encode(file_get_contents("assets/upvote-black-square-plus.svg")) ?>');
}
.downvote::before {
	background-image: url('data:image/svg+xml;base64,<?php echo base64_encode(file_get_contents("assets/downvote-black-square-minus.svg")) ?>');
}

/*===========================*/
/* COMMENTING AND POSTING UI */
/*===========================*/

.comment-controls .cancel-comment-button {
	height: 28px;
	color: #fff;
	padding: 4px 6px 2px 6px;
}

.posting-controls input[type='submit'],
.comment-controls .cancel-comment-button,
.new-comment-button {
	font-weight: 600;
}

.comment-controls .delete-button::before,
.comment-controls .retract-button::before,
.comment-controls .unretract-button::before {
	opacity: 0.8;
}

.posting-controls .action-button {
	border: none;
}
.posting-controls .action-button:hover,
.posting-controls .action-button:active,
.posting-controls .action-button:focus {
	box-shadow: none;
	text-decoration: dotted underline;
}
.posting-controls .action-button:active {
	transform: scale(0.9);
}

.posting-controls textarea {
	font-family: <?php echo $text_font; ?>;
	font-weight: 200;
	font-size: 1rem;
	line-height: 1.4;
	color: #000;
	background-color: #fff;
	border-color: #000;
	border-width: 28px 2px 2px 2px;
}
.posting-controls textarea:focus {
	border-style: dotted;
	border-width: 28px 2px 2px 2px;
}
.posting-controls textarea::selection {
	background-color: #000;
	color: #fff;
}
.posting-controls textarea::-webkit-scrollbar {
	width: 18px;
}
.posting-controls textarea::-webkit-scrollbar-track {
	background-color: #fff;
}
.posting-controls textarea::-webkit-scrollbar-thumb {
	background-color: #000;
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/checkerboard2_1px_gray.gif")) ?>');
	background-size: 2px 2px;
	box-shadow: 
		0 2px 0 1px #fff inset,
		0 0 0 1px #fff inset,
		0 2px 0 1.5px #777 inset,
		0 0 0 1.5px #777 inset;
}
.posting-controls textarea::-webkit-scrollbar-thumb:active {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/checkerboard2_1px.gif")) ?>');
	box-shadow: 
		0 2px 0 1px #fff inset,
		0 0 0 1px #fff inset,
		0 2px 0 1.5px #000 inset,
		0 0 0 1.5px #000 inset;
}

/* GUIEdit buttons */

.guiedit-buttons-container {
	background-color: #000;
	box-shadow: 0 0 0 1px #000;
}

.posting-controls.edit-existing-post .guiedit-buttons-container button,
.posting-controls form.edit-existing-comment .guiedit-buttons-container button {
	color: #0c0;
}

button.guiedit {
	font-family: Font Awesome, <?php echo $UI_font; ?>;
	color: #fff;
	border: none;
}
button.guiedit:hover,
button.guiedit:active,
button.guiedit:focus {
	box-shadow: none;
	color: #777;
}
button.guiedit::after {
	color: #fff;
	top: 2px;
	height: 25px;
	text-shadow: none;
}

/* Markdown hints */

#markdown-hints {
	border: 2px solid #000;
	background-color: #fff;
}

/*================*/
/* EDIT POST FORM */
/*================*/

#edit-post-form .post-meta-fields {
	grid-template-columns: 6em auto auto auto 1fr auto;
}
#edit-post-form .post-meta-fields input[type='checkbox'] + label::before {
	color: #000;
	border: 1px solid #000;
	top: 2px;
}
@media only screen and (hover:hover), not screen and (-moz-touch-enabled) {
	#edit-post-form .post-meta-fields input[type='checkbox'] + label:hover::before,
	#edit-post-form .post-meta-fields input[type='checkbox']:focus + label::before {
		box-shadow: 
			0 0 0 1px #fff inset,
			0 0 0 2px #000 inset;
	}
}
#edit-post-form .post-meta-fields input[type='checkbox']:active + label::before,
#edit-post-form .post-meta-fields input[type='checkbox']:checked:active + label::before {
	background-color: #fff;
	box-shadow: 
		0 0 0 3px #fff inset,
		0 0 0 4px #000 inset;
}
#edit-post-form .post-meta-fields input[type='checkbox']:checked + label::before {
	content: "";
	background-color: #000;
	box-shadow: 
		0 0 0 4px #fff inset;
}
#edit-post-form input[type='radio'] + label {
	border-color: #000;
	color: #000;
}
#edit-post-form input[type='radio'][value='all'] + label {
	border-width: 1px;
}
#edit-post-form input[type='radio'][value='drafts'] + label {
}
#edit-post-form input[type='radio'] + label:hover,
#edit-post-form input[type='radio']:focus + label {
	color: #000;
	box-shadow: 
		0 0 0 1px #fff inset,
		0 0 0 2px #000 inset;
}
#edit-post-form input[type='radio']:active + label {
	box-shadow: 
		0 0 0 2px #fff inset,
		0 0 0 3px #000 inset;
}
#edit-post-form input[type='radio']:focus + label {
	box-shadow: 
		0 0 0 1px #000;
}
#edit-post-form input[type='radio']:checked + label {
	border-color: #000;
	box-shadow: 
		0 0 0 1px #fff inset,
		0 0 0 2px #000 inset;
	font-weight: bold;
}

#edit-post-form #markdown-hints-checkbox + label {
	padding: 4px 0 0 6px;
}

/*=======*/
/* LINKS */
/*=======*/

a:link,
a:visited {
	color: inherit;
	text-decoration: none;
}

/*=========*/
/* BUTTONS */
/*=========*/

button,
input[type='submit'] {
	border: 2px solid #000;
	box-shadow: 0 0 0 1px transparent;
}
#ui-elements-container button {
	border: none;
}

button:hover,
input[type='submit']:hover,
button:focus,
input[type='submit']:focus {
	text-decoration: none;
	background-color: transparent;
	color: inherit;
	box-shadow: 
		0 0 0 2px #fff inset,
		0 0 0 4px #000 inset,
		0 0 0 1px transparent;
}
button:active,
input[type='submit']:active {
	box-shadow: 
		0 0 0 4px #fff inset,
		0 0 0 6px #000 inset,
		0 0 0 1px transparent;
}

/*==========*/
/* HEADINGS */
/*==========*/

.post-body h1,
.post-body h2,
.post-body h4,
.comment-body h1,
.comment-body h2,
.comment-body h4 {
	font-family: <?php echo $UI_font; ?>;
}
.post-body h3,
.comment-body h3,
.post-body h5,
.post-body h6,
.comment-body h5,
.comment-body h6 {
	font-weight: 600;
}
.post-body h6,
.comment-body h6 {
	color: #555;
}
.post-body h1,
.comment-body h1 {
	border-bottom: 2px solid #000;
}

/*========*/
/* QUOTES */
/*========*/

blockquote {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/checkerboard_1px.gif")) ?>');
	background-size: 5px 2px;
	background-repeat: repeat-y;
	background-position: top left;
	padding-left: calc(0.5em + 5px);
}

/*========*/
/* IMAGES */
/*========*/

#content img,
#content figure.image img {
	border: 1px dotted #000;
}
#content figure img {
	border: 1px solid #000;
}
#content img[src$='.svg'],
#content figure img[src$='.svg'] {
	border: none;
}
#content img[style^='float'] {
	border: 1px solid transparent;
}

/*========*/
/* TABLES */
/*========*/

.post-body table,
.comment-body table,
.post-body table th,
.post-body table td,
.comment-body table th,
.comment-body table td {
	border: 1px solid #000;
}

/*======*/
/* MISC */
/*======*/

hr {
	border-bottom: 1px solid #999;
}

code {
	border: 1px dotted #000;
}

input[type='text'],
input[type='search'],
input[type='password'],
textarea {
	background-color: transparent;
	border: 1px solid #000;
	color: #000;
}
input[type='text']:focus,
input[type='search']:focus,
input[type='password']:focus,
textarea:focus {
	border: 1px dashed #000;
}

select {
	color: #000;
}

.frac sup {
	position: relative;
	left: -1px;
}
.frac sub {
	position: relative;
	left: -0.5px;
}

/*============*/
/* ABOUT PAGE */
/*============*/

.about-page mark {
	background-color: #e6e6e6;
	text-decoration: none;
	box-shadow: 
		0 -1px 0 0 #000 inset, 
		0 -3px 1px -2px #000 inset;
	padding: 0 1px;
}

#content.about-page .accesskey-table {
	font-family: <?php echo $UI_font; ?>;
	border-color: #ddd;
}

#content.about-page img {
	border: 1px solid #000;
}

/*========================*/
/* QUALIFIED HYPERLINKING */
/*========================*/

#aux-about-link a:hover {
	color: #777;
}

.qualified-linking label:hover {
	color: #777;
}

.qualified-linking-toolbar {
	border: 2px solid #000;
	background-color: #fff;
}
.qualified-linking-toolbar a {
}
.qualified-linking-toolbar a:hover {
	box-shadow: 0 0 0 2px #000;
}
.qualified-linking-toolbar a:active {
	box-shadow: 0 0 0 2px #000 inset;
}
.qualified-linking label::after {
	background-color: #fff;
	opacity: 0.8;
}

/*======*/
/* MATH */
/*======*/

.mathjax-block-container::-webkit-scrollbar {
	height: 12px;
	background-color: #f6f6ff;
	border-radius: 6px;
	border: 1px solid #ddf;
}
.mathjax-block-container::-webkit-scrollbar-thumb {
	background-color: #dde;
	border-radius: 6px;
	border: 1px solid #cce;
}
.mathjax-inline-container::-webkit-scrollbar {
	height: 8px;
	background-color: #f6f6ff;
	border-radius: 4px;
	border: 1px solid #ddf;
}
.mathjax-inline-container::-webkit-scrollbar-thumb {
	background-color: #dde;
	border-radius: 4px;
	border: 1px solid #cce;
}

/*=================*/
/* ALIGNMENT FORUM */
/*=================*/

#content.alignment-forum-index-page::before {
	background-color: #f2f6ff;
}
#content.alignment-forum-index-page::after {
	font-family: "Concourse SmallCaps";
	font-weight: 600;
	background-color: #7f85b2;
	color: transparent;
	-webkit-background-clip: text;
	text-shadow: 
		rgba(255,255,255,0.5) 0px 3px 3px;
}
@media only screen and (hover: hover), not screen and (-moz-touch-enabled) {
	#content.alignment-forum-index-page h1.listing a:hover,
	#content.alignment-forum-index-page h1.listing a:focus {
		background-color: rgba(242,246,255,0.85);
	}	
}

/*====================*/
/* FOR NARROW SCREENS */
/*====================*/

@media only screen and (max-width: 1440px) {
	#hns-date-picker {
		right: -70px;
		bottom: 64px;
		background-color: #fff;
		opacity: 1.0;
	}
	#hns-date-picker::before {
		width: 56%;
		border: 2px solid #000;
		box-shadow: 0 0 0 1px #000;
	}
}
@media only screen and (max-width: 1160px) {
	#hns-date-picker {
		bottom: 203px;
		right: -24px;
	}
	#hns-date-picker::before {
		width: 85%;
	}
	#theme-selector:hover::after {
		width: calc(6.5em - 6px);
		background-color: #000;
	}
}
@media only screen and (max-width: 1080px) {
	#theme-tweaker-toggle button {
		border: 1px solid #999;
		box-shadow: 
			0 0 10px #999 inset,
			0 0 0 1px transparent;
		transform: scale(0.8);
	}
	#hns-date-picker {
		right: -14px;
	}
	#hns-date-picker::before {
		width: 92%;
	}
}
@media only screen and (max-width: 1040px) {
	#hns-date-picker {
		right: -8px;
	}
	#hns-date-picker::before {
		width: 94%;
	}
}
@media only screen and (max-width: 1020px) {
	#hns-date-picker {
		right: 19px;
	}
	#hns-date-picker::before {
		width: 100%;
	}
	#quick-nav-ui {
		background-color: #fff;
		box-shadow: 0 9px 0 0px #fff;
	}
	#new-comment-nav-ui .new-comments-count::before {
		background-color: #fff;
		box-shadow: 
			0 0 0 2px #000,
			0 0 0 10px #fff;
	}
	#anti-kibitzer-toggle {
		background-color: #fff;
		box-shadow: 
			0 0 0 2px #000,
			0 0 0 10px #fff;
	}
}
@media only screen and (max-width: 1000px) {
	#theme-selector,
	#quick-nav-ui,
	#new-comment-nav-ui,
	#new-comment-nav-ui + #hns-date-picker,
	#theme-tweaker-toggle button,
	#text-size-adjustment-ui,
	#anti-kibitzer-toggle {
		opacity: 1.0;
	}
	#theme-selector {
		background-color: #fff;
		border: 1px solid #000;
	}
	#theme-selector:hover::after {
		width: calc(6em + 6px);
		height: calc(100% - 5px);
		top: 3px;
		left: 100%;
	}	
	#text-size-adjustment-ui {
		background-color: #fff;
		box-shadow: 0 0 0 1px #000;
		padding: 2px 0 4px 0;
	}
	#theme-tweaker-toggle {
		left: -19px;
	}
	#theme-tweaker-toggle button {
		background-color: #fff;
		box-shadow: none;
		border: 1px solid #000;
	}
	#theme-tweaker-toggle button:hover {
		color: #777;
	}
}

/*========*/
/* MOBILE */
/*========*/

@media only screen and (hover: none), only screen and (-moz-touch-enabled) {
	#ui-elements-container > div[id$='-ui-toggle'] button,
	#theme-selector .theme-selector-close-button  {
		color: #000;
		opacity: 1.0;
	}
	#appearance-adjust-ui-toggle button,
	#post-nav-ui-toggle button {
		background-color: #fff;
		box-shadow:
			0 0 0 2px #fff,
			0 0 0 2px #000 inset;
	}

	#theme-selector {
		background-color: #fff;
		box-shadow: 
			0 0 0 1px #000,
			0 0 0 7px #fff;
	}
	#theme-selector::before {
		color: #000;
	}
	#theme-selector button {
		background-color: #fff;
		box-shadow: 
			0 0 0 1px #fff inset, 
			0 0 0 3px #000 inset, 
			0 0 0 5px #fff inset;
	}
	#theme-selector button::after {
		color: #000;
		max-width: calc(100% - 3.5em);
		overflow: hidden;
		text-overflow: ellipsis;
		padding-left: 1px;
	}
	#theme-selector button.selected {
		box-shadow: 
			0 0 0 1px #fff inset, 
			0 0 0 3px #000 inset, 
			0 0 0 5px #fff inset, 
			0 0 0 7px #000 inset;
	}
	#theme-selector button.selected::after {
		font-weight: bold;
	}
	#theme-selector .theme-selector-close-button {
		font-weight: 400;
	}

	#quick-nav-ui {
		background-color: #fff;
	}
	#quick-nav-ui a {
		box-shadow: 
			0 0 0 2px #000;
	}
	#quick-nav-ui a::after,
	#new-comment-nav-ui::before {
		font-family: <?php echo $UI_font; ?>;
		font-weight: bold;
		font-size: 0.5rem;
		background-color: #fff;
	}
	#new-comment-nav-ui {
		background-color: #fff;
		border: 1px solid #000;
		box-shadow: 0 0 0 1px #000;
	}
	#new-comment-nav-ui::before {
		color: #000;
		font-size: 0.6875rem;
		font-weight: bold;
		bottom: calc(100% + 2px);
		padding: 2px 0 0 0;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button {
		color: #000;
	}
	#new-comment-nav-ui .new-comments-count,
	#new-comment-nav-ui .new-comments-count::after {
		color: #000;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button:disabled {
		color: #bbb;
	}
	#new-comment-nav-ui button::after {
		font-family: <?php echo $UI_font; ?>;
	}

	#hns-date-picker {
		bottom: 130px;
		right: 52px;
		text-indent: 0;
	}

	/*****************************************/
	@media only screen and (max-width: 900px) {
	/*****************************************/
		h1.listing {
			line-height: 1;
			margin-bottom: 6px;
		}
		h1.listing + .post-meta {
			font-family: Input Sans Narrow;
			font-weight: 300;
		}
		h1.listing + .post-meta .post-section {
			overflow: visible;
			order: 1;
		}
		h1.listing + .post-meta .post-section::before {
			position: unset;
		}

		.archive-nav > *[class^='archive-nav-'] + *[class^='archive-nav-']::before {
			background-color: #000;
			height: 2px;
		}

		.comment-item .comment-item {
			margin: 0.75em 2px 3px 6px;
		}
		.comment-item .comment-item + .comment-item {
			margin: 1.5em 2px 3px 6px;
		}

		a.comment-parent-link::before {
			line-height: 1;
		}

		#edit-post-form textarea {
			min-height: calc(100vh - 400px)
		}
		#edit-post-form #markdown-hints {
			position: fixed;
			top: 74px;
			left: 0;
			right: 0;
			max-width: 330px;
			margin: auto;
		}
		#edit-post-form input[type='submit'] {
			background-color: #fff;
		}
		
		.comment-controls .cancel-comment-button {
			max-width: 1.4em;
		}
		.comment-controls .cancel-comment-button::before {
			opacity: 1.0;
			position: relative;
			top: -1px;
		}
		
		.sublevel-nav .sublevel-item,
		.sublevel-nav .sublevel-item:first-child,
		.sublevel-nav .sublevel-item:last-child {
			border-width: 1px;
		}
	/*******************************************/
	} @media only screen and (max-width: 720px) {
	/*******************************************/
		#content.conversations-user-page h1.listing + .post-meta .date {
			margin: 0 0 0 1em;
		}
	/*******************************************/
	} @media only screen and (max-width: 520px) {
	/*******************************************/
		h1.listing {
			font-size: 1.375rem;
			margin: 18px 6px 4px 6px;
			max-width: calc(100% - 12px);
		}
		h1.listing a[href^='http'] {
			top: 4px;
		}
		h1.listing + .post-meta {
			margin: 4px 6px;
			font-family: Input Sans Condensed;
			font-weight: 300;
		}
		h1.listing + .post-meta .post-section {
			order: 1;
			overflow: visible;
		}
		h1.listing + .post-meta .post-section::before {
			position: relative;
			top: -1px;
			left: 0;
		}
		#content.conversations-user-page h1.listing {
			font-size: 1.375rem;
		}
		#content.conversations-user-page h1.listing + .post-meta .conversation-participants {
			margin: 0;
		}
		#content.conversations-user-page h1.listing + .post-meta .messages-count {
			margin: 0 0 0 1em;
		}

		#content.compact > .comment-thread .comment-item {
			max-height: 110px;
		}
		
		.textarea-container:focus-within textarea {
			background-color: #fff;
			border: none;
			box-shadow:
				0 0 0 2px #000;
		}
		.textarea-container:focus-within .guiedit-mobile-auxiliary-button {
			padding: 5px 6px 6px 6px;
			color: #fff;
			font-weight: bold;
			box-shadow: none;
		}
		.textarea-container:focus-within .guiedit-mobile-help-button.active {
			box-shadow:
				0 0 0 1px #000 inset,
				0 0 0 3px #fff inset,
				0 0 0 2px #fff;
		}
		.textarea-container:focus-within .guiedit-buttons-container {
			border-top: 2px solid #000;
		}
		.posting-controls .textarea-container:focus-within .guiedit-buttons-container {
			padding-bottom: 5px;
		}
		#content.conversation-page .textarea-container:focus-within::after {
			background-color: #000;
		}
		.textarea-container:focus-within button.guiedit {
			border: 1px solid transparent;
		}
		#markdown-hints,
		#edit-post-form #markdown-hints {
			border: 2px solid #000;
			box-shadow:
				0 0 0 2px #fff,
				0 0 0 4px #000;
		}
		#markdown-hints::after {
			color: #000;
		}

		#edit-post-form .post-meta-fields input[type='checkbox'] + label {
			top: 0;
		}
		#edit-post-form .post-meta-fields input[type='checkbox'] + label::before {
			top: 1px;
		}
	}
}
