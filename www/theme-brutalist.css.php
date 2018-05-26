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
	background-color: #fff;
	box-shadow: 0 2px 0 #000;
	border-style: solid;
	border-color: #000;
	border-width: 0 2px;
	line-height: 1.55;
}
#content.no-nav-bars {
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
.sublevel-nav a.sublevel-item:hover {
	box-shadow: 
		0 0 0 2px #fff inset,
		0 0 0 3px #000 inset;
}
.sublevel-nav a.sublevel-item:active {
	box-shadow: 
		0 0 0 4px #fff inset,
		0 0 0 5px #000 inset;
	font-weight: normal;
}
.sublevel-nav span.sublevel-item {
	box-shadow: 
		0 0 0 2px #fff inset,
		0 0 0 3px #000 inset;
	font-weight: bold;
}

/*=====================*/
/* SORT ORDER SELECTOR */
/*=====================*/

.sublevel-nav.sort {
	position: absolute;
	top: 167px;
	right: 30px;
	padding: 18px 0 0 0;
	border: 2px solid #000;
}
.sublevel-nav.sort::before {
	text-transform: uppercase;
	font-weight: 600;
}
.sublevel-nav.sort .sublevel-item {
	padding: 6px 8px;
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

/*=========================*/
/* TEXT SIZE ADJUSTMENT UI */
/*=========================*/

#text-size-adjustment-ui button:hover,
#text-size-adjustment-ui button:active,
#text-size-adjustment-ui button:focus {
	box-shadow: none;
	color: #777;
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
@media only screen and (hover: hover), not screen and (-moz-touch-enabled) {
	#quick-nav-ui a:hover {
		box-shadow: 
			0 0 0 1px #fff inset,
			0 0 0 3px #000 inset,
			0 0 0 1px #fff,
			0 0 0 3px #000;
	}
}

/*======================*/
/* NEW COMMENT QUICKNAV */
/*======================*/

#quick-nav-ui a {
	color: #000;
	box-shadow: 
		0 0 0 1px #fff,
		0 0 0 3px #000;
}
#quick-nav-ui a[href='#bottom-bar'] {
	line-height: 1.8;
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
@media only screen and (hover: hover), not screen and (-moz-touch-enabled) {
	#quick-nav-ui a:hover {
		box-shadow: 
			0 0 0 1px #fff inset,
			0 0 0 3px #000 inset,
			0 0 0 1px #fff,
			0 0 0 3px #000;
	}
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
	font-family: <?php echo $UI_font; ?>, 'Font Awesome';
	font-size: 1.75rem;
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
	#content.user-page h1.listing:focus-within::before {
		left: -0.625em;
	}
	h1.listing:focus-within::before {
		color: #000;
		left: 1.25em;
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

/*===================*/
/* TOP PAGINATION UI */
/*===================*/

#top-nav-bar a:hover {
	color: #777;
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

#content.user-page .sublevel-nav + h1.listing {
	margin-top: 1.75em;
}
#content.user-page h1.listing {
	margin: 1.5em 0 0 0;
	padding: 0 6px;
	position: relative;
}
#content.user-page h1.listing::after {
	content: "";
	display: block;
	position: absolute;
	top: 0;
	left: 0;
	width: 100%;
	height: calc(100% + 1.25em);
	box-shadow: 0px 0px 10px #555;
}
#content.user-page h1.listing + .post-meta {
	margin: 0 6px 3em 35px;
}
#content.user-page h1.listing + .post-meta::after {
	display: none;
}

#content.conversations-user-page h1.listing {
	margin: 0.75em 0 0 0;
	font-size: 1.5rem;
}
#content.conversations-user-page .sublevel-nav + h1.listing {
	margin: 1em 0 0 0;
}
#content.conversations-user-page h1.listing::after {
	display: none;
}
#content.conversations-user-page h1.listing + .post-meta {
	padding: 0;
	margin: 0;
}

.user-stats .karma-total {
	font-weight: bold;
}

/*============*/
/* LOGIN PAGE */
/*============*/

.login-container form input[type='submit'] {
	font-weight: bold;
}

/* “Log in” form */
#login-form a:hover {
	text-decoration: dotted underline;
}
#login-form a:active {
	transform: scale(0.9);
}

/* “Create account” form */

#signup-form {
	padding: 0 0 0.5em 1em;
	border: 2px solid #000;
}
#signup-form label {
	width: 10em;
}
#signup-form input[type='text'],
#signup-form input[type='password'] {
	width: calc(100% - 12em);
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

/*=======*/
/* POSTS */
/*=======*/

.post-body {
	font-size: 1.125rem;
}
.post > h1:first-child {
	font-size: 2.25rem;
}

/*==========*/
/* COMMENTS */
/*==========*/

#comments {
	border-top: 2px solid #000;
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

/*===========================*/
/* COMMENT THREAD NAVIGATION */
/*===========================*/

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
	font-weight: normal;
}
div.comment-child-links a::first-letter {
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
#content.compact > .comment-thread .comment-item:hover .comment {
	background-color: #fff;
	outline: 3px solid #000;
}
#content.compact > .comment-thread .comment-item:hover .comment::before {
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

/*==============*/
/* VOTE BUTTONS */
/*==============*/

.vote {
	color: #ddd;
}
.upvote::before {
	content: "\F0FE";
}
.downvote::before {
	content: "\F146";
}
.upvote:hover,
.upvote.selected,
.downvote:hover,
.downvote.selected {
	color: inherit !important;
}
.vote:hover,
.vote:active,
.vote:focus {
	box-shadow: none;
}

/*===========================*/
/* COMMENTING AND POSTING UI */
/*===========================*/

.comment-controls .cancel-comment-button {
	height: 28px;
	color: #fff;
}

.posting-controls input[type='submit'],
.comment-controls .cancel-comment-button,
.new-comment-button {
	font-weight: 600;
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
.posting-controls.edit-existing-comment .guiedit-buttons-container button {
    color: #050;
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

.markdown-hints {
	border: 2px solid #000;
	background-color: #fff;
}

/*================*/
/* EDIT POST FORM */
/*================*/

#edit-post-form input[name='title'] {
	max-width: calc(100% - 14em);
}
#edit-post-form .link-post-checkbox + label::before {
	color: #000;
	border: 1px solid #000;
	top: 2px;
}
#edit-post-form .link-post-checkbox + label:hover::before,
#edit-post-form .link-post-checkbox:focus + label::before {
	box-shadow: 
		0 0 0 1px #fff inset,
		0 0 0 2px #000 inset;
}
#edit-post-form .link-post-checkbox:active + label::before,
#edit-post-form .link-post-checkbox:checked:active + label::before {
	background-color: #fff;
	box-shadow: 
		0 0 0 3px #fff inset,
		0 0 0 4px #000 inset;
}
#edit-post-form .link-post-checkbox:checked + label::before {
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

.about-page u {
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

/*======*/
/* MATH */
/*======*/

div > .MJXc-display {
	padding: 10px 6px;
	border-radius: 6px;
}
.MJXc-display::-webkit-scrollbar {
	height: 12px;
	background-color: #f6f6ff;
	border-radius: 6px;
	border: 1px solid #ddf;
}
.MJXc-display::-webkit-scrollbar-thumb {
	background-color: #dde;
	border-radius: 6px;
	border: 1px solid #cce;
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
}
@media only screen and (max-width: 1000px) {
	#theme-selector,
	#quick-nav-ui,
	#new-comment-nav-ui,
	#new-comment-nav-ui + #hns-date-picker,
	#theme-tweaker-toggle button,
	#text-size-adjustment-ui {
		opacity: 1.0;
	}
	#theme-selector {
		background-color: #fff;
		border: 1px solid #000;
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
	#appearance-adjust-ui-toggle button,
	#post-nav-ui-toggle button,
	#theme-selector .theme-selector-close-button  {
		color: #000;
		box-shadow: none;
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
	#new-comment-nav-ui {
		background-color: #fff;
		border: 1px solid #000;
		box-shadow: 0 0 0 1px #000;
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

	#hns-date-picker {
		bottom: 122px;
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

		#content.compact > #top-nav-bar + .comment-thread .comment-item {
			margin-top: 0;
		}

		.archive-nav > *[class^='archive-nav-'] + *[class^='archive-nav-']::before {
			background-color: #000;
			height: 2px;
		}
	
		.sublevel-nav.sort {
			top: 315px;
			right: 10px;
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
		#edit-post-form .markdown-hints {
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
	/*******************************************/
	} @media only screen and (max-width: 720px) {
	/*******************************************/
		#content.conversations-user-page h1.listing + .post-meta .date {
			margin: 0 0 0 1em;
		}
		
		.sublevel-nav .sublevel-item,
		.sublevel-nav .sublevel-item:first-child,
		.sublevel-nav .sublevel-item:last-child {
			border-width: 1px;
		}
		.sublevel-nav.sort {
			top: 275px;
		}
	/*******************************************/
	} @media only screen and (max-width: 520px) {
	/*******************************************/
		h1.listing {
			font-size: 1.375rem;
			margin: 18px 6px 4px 6px;
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
		
		.sublevel-nav.sort {
			top: 215px;
			right: 4px;
		}
		
		.comment-controls form:focus-within textarea,
		.textarea-container:focus-within textarea {
			background-color: #fff;
			border: none;
			box-shadow:
				0 0 0 2px #000;
		}
		.comment-controls form:focus-within .guiedit-mobile-auxiliary-button,
		.textarea-container:focus-within .guiedit-mobile-auxiliary-button {
			padding: 5px 6px 6px 6px;
			color: #fff;
			font-weight: bold;
			box-shadow: none;
		}
		.comment-controls form:focus-within .guiedit-mobile-help-button.active,
		.textarea-container:focus-within .guiedit-mobile-help-button.active {
			box-shadow:
				0 0 0 1px #000 inset,
				0 0 0 3px #fff inset,
				0 0 0 2px #fff;
		}
		.comment-controls form:focus-within .guiedit-buttons-container,
		.textarea-container:focus-within .guiedit-buttons-container {
			border-top: 2px solid #000;
		}
		.posting-controls .textarea-container:focus-within .guiedit-buttons-container {
			padding-bottom: 5px;
		}
		.comment-controls form:focus-within button.guiedit,
		.textarea-container:focus-within button.guiedit {
			border: 1px solid transparent;
		}
		.markdown-hints,
		#edit-post-form .markdown-hints {
			border: 2px solid #000;
			box-shadow:
				0 0 0 2px #fff,
				0 0 0 4px #000;
		}
		.markdown-hints::after {
			color: #000;
		}
		
		#edit-post-form .textarea-container:focus-within textarea {
			margin: 3px 0;
		}
		#edit-post-form label[for='title'],
		#edit-post-form label[for='url'] {
			width: 3.5em;
		}
		#edit-post-form label[for='section'] {
			width: 4.6em;
		}
		#edit-post-form input[name='title'],
		#edit-post-form input[name='url'] {
			max-width: calc(100% - 7.75em);
		}
	}
}
