<?php
	$UI_font = "'Anonymous Pro'";
	$hyperlink_color = "#00e";
	$white_glow = "0 0 1px #fff, 0 0 3px #fff, 0 0 5px #fff";
?>

/*****************/
/* DEFAULT THEME */
/*****************/

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

.nav-inner {
	font-size: 1.375em;
	font-weight: 600;
}
#secondary-bar .nav-inner {
	font-size: 1em;
}

.nav-current .nav-inner,
.nav-bar .nav-item:not(#nav-item-search) .nav-inner:hover {
	box-shadow: 
		0 0 0 4px #fff inset,
		0 0 0 6px #000 inset;
}
.nav-bar .nav-item:not(#nav-item-search) .nav-inner:active {
	box-shadow: 
		0 0 0 8px #fff inset,
		0 0 0 10px #000 inset;
}

/* Accesskey hints */

.nav-inner::after {
	display: block;
	position: absolute;
	left: 5px;
	top: -2px;
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
#nav-item-search button:hover {
	text-decoration: dotted underline;
}

/*==============*/
/* PAGE TOOLBAR */
/*==============*/

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

.sublevel-nav.sort .sublevel-item {
	font-family: <?php echo $UI_font_smallcaps; ?>;
}
.sublevel-nav.sort {
	position: absolute;
	top: 167px;
	right: 30px;
	border: 2px solid #bbb;
	padding: 18px 0 0 0;
	border-radius: 8px;
	box-shadow: 0 18px #bbb inset;
}
.sublevel-nav.sort::before {
	text-transform: uppercase;
	font-weight: 600;
	color: #444;
	text-shadow: 0.5px 0.5px 0 #fff;
}
.sublevel-nav.sort .sublevel-item {
	border-radius: 0;
	padding: 5px 6px;
	border-color: #aaa;
	border-style: solid;
	text-transform: uppercase;
}
.sublevel-nav.sort .sublevel-item:first-child {
	border-radius: 6px 6px 0 0;
	border-width: 1px;
}
.sublevel-nav.sort .sublevel-item:last-child {
	border-radius: 0 0 6px 6px;
	border-width: 0 1px 1px 1px;
}
.sublevel-nav.sort .sublevel-item:active {
	border-color: #aaa;
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

/*======================*/
/* THEME TWEAKER TOGGLE */
/*======================*/

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
#quick-nav-ui a:hover {
	box-shadow: 
		0 0 0 1px #fff inset,
		0 0 0 3px #000 inset,
		0 0 0 1px #fff,
		0 0 0 3px #000;
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
}

/*======================*/
/* NEW COMMENT QUICKNAV */
/*======================*/

#new-comment-nav-ui .new-comments-count {
	font-weight: 600;
	color: #666;
	text-shadow: 0.5px 0.5px 0 #fff;
}
#new-comment-nav-ui .new-comments-count:hover {
	text-shadow: 
		0 0 1px #fff,
		0 0 3px #fff,
		0 0 5px #fff,
		0 0 8px #fff,
		0.5px 0.5px 0 #fff;
}
#new-comment-nav-ui .new-comments-count::after {
	font-weight: 600;
	color: #777;
}
#new-comment-nav-ui .new-comment-sequential-nav-button:focus {
	color: #d00;
	text-shadow: <?php echo $white_glow; ?>;
}
#new-comment-nav-ui .new-comment-sequential-nav-button:disabled {
	color: #bbb;
	text-shadow: none;
}

/*=================*/
/* HNS DATE PICKER */
/*=================*/

#hns-date-picker span {
	color: #777;
	text-shadow: 0.5px 0.5px 0 #fff;
	font-weight: 600;
}
#hns-date-picker input {
	border: 1px solid #777;
	background-color: transparent;
	color: #666;
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
	border: 1px solid #aaa;
}
.archive-nav *[class^='archive-nav-item'] {
	border-style: solid;
	border-color: #ddd;
	border-width: 1px 0 1px 1px;
	background-color: #eee;
}
.archive-nav div[class^='archive-nav-']:nth-of-type(2) *[class^='archive-nav-item'] {
	border-top-width: 0;
	border-bottom-width: 0;
}
.archive-nav div[class^='archive-nav-']:last-of-type *[class^='archive-nav-item'] {
	border-bottom-width: 1px;
}
.archive-nav *[class^='archive-nav-item']:last-child {
	border-right-width: 1px;
}
.archive-nav span[class^='archive-nav-item'] {
	font-weight: bold;
	background-color: #ddd;
}

.archive-nav a:link,
.archive-nav a:visited {
	color: rgba(0, 0, 238, 0.7);
}
.archive-nav a:hover {
	text-decoration: none;
	color: #c00;
	background-color: #e0e0e0;
	text-shadow: <?php echo $white_glow; ?>;
}
.archive-nav a:active {
	transform: scale(0.9);
}
.archive-nav a:focus:not(:hover) {
	transform: none;
}
.archive-nav a.archive-nav-item-day:hover {
	background-color: #ddd;
}

/*==========*/
/* LISTINGS */
/*==========*/

h1.listing {
	font-family: <?php echo $UI_font; ?>, 'Font Awesome';
	font-size: 1.75rem;
}

h1.listing a[href^="http"] {
	font-size: 0.7em;
	top: 5px;
    color: #fff;
    text-shadow: 
         0.5px 0.5px 0 #000, 
        -0.5px -0.5px 0 #000,
         0 0 2px #000;
}

@media only screen and (hover: hover) {
	h1.listing a:hover,
	h1.listing a:focus {
		color: #777;
		background-color: rgba(255,255,255,0.85);
	}	
	#content.user-page h1.listing:focus-within::before {
		left: 1.25em;
	}
	h1.listing:focus-within::before {
		color: #000;
		left: 1.375em;
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
@-moz-document url-prefix() {
	h1.listing a:hover,
	h1.listing a:focus {
		color: #777;
		background-color: rgba(255,255,255,0.85);
	}	
	#content.user-page h1.listing:focus-within::before {
		left: 1.25em;
	}
	h1.listing:focus-within::before {
		color: #000;
		left: 1.375em;
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
/* LISTING POST-META */
/*===================*/

h1.listing + .post-meta {
	padding-right: 320px;
}
h1.listing + .post-meta .karma-value,
h1.listing + .post-meta .comment-count,
h1.listing + .post-meta .lw2-link,
h1.listing + .post-meta .read-time {
	border-radius: 4px;
	padding: 0 4px 0 2px;
	text-shadow: 0.5px 0.5px 0.5px #999;
	margin: 0 0.25em 0 0.5em;
	position: absolute;
	line-height: 1.15;
	top: 12px;
}
h1.listing + .post-meta .karma-value span,
h1.listing + .post-meta .comment-count span,
h1.listing + .post-meta .lw2-link span,
h1.listing + .post-meta .read-time span {
	display: none;
}
h1.listing + .post-meta .karma-value::before,
h1.listing + .post-meta .comment-count::before,
h1.listing + .post-meta .lw2-link::before,
h1.listing + .post-meta .read-time::before {
	color: #fff;
	font-family: Font Awesome;
	font-weight: 900;
	margin: 0 8px 0 0;
	box-shadow: 0 0 0 2px #ddd;
}

h1.listing + .post-meta .karma-value {
	box-shadow: 
		22px 0 0 0 #ddd inset,
		0 0 0 3px #ddd;
	cursor: default;
	color: #c00;
	right: 248px;
}
h1.listing + .post-meta .karma-value::before {
	content: "\F139";
	text-shadow: none;
    font-size: 0.9375em;
    line-height: 1.3;
}

h1.listing + .post-meta .comment-count::before {
	content: "\F086";
}
h1.listing + .post-meta .comment-count {
	box-shadow: 
		25px 0 0 0 #ddd inset,
		0 0 0 3px #ddd;
	color: #009100;
	right: 176px;
}
h1.listing + .post-meta .comment-count:hover {
	text-decoration: none;
	color: #fff;
	background-color: #009100;
}
h1.listing + .post-meta .comment-count:hover::before {
	color: #009100;
}
h1.listing + .post-meta .comment-count.new-comments::before {
	color: #009100;
	text-shadow: 0.5px 0.5px 0.5px #fff;
}
h1.listing + .post-meta .comment-count.new-comments:hover::before {
	text-shadow: 0.5px 0.5px 0.5px #999;
}


h1.listing + .post-meta .lw2-link {
	box-shadow: 
		23px 0 0 0 #ddd inset,
		0 0 0 3px #ddd;
	right: 0;
}
h1.listing + .post-meta .lw2-link::before {
	content: "\F0C1";
}
h1.listing + .post-meta .lw2-link:hover {
	text-decoration: none;
	color: #fff;
	background-color: #00f;
}
h1.listing + .post-meta .lw2-link:hover::before {
	color: #00f;
}

h1.listing + .post-meta .read-time {
	box-shadow: 
		21px 0 0 0 #ddd inset,
		0 0 0 3px #ddd;
	right: 80px;
}
h1.listing + .post-meta .read-time::before {
	content: "\F2F2";
	cursor: pointer;
}
h1.listing + .post-meta .read-time::after {
	content: " min";
}
h1.listing + .post-meta .read-time:hover::before {
	color: #777;
}

h1.listing + .post-meta .word-count {
	box-shadow: 
		22px 0 0 0 #ddd inset,
		0 0 0 3px #ddd;
	padding: 0 4px 0 4px;
}
h1.listing + .post-meta .word-count::before {
	content: "\F15C";
	margin: 0 10px 0 0;
}
h1.listing + .post-meta .read-time.word-count::after {
	content: none;
}

h1.listing + .post-meta .link-post-domain {
	margin: 0 0 0 0.5em;
}

h1.listing + .post-meta::after {
	content: "";
	display: block;
	height: 1px;
	width: calc(100% + 320px);
	background-color: #ddd;
	position: relative;
	top: 14px;
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
	height: calc(100% + 1.375em);
	box-shadow: 0px 0px 10px #555;
}
#content.user-page h1.listing + .post-meta {
	margin: 0 6px 3em 35px;
}
#content.user-page h1.listing + .post-meta::after {
	display: none;
}

.user-stats .karma-total {
	font-weight: bold;
}

/*============*/
/* LOGIN PAGE */
/*============*/

.login-container form input[type='submit'] {
	font-weight: bold;
	background-color: #eee;
	border: 1px solid #ccc;
}
.login-container form input[type='submit']:hover,
.login-container form input[type='submit']:focus {
	background-color: #ddd;
	border: 1px solid #aaa;
}

/* “Create account” form */

#signup-form {
	background-color: #f3f3f3;
	padding: 0 0 0.5em 1em;
	border: 1px solid #ddd;
}
#signup-form input[type='submit'] {
	background-color: #e4e4e4;
	border: 1px solid #ccc;
}
#signup-form input[type='submit']:hover {
	background-color: #d8d8d8;
	border: 1px solid #aaa;
}

/* Log in tip */

.login-container .login-tip {
	border: 1px solid #eee;
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

.reset-password-container input[type='submit'] {
	background-color: #e4e4e4;
	border: 1px solid #ccc;
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
	font-family: Input Sans, sans-serif;
	font-weight: 200;
}
.post-body a,
.comment-body a {
	border-bottom: 2px dotted #000;
}
.post-body a:hover,
.comment-body a:hover {
	text-decoration: none;
	color: #999;
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
.post-meta .author {
	color: #090;
}
.bottom-post-meta {
	border-top: 1px dotted #000;
}

/*============*/
/* LINK POSTS */
/*============*/

.post.link-post a.link-post-link {
	text-decoration: none;
	font-family: <?php echo $UI_font; ?>;
	font-weight: 600;
}
.post.link-post a.link-post-link:hover {
	color: #c00;
}
.post.link-post a.link-post-link:hover::before {
	color: #4879ec;
	text-shadow: 
		0.5px 0.5px 0 #fff,
		-0.5px -0.5px 0 #fff,
		0 0 2px #fff,
		0 0 3px #00c;
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
	color: <?php echo $hyperlink_color; ?>;
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
	color: #000;
}
.comment-meta .author:hover {
	text-decoration: none;
	color: #090;
}

/*===========================*/
/* COMMENT THREAD NAVIGATION */
/*===========================*/

a.comment-parent-link::before {
	color: #bbb;
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
}
.comment-popup .comment-body {
    font-size: 0.9375rem;
}

/*=======================*/
/* COMMENTS COMPACT VIEW */
/*=======================*/

#comments-list-mode-selector button {
	box-shadow:
		0 0 0 4px #fff inset,
		0 0 0 5px #000 inset;
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
	color: <?php echo $hyperlink_color; ?>;
	background: linear-gradient(to right, transparent 0%, #fff 50%, #fff 100%);
}
#content.compact > .comment-thread .comment-item:hover .comment {
	background-color: #fff;
	outline: 3px solid #00c;
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
}
.comment-minimize-button:hover {
	color: #aaa;
	text-shadow: <?php echo $white_glow; ?>;
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
.vote:hover,
.vote.selected {
	color: inherit;
}

/*===========================*/
/* COMMENTING AND POSTING UI */
/*===========================*/

.posting-controls input[type='submit'] {
	background-color: #fff;
	border: 1px solid #aaa;
	font-weight: bold;
}
.posting-controls input[type='submit']:hover,
.posting-controls input[type='submit']:focus {
	background-color: #ddd;
	border: 1px solid #999;
}

.comment-controls .cancel-comment-button {
	height: 28px;
	font-weight: 600;
	color: #fff;
}

.new-comment-button {
	font-weight: 600;
}

.comment-controls .edit-button {
	color: #0b0;
}
.comment-controls .edit-button:hover {
	color: #f00;
}

.edit-post-link,
.edit-post-link:visited {
	color: #090;
}

.posting-controls textarea {
	font-family: Input Sans, sans-serif;
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
	color: #fff;
	box-shadow: 0 0 0 1px #000;
}

.posting-controls.edit-existing-post .guiedit-buttons-container button,
.posting-controls.edit-existing-comment .guiedit-buttons-container button {
    color: #050;
}
.guiedit-buttons-container button {
	font-family: Font Awesome, Input Sans, sans-serif;
	font-weight: 200;
}

button.guiedit::after {
	color: #fff;
	top: 2px;
	height: 25px;
	text-shadow: none;
}

/* Markdown hints */

#markdown-hints-checkbox + label {
	color: <?php echo $hyperlink_color; ?>;
}
#markdown-hints-checkbox + label:hover {
	color: #e00;
	text-shadow: <?php echo $white_glow; ?>;
}
.markdown-hints {
	border: 1px solid #c00;
	background-color: #ffa;
}

/*================*/
/* EDIT POST FORM */
/*================*/

#edit-post-form .link-post-checkbox + label::before {
	border-radius: 3px;
	border: 1px solid #ddd;
	color: #777;
}
#edit-post-form .link-post-checkbox + label:hover,
#edit-post-form .link-post-checkbox:focus + label {
	text-shadow: 
		0 0 1px #fff,
		0 0 2px #fff,
		0 0 2.5px #aaa;
}
#edit-post-form .link-post-checkbox + label:hover::before,
#edit-post-form .link-post-checkbox:focus + label::before {
	border-color: #aaa;
}
#edit-post-form .link-post-checkbox:checked + label::before {
	content: "\F00C";
}
#edit-post-form input[type='radio'] + label {
	color: #777;
	border-color: #ddd;
}
#edit-post-form input[type='radio'][value='all'] + label {
	border-radius: 8px 0 0 8px;
	border-width: 1px;
}
#edit-post-form input[type='radio'][value='drafts'] + label {
	border-radius: 0 8px 8px 0;
}
#edit-post-form input[type='radio'] + label:hover,
#edit-post-form input[type='radio']:focus + label {
	background-color: #ddd;
	color: #000;
}
#edit-post-form input[type='radio']:focus + label {
	color: #000;
	box-shadow: 
		0 0 0 1px #aaa;
}
#edit-post-form input[type='radio']:checked + label {
	background-color: #ddd;
	border-color: #ddd;
	color: #000;
	text-shadow: 
		0 -1px 0 #fff,
		0 0.5px 0.5px #000;
}

/*=======*/
/* LINKS */
/*=======*/

a:link,
a:visited {
	color: inherit;
	text-decoration: none;
}
a:hover {
	text-decoration: dotted underline;
}

/*=========*/
/* BUTTONS */
/*=========*/

button,
input[type='submit'] {
	border: 2px solid #000;
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
		0 0 0 4px #000 inset;
}
button:active,
input[type='submit']:active {
	box-shadow: 
		0 0 0 4px #fff inset,
		0 0 0 6px #000 inset;
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
	font-family: <?php echo $UI_font_smallcaps; ?>;
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

#aux-about-link a {
	color: #777;
}
#aux-about-link a:hover {
	opacity: 1.0;
	text-shadow: <?php echo $white_glow; ?>;
}

.qualified-linking label {
	color: <?php echo $hyperlink_color; ?>;
}
.qualified-linking label:hover {
	text-shadow:
		0 0 1px #fff,
		0 0 3px #fff,
		0 0 5px #00e;
}

.qualified-linking-toolbar {
	border: 1px solid #000;
	background-color: #fff;
}
.qualified-linking-toolbar a {
	background-color: #eee;
	border: 1px solid #ccc;
	border-radius: 4px;
}
.qualified-linking-toolbar a:visited {
	color: <?php echo $hyperlink_color; ?>;
}
.qualified-linking-toolbar a:hover {
	text-decoration: none;
	background-color: #ddd;
	text-shadow: <?php echo $white_glow; ?>;
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
		background-color: #d8d8d8;
		opacity: 1.0;
	}
	#hns-date-picker::before {
		border: 1px solid #999;
		border-width: 1px 0 1px 1px;
	}
}
@media only screen and (max-width: 1080px) {
	#text-size-adjustment-ui button {
		border: 1px solid #999;
		padding: 0 0 0 1px;
		border-radius: 50%;
		box-shadow: 
			0 0 6px #999 inset,
			0 0 0 1px transparent;
	}
	#theme-tweaker-toggle button {
		border: 1px solid #999;
		box-shadow: 
			0 0 10px #999 inset,
			0 0 0 1px transparent;
		border-radius: 50%;
		transform: scale(0.8);
	}
}
@media only screen and (max-width: 1020px) {
	#quick-nav-ui a {
		box-shadow: 
			0 0 0 1px #999,
			0 0 0 2px transparent;
	}
	#new-comment-nav-ui .new-comments-count::before {
		background-color: #d8d8d8;
		box-shadow: 
			0 0 0 1px #999,
			0 0 0 2px transparent;
		border-radius: 8px;
	}
}
@media only screen and (max-width: 1000px) {
	#theme-selector {
		background-color: #d8d8d8;
		box-shadow: 
			0 0 0 1px #999,
			0 0 0 2px transparent;
	}
	#text-size-adjustment-ui button {
		background-color: #ddd;
	}
	#text-size-adjustment-ui button:hover {
		background-color: #eee;
	}
	#theme-tweaker-toggle button {
		background-color: #ddd;
	}
}

/*========*/
/* MOBILE */
/*========*/

@media only screen and (max-width: 900px) {
	h1.listing + .post-meta .author {
		margin: 0 0.75em 0 1.5em;
	}
	h1.listing + .post-meta .post-section::before {
		left: 0;
		top: 0;
	}
	h1.listing + .post-meta .post-section {
		display: inline-block;
	}
}
@media only screen and (max-width: 520px) {
	h1.listing + .post-meta {
		padding: 0 0 25px 0;
		white-space: unset;
	}
	h1.listing + .post-meta::after {
		width: 100%;
		top: 36px;
	}
	h1.listing + .post-meta > * {
		margin: 0;
	}
	h1.listing + .post-meta .karma-value,
	h1.listing + .post-meta .comment-count,
	h1.listing + .post-meta .lw2-link,
	h1.listing + .post-meta .read-time {
		top: unset;
		bottom: -2px;
	}
	h1.listing + .post-meta .karma-value {
		right: 250px;
	}
	h1.listing + .post-meta .comment-count {
		right: 175px;
	}
	h1.listing + .post-meta .read-time {
		right: 75px;
	}
	h1.listing + .post-meta .lw2-link {
		right: 0;
		opacity: 1;
	}
}

