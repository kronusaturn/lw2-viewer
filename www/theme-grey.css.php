<?php
	$UI_font = ($platform == 'Windows') ? "'Whitney', 'a_Avante'" : "'Concourse', 'a_Avante'";
	$UI_font_smallcaps = ($platform == 'Windows') ? "'Whitney Smallcaps', 'a_Avante'" : "'Concourse Smallcaps', 'a_Avante'";
	$hyperlink_color = "#f60";
	$white_glow = "0 0 1px #fff, 0 0 3px #fff, 0 0 5px #fff";
?>

/**************/
/* GREY THEME */
/**************/

body {
	color: #000;
    background-color: #eee;
	font-family: <?php echo $UI_font; ?>;
	font-feature-settings: 'ss07';
}
#content {
	background-color: #fff;
	box-shadow: 0px 0px 10px #bbb;
	line-height: 1.55;
}

/*=========*/
/* NAV BAR */
/*=========*/

.nav-inner {
	font-weight: normal;
	font-size: 1.1875em;
}
#secondary-bar .nav-inner {
    font-size: 0.875rem;
}

.nav-bar .nav-item:not(.nav-current):not(#nav-item-search):hover,
#bottom-bar a:hover,
#nav-item-search:not(.nav-current):focus-within {
	background-color: #ddd;
}
.inactive-bar .nav-item:not(.nav-current):not(#nav-item-search):hover,
.inactive-bar #nav-item-search:not(.nav-current):focus-within {
	background-color: #d8d8d8;
}

.nav-bar a:link,
.nav-bar a:visited {
    color: #888;
}
.nav-bar a:hover,
.nav-bar a:focus {
	text-decoration: none;
	text-shadow: <?php echo $white_glow; ?>;
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

/* This makes the navbar items look like tabs: */

.nav-inactive {
	box-shadow: 
		 0 -1px #d8d8d8 inset,
		 1px 0 #fff inset;
}
.nav-inactive:first-child {
	box-shadow: 0 -1px #d8d8d8 inset;
}
.inactive-bar .nav-inactive {
	background-color: #e4e4e4;
}
.active-bar .nav-inactive {
	background-color: #eee;
}
.active-bar {
	position: relative;
}

/* For Webkit: */
.active-bar {
	box-shadow: 0 -3px 8px -2px #ccc;
}
.active-bar .nav-inactive {
	box-shadow: 
		0 -4px 8px -4px #bbb inset,
		1px 0 #fff inset;
}
.active-bar .nav-inactive:first-child {
	box-shadow: 
		0 -4px 8px -4px #bbb inset;
}
.active-bar .nav-current + .nav-inactive {
	box-shadow: 
		5px -4px 8px -4px #bbb inset;
}
.active-bar .nav-item-last-before-current {
	box-shadow: 
		-5px -4px 8px -4px #bbb inset,
		1px 0 #fff inset;
}
.active-bar .nav-item-last-before-current:first-child {
	box-shadow: 
		-5px -4px 8px -4px #bbb inset;
}
/* And for Gecko: */
@-moz-document url-prefix() {
	.active-bar {
		box-shadow: 0 -3px 4px -2px #ccc;
	}

	.active-bar .nav-inactive {
		box-shadow: 
			0 -4px 4px -4px #bbb inset,
			1px 0 #fff inset;
	}
	.active-bar .nav-inactive:first-child {
		box-shadow: 
			0 -4px 4px -4px #bbb inset;
	}
	.active-bar .nav-current + .nav-inactive {
		box-shadow: 
			5px -4px 4px -4px #bbb inset;
	}
	.active-bar .nav-item-last-before-current {
		box-shadow: 
			-5px -4px 4px -4px #bbb inset,
			1px 0 #fff inset;
	}
	.active-bar .nav-item-last-before-current:first-child {
		box-shadow: 
			-5px -4px 4px -4px #bbb inset;
	}
}

/* Search tab */

#nav-item-search form::before {
    opacity: 0.4;
	font-size: 0.9375rem;
}
#nav-item-search button {
    color: #999;
	border: none;
	font-weight: inherit;
}

/*==============*/
/* PAGE TOOLBAR */
/*==============*/

.new-post,
.new-post:visited,
.new-private-message,
.new-private-message:visited {
	color: #999;
}
.logout-button {
	color: #999;
}

/*==============*/
/* SUBLEVEL NAV */
/*==============*/

.sublevel-nav .sublevel-item {
	border-color: #ddd;
	border-style: solid;
	border-width: 1px 1px 1px 0;
	color: #777;
}
.sublevel-nav .sublevel-item:first-child {
	border-radius: 8px 0 0 8px;
	border-width: 1px;
}
.sublevel-nav .sublevel-item:last-child {
	border-radius: 0 8px 8px 0;
}
.sublevel-nav a.sublevel-item:hover {
	background-color: #ddd;
	color: #000;
	text-decoration: none;
}
.sublevel-nav a.sublevel-item:active,
.sublevel-nav span.sublevel-item {
	background-color: #ddd;
	border-color: #ddd;
	color: #000;
	text-shadow: 
		0 -1px 0 #fff,
		0 0.5px 0.5px #000;
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
/* THEME SELECTOR */
/*================*/

#theme-selector button,
#width-selector button {
	box-shadow:
		0 0 0 4px #eee inset,
		0 0 0 5px #ccc inset;
}
#theme-selector button:hover,
#theme-selector button.selected,
#width-selector button:hover,
#width-selector button.selected {
	box-shadow:
		0 0 0 5px #ccc inset;
}

/*======================*/
/* THEME TWEAKER TOGGLE */
/*======================*/

#theme-tweaker-toggle button {
	color: #777;
}

/*=================*/
/* QUICKNAV WIDGET */
/*=================*/

#quick-nav-ui a {
	color: #999;
	background-color: #e4e4e4;
	border-radius: 4px;
}
#quick-nav-ui a[href='#bottom-bar'] {
	line-height: 1.8;
}
#quick-nav-ui a:hover  {
	color: #000;
	background-color: #d8d8d8;
	text-decoration: none;
}
#quick-nav-ui a:active {
	transform: scale(0.9);
}
#quick-nav-ui a:focus:not(:hover) {
	transform: none;
	text-shadow: none;
}
#quick-nav-ui a[href='#comments'].no-comments {
	opacity: 0.4;
	color: #bbb;
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

#text-size-adjustment-ui button {
	color: #777;
}
#text-size-adjustment-ui button.default {
	font-weight: 600;
}
#text-size-adjustment-ui button:disabled:hover {
	text-shadow: none;
}

/*=============================*/
/* COMMENTS VIEW MODE SELECTOR */
/*=============================*/

#comments-view-mode-selector a {
	color: #777;
}

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
	color: #888;
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
	margin: 0.6em 20px 0.1em 20px;
    font-size: 1.5rem;
}

h1.listing a[href^='/'] {
	font-weight: normal;
}
h1.listing a[href^="http"] {
	color: #bbb;
}

@media only screen and (hover: hover) {
	h1.listing a:hover,
	h1.listing a:focus {
		color: #777;
	}	
	#content.user-page h1.listing:focus-within::before {
		left: -0.75em;
	}
	h1.listing:focus-within::before {
		color: #00f;
	}
	h1.listing a[href^="http"]:hover {
		color: #4879ec;
		text-shadow: 
			 0.5px 0.5px 0 #fff,
			 -0.5px -0.5px 0 #fff,
			 0 0 2px #fff,
			 0 0 3px #00c;
	}
}
@-moz-document url-prefix() {
	h1.listing a:hover,
	h1.listing a:focus {
		color: #777;
	}	
	#content.user-page h1.listing:focus-within::before {
		left: -0.75em;
	}
	h1.listing:focus-within::before {
		color: #00f;
	}
	h1.listing a[href^="http"]:hover {
		color: #4879ec;
		text-shadow: 
			 0.5px 0.5px 0 #fff,
			 -0.5px -0.5px 0 #fff,
			 0 0 2px #fff,
			 0 0 3px #00c;
	}
}

/*===================*/
/* LISTING POST-META */ /************************************/
/*===================*/

h1.listing + .post-meta > * {
    color: #222;
    font-size: 1em;
}

h1.listing + .post-meta .comment-count::before,
h1.listing + .post-meta .lw2-link::before,
h1.listing + .post-meta .read-time::before {
	font-family: Font Awesome;
	font-weight: 900;
}

h1.listing + .post-meta .karma-value::before {
	content: "\F139";
}

h1.listing + .post-meta .karma {
    float: left;
    margin-right: 3px;
}

h1.listing + .post-meta .karma::after {
    content: " by";
}

h1.listing + .post-meta .author {
    margin-right: 0;
}

h1.listing + .post-meta .date::before {
    content: "on ";
}

h1.listing + .post-meta .date::after {
    content: " — ";
	opacity: 0.5;
}

h1.listing + .post-meta .date {
    margin-right: 5px;
}

h1.listing + .post-meta .comment-count {
    margin-left: 0;
    margin-right: 4px;
}


h1.listing + .post-meta .comment-count.new-comments::before {
	color: #0c0;
}

h1.listing + .post-meta .post-section::before {
    left: -26px;
    top: 0px;
    font-size: 0.9375em;
}

/*============*/
/* USER PAGES */
/*============*/

/*============*/
/* USER PAGES */
/*============*/

/*============*/
/* LOGIN PAGE */
/*============*/

/*=====================*/
/* PASSWORD RESET PAGE */
/*=====================*/

/*===================*/
/* TABLE OF CONTENTS */
/*===================*/

.contents {
	padding-right: 0.5em;
}
.post-body .contents a:link {
	color: #d64400;
}
.post-body .contents ul {
	font-size: 0.85em;
}

/*==================*/
/* POSTS & COMMENTS */ /************************************/
/*==================*/

.post-body,
.comment-body {
	font-family: Source Sans Pro, Trebuchet MS, Helvetica, Arial, Verdana, sans-serif;
	font-weight: 400;
}
@-moz-document url-prefix() {
	.post-body,
	.comment-body {
		font-weight: <?php global $platform; echo ($platform == 'Windows' ? '300' : '400'); ?>;
	}
}

.post > h1:first-child {
	margin: 1.1em 0 0.25em 0;
	font-weight: 400;
	color: #222;
	font-size: 3em;
}

.post-body {
	font-size: 1.1875rem;
	line-height: 1.6;
}
@media (-webkit-max-device-pixel-ratio: 1), (max-resolution: 191dpi) { 
	.post-body {
		font-size: 1.125rem;
	}
}
.comment-body {
	font-size: 1.125rem;
}

/*===========*/
/* POST-META */ /************************************/
/*===========*/

.post-meta a,
.post-meta .author,
.post-meta .date {
	color: #222;
}

.post-meta > * {
	margin: 0;
}
.post-meta .comment-count span,
.post-meta .read-time span,
.post-meta .word-count span,
.post-meta .lw2-link span {
	display: none;
}
.post-meta .comment-count::before,
.post-meta .read-time::before,
.post-meta .word-count::before,
.post-meta .lw2-link::before {
	font-family: Font Awesome;
	margin: 0 0.25em 0 0;
	font-size: 0.875em;
	color: #ccc;
}
.post-meta .comment-count {
	margin: 0 0.25em 0 0;
}
.post-meta .read-time,
.post-meta .word-count,
.post-meta .lw2-link {
	margin: 0 0.25em 0 0.5em;
}
.post-meta .lw2-link {
	opacity: 1;
}
.post-meta .comment-count:hover,
.post-meta .lw2-link:hover {
	text-decoration: none;
	text-shadow: 
		0 0 0.5px #fff,
		0 0 1px #fff,
		0 0 8px #777;
}
.post-meta .comment-count:hover::before,
.post-meta .lw2-link:hover::before {
	color: #777;
}
.post-meta .read-time:hover::before {
	color: #777;
	cursor: pointer;
}
.post-meta .comment-count::before {
	content: "\F086";
}
.post-meta .read-time::before {
	content: "\F017";
}
.post-meta .read-time::after {
	content: " min";
}
.post-meta .word-count::before {
	content: "\F15C";
}
.post-meta .word-count::after {
	content: "";
}
.post-meta .lw2-link::before {
	content: "\F0C1";
	font-weight: 900;
	opacity: 0.8;
	font-size: 0.75em;
	position: relative;
	bottom: 1px;
}

.post .post-meta .comment-count {
	margin: 0 0.5em;
}
.post .post-meta .lw2-link {
	margin: 0 1em 0 0.5em;
}
.post .post-meta .karma {
	margin: 0 0 0 0.5em;
}

/*============*/
/* LINK POSTS */
/*============*/

/*==========*/
/* COMMENTS */
/*==========*/

/*================================*/
/* DEEP COMMENT THREAD COLLAPSING */
/*================================*/

/*==============*/
/* COMMENT-META */
/*==============*/

.comment-meta a {
	color: #222;
}
.comment-meta .author {
	color: #999;
	font-weight: 600;
}

/*===========================*/
/* COMMENT THREAD NAVIGATION */
/*===========================*/

/*=======================*/
/* COMMENTS COMPACT VIEW */
/*=======================*/

.new-comment::before {
	outline: 2px solid #9037ff;
	box-shadow:
		0 0 6px -2px #9037ff inset, 
		0 0 4px #9037ff, 
		0 0 6px #9037ff;
}

/*===========================*/
/* HIGHLIGHTING NEW COMMENTS */
/*===========================*/

/*=================================*/
/* COMMENT THREAD MINIMIZE BUTTONS */
/*=================================*/

/*==============*/
/* VOTE BUTTONS */
/*==============*/

.upvote,
.downvote {
	color: #c8c8c8;	
}
.upvote::before {
	content: "\F077";
}
.downvote::before {
	content: "\F078";
	position: relative;
	left: -2px;
	top: 1px;
}
.upvote:hover,
.upvote.selected {
	text-shadow:
		0 0 0.5px #fff, 
		0 0 8px #0f0;
}
.downvote:hover,
.downvote.selected {
	text-shadow:
		0 0 0.5px #fff, 
		0 0 8px #f00;
}
/*===========================*/
/* COMMENTING AND POSTING UI */
/*===========================*/

.posting-controls textarea {
	font-weight: 400;
	font-family: Source Sans Pro, Trebuchet MS, Helvetica, Arial, Verdana, sans-serif;
}
@-moz-document url-prefix() {
	.posting-controls textarea {
		font-weight: <?php global $platform; echo ($platform == 'Windows' ? '300' : '400'); ?>;
	}
}

.guiedit-buttons-container button {
	font-family: Font Awesome, Source Sans Pro, Trebuchet MS, Helvetica, Arial, Verdana, sans-serif;
	color: #00e;
}
#markdown-hints-checkbox + label {
	color: #999;
}

/*================*/
/* EDIT POST FORM */
/*================*/

/*=======*/
/* LINKS */
/*=======*/

a {
	text-decoration: none;
	color: <?php echo $hyperlink_color; ?>;
}
a:visited {
	color: #ff943b;
}
a:hover {
	text-decoration: underline;
}

/*=========*/
/* BUTTONS */
/*=========*/

button,
input[type='submit'] {
	color: <?php echo $hyperlink_color; ?>;
}

.button,
.button:visited {
	color: #999;
}

/*==========*/
/* HEADINGS */
/*==========*/

.post-body h1,
.post-body h2,
.post-body h3,
.post-body h4,
.post-body h5,
.post-body h6,
.comment-body h1,
.comment-body h2,
.comment-body h3,
.comment-body h4,
.comment-body h5,
.comment-body h6 {
	font-weight: 400;
}
.post-body h2 {
	border-bottom: 1px dotted #ccc;
}

/*========*/
/* QUOTES */
/*========*/

/*========*/
/* IMAGES */
/*========*/

/*======*/
/* MISC */
/*======*/

.frac {
	padding-left: 2px;
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

/*============*/
/* USER STATS */
/*============*/

/*========================*/
/* QUALIFIED HYPERLINKING */
/*========================*/

/*======*/
/* MATH */
/*======*/

/*====================*/
/* FOR NARROW SCREENS */
/*====================*/

@media only screen and (max-width: 1440px) {
	#hns-date-picker {
		background-color: #eee;
		opacity: 1.0;
	}
	#hns-date-picker::before {
		border: 1px solid #ccc;
		border-width: 1px 0 1px 1px;
	}
}