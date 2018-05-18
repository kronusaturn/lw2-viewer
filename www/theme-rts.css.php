<?php
	$UI_font = "'Proxima Nova', sans-serif";
	$text_font = "'Garamond Premier Pro', 'Georgia', serif";
	$hyperlink_color = "#00e";
	$white_glow = "0 0 1px #fff, 0 0 3px #fff, 0 0 5px #fff";
?>

/******************************/
/* READTHESEQUENCES.COM THEME */
/******************************/

body {
	color: #000;
	background-color: #fffffa;
	font-family: <?php echo $UI_font; ?>;
}
#content {
	line-height: 1.55;
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

a.nav-inner,
#nav-item-search button {
	font-weight: <?php echo ($platform == 'Mac') ? '200' : '300'; ?>;
	color: #888;
}


.nav-bar a:visited {
	color: <?php echo $hyperlink_color; ?>;
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
	font-weight: <?php echo ($platform == 'Mac') ? '200' : '300'; ?>;
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

#bottom-bar {
	border-top: 1px solid #bbb;
	margin-top: 2em;
}
.post-page #bottom-bar {
	margin-top: 0.5em;
}
#content.no-nav-bars {
	margin: auto;
}
#content.no-comments #bottom-bar {
	margin-top: 0.125em;
}
.nav-bar .nav-item:not(:last-child) {
	border-right: 1px solid #bbb;
}
.nav-bar .nav-item:not(.nav-current):not(#nav-item-search):hover,
.inactive-bar #nav-item-search:focus-within {
	background-color: #f0f0eb;
}
.nav-bar a:hover,
.nav-bar a:focus,
.nav-bar button:hover,
.nav-bar button:focus {
	color: #333;
	text-shadow: 0px 0px 0.5px #333;
}
#bottom-bar .nav-item a::before,
#top-nav-bar a::before {
	font-size: 1em;
	bottom: -2px;
	color: #aaa;
}
#bottom-bar .nav-item a:hover::before,
#top-nav-bar a:hover::before {
	color: #333;
}
#bottom-bar #nav-item-first a::before,
#top-nav-bar a.nav-item-first::before {
	content: "\F0D9\F0D9";
}
#bottom-bar #nav-item-prev a::before,
#top-nav-bar a.nav-item-prev::before {
	content: "\F0D9";	
}
#bottom-bar #nav-item-top a::before {
	content: "\F0D8";
}
#bottom-bar #nav-item-next a::before,
#top-nav-bar a.nav-item-next::before {
	content: "\F0DA";
}
#bottom-bar #nav-item-next a::before {
	margin-left: -1em;
	left: 2.85em;
}
.active-bar {
	position: relative;
	border-color: #bbb;
	border-width: 1px 0;
}

/* Search tab */

#nav-item-search form::before {
	color: #999;
}
/* 
#nav-item-search button {
	border: none;
	font-weight: inherit;
}
 */

/*==============*/
/* PAGE TOOLBAR */
/*==============*/

.page-toolbar * {
	color: #888;
	font-weight: <?php echo ($platform == 'Mac') ? '300' : '400'; ?>;
}
.logout-button {
	color: #d33;
}

/*===================*/
/* TOP PAGINATION UI */
/*===================*/

#top-nav-bar {
	margin: 0 0 -1.5em 0;
}
#top-nav-bar .page-number {
	font-family: <?php echo $text_font; ?>;
}
#top-nav-bar .page-number-label {
	font-family: <?php echo $UI_font; ?>;
}
#top-nav-bar::after {
	content: "";
	background-image: linear-gradient(to right, rgba(0, 0, 0, 0), rgba(0, 0, 0, 0.75), rgba(0, 0, 0, 0));
	display: block;
	width: 75%;
	margin: 0.125em auto 2.75em auto;
	height: 1px;
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
.sublevel-nav + #top-nav-bar {
	margin-top: 0;
}

/*=====================*/
/* SORT ORDER SELECTOR */
/*=====================*/

.sublevel-nav.sort .sublevel-item {
	font-feature-settings: 'smcp';
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

#width-selector button,
#theme-selector button {
	box-shadow: 
		0 0 0 4px #fffffa inset, 
		0 0 0 5px #bbb inset;
}
#width-selector button:hover,
#width-selector button.selected,
#theme-selector button:hover,
#theme-selector button.selected {
	box-shadow: 
		0 0 0 1px #bbb inset,
		0 0 0 4px #fffffa inset,
		0 0 0 5px #bbb inset;
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
	box-shadow: 0 0 0 1px #ccc inset;
	color: #bbb;
	border-radius: 4px;
}
#quick-nav-ui a[href='#bottom-bar'] {
	line-height: 1.8;
}
#quick-nav-ui a:hover {
	background-color: #f0f0eb;
	color: #333;
	text-shadow: 0px 0px 0.5px #333;
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
	border: 1px solid transparent;
}
.archive-nav *[class^='archive-nav-item'] {
	border-style: solid;
	border-color: #ddd;
	border-width: 1px 0 1px 1px;
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
	color: #999;
	font-weight: 300;
}
.archive-nav a:hover,
.archive-nav span[class^='archive-nav-item'] {
	font-weight: 300;
	background-color: #f0f0eb;
	color: #333;
    text-shadow: 0px 0px 0.5px #333;
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
	text-align: center;
}

h1.listing a[href^="http"] {
	font-size: 0.6875em;
	top: 3px;
	color: #888;
}
h1.listing a[href^='/'] {
	font-family: <?php echo $text_font; ?>;
	color: #690010;
	font-weight: 500;
	text-shadow: 0.5px 0.5px 0.5px #de7069;
	padding: 0 2px 1px 1px;
}

@media only screen and (hover: hover) {
	h1.listing a:hover,
	h1.listing a:focus {
		color: #777;
		text-shadow: 
			0px 0px 0.5px #ff987b, 
			0px 0px 1.5px #c05651,
			0.5px 0.5px 0.5px #de7069;
		background-color: rgba(255,255,255,0.85);
	}	
	#content.user-page h1.listing:focus-within::before {
		left: -0.75em;
	}
	h1.listing:focus-within::before {
	color: #690010;
	}
	h1.listing a[href^="http"]:hover {
		color: #690010;
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
		text-shadow: 
			0px 0px 0.5px #ff987b, 
			0px 0px 1.5px #c05651,
			0.5px 0.5px 0.5px #de7069;
		background-color: rgba(255,255,255,0.85);
	}	
	#content.user-page h1.listing:focus-within::before {
		left: -0.75em;
	}
	h1.listing:focus-within::before {
	color: #690010;
	}
	h1.listing a[href^="http"]:hover {
		color: #690010;
		text-shadow: 
			 0.5px 0.5px 0 #fff,
			 -0.5px -0.5px 0 #fff,
			 0 0 2px #fff,
			 0 0 3px #00c;
	}
}

/*===================*/
/* LISTING POST-META */
/*===================*/


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
h1.listing + .post-meta .post-section::before {
	left: unset;
}

/*============*/
/* USER PAGES */
/*============*/

#content.user-page h1.page-main-heading {
	border-bottom: 1px solid #ccc;
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
	border: 1px solid #ddd;
	background-color: #eee;
}
.contents-head {
	font-weight: bold;
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
}
.post > h1:first-child {
	font-family: <?php echo $text_font; ?>;
	font-weight: 600;
}
.post-body,
.comment-body {
	font-family: <?php echo $text_font; ?>;
	font-weight: 500;
	line-height: 1.45;
}

.post-body a,
.post-body a:visited,
.comment-body a,
.comment-body a:visited {
	text-decoration: none;
	color: inherit;
}
.post-body a:link::after,
.post-body a:visited::after,
.comment-body a:link::after,
.comment-body a:visited::after {
    position: relative;
    content: "﻿°";
    margin-left: 2px;
    margin-right: 1px;
    color: #933;
}
.post-body a:hover {
    color: #999;
    border-bottom: 1px dotted #999;
}

/*===========*/
/* POST-META */
/*===========*/

.post-meta {
	text-align: center;
	font-weight: <?php echo ($platform == 'Mac') ? '300' : '400'; ?>;
}
.post-meta .post-section::before {
	color: #fff;
	top: -1px;
	text-shadow: 
		1px 1px 0 #999, 
		0 1px 0 #999, 
		0 0 5px #999;
	margin: 0 0 0 0.5em;
}
.post-meta > *,
.comment-meta a {
	color: #999;
}
.post-meta a:hover,
.comment-meta a:hover {
	color: #333;
	text-shadow: 0px 0px 0.5px #333;=
}
.bottom-post-meta {
	border-top: 1px solid #ddd;
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
	font-size: 1.375rem;
}
.post-page .post-meta::after {
	display: block;
	margin: 1em 0 0 0;
	font-size: 1.5rem;
}
.post-page .post-meta:first-of-type::after {
	content: "❦";
}
.post-page .post-meta:last-of-type::after {
	content: "❖";
	font-size: 1.75rem;
	margin: 0.625em 0 0 0;
}
.post-body {
	margin: 0;
}

/*==========*/
/* COMMENTS */
/*==========*/

.comment-item {
	border: 1px solid #ccc;
}

<?php
	function nested_stuff($segment, $tip, $last_tip, $nesting_levels) {
		for ($i = $nesting_levels; $i > 0; $i--) {
			for ($j = $i; $j > 0; $j--)
				echo $segment;
			echo $tip;
		}
		echo $last_tip;
	}
	$comment_nesting_depth = 10;
?>

.comment-item {
	box-shadow: 
		0 0  3px #bbb,
		0 0  5px #bbb,
		0 0  7px #bbb,
		0 0 10px #bbb;
}
.comment-body {
	font-size: 1.25rem;
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

.comment-meta {
	font-weight: <?php echo ($platform == 'Mac') ? '300' : '400'; ?>;
	padding-top: 5px;
	padding-bottom: 4px;
	border-bottom: 1px dotted #bbb;
}

.comment-meta .author {
	font-size: 1.125em;
	color: #666;
	font-weight: 600;
}

/*===========================*/
/* COMMENT THREAD NAVIGATION */
/*===========================*/

a.comment-parent-link::before {
	color: #bbb;
}
a.comment-parent-link:hover::before {
	background-color: #ffd;
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

.comment-item-highlight {
	box-shadow:
		0 0	2px #e7b200,
		0 0	3px #e7b200,
		0 0	5px #e7b200,
		0 0	7px #e7b200,
		0 0 10px #e7b200;
	border: 1px solid #e7b200;
}
.comment-item-highlight-faint {
	box-shadow:
		0 0	2px #f8e7b5,
		0 0	3px #f8e7b5,
		0 0	5px #f8e7b5,
		0 0	7px #f8e7b5,
		0 0 10px #f8e7b5;
	border: 1px solid #f8e7b5;
}

.comment-popup {
	background-color: #fff;
}

/*=======================*/
/* COMMENTS COMPACT VIEW */
/*=======================*/

#comments-list-mode-selector button {
	box-shadow:
		0 0 0 4px #fff inset,
		0 0 0 5px #bbb inset;
}
#comments-list-mode-selector button:hover,
#comments-list-mode-selector button.selected {
	box-shadow:
		0 0 0 5px #bbb inset;
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

.new-comment::before {
	outline: 2px solid #5a5;
	box-shadow:
		0 0 6px -2px #5a5 inset, 
		0 0 4px #5a5, 
		0 0 6px #5a5;
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
	font-family: <?php echo $UI_font; ?>;
	color: #777;
}
.comment-minimize-button.maximized::after {
	color: #ccc;
}

/*==============*/
/* VOTE BUTTONS */
/*==============*/

.post-meta .vote {
	color: #bbb;
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

#comments > .comment-controls {
	margin: 0;
}
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
.comment + .comment-controls .action-button {
	font-weight: <?php echo ($platform == 'Mac') ? '300' : '400'; ?>;
}

.comment-controls .cancel-comment-button {
	font-weight: 600;
	color: #c00;
	text-shadow: 
		0 0 1px #fff,
		0 0 2px #fff;
}
.comment-controls .cancel-comment-button:hover {
	color: #f00;
	text-shadow: <?php echo $white_glow; ?>;
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
	font-family: <?php echo $text_font; ?>;
	font-size: 1.25rem;
	font-weight: 500;
	color: #000;
	background-color: #fff;
	border-color: #aaa;
	box-shadow: 
		0 0 0 1px #eee inset;
}
.posting-controls textarea:focus {
	background-color: #ffd;
	border-color: <?php echo $hyperlink_color; ?>;
	box-shadow: 
		0 0 0 1px #ddf inset,
		0 0 0 1px #fff,
		0 0 0 2px <?php echo $hyperlink_color; ?>;
}
.posting-controls.edit-existing-post textarea:focus,
.posting-controls.edit-existing-comment textarea:focus {
	border-color: #090;
    box-shadow: 
    	0 0 0 1px #81ff7f inset,
    	0 0 0 1px #fff,
    	0 0 0 2px #090;
}

/* GUIEdit buttons */

.guiedit-buttons-container {
	background-image: linear-gradient(to bottom, #fff 0%, #ddf 50%, #ccf 75%, #aaf 100%);
}

.posting-controls.edit-existing-post .guiedit-buttons-container button,
.posting-controls.edit-existing-comment .guiedit-buttons-container button {
    color: #050;
}
.guiedit-buttons-container button {
	font-family: Font Awesome, <?php echo $text_font; ?>;
}

.guiedit::after {
	font-family: <?php echo $UI_font; ?>;
	color: #777;
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

a {
	text-decoration: none;
	color: <?php echo $hyperlink_color; ?>;
}
a:visited {
	color: #551a8b;
}
a:hover {
	text-decoration: underline;
}

/*=========*/
/* BUTTONS */
/*=========*/

button,
input[type='submit'] {
	color: #888;
}

button:hover,
input[type='submit']:hover,
button:focus,
input[type='submit']:focus {
	color: #333;
	text-shadow: 0px 0px 0.5px #333;
}
button:active,
input[type='submit']:active {
	color: #f00;
	transform: scale(0.9);
}
.button:visited {
	color: <?php echo $hyperlink_color; ?>;
}
.button:hover {
	color: #333;
	text-shadow: 0px 0px 0.5px #333;
	text-decoration: none;
}
.button:active {
	transform: scale(0.9);
}
.button:focus:not(:hover) {
	transform: none;
}
@-moz-document url-prefix() {
	.button:active {
		transform: none;
	}
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
	margin: 1.5em 0 0.25em 0;
}
.post-body h4,
.comment-body h4 {
	font-size: 1.15em;
}
.post-body h3, 
.comment-body h3 {
	font-variant: small-caps;
	font-size: 1.3em;
}
.post-body h2, 
.comment-body h2 {
	font-style: italic;
	font-size: 1.5em;
}
.post-body h1,
.comment-body h1 {
	font-size: 1.9em;
	border: none;
}
.post-body h1 {
	text-align: center;
	margin: 1em 0 0 0;
}
.post-body h2 {
	margin: 1em 0 0 0;
}
.post-body h1::before {
	content: "❦";
	display: block;
	margin: 0 auto 1em auto;
	font-size: 0.625em;
	font-weight: normal;
}
.post-body h2::before {
	content: "☙";
	text-align: center;
	display: block;
	margin: 0 auto 0.5em auto;
	font-weight: normal;
	font-style: normal;
}
.post-body h1:first-child::before,
.post-body .contents + h1::before,
.post-body h2:first-child::before,
.post-body .contents + h2::before {
	content: "";
}

/*========*/
/* QUOTES */
/*========*/

blockquote {
	border-left: 5px solid #ccc;
}

/*========*/
/* IMAGES */
/*========*/

#content img {
	border: 1px solid #ccc;
}
#content img[style^='float'] {
	border: 1px solid transparent;
}
#content img[src$='.svg'] {
	border: none;
}
#content figure img {
	border: 1px solid #000;
}
#content figure img[src$='.svg'] {
	border: none;
}

/*======*/
/* MISC */
/*======*/

hr {
	height: 1px;
	background-image: linear-gradient(to right, rgba(0, 0, 0, 0), rgba(0, 0, 0, 0.75), rgba(0, 0, 0, 0));
}

code {
	background-color: #f6f6ff;
	border: 1px solid #ddf;
	border-radius: 4px;
}

input[type='text'],
input[type='search'],
input[type='password'],
textarea {
	background-color: transparent;
	border: 1px solid #ccc;
	color: #000;
}
input[type='text']:focus,
input[type='search']:focus,
input[type='password']:focus,
textarea:focus {
	background-color: #ffd;
	border: 1px solid #bbb;
	box-shadow: 0 0 1px #bbb;
}

select {
	color: #000;
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
	color: #bbb;
}
.qualified-linking label:hover {
	color: #333;
	text-shadow: 0px 0px 0.5px #333;
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

