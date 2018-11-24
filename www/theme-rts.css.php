<?php
	$UI_font = "'Proxima Nova', sans-serif";
	$text_font = "'Garamond Premier Pro', 'Georgia', 'Times New Roman', serif";
	$hyperlink_color = "#888";
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

#bottom-bar.decorative {
	border: none;
}
#bottom-bar.decorative::before,
#bottom-bar.decorative::after {
	content: "GW";
	font-weight: 200;
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
}
#bottom-bar.decorative::after {
	color: #d8d8d8;
	position: absolute;
	left: 0;
	right: 0;
	margin: auto;
	background-color: #fff;
	padding-right: 4px;
	padding-left: 4px;
}
<?php fit_content("#bottom-bar.decorative::after"); ?>

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
#nav-item-search:focus-within {
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
	border-style: solid;
	border-color: #bbb;
	border-width: 1px 0;
}

/* Search tab */

#nav-item-search form::before {
	color: #999;
}
#nav-item-search button:hover {
	color: #333;
}
#nav-item-search input::placeholder {
	color: #d00;
	font-weight: normal;
}

/* User/login tab */

#inbox-indicator::before {
	color: #ddd;
}

/*= Top pagination UI hover tooltips =*/

#top-nav-bar a::after,
#bottom-bar a::after {
	color: #222;
}

/*==============*/
/* PAGE TOOLBAR */
/*==============*/

.page-toolbar {
	grid-row: 3;
}
.page-toolbar * {
	color: #888;
	font-weight: <?php echo ($platform == 'Mac') ? '300' : '400'; ?>;
}
.page-toolbar a:hover,
.page-toolbar button:hover {
	text-shadow: 0px 0px 0.5px #333;
}
.logout-button {
	color: #d33;
}

/*===================*/
/* TOP PAGINATION UI */
/*===================*/

#top-nav-bar {
	margin: 0;
	flex-wrap: wrap;
	grid-column: 1 / span 3;
	grid-row: 3;
}
#top-nav-bar .page-number {
	font-family: <?php echo $text_font; ?>;
	padding-top: 7px;
}
#top-nav-bar .page-number-label {
	font-family: <?php echo $UI_font; ?>;
	bottom: 70%;
}
#top-nav-bar::after {
	content: "";
	background-image: linear-gradient(to right, rgba(0, 0, 0, 0), rgba(0, 0, 0, 0.75), rgba(0, 0, 0, 0));
	background-origin: content-box;
	background-repeat: no-repeat;
	display: block;
	margin: 0.125em auto 0 auto;
	padding: 0 12.5%;
	height: 1px;
	flex: 1 0 100%;
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
.sublevel-nav .sublevel-item:hover {
	background-color: #ddd;
	color: #000;
	text-decoration: none;
}
.sublevel-nav .sublevel-item:active,
.sublevel-nav .sublevel-item:disabled,
.sublevel-nav span.sublevel-item {
	background-color: #ddd;
	border-color: #ddd;
	color: #000;
	text-shadow: 
		0 -1px 0 #fff,
		0 0.5px 0.5px #000;
	transform: none;
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
	padding: 20px 0 0 0;
	border-radius: 8px;
	box-shadow: 0 20px #bbb inset;
}
.sublevel-nav.sort::before {
	text-transform: uppercase;
	font-weight: 600;
	color: #444;
	text-shadow: 0.5px 0.5px 0 #fff;
	line-height: 2;
}
.sublevel-nav.sort .sublevel-item {
	padding: 6px 6px 4px 6px;
	text-transform: uppercase;
	border: 1px solid #aaa;
}

/* Vertical */
.sublevel-nav.sort .sublevel-item:first-child {
	border-radius: 6px 6px 0 0;
}
.sublevel-nav.sort .sublevel-item:last-child {
	border-radius: 0 0 6px 6px;
}
.sublevel-nav.sort .sublevel-item:nth-child(n+2) {
	border-width: 0 1px 1px 1px;	
}

/* Horizontal */
.sublevel-nav.sort.horizontal .sublevel-item:first-child {
	border-radius: 6px 0 0 6px;
}
.sublevel-nav.sort.horizontal .sublevel-item:last-child {
	border-radius: 0 6px 6px 0;
}
.sublevel-nav.sort.horizontal .sublevel-item:nth-child(n+2) {
	border-width: 1px 1px 1px 0;
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

#theme-selector button::before {
	font-size: 0.9375em;
	padding: 6px;
	font-weight: 300;
	color: #bbb;
	background-color: #fff;
}
#theme-selector button:hover::before,
#theme-selector button.selected::before {
	color: #444;
}
#width-selector button::after {
	color: #aaa;
	font-weight: 300;
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
#quick-nav-ui a:active {
	transform: scale(0.9);
}
#quick-nav-ui a[href='#comments'].no-comments {
	opacity: 0.4;
	color: #bbb;
}
@media only screen and (hover: hover), not screen and (-moz-touch-enabled) {
	#quick-nav-ui a:hover {
		background-color: #f0f0eb;
		color: #333;
		text-shadow: 0px 0px 0.5px #333;
	}
	#quick-nav-ui a:focus:not(:hover) {
		transform: none;
		text-shadow: none;
	}
}

/*======================*/
/* NEW COMMENT QUICKNAV */
/*======================*/

#new-comment-nav-ui .new-comments-count,
#new-comment-nav-ui .new-comments-count::after {
	color: #999;
	font-weight: normal;
}
#new-comment-nav-ui .new-comments-count {
	text-shadow: 0.5px 0.5px 0 #fff;
}
#new-comment-nav-ui .new-comment-sequential-nav-button {
	color: #999;
}
#new-comment-nav-ui .new-comment-sequential-nav-button:disabled {
	color: #e6e6e6;
	text-shadow: none;
}
@media only screen and (hover: hover), not screen and (-moz-touch-enabled) {
	#new-comment-nav-ui .new-comments-count:hover {
		text-shadow: 
			0 0 1px #fff,
			0 0 3px #fff,
			0 0 5px #fff,
			0 0 8px #fff,
			0.5px 0.5px 0 #fff;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button:focus {
		color: #d00;
	}
}

/*=================*/
/* HNS DATE PICKER */
/*=================*/

#hns-date-picker span {
	color: #777;
	text-shadow: 0.5px 0.5px 0 #fff;
	font-weight: 600;
}
#hns-date-picker input[type='text'] {
}
#hns-date-picker input {
	border: 1px solid <?php echo ($platform == 'Mac') ? '#bbb' : '#aaa'; ?>;
	color: <?php echo ($platform == 'Mac') ? '#888' : '#666'; ?>;
	background-color: transparent;
}
#hns-date-picker input:hover,
#hns-date-picker input:focus {
	color: <?php echo ($platform == 'Mac') ? '#666' : '#444'; ?>;
}
#hns-date-picker span {
	color: <?php echo ($platform == 'Mac') ? '#aaa' : '#888'; ?>;
}

/*======================*/
/* ANTI-KIBITZER TOGGLE */
/*======================*/

#anti-kibitzer-toggle button::before,
#anti-kibitzer-toggle button::after {
	background-color: #bbb;
	-webkit-background-clip: text;
	color: transparent;
	text-shadow: rgba(255,255,255,0.5) 0px 1px 1px;
}
#anti-kibitzer-toggle button:hover::before,
#anti-kibitzer-toggle button:hover::after {
	background-color: #777;
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
#text-size-adjustment-ui::after {
	color: #aaa;
	font-weight: 300;
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
	margin: 0.7em 20px 0.1em 20px;
	max-width: calc(100% - 40px);
	font-family: <?php echo $UI_font; ?>, 'Font Awesome';
	text-align: center;
}

h1.listing a[href^="http"] {
	font-size: 0.6875em;
	top: 6px;
	color: #888;
}
h1.listing a[href^='/posts'] {
	font-family: <?php echo $text_font; ?>;
	text-decoration: none;
	color: #690010;
	font-weight: 500;
	text-shadow: 0.5px 0.5px 0.5px #de7069;
	padding: 0 2px 1px 1px;
}

@media only screen and (hover: hover), not screen and (-moz-touch-enabled) {
	h1.listing a[href^='/posts'] {
		max-width: calc(100% - 60px);
		padding: 2px 2px 1px 1px;
	}
	h1.listing a:hover,
	h1.listing a:focus {
		text-shadow: 
			0px 0px 0.5px #ff987b, 
			0px 0px 1.5px #c05651,
			0.5px 0.5px 0.5px #de7069;
		background-color: rgba(255,255,250,0.85);
	}	
	h1.listing:focus-within::before {
		display: inline-block;
		vertical-align: top;
		position: relative;
		left: -0.125em;
		top: 1px;
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
/* LISTING POST-META */
/*===================*/

h1.listing + .post-meta {
	justify-content: center;
}
h1.listing + .post-meta > * {
	line-height: 1.5;
}
h1.listing + .post-meta .post-section {
	order: 1;
}
h1.listing + .post-meta .post-section::before {
	left: unset;
}
h1.listing + .post-meta .link-post-domain {
	order: 2;
	flex-basis: 100%;
}

/*============*/
/* USER PAGES */
/*============*/

#content.user-page h1.page-main-heading {
	border-bottom: 1px solid #ccc;
}

#content.user-page #top-nav-bar {
	margin: -0.5em 0 0.25em 0;
	grid-column: 1 / span 3;
}

#content.user-page h1.listing {
	padding: 6px 6px 0 6px;
	max-width: 100%;
	margin: 1rem 0 0 0;
}
#content.own-user-page h1.listing,
h1.listing.own-post-listing {
	padding-right: 36px;
}
@media only screen and (hover: hover), not screen and (-moz-touch-enabled) {
	#content.user-page h1.listing:focus-within::before {
		top: -2px;
	}
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
#content.user-page h1.listing.link-post-listing::after {
	height: calc(100% + 2.125em);
}
#content.user-page h1.listing + .post-meta {
	margin: 6px 6px 1.5rem 35px;
}
#content.user-page h1.listing + .post-meta::after {
	display: none;
}

#content.conversations-user-page h1.listing {
	padding: 8px 6px;
	font-size: 1.75rem;
}
#content.conversations-user-page h1.listing + .post-meta {
	padding: 6px 4px;
	margin: 0;
}

.user-stats .karma-total {
	font-weight: bold;
}

/*===============*/
/* CONVERSATIONS */
/*===============*/

#content.conversation-page h1.page-main-heading {
	font-family: <?php echo $text_font; ?>;
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
	margin-left: 1.5em;
	font-family: Garamond Premier Pro;
	min-width: unset;
}
.contents-head {
	font-weight: bold;
}
.contents-head::after {
	content: "";
	background-image: linear-gradient(to right, rgba(0, 0, 0, 0), rgba(0, 0, 0, 0.75), rgba(0, 0, 0, 0));
	display: block;
	margin: 0 auto 0.5em auto;
	width: 75%;
	height: 1px;
}
.contents a::after {
	display: none;
}
.post-body .contents ul {
	margin: 0 0.5em;
	padding: 0 1em;
}
.post-body .contents a,
.post-body .contents a:visited {
	color: #690010;
	font-weight: 600;
}
.post-body .contents a:hover {
	color: #690010;
	text-shadow: 
		0px 0px 0.5px #ff987b, 
		0px 0px 1px #c05651;
	border: none;
}
.post-body .contents li::before {
	color: #999;
	font-feature-settings: 'onum';
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
.post-body a:hover,
.comment-body a:hover {
	color: #999;
	border-bottom: 1px dotted #999;
}

/*===========*/
/* POST-META */
/*===========*/

.post-meta,
h1.listing + .post-meta {
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
a.post-section:hover {
	text-decoration: none;
}
.post-meta .post-section.alignment-forum::before {
	text-shadow:
		1px 1px 0   #b9bbff,
		0   1px 0   #b9bbff,
		0   0   5px #b9bbff;
}
a.post-section.alignment-forum:hover::before {
	color: #e7e8ff;
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
	color: #77121a;
}
.post.link-post a.link-post-link:hover {
	color: #942f2f;
	text-shadow: 
		0px 0px 0.5px #ff987b, 
		0px 0px 1.5px #c05651, 
		0.5px 0.5px 0.5px #de7069;
}
.post.link-post a.link-post-link:hover::before {
	color: #4879ec;
	text-shadow: 
		0.5px 0.5px 0 #fff,
		-0.5px -0.5px 0 #fff,
		0 0 2px #fff,
		0 0 3px #00c;
}
.post.link-post a.link-post-link:focus {
	text-shadow: 
		0px 0px 0.5px #ff987b, 
		0px 0px 1.5px #c05651, 
		0.5px 0.5px 0.5px #de7069;
}

/*=======*/
/* POSTS */
/*=======*/

.post-body {
	font-size: 1.375rem;
}
.post-page .post-meta::after {
	display: block;
	margin: 0.625em 0 0 0;
	font-size: 1.5rem;
	flex-basis: 100%;
	order: 2;
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

#content > .comment-thread .comment-meta a.date:focus,
#content > .comment-thread .comment-meta a.permalink:focus {
	color: #444;
	font-weight: normal;
	outline: 2px dotted #444;
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
	border: 1px solid transparent;
}

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
.comment-item input[id^="expand"] ~ .comment-thread {
	max-height: 39px;
	padding-top: 5px;
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
.comment-item .author:not(.redacted).original-poster::after {
	opacity: 0.8;
}

.comment-item .karma.active-controls::after,
.comment-item .karma .karma-value::after,
.post .karma.active-controls::after,
.post .karma .karma-value::after {
	background-color: #fff;
	color: #777;
	box-shadow: 0 0 0 1px #ccc inset;
	border-radius: 4px;
}
.comment-item .karma.active-controls::after,
.post .karma.active-controls::after {
	padding: 6px;
	bottom: -46px;
}
.comment-item .karma .karma-value::after,
.post .karma .karma-value::after {
	padding: 2px 8px;
	top: -26px;
	min-width: 64px;
}

/*====================*/
/* ANTI-KIBITZER MODE */
/*====================*/

.author.redacted,
.inline-author.redacted {
	opacity: 0.8;
	font-weight: 300;
}

.karma-value.redacted {
	opacity: 0.5;
}

.link-post-domain.redacted {
	opacity: 0.5;
}

/*===========================*/
/* COMMENT THREAD NAVIGATION */
/*===========================*/

a.comment-parent-link::after {
	display: none;
}
a.comment-parent-link::before {
	color: #bbb;
}
a.comment-parent-link:hover::before {
	color: #999;
	text-shadow: none;
	background-image: linear-gradient(to right, transparent 0%, #bbb 100%);
	background-repeat: no-repeat;
	box-shadow: 1px 0 0 0 #bbb;
}

.comment-child-link::before {
	color: #ccc;
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

/*====================*/
/* COMMENT PERMALINKS */
/*====================*/

.comment-meta .permalink,
.comment-meta .lw2-link,
.individual-thread-page .comment-parent-link:empty {
	filter: grayscale(50%);
}
.comment-meta .permalink:hover,
.comment-meta .lw2-link:hover,
.individual-thread-page .comment-parent-link:empty:hover {
	filter: unset;
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
#content.compact > .comment-thread .comment-item {
	max-height: 56px;
}
#content.compact > .comment-thread .comment-item::after {
	color: <?php echo $hyperlink_color; ?>;
	background: linear-gradient(to right, transparent 0%, #fff 50%, #fff 100%);
}

@media only screen and (hover: hover), not screen and (-moz-touch-enabled) {
	#content.compact > .comment-thread .comment-item:hover .comment,
	#content.compact > .comment-thread .comment-item.expanded .comment {
		background-color: #fff;
		outline: 3px solid #888;
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
		outline: 3px solid #888;
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
	margin-bottom: 1rem;
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
}
.comment-minimize-button::after {
	font-family: <?php echo $UI_font; ?>;
	color: #777;
}
.comment-minimize-button.maximized::after {
	color: #ccc;
}

/*=================================*/
/* INDIVIDUAL COMMENT THREAD PAGES */
/*=================================*/

.individual-thread-page > h1 a {
	color: #690010;
}
.individual-thread-page > h1 a:hover {
	text-shadow: 
		0px 0px 0.5px #ff987b, 
		0px 0px 1.5px #c05651, 
		0.5px 0.5px 0.5px #de7069;
}

.individual-thread-page > h1 {
	font-family: <?php echo $text_font; ?>;
}

/*==============*/
/* VOTE BUTTONS */
/*==============*/

.upvote,
.downvote {
	color: #bbb;
	position: relative;	
}
.vote::before {
	position: relative;
	z-index: 1;
}
.upvote::before {
	content: "\F077";
	top: 1px;
}
.downvote::before {
	content: "\F078";
	position: relative;
	left: -2px;
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

.vote::after {
	position: absolute;
	color: transparent;
}
.vote:not(:hover)::after {
	text-shadow: none;
}
.karma.waiting .vote.big-vote::after {
	color: transparent;
}
.vote.big-vote::after,
.vote:not(.big-vote).clicked-twice::after {
	color: inherit;
}
.karma:not(.waiting) .vote.clicked-once::after {
	color: #bbb;
}

.upvote::after {
	content: "\F325";
	left: 7px;
	bottom: 4px;
}
.downvote::after {
	content: "\F322";
	left: 5px;
	top: 4px;
}
@-moz-document url-prefix() {
	.upvote::after {
		bottom: 2px;
		left: 8px;
	}
	.downvote::after {
		top: 3px;
		left: 6px;
	}
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

.post-controls {
	margin: 0.5em -0.75em 0 0;
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
.posting-controls form.edit-existing-comment textarea:focus {
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
.posting-controls form.edit-existing-comment .guiedit-buttons-container button {
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
}
#markdown-hints {
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
	color: <?php echo $hyperlink_color; ?>;
}

/*=========*/
/* BUTTONS */
/*=========*/

button,
input[type='submit'] {
	color: #888;
}

button:active,
input[type='submit']:active {
	color: #f00;
	transform: scale(0.9);
}
.button:visited {
	color: #888;
}
.button:active {
	transform: scale(0.9);
}
@-moz-document url-prefix() {
	.button:active {
		transform: none;
	}
}
@media only screen and (hover: hover), not screen and (-moz-touch-enabled) {
	button:hover,
	input[type='submit']:hover,
	button:focus,
	input[type='submit']:focus {
		color: #333;
		text-shadow: 0px 0px 0.5px #333;
	}

	.button:hover {
		color: #333;
		text-shadow: 0px 0px 0.5px #333;
		text-decoration: none;
	}
	.button:focus:not(:hover) {
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

/*========*/
/* TABLES */
/*========*/

.post-body table,
.comment-body table,
.post-body table th,
.post-body table td,
.comment-body table th,
.comment-body table td {
	border: 1px solid #ddd;
}

/*======*/
/* MISC */
/*======*/

hr {
	height: 1px;
	background-image: linear-gradient(to right, rgba(0, 0, 0, 0), rgba(0, 0, 0, 0.75), rgba(0, 0, 0, 0));
}

code,
pre {
	font-family: Triplicate Code, Courier, Courier New, monospace;
	font-size: 0.8125em;
}

pre {
	border: 1px solid #ceccc3;
	background-color: #f6f4ea;
	border-radius: 4px;
	padding: 0 1px;
}

input[type='text'],
input[type='search'],
input[type='password'] {
	background-color: transparent;
	border: 1px solid #ccc;
	color: #000;
}
input[type='text']:focus,
input[type='search']:focus,
input[type='password']:focus {
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
}
.qualified-linking label::after {
	background-color: #fffffa;
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
	background-color: #f4f5ff;
	border-color: #bbb;
	border-style: solid;
	border-width: 0 1px;
}
#content.alignment-forum-index-page::after {
	grid-column: 1;
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
		background-color: rgba(244,245,255,0.85);
	}	
}

/*====================*/
/* FOR NARROW SCREENS */
/*====================*/

@media only screen and (max-width: 1440px) {
	#hns-date-picker {
		background-color: #fffffa;
		opacity: 1.0;
	}
}
@media only screen and (max-width: 1160px) {
	#theme-selector:hover::after {
		background-color: #bbb;
		width: calc(6em - 13px);
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
@media only screen and (max-width: 1000px) {
	#theme-selector:hover::after {
		width: calc(6em - 9px);
		height: calc(100% - 5px);
		top: 3px;
		left: 100%;
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

@media only screen and (hover: none), only screen and (-moz-touch-enabled) {
	#ui-elements-container > div[id$='-ui-toggle'] button  {
		color: #bbb;
		text-shadow:
			0 0 1px #fffffa,
			0 0 3px #fffffa,
			0 0 5px #fffffa,
			0 0 10px #fffffa,
			0 0 20px #fffffa,
			0 0 30px #fffffa;
	}

	#theme-selector {
		background-color: #fffffa;
		box-shadow: 
			0 0 0 1px #ccc,
			0 0 1px 3px #fffffa,
			0 0 3px 3px #fffffa,
			0 0 5px 3px #fffffa,
			0 0 10px 3px #fffffa,
			0 0 20px 3px #fffffa;
		border-radius: 8px;
	}
	#theme-selector::before {
		color: #999;
		font-weight: 300;
		text-shadow: 0.5px 0.5px 0 #fff;
	}
	#theme-selector button {
		background-color: #fffffa;
		border-radius: 8px;
	}
	#theme-selector button::after {
		color: #777;
		max-width: calc(100% - 3.5em);
		overflow: hidden;
		text-overflow: ellipsis;
	}
	#theme-selector button.selected::after {
		color: #222;
		text-shadow: 
			0 -1px 0 #fff,
			0 0.5px 0.5px #000;
	}
	#theme-selector .theme-selector-close-button {
		color: #fffffa;
		text-shadow:
			1px 1px 0 #ccc,
			0 0 8px #ccc;
		opacity: 1.0;
	}

	#quick-nav-ui {
		background-color: #fffffa;
	}
	#quick-nav-ui,
	#new-comment-nav-ui,
	#hns-date-picker {
		box-shadow:
			0 0 1px 3px #fffffa,
			0 0 3px 3px #fffffa,
			0 0 5px 3px #fffffa,
			0 0 10px 3px #fffffa,
			0 0 20px 3px #fffffa;
	}
	#quick-nav-ui a::after,
	#new-comment-nav-ui::before {
		font-family: <?php echo $UI_font; ?>;
		font-weight: bold;
		box-shadow:
			0 0 1px 0 #fffffa,
			0 0 3px 0 #fffffa,
			0 0 5px 0 #fffffa;
		background-color: #fffffa;
		border-radius: 4px;
	}
	#quick-nav-ui,
	#new-comment-nav-ui {
		border-radius: 8px;
	}
	#new-comment-nav-ui {
		background-color: #fffffa;
		border: 1px solid #ccc;
	}
	#new-comment-nav-ui::before {
		color: #777;
		font-weight: 600;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button {
		box-shadow: 0 0 0 1px #ccc;
		color: #777;
	}
	#new-comment-nav-ui .new-comments-count {
		background-color: inherit;
		box-shadow: 0 -1px 0 0 #ccc;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button.new-comment-previous {
		border-radius: 7px 0 0 7px;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button.new-comment-next {
		border-radius: 0 7px 7px 0;
	}
	#new-comment-nav-ui button::after {
		font-family: <?php echo $UI_font; ?>;
	}

	/*****************************************/
	@media only screen and (max-width: 900px) {
	/*****************************************/
		h1.listing {
			line-height: 1;
		}
		h1.listing + .post-meta .post-section::before {
			position: unset;
		}
		h1.listing + .post-meta .post-section {
			overflow: visible;
		}

		#secondary-bar .nav-inner {
			font-size: 1.125em;
		}
		#secondary-bar .nav-item:not(#nav-item-search) .nav-inner {
			padding: 6px 10px;
		}

		#top-nav-bar .page-number {
			padding-top: 11px;
		}
		#top-nav-bar::after {
			margin: 0 auto;
		}

		.archive-nav *[class^='archive-nav-item-'] {
			border-width: 1px !important;
		}
		.archive-nav > *[class^='archive-nav-'] + *[class^='archive-nav-']::before {
			background-color: #bbb;
		}

		.comment-item .comment-item {
			margin: 0.75em 3px 3px 6px;
		}
		.comment-item .comment-item + .comment-item {
			margin: 1.5em 3px 3px 6px;
		}

		.sublevel-nav .sublevel-item,
		.sublevel-nav .sublevel-item:first-child,
		.sublevel-nav .sublevel-item:last-child {
			border-width: 1px;
			border-radius: 8px;
		}

		.contents {
			margin-left: auto;
		}
	/*******************************************/
	} @media only screen and (max-width: 720px) {
	/*******************************************/
	/*******************************************/
	} @media only screen and (max-width: 520px) {
	/*******************************************/
		h1.listing {
			font-size: 1.5rem;
			margin: 18px 6px 4px 6px;
			max-width: calc(100% - 12px);
		}
		h1.listing + .post-meta {
			margin: 4px 6px;
		}
		h1.listing + .post-meta > * {
			line-height: 1.4;
		}
		h1.listing a[href^='http'] {
			top: 4px;
		}

		#content.user-page h1.listing::after {
			height: calc(100% + 2.375em);
		}
		#content.user-page h1.listing.link-post-listing::after {
			height: calc(100% + 3.375em);
		}
		#content.user-page h1.listing + .post-meta {
			margin-bottom: 1.5rem;
		}
	
		#content.compact > .comment-thread .comment-item {
			max-height: 104px;
		}
		#content.compact.user-page h1.listing {
			margin-top: 0.5rem;
		}
		#content.compact.user-page h1.listing + .post-meta {
			margin-bottom: 0.75rem;
		}
	
		.comment-body {
			font-size: 1.1875rem;
			line-height: 1.35;
		}
		
		.textarea-container:focus-within .guiedit-mobile-auxiliary-button {
			padding: 5px 6px 6px 6px;
			font-weight: 600;
		}
		.textarea-container:focus-within .guiedit-mobile-help-button.active {
			box-shadow:
				0 0 0 1px #c00,
				0 0 0 1px #c00 inset;
			color: #c00;
			border-color: transparent;
		}
		.textarea-container:focus-within .guiedit-buttons-container {
			background-color: #fff;
			border-top: 1px solid #ddf;
		}
		#content.conversation-page .textarea-container:focus-within::after {
			background-color: #fff;
		}
		.textarea-container:focus-within .guiedit-mobile-auxiliary-button,
		.textarea-container:focus-within button.guiedit {
			border: 1px solid #bbb;
			border-radius: 6px;
		}
		#markdown-hints::after {
			color: #090;
		}
		
		#edit-post-form label[for='section'] {
			width: 4.3em;
		}
		#edit-post-form input[name='title'],
		#edit-post-form input[name='url'] {
			max-width: calc(100% - 6.75em);
		}
		#edit-post-form textarea {
			min-height: calc(100vh - 360px);
		}
	}
}
