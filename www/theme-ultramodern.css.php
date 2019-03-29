<?php
	$UI_font = "'Proxima Nova'";
	$text_font = "'Raleway', 'Helvetica', 'Arial', 'Verdana', sans-serif;";
	$hyperlink_color = "#f60";
	$white_glow = "0 0 1px #fff, 0 0 3px #fff, 0 0 5px #fff";
?>

/*********************/
/* ULTRAMODERN THEME */
/*********************/

body {
	color: #444;
	background-color: #888;
	font-family: <?php echo $UI_font; ?>;
	font-weight: 300;
}
#content {
	line-height: 1.55;
}

/*=========*/
/* NAV BAR */
/*=========*/

.active-bar {
	border: 1px solid transparent;
	border-bottom-color: #666;
	box-shadow:
		0 1.5px 1.5px -1.5px #bbb,
		0 1px 1px -1px #777;
	margin: 0 -30px 0 -2px;
}

.nav-inner {
	font-weight: normal;
	font-size: 1.1875em;
	padding: 11px 30px 13px 30px;
}
.nav-current .nav-inner {
	font-weight: 300;
	color: #ccc;
}
#secondary-bar .nav-inner {
	font-size: 0.875rem;
}
#secondary-bar .nav-item:not(#nav-item-search) .nav-inner {
	padding: 5px 0 3px 0;
}
@media only screen and (min-width: 901px) {
	#secondary-bar #nav-item-sequences .nav-inner {
		line-height: 1.1;
	}
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
	filter: brightness(50%) opacity(0.6);
}
#bottom-bar.decorative::after {
	color: #777;
	position: absolute;
	left: 0;
	right: 0;
	margin: auto;
	background-color: #888;
	padding-right: 4px;
	padding-left: 4px;
}
<?php fit_content("#bottom-bar.decorative::after"); ?>

.nav-bar a:link,
.nav-bar a:visited {
	color: #444;
	font-weight: 300;
}
.nav-bar a:hover,
.nav-bar a:focus {
	text-decoration: underline;
}

/* Accesskey hints */

.nav-inner::after {
	display: block;
	position: absolute;
	left: 5px;
	top: -2px;
	font-weight: 400;
	font-size: 0.7em;
	color: #7c7c7c;
}
.inactive-bar .nav-inner::after {
	color: #777;
	top: 0;
}
.nav-inner:hover::after {
	color: #666;
}

/* Search tab */

#nav-item-search form::before {
	opacity: 0.4;
	font-size: 0.9375rem;
}
#nav-item-search button {
	border: none;
	font-weight: 300;
}
#nav-item-search input::placeholder {
	color: #f00;
	font-weight: normal;
}

/* Inbox indicator */

#inbox-indicator {
	top: 0;
}

/*= Top pagination UI =*/

#top-nav-bar a::before {
	font-weight: 300;
}

/*= Bottom pagination UI =*/

#bottom-bar .nav-item a::before {
	font-weight: 400;
}

/*= Pagination UI hover tooltips =*/

#top-nav-bar a::after,
#bottom-bar a::after {
	color: #222;
}

/*==============*/
/* PAGE TOOLBAR */
/*==============*/

.page-toolbar > * {
	color: #444;
	font-weight: 300;
}
.new-post::before,
.logout-button::before {
	opacity: 0.8;
}
.page-toolbar .button:hover {
	text-decoration: none;
}

/*==============*/
/* SUBLEVEL NAV */
/*==============*/

.sublevel-nav .sublevel-item {
	color: #444;
	background-color: #888;
}
.sublevel-nav .sublevel-item:not(.selected):hover {
	color: #000;
	text-decoration: none;
}
.sublevel-nav .sublevel-item:hover,
.sublevel-nav .sublevel-item.selected {
	background-color: #999;
}
.sublevel-nav .sublevel-item:not(.selected):active,
.sublevel-nav .sublevel-item.selected {
	color: #fff;
}

.sublevel-nav:not(.sort) .sublevel-item {
	border-style: solid;
	border-color: #999;
	border-width: 1px 0 1px 1px;
}
.sublevel-nav:not(.sort) .sublevel-item:first-child {
	border-radius: 8px 0 0 8px;
}
.sublevel-nav:not(.sort) .sublevel-item:last-child {
	border-width: 1px;
	border-radius: 0 8px 8px 0;
}

/*=====================*/
/* SORT ORDER SELECTOR */
/*=====================*/

.sublevel-nav.sort .sublevel-item {
	font-family: <?php echo $UI_font; ?>;
	padding: 8px 8px 6px 8px;
	text-transform: uppercase;
	pointer-events: auto;
	box-shadow: 1px 1px 0 0 #777 inset;
}
.sublevel-nav.sort {
	border: 2px solid transparent;
	padding: 18px 0 0 0;
	border-radius: 8px;
	pointer-events: none;
	background-color: #999;
}
.sublevel-nav.sort::before {
	text-transform: uppercase;
	font-weight: 600;
	color: #444;
	z-index: 1;
}
.sublevel-nav.sort::after {
	content: "";
	position: absolute;
	display: block;
	top: 0;
	left: 0;
	width: 100%;
	height: 100%;
	border-radius: 6px;
	box-shadow:
		0 18px 0 0 #888 inset,
		0 0 0 1px #777 inset,
		0 18px 0 1px #777 inset,
		0 0 0 2px #888;
}

/*================*/
/* WIDTH SELECTOR */
/*================*/

#width-selector button {
	box-shadow:
		0 0 0 4px #888 inset,
		0 0 0 5px #ccc inset;
}
#width-selector button:hover,
#width-selector button.selected {
	box-shadow:
		0 0 0 1px #888 inset,
		0 0 0 2px #ccc inset,
		0 0 0 4px #888 inset,
		0 0 0 5px #ccc inset;
}
#width-selector button::after {
	color: #ccc;
	font-weight: 300;
}

/*================*/
/* THEME SELECTOR */
/*================*/

#theme-selector button {
	box-shadow:
		0 0 0 5px #888 inset;
}
#theme-selector button:hover,
#theme-selector button.selected {
	box-shadow:
		0 0 0 2px #888 inset,
		0 0 0 3px #ccc inset,
		0 0 0 5px #888 inset;
}

#theme-selector button::before {
	color: #aaa;
	background-color: #888;
}
#theme-selector button:hover::before,
#theme-selector button.selected::before {
	color: #ccc;
}

/*======================*/
/* THEME TWEAKER TOGGLE */
/*======================*/

#theme-tweaker-toggle button:hover {
	text-decoration: none;
}

/*=================*/
/* QUICKNAV WIDGET */
/*=================*/

#quick-nav-ui a {
	color: #666;
	border-radius: 4px;
	box-shadow: 0 0 0 1px #999;
	text-decoration: none;
}
#quick-nav-ui a[href='#bottom-bar'] {
	line-height: 1.8;
}
#quick-nav-ui a:active {
	transform: scale(0.9);
}
#quick-nav-ui a[href='#comments'].no-comments {
	opacity: 0.4;
	color: #777;
}
@media only screen and (hover: hover), not screen and (-moz-touch-enabled) {
	#quick-nav-ui a:hover  {
		color: #444;
		box-shadow: 0 0 0 1px #ccc;
	}
	#quick-nav-ui a:focus:not(:hover) {
		transform: none;
		text-shadow: none;
	}
}

/*======================*/
/* NEW COMMENT QUICKNAV */
/*======================*/

#new-comment-nav-ui .new-comments-count {
	font-weight: 600;
	color: #666;
}
#new-comment-nav-ui .new-comments-count::after {
	font-weight: 600;
	color: #666;
}
#new-comment-nav-ui .new-comment-sequential-nav-button {
	color: #bbb;
}
#new-comment-nav-ui .new-comment-sequential-nav-button:disabled {
	color: #929292;
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
	#new-comment-nav-ui .new-comment-sequential-nav-button:hover {
		color: #444;
		text-decoration: none;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button:focus {
		color: #d00;
		text-shadow: 0 0 1px #fff, 0 0 3px #fff, 0 0 5px #fff;
	}
}

/*=================*/
/* HNS DATE PICKER */
/*=================*/

#hns-date-picker span {
	color: #666;
	font-weight: 600;
}
#hns-date-picker input {
	border: 1px solid #999;
	background-color: transparent;
	color: #666;
}
#hns-date-picker input:focus {
	color: #000;
	border: 1px solid #ccc;
}

/*======================*/
/* ANTI-KIBITZER TOGGLE */
/*======================*/

#anti-kibitzer-toggle button::before,
#anti-kibitzer-toggle button::after {
	background-color: #222;
	-webkit-background-clip: text;
	color: transparent;
	text-shadow: rgba(255,255,255,0.4) 0px 1px 1px;
}
#anti-kibitzer-toggle button:hover::before,
#anti-kibitzer-toggle button:hover::after {
	background-color: #000;
}

/*======================*/
/* TEXT SIZE ADJUSTMENT */
/*======================*/

#text-size-adjustment-ui button {
	color: #444;
}
#text-size-adjustment-ui button.default {
	font-weight: 600;
}
#text-size-adjustment-ui button:hover {
	text-decoration: none;
	color: #aaa;
}
#text-size-adjustment-ui::after {
	color: #ccc;
	font-weight: 300;
}

/*=============================*/
/* COMMENTS VIEW MODE SELECTOR */
/*=============================*/

#comments-view-mode-selector a {
	color: #ccc;
}

/*==========*/
/* ARCHIVES */
/*==========*/

.archive-nav {
	border: 1px solid #ccc;
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
}

.archive-nav span[class^="archive-nav-item"],
.archive-nav a:hover {
	color: #ffb359;
	box-shadow: 
		0 0 0 3px #888 inset,
		0 0 0 4px #ccc inset,
		0 0 0 5px #888 inset;
	text-decoration: none;
}
.archive-nav span[class^="archive-nav-item"] {
	font-weight: normal;
	box-shadow: 
		0 0 0 1px #ccc inset,
		0 0 0 3px #888 inset,
		0 0 0 4px #ccc inset,
		0 0 0 5px #888 inset;
}
.archive-nav a:active {
	transform: scale(0.9);
}
.archive-nav a:focus:not(:hover) {
	transform: none;
}

/*==========*/
/* LISTINGS */
/*==========*/

h1.listing {
	margin: 0.7em 20px 0.1em 20px;
	max-width: calc(100% - 40px);
	font-family: <?php echo $UI_font; ?>, 'Font Awesome';
	font-size: 1.5rem;
}
h1.listing a[href^='/posts'] {
	font-family: <?php echo $text_font; ?>;
	font-weight: <?php global $platform; echo ($platform == 'Mac' ? '100' : '200'); ?>;
	text-shadow: 
		0px 0px 1px #777,
		0.5px 0.5px 1px #aaa,
		0.5px 0.5px 1px #bbb;
}
h1.listing a[href^="http"] {
	color: #aaa;
}

@media only screen and (hover: hover), not screen and (-moz-touch-enabled) {
	h1.listing a:hover,
	h1.listing a:focus {
		background-color: rgba(136,136,136,0.85);
		color: #f60;
		text-shadow: 
			0px 0px 1px #777,
			0.5px 0.5px 1px #aaa,
			0.5px 0.5px 1px #bbb,
			0 0 1px #f60,
			0 0 2px #f60,
			0 0 3px #f60;
	}	
	h1.listing:focus-within::before {
		color: #f60;
		left: -0.625em;
		top: 1px;
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

h1.listing .edit-post-link {
	padding: 10px 3px 30px 0.5em;
	top: 0;
	right: -1.5em;
}
h1.listing .edit-post-link:hover {
	text-decoration: none;
}
#content.user-page h1.listing .edit-post-link {
	background-color: #888;
}

/*======*/
/* SPAM */
/*======*/

h1.listing.spam {
	opacity: 0.35;
}
h1.listing.spam + .post-meta {
	opacity: 0.3;
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
	color: #222;
	font-size: 1em;
}
h1.listing + .post-meta .karma::after {
	content: " by";
}
h1.listing + .post-meta .date::before {
	content: "on ";
}
h1.listing + .post-meta .date::after {
	content: " — ";
	opacity: 0.5;
	margin: 0 0.5em 0 0.125em;
}
h1.listing + .post-meta .comment-count.new-comments::before {
	color: #0f0;
}
h1.listing:last-of-type + .post-meta {
	margin-bottom: 0;
}
h1.listing + .post-meta .karma {
	order: -1;
	margin-right: 0.25em;
}
h1.listing + .post-meta .author {
	margin-right: 0.25em;
}
h1.listing + .post-meta .date {
	margin: 0;
}
h1.listing + .post-meta .post-section {
	overflow: visible;
	order: 2;
}
h1.listing + .post-meta .post-section::before {
	font-size: 0.9375em;
	left: -32px;
}
h1.listing + .post-meta .link-post-domain {
	order: 1;
}

/*============*/
/* USER PAGES */
/*============*/

#content.user-page h1.page-main-heading {
	border-bottom: 1px solid #777;
}

#content.user-page h1.listing,
#content.user-page h1.listing + .post-meta {
	border-style: solid;
	border-color: #666;
	border-width: 0 0 0 1px;
	box-shadow:
		1.5px 0 1.5px -1.5px #bbb inset,
		1px 0 1px -1px #777 inset;
}
#content.user-page h1.listing {
	max-width: 100%;
	margin: 1rem 0 0 0;
	padding: 6px;
}
@media only screen and (hover: hover), not screen and (-moz-touch-enabled) {
	#content.user-page h1.listing:focus-within::before {
		left: -0.625em;
		top: 8px;
	}
}
#content.user-page h1.listing + .post-meta {
	margin: 0 0 1rem 0;
	padding: 0.5em 6px 6px 34px;
}
#content.user-page h1.listing + .post-meta .post-section::before {
	left: 1px;
}

#content.conversations-user-page h1.listing {
	padding: 6px 6px 4px 8px;
	font-size: 1.5rem;
}
#content.conversations-user-page h1.listing + .post-meta {
	padding: 6px 4px;
	margin: 0 0 0.25rem 0;
}
#content.conversations-user-page h1.listing + .post-meta .date::after {
	display: none;
}

.user-stats .karma-total {
	font-weight: bold;
}

/*===============*/
/* CONVERSATIONS */
/*===============*/

#content.conversation-page h1.page-main-heading {
	font-family: <?php echo $text_font; ?>;
	font-weight: <?php global $platform; echo ($platform == 'Mac' ? '100' : '200'); ?>;
	color: #000;
	text-shadow: 
		0px 0px 1px #777,
		0.5px 0.5px 1px #aaa,
		0.5px 0.5px 1px #bbb;
}

/*============*/
/* LOGIN PAGE */
/*============*/

.login-container h1 {
	font-weight: 300;
}

/* “Create account” form */

#signup-form {
	border: 1px solid #aaa;
}

/* Log in tip */

.login-container .login-tip {
	border: 1px solid transparent;
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
	font-weight: 600;
}

/*===================*/
/* TABLE OF CONTENTS */
/*===================*/

.contents {
	background-color: #888;
}
.contents-head {
	font-weight: 300;
}
.post-body .contents ul {
	font-size: 0.85em;
}
.post-body .contents li::before {
	color: #999;
	font-feature-settings: "tnum";
}

/*==================*/
/* POSTS & COMMENTS */
/*==================*/

.body-text {
	font-family: <?php echo $text_font; ?>;
	font-weight: <?php global $platform; echo ($platform == 'Mac' ? '300' : '400'); ?>;
	color: #000;
	text-shadow: 
		0px 0px 1px #777,
		0.5px 0.5px 1px #aaa,
		0.5px 0.5px 1px #bbb;
}
.body-text strong {
	font-weight: 500;
}

.body-text a:link {
	color: inherit;
	text-shadow: 
		0px 0px 1px #bd5984, 
		0.5px 0.5px 1px #f68a84, 
		0.5px 0.5px 1px #ff9b8c;
}
.body-text a:visited {
	color: inherit;
	text-shadow:
		0px 0px 1px #a766dd, 
		0.5px 0.5px 1px #d9f, 
		0.5px 0.5px 1px #efa9ff;
}
.body-text a:hover {
	color: #f60;
	text-shadow:
		0px 0px 1px #bd5984, 
		0.5px 0.5px 1px #f68a84, 
		0.5px 0.5px 1px #ff9b8c,
		0px 0px 5px #f60;
}

h1.post-title {
	margin: 1.1em 0 0.25em 0;
	font-family: <?php echo $text_font; ?>;
	font-weight: <?php global $platform; echo ($platform == 'Mac' ? '100' : '200'); ?>;
	color: #000;
	font-size: 3em;
	text-shadow: 
		0px 0px 1px #777,
		0.5px 0.5px 1px #aaa,
		0.5px 0.5px 1px #bbb;
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
/* POST-META */
/*===========*/

.post-meta a,
.post-meta .date {
	color: #444;
}

.post-meta > * {
	margin: 0;
	text-shadow:
		0px 0px 1px #777,
		0.5px 0.5px 1px #aaa,
		0.5px 0.5px 1px #bbb;
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
	color: #666;
}
.post-meta .comment-count {
	margin: 0 0.25em 0 0;
}
.post-meta .read-time,
.post-meta .word-count,
.post-meta .lw2-link {
	margin: 0 0.25em 0 0.5em;
}
.post-meta .comment-count:hover,
.post-meta .lw2-link:hover {
	text-decoration: none;
	text-shadow: 
		0 0 0.5px #000,
		0 0 1px #fff,
		0 0 8px #000;
}
.post-meta .comment-count:hover::before,
.post-meta .lw2-link:hover::before,
.post-meta .read-time:hover::before {
	color: #ccc;
}
.post-meta .read-time:hover::before {
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
	font-size: 0.75em;
	position: relative;
	bottom: 1px;
}

.post .post-meta .author {
	margin: 0 0.75em 0 0;
}
.post-meta .author:hover,
.comment-meta .author:hover {
	text-decoration: none;
	color: #090;
}
.post .post-meta .comment-count {
	margin: 0 0.5em;
}
.post .post-meta .lw2-link {
	margin: 0 1em 0 0.5em;
}
.post .post-meta .karma {
	margin: 0 0.5em 0 1em;
}

.post-meta .post-section::before,
.comment-meta .alignment-forum {
	color: #888;
	text-shadow: 
		1px 1px 0 #ccc, 
		0 1px 0 #ccc, 
		0 0 5px #ccc;
}
a.post-section:hover {
	text-decoration: none;
}
a.post-section:hover::before {
	color: #999;
}
.post-meta .post-section.alignment-forum::before {
	text-shadow:
		1px 1px 0   #b9bbff,
		0   1px 0   #b9bbff,
		0   0   5px #b9bbff;
}
a.post-section.alignment-forum:hover::before {
	color: #9093d4;
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
	color: #f60;
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
	color: #f60;
	border-bottom: 2px dotted #f60;
	text-shadow: 
		0px 0px 1px #777, 
		0.5px 0.5px 1px #aaa, 
		0.5px 0.5px 1px #bbb, 
		0 0 1px #f60, 
		0 0 2px #f60, 
		0 0 3px #f60;
}

/*==========*/
/* COMMENTS */
/*==========*/

#content > .comment-thread .comment-meta a.date:focus,
#content > .comment-thread .comment-meta a.permalink:focus {
	color: #ccc;
	outline: 2px dotted #ccc;
	position: relative;
	background-color: #444;
}
#content > .comment-thread .comment-meta a.date:focus {
	padding: 0 6px;
	left: -6px;
}
#content > .comment-thread .comment-meta a.date:focus + * {
	margin-left: -12px;
}
#content > .comment-thread .comment-meta a.permalink:focus {
	padding: 0 5px;
	left: -5px;
}
#content > .comment-thread .comment-meta a.permalink:focus + *:not(.comment-post-title) {
	margin-left: -10px;
}
.comment-item {
	border: 1px solid transparent;
	border-left-color: #666;
	box-shadow:
		1.5px 0 1.5px -1.5px #bbb inset, 
		1px 0 1px -1px #777 inset;
}
@-moz-document url-prefix() {
	.comment-item {
		box-shadow:
			1.5px 0 1.5px -1px #bbb inset, 
			1px 0 1px -1px #777 inset;
	}
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

a.comment-parent-link::after {
	display: none;
}
a.comment-parent-link::before {
	padding: 2px 3px 0 4px;
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

.comment-meta a {
	color: #222;
}
.comment-meta .author {
	font-weight: 300;
	font-size: 1.125em;
	color: #444;
	font-weight: normal;
}
.comment-item .author:not(.redacted).original-poster::after {
	opacity: 0.8;
}

.comment-item .karma.active-controls::after,
.comment-item .karma .karma-value::after,
.post .karma.active-controls::after,
.post .karma .karma-value::after,
.author::before {
	background-color: #888;
	color: #ccc;
	border-radius: 4px;
	box-shadow: 0 0 0 1px #bbb inset;
}
.comment-item .karma.active-controls::after,
.post .karma.active-controls::after {
	padding: 6px;
	bottom: -46px;
}
.comment-item .karma .karma-value::after,
.post .karma .karma-value::after {
	padding: 2px 8px;
	top: -28px;
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
	opacity: 0.6;
}

.link-post-domain.redacted {
	opacity: 0.6;
}

/*===========================*/
/* COMMENT THREAD NAVIGATION */
/*===========================*/

div.comment-parent-link {
	font-weight: 400;
}
a.comment-parent-link {
	font-weight: 300;
}
a.comment-parent-link::before {
	color: #666;
}
a.comment-parent-link:hover::before {
	color: #aaa;
}

div.comment-child-links {
	font-weight: 400;
}
div.comment-child-links a {
	font-weight: 300;
}
.comment-child-link::before {
	color: #666;
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
	background-color: #949494;
}

/*=======================*/
/* COMMENTS COMPACT VIEW */
/*=======================*/

#comments-list-mode-selector button {
	box-shadow:
		0 0 0 4px #888 inset,
		0 0 0 5px #ccc inset;
}
#comments-list-mode-selector button:hover,
#comments-list-mode-selector button.selected {
	box-shadow:
		0 0 0 1px #888 inset,
		0 0 0 2px #ccc inset,
		0 0 0 4px #888 inset,
		0 0 0 5px #ccc inset;
}
#content.compact > .comment-thread .comment-item::after {
	color: #ccc;
	background: linear-gradient(to right, transparent 0%, #888 50%, #888 100%);
}

@media only screen and (hover: hover), not screen and (-moz-touch-enabled) {
	#content.compact > .comment-thread .comment-item:hover .comment,
	#content.compact > .comment-thread .comment-item.expanded .comment {
		background-color: #999;
		outline: 3px solid #ccc;
	}
	#content.compact > .comment-thread .comment-item:hover .comment::before,
	#content.compact > .comment-thread .comment-item.expanded .comment::before {
		background-color: #999;
		box-shadow: 
			0 0  3px #999,
			0 0  5px #999,
			0 0  7px #999,
			0 0 10px #999,
			0 0 20px #999,
			0 0 30px #999,
			0 0 40px #999;
	}
}
@media only screen and (hover: none), only screen and (-moz-touch-enabled) {
	#content.compact > .comment-thread.expanded .comment-item .comment {
		background-color: #999;
		outline: 3px solid #ccc;
	}
	#content.compact > .comment-thread.expanded .comment-item .comment::before {
		background-color: #999;
		box-shadow: 
			0 0  3px #999,
			0 0  5px #999,
			0 0  7px #999,
			0 0 10px #999,
			0 0 20px #999,
			0 0 30px #999,
			0 0 40px #999;
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

.new-comment::before {
	display: none;
}
.new-comment {
	border: 1px solid #e00;
	box-shadow: 
		0 0 1px #f00, 
		0 0 1px #f00 inset;
}

/*=================================*/
/* COMMENT THREAD MINIMIZE BUTTONS */
/*=================================*/

.comment-minimize-button {
	color: #777;
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

/*====================*/
/* COMMENT PERMALINKS */
/*====================*/
/*==================*/
/* COMMENT LW LINKS */
/*==================*/

.comment-meta .permalink::before,
.comment-meta .lw2-link::before,
.individual-thread-page a.comment-parent-link:empty::before {
	opacity: 1.0;
	filter: saturate(10%) contrast(20%);
}

/*=================================*/
/* INDIVIDUAL COMMENT THREAD PAGES */
/*=================================*/

.individual-thread-page > h1 {
	font-family: <?php echo $text_font; ?>;
	font-weight: <?php global $platform; echo ($platform == 'Mac' ? '200' : '300'); ?>;
}

/*==============*/
/* VOTE BUTTONS */
/*==============*/

.upvote,
.downvote {
	color: #666;
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
.vote:hover::after {
	color: #888;
}
.vote:not(:hover)::after {
	text-shadow: none;
}
.karma.waiting .vote.big-vote::after {
	color: #888;
}
.vote.big-vote::after,
.vote:not(.big-vote).clicked-twice::after {
	color: inherit;
}
.karma:not(.waiting) .vote.clicked-once::after {
	color: #666;
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
		bottom: 3px;
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

.comment-controls .cancel-comment-button {
	font-weight: normal;
	color: #f00;
}
.comment-controls .cancel-comment-button:hover {
	color: #f00;
	text-shadow: <?php echo $white_glow; ?>;
}

.posting-controls .action-button,
.posting-controls input[type='submit'] {
	font-weight: normal;
}
.posting-controls .action-button:hover,
.posting-controls input[type='submit']:hover {
	text-decoration: underline;
	color: #444;
}

.comment-controls .delete-button,
.comment-controls .retract-button {
	color: #900;
}
.comment-controls .edit-button,
.comment-controls .unretract-button {
	color: #070;
}
.comment-controls .action-button:hover {
	color: #bbb;
	text-decoration: none;
}

.edit-post-link,
.edit-post-link:visited {
	color: #090;
}

.posting-controls textarea {
	font-weight: 300;
	font-family: <?php echo $text_font; ?>;
	color: #000;
	background-color: transparent;
	border-color: #999;
	text-shadow: 
		0px 0px 1px #777,
		0.5px 0.5px 1px #aaa,
		0.5px 0.5px 1px #bbb;
}
@-moz-document url-prefix() {
	.posting-controls textarea {
		font-weight: <?php global $platform; echo ($platform == 'Windows' ? '300' : '400'); ?>;
	}
}
.posting-controls textarea:focus {
	border-color: #ccc;
}

/*= Scroll bars =*/

.posting-controls textarea::-webkit-scrollbar {
	width: 16px;
	background-color: transparent;
}
.posting-controls textarea::-webkit-scrollbar-track {
	border-left: 1px solid #999;
}
.posting-controls textarea:focus::-webkit-scrollbar-track {
	border-left: 1px solid #999;
}
.posting-controls textarea::-webkit-scrollbar-thumb {
	background-color: #999;
	box-shadow: 0 0 0 1px #888 inset;
	border-left: 1px solid #999;
}
.posting-controls textarea:focus::-webkit-scrollbar-thumb {
	border-left: 1px solid #999;
	background-color: #ccc;
	box-shadow: 0 0 0 1px #888 inset;
}

/* GUIEdit buttons */

.guiedit-buttons-container {
	background-color: #888;
	box-shadow: 0 -1px 0 0 #999 inset;
}
.textarea-container:focus-within .guiedit-buttons-container {
	box-shadow: 0 -1px 0 0 #ccc inset;
}

button.guiedit {
	color: #444;
	background-color: transparent;
	font-family: Font Awesome, Source Sans Pro, Trebuchet MS, Helvetica, Arial, Verdana, sans-serif;
}
button.guiedit::after {
	font-family: Proxima Nova;
	font-weight: 300;
	color: #444;
	top: 2px;
	height: 25px;
}
button.guiedit:hover {
	color: #ccc;
}

/* Markdown hints */

#markdown-hints-checkbox + label {
	color: #444;
}
#markdown-hints-checkbox + label:hover {
	text-decoration: underline;
}
#markdown-hints {
	background-color: #888;
	border: 1px solid #ccc;
}

/*================*/
/* EDIT POST FORM */
/*================*/

#edit-post-form .post-meta-fields input[type='checkbox'] + label::before {
	border-radius: 3px;
	border: 1px solid #999;
	color: #aaa;
}
@media only screen and (hover:hover), not screen and (-moz-touch-enabled) {
	#edit-post-form .post-meta-fields input[type='checkbox'] + label:hover,
	#edit-post-form .post-meta-fields input[type='checkbox']:focus + label {
		text-decoration: underline;
	}
	#edit-post-form .post-meta-fields input[type='checkbox'] + label:hover::before,
	#edit-post-form .post-meta-fields input[type='checkbox']:focus + label::before {
		border-color: #ccc;
	}
}
#edit-post-form .post-meta-fields input[type='checkbox']:checked + label::before {
	content: "\F00C";
}
#edit-post-form input[type='radio'] + label {
	color: #444;
	border-color: #999;
}
#edit-post-form input[type='radio'][value='all'] + label {
	border-radius: 8px 0 0 8px;
	border-width: 1px;
}
#edit-post-form input[type='radio'][value='drafts'] + label {
	border-radius: 0 8px 8px 0;
}
#edit-post-form input[type='radio'] + label:hover,
#edit-post-form input[type='radio']:focus + label,
#edit-post-form input[type='radio']:checked + label {
	background-color: #999;
}
#edit-post-form input[type='radio'] + label:hover,
#edit-post-form input[type='radio']:focus + label {
	color: #000;
}
#edit-post-form input[type='radio']:active + label,
#edit-post-form input[type='radio']:checked + label {
	color: #fff;
}

/*=======*/
/* LINKS */
/*=======*/

a {
	text-decoration: none;
	color: <?php echo $hyperlink_color; ?>;
}
a:hover {
	text-decoration: underline;
}

/*=========*/
/* BUTTONS */
/*=========*/

button,
input[type='submit'] {
	color: #444;
	font-weight: normal;
}

.button,
.button:visited {
	color: #444;
}

button:hover,
input[type='submit']:hover,
button:focus,
input[type='submit']:focus {
	color: #aaa;
}
input[type='submit']:hover,
input[type='submit']:focus {
	text-decoration: underline;
}
button:active,
input[type='submit']:active {
	color: #ccc;
	transform: scale(0.9);
}
.button:hover {
	color: #aaa;
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

.body-text h1,
.body-text h2,
.body-text h3,
.body-text h4,
.body-text h5,
.body-text h6 {
	font-weight: <?php global $platform; echo ($platform == 'Mac' ? '100' : '200'); ?>;
	text-shadow: 
		0px 0px 1px #777,
		0.5px 0.5px 1px #aaa,
		0.5px 0.5px 1px #bbb;
}
.post-body h1 strong,
.post-body h2 strong,
.post-body h3 strong,
.post-body h4 strong,
.post-body h5 strong,
.post-body h6 strong {
	font-weight: normal;
}
.body-text h6 {
	color: #555;
}
.body-text h1 {
	padding-bottom: 2px;
	border-bottom-color: #777;
}
.post-body h2 {
	border-bottom: 1px dotted #ccc;
}

/*========*/
/* QUOTES */
/*========*/

blockquote {
	border-left: 5px solid #777;
}

/*========*/
/* IMAGES */
/*========*/

#content img,
#content figure.image img {
	border: 1px solid #666;
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

.body-text table,
.body-text table th,
.body-text table td {
	border: 1px solid #ccc;
}

/*======*/
/* MISC */
/*======*/

hr {
	border-bottom: 1px solid #999;
}

code,
pre {
	font-family: 'Tired of Courier', Courier, Courier New, monospace;
	font-size: 0.9375em;
}

pre {
	border: 1px solid #444;
	box-shadow:
		0px 0px 1px #777,
		1px 1px 1px #aaa inset,
		1px 1px 1px #bbb;
}

input[type='text'],
input[type='search'],
input[type='password'] {
	border: 1px solid #999;
	color: #000;
	background-color: transparent;
}
input[type='text']:focus,
input[type='search']:focus,
input[type='password']:focus {
	border: 1px solid #ccc;
}

select {
	color: #000;
}

.frac {
	padding-left: 2px;
	font-feature-settings: 'lnum';
	font-size: 0.95em;
}
.frac sup {
	position: relative;
	left: -1px;
}
.frac sub {
	position: relative;
	left: -0.5px;
}

.body-text *::selection,
textarea::selection,
input::selection {
	background-color: #d8d8d8;
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

#aux-about-link a {
	color: #444;
}
#aux-about-link a:hover {
	opacity: 1.0;
	text-shadow: <?php echo $white_glow; ?>;
}

.qualified-linking label:hover {
	text-shadow: <?php echo $white_glow; ?>;
}

.qualified-linking-toolbar {
	border: 1px solid #000;
	background-color: #777;
}
.qualified-linking-toolbar a {
	border: 1px solid #888;
	border-radius: 4px;
	color: #444;
}
.qualified-linking-toolbar a:hover {
	border: 1px solid #999;
	text-decoration: none;
	text-shadow: <?php echo $white_glow; ?>;
}
.qualified-linking label::after {
	background-color: #888;
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
	background-color: #878a9f;
}
#content.alignment-forum-index-page::after {
	font-family: "Concourse SmallCaps";
	font-weight: 600;
	background-color: #222d4b;
	color: transparent;
	-webkit-background-clip: text;
	text-shadow: 
		rgba(136,136,136,0.5) 0px 3px 3px;
}
@media only screen and (hover: hover), not screen and (-moz-touch-enabled) {
	#content.alignment-forum-index-page h1.listing a:hover,
	#content.alignment-forum-index-page h1.listing a:focus {
		background-color: rgba(135,138,159,0.85);
	}	
}

/*====================*/
/* FOR NARROW SCREENS */
/*====================*/

@media only screen and (max-width: 1440px) {
	#hns-date-picker {
		background-color: #888;
		bottom: 61px;
		opacity: 1.0;
		right: -77px;
	}
	#hns-date-picker::before {
		display: none;
	}
}
@media only screen and (max-width: 1160px) {
	#hns-date-picker {
		bottom: 204px;
		right: -30px;
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
	#hns-date-picker {
		right: -18px;
	}
}
@media only screen and (max-width: 1040px) {
	#hns-date-picker {
		right: -13px;
	}
}
@media only screen and (max-width: 1020px) {
	#hns-date-picker {
		right: 15px;
	}
}
@media only screen and (max-width: 1000px) {
	#theme-tweaker-toggle {
		left: -19px;
	}
	#quick-nav-ui,
	#new-comment-nav-ui,
	#new-comment-nav-ui + #hns-date-picker {
		opacity: 1.0;
	}
}

/*========*/
/* MOBILE */
/*========*/

@media only screen and (hover: none), only screen and (-moz-touch-enabled) {
	#ui-elements-container > div[id$='-ui-toggle'] button,
	#theme-selector .theme-selector-close-button  {
		color: #444;
		text-shadow:
			0 0 1px #999,
			0 0 3px #999,
			0 0 5px #999,
			0 0 10px #999,
			0 0 20px #999,
			0 0 30px #999;
	}

	#theme-selector {
		background-color: #888;
		box-shadow: 
			0 0 0 1px #444,
			0 0 1px 3px #999,
			0 0 3px 3px #999,
			0 0 5px 3px #999,
			0 0 10px 3px #999,
			0 0 20px 3px #999;
		border-radius: 12px;
	}
	#theme-selector::before {
		color: #222;
		font-weight: 300;
		text-shadow: 
			0px 0px 1px #777, 
			0.5px 0.5px 1px #aaa, 
			0.5px 0.5px 1px #bbb;
	}
	#theme-selector button {
		border-radius: 10px;
	}
	#theme-selector button::after {
		color: #444;
		max-width: calc(100% - 3.5em);
		overflow: hidden;
		text-overflow: ellipsis;
	}
	#theme-selector button.selected::after {
		color: #000;
		text-shadow: 
			0 -1px 0 #fff,
			0 0.5px 0.5px #000;
	}

	#quick-nav-ui {
		background-color: #999;
	}
	#quick-nav-ui a {
		background-color: #888;
		box-shadow: 0 0 0 1px #444;
		color: #444;
	}
	#quick-nav-ui,
	#new-comment-nav-ui,
	#hns-date-picker {
		box-shadow:
			0 0 1px 3px #999,
			0 0 3px 3px #999,
			0 0 5px 3px #999,
			0 0 10px 3px #999,
			0 0 20px 3px #999;
	}
	#quick-nav-ui a::after,
	#new-comment-nav-ui::before {
		font-family: <?php echo $UI_font; ?>;
		font-weight: bold;
		box-shadow:
			0 0 1px 0 #999,
			0 0 3px 0 #999,
			0 0 5px 0 #999;
		background-color: #999;
		border-radius: 4px;
	}
	#quick-nav-ui,
	#new-comment-nav-ui {
		border-radius: 8px;
	}
	#new-comment-nav-ui {
		background-color: #888;
		border: 1px solid #444;
	}
	#new-comment-nav-ui::before {
		color: #444;
		font-weight: bold;
	}
	#new-comment-nav-ui .new-comments-count,
	#new-comment-nav-ui .new-comments-count::after {
		color: #444;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button {
		box-shadow: 0 0 0 1px #444;
		color: #444;
	}
	#new-comment-nav-ui .new-comments-count {
		background-color: inherit;
		box-shadow: 0 -1px 0 0 #444;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button:disabled {
		color: #999;
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

	#hns-date-picker.engaged {
		bottom: 124px;
		right: 61px;
		border: 1px solid #444;
	}
	#hns-date-picker span,
	#hns-date-picker input {
		color: #444;
	}

	/*****************************************/
	@media only screen and (max-width: 900px) {
	/*****************************************/
		h1.listing + .post-meta .post-section::before {
			position: unset;
		}

		#secondary-bar .nav-inner {
			font-size: 1em;
		}
		#secondary-bar .nav-item:not(#nav-item-search) .nav-inner {
			padding: 6px 10px;
		}

		.archive-nav > *[class^='archive-nav-'] + *[class^='archive-nav-']::before {
			background-color: #ccc;
		}

		.comment-item .comment-item {
			margin: 0.75em 0 4px 6px;
		}
		.comment-item .comment-item + .comment-item {
			margin: 1.5em 0 4px 6px;
		}

		.comment-controls .cancel-comment-button::before {
			text-shadow:
				0 0 1px #fff,
				0 0 3px #fff;
		}

		.sublevel-nav:not(.sort) .sublevel-item,
		.sublevel-nav:not(.sort) .sublevel-item:first-child,
		.sublevel-nav:not(.sort) .sublevel-item:last-child {
			border-radius: 8px;
			border-width: 1px;
			margin: 2px;
		}
	/*****************************************/
	} @media only screen and (max-width: 720px) {
	/*****************************************/
	/*******************************************/
	} @media only screen and (max-width: 520px) {
	/*******************************************/
		h1.listing {
			font-size: 1.25rem;
			margin: 18px 6px 4px 6px;
			max-width: calc(100% - 12px);
		}
		h1.listing + .post-meta {
			margin: 4px 6px;
		}
		h1.listing + .post-meta > * {
			line-height: 1.5;
		}
		h1.listing a[href^='http'] {
			top: 3px;
		}

		#content.compact > .comment-thread .comment-item {
			max-height: 105px;
		}
		
		.textarea-container:focus-within textarea {
			background-color: #888;
		}
		.textarea-container:focus-within .guiedit-mobile-auxiliary-button {
			border: 1px solid transparent;
			padding: 6px;
		}
		.textarea-container:focus-within .guiedit-mobile-help-button.active {
			box-shadow:
				0 0 0 1px #ccc,
				0 0 0 2px #888,
				0 0 0 3px #ccc;
			color: #ccc;
			font-weight: 600;
		}
		.textarea-container:focus-within .guiedit-buttons-container {
			background-color: #888;
			border-top: 1px solid #ddf;
		}
		#content.conversation-page .textarea-container:focus-within::after {
			background-color: #888;
		}
		.textarea-container:focus-within button.guiedit {
			border: 1px solid transparent;
		}
		#markdown-hints::after {
			color: #0f0;
		}

		#edit-post-form .post-meta-fields input[type='checkbox'] + label {
			top: 2px;
		}
		#edit-post-form .post-meta-fields input[type='checkbox'] + label::before {
			top: 1px;
		}
	}
}
