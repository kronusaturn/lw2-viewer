<?php
	$UI_font = (($platform == 'Mac') ? "'Concourse', 'a_Avante'" : "'Whitney', 'a_Avante'") . ", Arial, sans-serif";
	$UI_font_smallcaps = (($platform == 'Mac') ? "'Concourse Smallcaps', 'a_Avante'" : "'Whitney Smallcaps', 'a_Avante'") . ", Arial, sans-serif";
	$text_font = "'Source Sans Pro', 'Trebuchet MS', 'Helvetica', 'Arial', 'Verdana', sans-serif";
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
	padding: 11px 30px 13px 30px;
}
.nav-current .nav-inner {
	font-weight: 600;
}
#secondary-bar .nav-inner {
	font-size: 0.875rem;
}
#secondary-bar .nav-item:not(#nav-item-search) .nav-inner {
	padding: 5px 0 3px 0;
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
	font-weight: 600;
}

/* Inbox indicator */

#inbox-indicator {
	top: 0;
}

/*==============*/
/* PAGE TOOLBAR */
/*==============*/

.page-toolbar > * {
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
.sublevel-nav .sublevel-item:hover {
	background-color: #ddd;
	color: #000;
	text-decoration: none;
	text-shadow: none;
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

/*=====================*/
/* SORT ORDER SELECTOR */
/*=====================*/

.sublevel-nav.sort .sublevel-item {
	font-family: <?php echo $UI_font_smallcaps; ?>;
}
.sublevel-nav.sort {
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
	text-shadow: none;
	box-shadow:
		0 0 0 5px #ccc inset;
}

#theme-selector button::before {
	color: #aaa;
	background-color: #eee;
}
#theme-selector button:hover::before,
#theme-selector button.selected::before {
	color: #777;
}
#width-selector button::after {
	color: #aaa;
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
	color: #bbb;
}
@media only screen and (hover:hover), not screen and (-moz-touch-enabled) {
	#quick-nav-ui a:hover  {
		color: #000;
		background-color: #d8d8d8;
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
	text-shadow: 0.5px 0.5px 0 #fff;
}
#new-comment-nav-ui .new-comments-count::after {
	font-weight: 600;
	color: #777;
}
#new-comment-nav-ui .new-comment-sequential-nav-button:disabled {
	color: #bbb;
	text-shadow: none;
}
@media only screen and (hover:hover), not screen and (-moz-touch-enabled) {
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
		text-shadow: 0 0 1px #fff, 0 0 3px #fff, 0 0 5px #fff;
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
#hns-date-picker input {
	border: 1px solid #777;
	background-color: transparent;
	color: #666;
}
#hns-date-picker input:focus {
	color: #000;
}

/*======================*/
/* ANTI-KIBITZER TOGGLE */
/*======================*/

#anti-kibitzer-toggle button::before,
#anti-kibitzer-toggle button::after {
	background-color: #aaa;
	-webkit-background-clip: text;
	color: transparent;
	text-shadow: rgba(255,255,255,0.5) 0px 1px 1px;
}
#anti-kibitzer-toggle button:hover::before,
#anti-kibitzer-toggle button:hover::after {
	background-color: #555;
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
	margin: 0.6em 20px 0 20px;
	max-width: calc(100% - 40px);
	line-height: 1.1;
	font-family: <?php echo $UI_font; ?>, 'Font Awesome';
	font-size: 1.5rem;
}
h1.listing a[href^='/'] {
	font-weight: normal;
}
h1.listing a[href^="http"] {
	color: #ccc;
	top: 3px;
}

@media only screen and (hover: hover), not screen and (-moz-touch-enabled) {
	h1.listing a:hover,
	h1.listing a:focus {
		color: #777;
		background-color: rgba(255,255,255,0.85);
	}	
	h1.listing:focus-within::before {
		color: #00f;
		left: -0.625em;
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
	color: #0c0;
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
	top: 1px;
	left: -32px;
}
h1.listing + .post-meta .link-post-domain {
	order: 1;
}

/*============*/
/* USER PAGES */
/*============*/

#content.user-page h1.page-main-heading {
	border-bottom: 1px solid #ccc;
}

#content.user-page h1.listing,
#content.user-page h1.listing + .post-meta {
	background-color: #eee;
	border-style: solid;
	border-color: #ccc;
}
#content.user-page h1.listing {
	max-width: 100%;
	margin: 1rem 0 0 0;
	padding: 6px;
	border-width: 1px 1px 0 1px;
}
@media only screen and (hover: hover), not screen and (-moz-touch-enabled) {
	#content.user-page h1.listing a:hover,
	#content.user-page h1.listing a:focus {
		background-color: #eee;
	}	
	#content.user-page h1.listing:focus-within::before {
		left: -0.625em;
	}
}
#content.user-page h1.listing + .post-meta {
	margin: 0 0 1rem 0;
	padding: 0.5em 6px 6px 34px;
	border-width: 0 1px 1px 1px;
}
#content.user-page h1.listing + .post-meta .post-section::before {
	left: 1px;
	top: unset;
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
	font-weight: normal;
	color: #222;
}

/*============*/
/* LOGIN PAGE */
/*============*/

.login-container form input[type='submit'] {
	font-weight: 600;
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
	font-weight: 600;
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
.contents a:link {
	color: #d64400;
}
.post-body .contents ul {
	font-size: 0.875em;
}
.contents li::before {
	color: #999;
	font-feature-settings: "tnum";
}

/*==================*/
/* POSTS & COMMENTS */
/*==================*/

.post-body,
.comment-body {
	font-family: <?php echo $text_font; ?>;
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
/* POST-META */
/*===========*/

.post-meta a,
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

.post .post-meta .author {
	margin: 0 0.75em 0 0;
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

.post-meta .post-section::before {
	color: #fff;
	text-shadow: 
		1px 1px 0 #090, 
		0 1px 0 #090, 
		0 0 5px #090;
}
a.post-section:hover {
	text-decoration: none;
}
a.post-section:hover::before {
	color: #97ff7c;
}
.post-meta .post-section.alignment-forum::before {
	text-shadow:
		1px 1px 0   #626dd7,
		0   1px 0   #626dd7,
		0   0   5px #626dd7;
}
a.post-section.alignment-forum:hover::before {
	color: #e6e5ff;
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
.post.link-post a.link-post-link:focus {
	color: #777;
	border-bottom: 2px dotted #777;
}

/*==========*/
/* COMMENTS */
/*==========*/

#comments {
	border-top: 1px solid #000;
	box-shadow: 0 3px 4px -4px #000 inset;
}
#content > .comment-thread .comment-meta a.date:focus,
#content > .comment-thread .comment-meta a.permalink:focus {
	color: #999;
	outline: 2px dotted #aaa;
	padding: 0 4px;
	position: relative;
	background-color: #fff;
}
#content > .comment-thread .comment-meta a.permalink:focus {
	padding: 0 5px;
}
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

<?php nested_stuff(".comment-item .comment-item ", ".comment-item,\n", ".comment-item", $comment_nesting_depth); ?> {
	background-color: #eee;
}
<?php nested_stuff(".comment-item .comment-item ", ".comment-item a.comment-parent-link::after,\n", ".comment-item a.comment-parent-link::after", $comment_nesting_depth); ?> {
	box-shadow: 
		0 28px 16px -16px #fff inset,
		4px 16px 0 12px #ffd inset,
		4px	4px 0 12px #ffd inset;
}

<?php nested_stuff(".comment-item .comment-item ", ".comment-item .comment-item,\n", ".comment-item .comment-item", $comment_nesting_depth); ?> {
	background-color: #fff;
}
<?php nested_stuff(".comment-item .comment-item ", ".comment-item .comment-item a.comment-parent-link::after,\n", ".comment-item .comment-item a.comment-parent-link::after", $comment_nesting_depth); ?> {
	box-shadow: 
		0 28px 16px -16px #eee inset,
		4px 16px 0 12px #ffd inset,
		4px	4px 0 12px #ffd inset;
}

<?php nested_stuff(".comment-item ", ".comment-item:target,\n", ".comment-item:target", (2 * $comment_nesting_depth) - 1); ?> {
	background-color: #ffd;
}
.comment-item:target > .comment-thread > .comment-item > .comment > .comment-meta > a.comment-parent-link::after {
	box-shadow: 
		0 28px 16px -16px #ffd inset,
		4px 16px 0 12px #ffd inset,
		4px	4px 0 12px #ffd inset !important;
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
	color: #999;
	font-size: 1.25em;
	font-weight: 600;
}
.comment-meta .author:hover {
	text-decoration: none;
	color: #090;
}
.comment-item .author:not(.redacted).original-poster::after {
	opacity: 0.8;
}

/*====================*/
/* ANTI-KIBITZER MODE */
/*====================*/

.author.redacted,
.inline-author.redacted {
	opacity: 0.8;
	font-weight: 400;
}

.karma-value.redacted {
	opacity: 0.4;
}

.link-post-domain.redacted {
	opacity: 0.7;
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
	font-weight: 400;
}
.comment-child-link::before {
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

@media only screen and (hover: hover), not screen and (-moz-touch-enabled) {
	#content.compact > .comment-thread .comment-item:hover .comment {
		background-color: #fff;
		outline: 3px solid <?php echo $hyperlink_color; ?>;
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
}
@media only screen and (hover: none), only screen and (-moz-touch-enabled) {
	#content.compact > .comment-thread.expanded .comment-item .comment {
		background-color: #fff;
		outline: 3px solid <?php echo $hyperlink_color; ?>;
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

.new-comment::before {
	outline: 2px solid #9037ff;
	box-shadow:
		0 0 6px -2px #9037ff inset, 
		0 0 4px #9037ff, 
		0 0 6px #9037ff;
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

/*=================================*/
/* INDIVIDUAL COMMENT THREAD PAGES */
/*=================================*/

.individual-thread-page > h1 {
	font-weight: normal;
}

/*==============*/
/* VOTE BUTTONS */
/*==============*/

.upvote,
.downvote {
	color: #c8c8c8;
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
	color: #c8c8c8;
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
	font-weight: 400;
	font-family: <?php echo $text_font; ?>;
	color: #000;
	background-color: #fff;
	border-color: #aaa;
	box-shadow: 
		0 0 0 1px #eee inset;
}
@-moz-document url-prefix() {
	.posting-controls textarea {
		font-weight: <?php global $platform; echo ($platform == 'Windows' ? '300' : '400'); ?>;
	}
}
.posting-controls textarea:focus {
	background-color: #ffe;
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
	background-image: linear-gradient(to bottom, #fff 0%, #ddd 50%, #ccc 75%, #aaa 100%);
}

.posting-controls.edit-existing-post .guiedit-buttons-container button,
.posting-controls form.edit-existing-comment .guiedit-buttons-container button {
	color: #050;
}
button.guiedit {
	font-family: Font Awesome, <?php echo $text_font; ?>;
}

button.guiedit::after {
	font-family: <?php echo $UI_font; ?>;
	color: #777;
	text-shadow: none;
}

/* Markdown hints */

#markdown-hints-checkbox + label {
	color: #999;
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

#edit-post-form .link-post-checkbox + label {
	top: -2px;
}
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
	padding: 4px 12px 5px 12px;
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

button:hover,
input[type='submit']:hover,
button:focus,
input[type='submit']:focus {
	color: #d00;
	text-shadow: <?php echo $white_glow; ?>;
}
button:active,
input[type='submit']:active {
	color: #f00;
	transform: scale(0.9);
}
.button:hover {
	color: #d00;
	text-shadow: <?php echo $white_glow; ?>;
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
	font-family: <?php echo $UI_font_smallcaps; ?>;
}
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
.post-body h6,
.comment-body h6 {
	color: #555;
}
.post-body h1,
.comment-body h1 {
	border-bottom: 1px solid #aaa;
}
.post-body h2 {
	border-bottom: 1px dotted #ccc;
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
	border-bottom: 1px solid #999;
}

code {
	background-color: #f6f6ff;
	border: 1px solid #ddf;
	border-radius: 4px;
}

input[type='text'],
input[type='search'],
input[type='password'] {
	background-color: #fff;
	border: 1px solid #ddd;
	color: #000;
}
input[type='text']:focus,
input[type='search']:focus,
input[type='password']:focus {
	background-color: #ffe;
	border: 1px solid #bbb;
	box-shadow: 0 0 1px #bbb;
}

select {
	color: #000;
}

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
	color: <?php echo $hyperlink_color; ?>;
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
@media only screen and (max-width: 1160px) {
	#theme-selector:hover::after {
		background-color: #ddd;
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
			0 0 0 1px #ccc,
			0 0 0 2px transparent;
	}
	#new-comment-nav-ui .new-comments-count::before {
		background-color: #eee;
		box-shadow: 
			0 0 0 1px #ccc,
			0 0 0 2px transparent;
		border-radius: 8px;
	}
	#anti-kibitzer-toggle {
		box-shadow: 
			0 0 0 1px #ccc,
			0 0 0 2px transparent;
		background-color: #eee;
		border-radius: 6px;
		overflow: hidden;
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

/**************************************************************************/
@media only screen and (hover: none), only screen and (-moz-touch-enabled) {
/**************************************************************************/
	#ui-elements-container > div[id$='-ui-toggle'] button  {
		color: #aaa;
		text-shadow:
			0 0 1px #fff,
			0 0 3px #fff,
			0 0 5px #fff,
			0 0 10px #fff,
			0 0 20px #fff,
			0 0 30px #fff;
	}

	#theme-selector {
		background-color: #eee;
		box-shadow: 
			0 0 0 1px #999,
			0 0 1px 3px #fff,
			0 0 3px 3px #fff,
			0 0 5px 3px #fff,
			0 0 10px 3px #fff,
			0 0 20px 3px #fff;
		border-radius: 12px;
	}
	#theme-selector::before,
	#theme-selector .theme-selector-close-button {
		color: #888;
		text-shadow: 0.5px 0.5px 0 #fff;
	}
	#theme-selector button {
		background-color: #e6e6e6;
		border-radius: 10px;
	}
	#theme-selector button::after {
		color: #000;
		padding-bottom: 2px;
		max-width: calc(100% - 3.25em);
		overflow: hidden;
		text-overflow: ellipsis;
	}
	#theme-selector button.selected::after {
		text-shadow: 
			0 -1px 0 #fff,
			0 0.5px 0.5px #000;
	}

	#quick-nav-ui {
		background-color: #fff;
	}
	#quick-nav-ui a {
		background-color: #eee;
		box-shadow: 0 0 0 1px #999;
	}
	#quick-nav-ui,
	#new-comment-nav-ui,
	#hns-date-picker {
		box-shadow:
			0 0 1px 3px #fff,
			0 0 3px 3px #fff,
			0 0 5px 3px #fff,
			0 0 10px 3px #fff,
			0 0 20px 3px #fff;
	}
	#quick-nav-ui a::after,
	#new-comment-nav-ui::before {
		font-family: <?php echo $UI_font; ?>;
		font-weight: 600;
		box-shadow:
			0 0 1px 0 #fff,
			0 0 3px 0 #fff,
			0 0 5px 0 #fff;
		background-color: #fff;
		border-radius: 4px;
	}
	#quick-nav-ui,
	#new-comment-nav-ui {
		border-radius: 8px;
	}
	#new-comment-nav-ui {
		background-color: #eee;
		border: 1px solid #999;
	}
	#new-comment-nav-ui::before {
		color: #777;
		font-weight: 600;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button {
		box-shadow: 0 0 0 1px #999;
		color: #777;
	}
	#new-comment-nav-ui .new-comments-count {
		background-color: inherit;
		box-shadow: 0 -1px 0 0 #999;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button:disabled {
		color: #bbb;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button.new-comment-previous {
		border-radius: 7px 0 0 7px;
	}
	#new-comment-nav-ui button::after {
		font-family: <?php echo $UI_font; ?>;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button.new-comment-next {
		border-radius: 0 7px 7px 0;
	}

	/*****************************************/
	@media only screen and (max-width: 900px) {
	/*****************************************/
		h1.listing + .post-meta > * {
			line-height: 1.5;
		}
		h1.listing + .post-meta .post-section::before {
			position: unset;
		}

		#secondary-bar .nav-inner {
			font-size: 1em;
		}
		#secondary-bar .nav-item:not(#nav-item-search) .nav-inner {
			padding: 6px 10px;
		}
		#primary-bar .nav-inner::before, 
		#secondary-bar .nav-inner::before {
			opacity: 0.8;
		}

		.archive-nav > *[class^='archive-nav-'] + *[class^='archive-nav-']::before {
			background-color: #aaa;
		}

		.comment-item .comment-item {
			margin: 0.75em 2px 4px 6px;
		}
		.comment-item .comment-item + .comment-item {
			margin: 1.5em 2px 4px 6px;
		}

		a.comment-parent-link:hover::before {
			background-color: unset;
		}

		.sublevel-nav .sublevel-item,
		.sublevel-nav .sublevel-item:first-child,
		.sublevel-nav .sublevel-item:last-child {
			border-width: 1px;
			border-radius: 8px;
		}
	/*******************************************/
	} @media only screen and (max-width: 720px) {
	/*******************************************/
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
		h1.listing a[href^='http'] {
			top: 2px;
		}

		#content.compact > .comment-thread .comment-item {
			max-height: 108px;
		}
	
		.textarea-container:focus-within button:active {
			background-color: #ccc;
		}
		.textarea-container:focus-within .guiedit-mobile-auxiliary-button {
			background-color: #eee;
			border: 1px solid #ddd;
			border-radius: 6px;
		}
		.textarea-container:focus-within .guiedit-mobile-help-button.active {
			border-color: #c00;
			box-shadow:
				0 0 0 1px #fff,
				0 0 0 2px #c00;
			color: #c00;
			font-weight: 600;
		}
		.textarea-container:focus-within .guiedit-buttons-container {
			background-color: #fff;
			border-top: 1px solid #ddf;
		}
		#content.conversation-page .textarea-container:focus-within::after {
			background-color: #fff;
		}
		.textarea-container:focus-within button.guiedit {
			background-color: #eee;
			border: 1px solid #ddd;
			border-radius: 6px;
		}
		.markdown-hints::after {
			color: #090;
		}

		#edit-post-form .link-post-checkbox + label {
			margin-left: 1.25em;
		}
	}
}
