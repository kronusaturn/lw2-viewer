<?php
	$UI_font = "'Arial', sans-serif";
	$text_font = "'Arial', sans-serif";
	$hyperlink_color = "#6a8a6b";
?>

/*****************/
/* CLASSIC THEME */
/*****************/

body {
	color: #000;
	background-color: #d8d8d8;
	font-family: <?php echo $UI_font; ?>;
}
#content {
	background-color: #fff;
	box-shadow: 0px 0px 10px #555;
	line-height: 1.5;
}

/*=========*/
/* NAV BAR */
/*=========*/

.nav-inner {
	font-size: 1.125em;
	font-weight: bold;
}
.nav-inner,
#primary-bar.inactive-bar .nav-inner {
	padding: 13px 30px 11px 30px;
}
#secondary-bar .nav-inner {
	font-size: 0.875em;
}
#secondary-bar .nav-item:not(#nav-item-search) .nav-inner {
	padding: 6px 0 4px 0;
}

#bottom-bar.decorative {
	background-color: #fff;
}
#bottom-bar.decorative::before,
#bottom-bar.decorative::after {
	content: "GW";
	font-weight: 200;
	display: block;
	text-align: center;
	padding: 0.5em 0 0.75em 0;
}
#bottom-bar.decorative::before {
	width: 100%;
	color: transparent;
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/one_pixel_DDD.gif")) ?>');
	background-repeat: repeat-x;
	background-position: center 44%;
	margin: 0 30px;
	filter: opacity(0.7);
}
#bottom-bar.decorative::after {
	color: #eee;
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
	top: 0;
	font-weight: normal;
	font-size: 0.7em;
	color: #ddd;
}
.nav-inner:hover::after {
	color: #bbb;
}

/* This makes the navbar items look like tabs: */

.active-bar {
	position: relative;
}
.nav-bar {
	background-color: #f5f5f5;
}
.nav-bar {
	border-bottom: 1px solid #d6d5d6;
}
.nav-bar a,
.nav-bar a:visited {
	color: #999;
}
.nav-bar a:hover {
	color: #777;
	text-decoration: none;
}
.nav-bar .nav-current {
	color: #666;
}
.nav-item:nth-of-type(n+2) {
	box-shadow: -9px 0 0 -8px #d6d5d6;
}

/* Search tab */

#nav-item-search form::before {
	position: relative;
	top: 2px;
	opacity: 0.3;
}
#nav-item-search button {
	border: none;
	font-weight: inherit;
}

/* User/login tab */

#inbox-indicator::before {
	top: 1px;
	color: #ccc;
}

/*==============*/
/* PAGE TOOLBAR */
/*==============*/

.new-post,
.new-post:visited,
.new-private-message,
.new-private-message:visited {
	color: #090;
}
.logout-button {
	color: #d33;
}

/*==============*/
/* SUBLEVEL NAV */
/*==============*/

.sublevel-nav .sublevel-item {
	border-color: #ddd;
	border-style: solid;
	border-width: 1px 1px 1px 0;
	color: #777;
    line-height: 1;
    padding: 7px 3px;
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
	top: 1px;
}
.sublevel-nav.sort .sublevel-item {
	padding: 7px 7px 6px 7px;
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
		0 0 0 4px #d8d8d8 inset,
		0 0 0 5px #bbb inset;
}
#width-selector button:hover,
#width-selector button.selected,
#theme-selector button:hover,
#theme-selector button.selected {
	box-shadow:
		0 0 0 5px #bbb inset;
}

#theme-selector button::before {
	font-size: 0.9375em;
	padding: 5px 6px 7px 6px;
	color: #999;
	background-color: #d8d8d8;
}
#theme-selector button:hover::before,
#theme-selector button.selected::before {
	color: #222;
}
#width-selector button::after {
	color: #999;
	font-size: 0.9em;
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
#quick-nav-ui a:active {
	transform: scale(0.9);
}
#quick-nav-ui a[href='#comments'].no-comments {
	opacity: 0.4;
	color: #bbb;
}
@media only screen and (hover: hover), not screen and (-moz-touch-enabled) {
	#quick-nav-ui a:hover {
		color: #000;
		background-color: #eee;
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
	background-color: #888;
	-webkit-background-clip: text;
	color: transparent;
	text-shadow: rgba(255,255,255,0.5) 0px 1px 1px;
}
#anti-kibitzer-toggle button:hover::before,
#anti-kibitzer-toggle button:hover::after {
	background-color: #444;
}

/*======================*/
/* TEXT SIZE ADJUSTMENT */
/*======================*/

#text-size-adjustment-ui button {
	color: #777;
}
#text-size-adjustment-ui button:hover {
	color: #222;
}
#text-size-adjustment-ui button.default {
	font-weight: 600;
}
#text-size-adjustment-ui button:disabled:hover {
	text-shadow: none;
}
#text-size-adjustment-ui::after {
	color: #999;
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
	color: rgba(0, 0, 238, 0.7);
}
.archive-nav a:hover {
	text-decoration: none;
	color: #c00;
	background-color: #e0e0e0;
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
	margin: 1.125em 20px 0.25em 20px;
	max-width: calc(100% - 40px);
	font-family: <?php echo $UI_font; ?>, 'Font Awesome';
	font-size: 1.125rem;
	padding-left: 30px;
	max-height: unset;
}

h1.listing a[href^='/posts'] {
	color: #538d4d;
	white-space: unset;
	display: inline;
}
h1.listing a[href^='/posts']:visited {
	color: #5a5a5b;
}
h1.listing a[href^="http"] {
	top: 2px;
}

@media only screen and (hover: hover), not screen and (-moz-touch-enabled) {
	h1.listing a:hover,
	h1.listing a:focus {
		color: #3d3d3e;
		text-decoration: none;
	}	
	h1.listing a:focus {
		border-bottom: 1px dotted #999;
	}	
	h1.listing:focus-within::before {
		display: none;
	}
	h1.listing:focus-within + .post-meta .karma-value {
		box-shadow: 
			0 0 0 3px #fff,
			0 0 0 6px #3d3d3e;
	}
	h1.listing a[href^="http"]:hover,
	h1.listing a[href^="http"]:focus {
		color: #4879ec;
		text-shadow: 
			 0.5px 0.5px 0 #fff,
			 -0.5px -0.5px 0 #fff,
			 0 0 2px #fff,
			 0 0 3px #00c;
		border: none;
	}
}

h1.listing .edit-post-link {
	padding: 5px 3px 12px 0.5em;
	top: 0;
	right: 0;
}
h1.listing .edit-post-link:hover {
	text-decoration: none;
}
#content.user-page h1.listing .edit-post-link {
	background-color: #f7f7f8;
}

/*===================*/
/* LISTING POST-META */
/*===================*/

h1.listing + .post-meta {
	font-size: 0.8125rem;
	padding-left: 30px;
	display: flex;
	justify-content: flex-end;
}

h1.listing + .post-meta .author {
	font-weight: bold;
	color: #6a8a6b;
	text-decoration: none;
	margin: 0 0 0 1.5em;
	order: 0;
}
h1.listing + .post-meta .author:hover {
	color: #3d3d3e;
}
h1.listing + .post-meta .date,
h1.listing + .post-meta .read-time {
	color: #999;
	font-style: italic;
}
h1.listing + .post-meta a {
	color: #8a8a8b;
	text-decoration: underline;
}
h1.listing + .post-meta a:hover {
	color: #3d3d3e;
}

h1.listing + .post-meta .karma-value {
	background-color: #538d4d;
	color: #fff;
	font-weight: bold;
	font-size: 0.8125rem;
	border-radius: 1.0625em;
	padding: 2px 6px 1px 6px;
	text-align: center;
	display: block;
	min-width: 2.125em;
	position: absolute;
	right: calc(100% - 1.75em);
	top: -2.1em;
}
h1.listing + .post-meta .karma-value span,
h1.listing + .post-meta .lw2-link span,
h1.listing + .post-meta .comment-count span {
	display: none;
}

h1.listing + .post-meta > * {
	text-align: right;
	margin: 0 0 0 0.5em;
}
h1.listing + .post-meta .read-time {
	width: 6.5em;
	order: 2;
}
h1.listing + .post-meta .comment-count {
	order: -1;
}
h1.listing + .post-meta .comment-count::before {
	content: "Comments ("
}
h1.listing + .post-meta .comment-count::after {
	content: ")"
}
h1.listing + .post-meta .date {
	width: 11em;
	order: 1;
}
h1.listing + .post-meta .lw2-link {
	margin: 0 0 0 1em;
	order: 3;
}
h1.listing + .post-meta .post-section {
	margin: 0;
	text-decoration: none;
}
h1.listing + .post-meta .post-section::before {
	left: unset;
	right: calc(100% + 1.15em);
	top: -1.95em;
}
h1.listing + .post-meta .link-post-domain {
	order: -2;
	margin: 0 1em 0 0;
}

/*============*/
/* USER PAGES */
/*============*/

#content.user-page h1.page-main-heading {
	border-bottom: 1px solid #ccc;
}

#content.user-page h1.listing,
#content.user-page h1.listing + .post-meta {
	background-color: #f7f7f8;
	border-style: solid;
	border-color: #bbbcbf;
}

#content.user-page h1.listing {
	padding: 0.5em 6px 0 48px;
	border-width: 1px 1px 0 1px;
	margin: 1rem 0 0 0;
	max-width: 100%;
}
#content.own-user-page h1.listing,
h1.listing.own-post-listing {
	padding-right: 36px;
}
#content.user-page h1.listing a:hover {
	background-color: #f7f7f8;
}

#content.user-page h1.listing + .post-meta {
	padding: 0.25em 10px 0.5em 32px;
	border-width: 0 1px 1px 1px;
	margin: 0 0 1rem 0;
}
#content.user-page h1.listing + .post-meta .karma-value {
	right: calc(100% - 3.25em);
	top: -1.8em;
}
#content.user-page h1.listing + .post-meta .post-section::before {
	right: calc(100% - 5.1em);
	top: 4px;
}

#content.conversations-user-page h1.listing {
	padding: 8px 6px 8px 10px;
	font-size: 1.25rem;
}
#content.conversations-user-page h1.listing + .post-meta {
	padding: 0 10px 6px 4px;
	margin: 0;
}

.user-stats .karma-total {
	font-weight: bold;
}

/*================*/
/* SEARCH RESULTS */
/*================*/

#content.search-results-page h1.listing {
	font-size: 1.125rem;
}

/*===============*/
/* CONVERSATIONS */
/*===============*/

#content.conversation-page h1.page-main-heading {
	font-size: 1.375em;
	text-align: left;
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
	border: 1px solid #ddd;
	background-color: #eee;
	padding: 0.75em 0.5em 0.5em 0.5em;
}
.contents-head {
	font-weight: bold;
	font-size: 1.25em;
}
.post-body .contents ul {
	font-size: 1em;
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
	font-size: 1rem;
}
.post-body a,
.comment-body a {
	text-decoration: underline;
}
.post-meta > *,
.comment-meta > * {
	white-space: unset;
}

/*===========*/
/* POST-META */
/*===========*/

.post-meta {
	line-height: 1.9;
}
.post-meta .post-section::before {
	color: #fff;
	text-shadow: 
		1px 1px 0 #090, 
		0 1px 0 #090, 
		0 0 5px #090;
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

.post .post-meta {
	position: relative;
	font-size: 0.875rem;
	justify-content: flex-start;
	padding-left: 30px;
}
.post .post-meta .author {
	font-weight: bold;
	color: #6a8a6b;
	text-decoration: none;
}
.post .post-meta .author:hover {
	color: #3d3d3e;
}
.post .post-meta .date{
	color: #999;
	font-style: italic;
}
.post .post-meta a {
	color: #8a8a8b;
	text-decoration: underline;
}
.post .post-meta a:hover {
	color: #3d3d3e;
}

.post .post-meta .karma {
	order: -1;
}
.post .post-meta .karma-value {
	background-color: #538d4d;
	color: #fff;
	font-weight: bold;
	font-size: 0.8125rem;
	border-radius: 1.125em;
	padding: 2px 6px 1px 6px;
	text-align: center;
	min-width: 2.125em;
	display: block;
	float: left;
	margin: 0 0.5em 0 0;
}
.post .post-meta .karma-value span,
.post .post-meta .lw2-link span,
.post .post-meta .comment-count span {
	display: none;
}

.post .post-meta .comment-count::before {
	content: "Comments ("
}
.post .post-meta .comment-count::after {
	content: ")"
}

.post .post-meta .post-section::before {
	position: absolute;
	top: 1px;
	left: 0;
}
.post .bottom-post-meta .post-section::before {
	top: 21px;
}

/*============*/
/* LINK POSTS */
/*============*/

.post.link-post > .post-body > p:first-child {
	margin: 0;
}
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
	color: #aaa;
	border-bottom: 2px dotted #aaa;
}

/*=======*/
/* POSTS */
/*=======*/

.post > h1:first-child {
	font-size: 1.375rem;
	text-align: left;
	margin: 2em 0 0.5em 0;
	line-height: 1.2;
}

/*==========*/
/* COMMENTS */
/*==========*/

#comments {
	border-top: 1px solid #000;
	box-shadow: 0 3px 4px -4px #000 inset;
}
@-moz-document url-prefix() {
	#comments {
		box-shadow: 0 3px 3px -4px #000 inset;
	}
}
#content > .comment-thread .comment-meta a.date:focus,
#content > .comment-thread .comment-meta a.permalink:focus {
	color: #444;
	outline: 2px dotted #444;
	padding: 0 5px;
	position: relative;
	background-color: #fff;
}
.comment-item {
	border: 1px solid #bbbcbf;
}
.comment-item .comment-item {
	margin: 1em -1px 8px 16px;
}
.comment-item .comment-item + .comment-item {
	margin: 2em -1px 8px 16px;
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
	background-color: #f7f7f8;
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

a.comment-parent-link::after {
	display: none;
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

.comment-meta > * {
	font-size: 0.9375rem;
	padding-top: 2px;
}
.comment-meta .author {
	font-weight: bold;
	color: #538d4d;
}
.comment-meta .date {
	color: #999;
	font-style: italic;
}
.comment-meta .date:hover {
	color: #777;
}
.comment-meta .karma-value,
.comment-controls .karma .karma-value {
	color: #666;
	float: left;
	margin-right: 0.5em;
}
.comment-meta .karma-value:only-child {
	float: none;
}
.comment-item .author:not(.redacted).original-poster::after {
	opacity: 0.8;
}

.comment-item .karma.active-controls::after,
.comment-item .karma .karma-value::after,
.post .karma.active-controls::after,
.post .karma .karma-value::after {
	background-color: #fff;
	color: #888;
	box-shadow: 0 0 0 1px #ccc inset;
	border-radius: 4px;
}
.comment-item .karma.active-controls::after,
.post .karma.active-controls::after {
	padding: 6px;
	max-width: unset;
	bottom: -46px;
	width: 110px;
}
.comment-item .karma .karma-value::after,
.post .karma .karma-value::after {
	padding: 2px 8px;
	top: -36px;
	min-width: 64px;
	font-weight: normal;
}

/*====================*/
/* COMMENT PERMALINKS */
/*====================*/

.comment-meta .permalink,
.comment-meta .lw2-link,
.individual-thread-page .comment-parent-link:empty {
	top: 1px;
	filter: grayscale(50%);
}
.comment-meta .permalink,
.comment-meta .lw2-link,
.individual-thread-page .comment-parent-link:empty {
	filter: unset;
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
	opacity: 0.5;
}

.link-post-domain.redacted {
	opacity: 0.5;
}

/*===========================*/
/* COMMENT THREAD NAVIGATION */
/*===========================*/

div.comment-parent-link {
	font-weight: bold;
}
a.comment-parent-link {
	font-weight: normal;
}
a.comment-parent-link::before {
	color: #bbb;
}
a.comment-parent-link:hover::before {
	background-color: #ffd;
	color: #999;
}

div.comment-child-links {
	font-weight: bold;
}
div.comment-child-links a {
	font-weight: normal;
}
.comment-child-link::before {
	color: #bbb;
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
#content.compact > .comment-thread .comment-item {
    max-height: 57px;
}
#content.compact > .comment-thread .comment-item::after {
	color: <?php echo $hyperlink_color; ?>;
	background: linear-gradient(to right, transparent 0%, #fff 50%, #fff 100%);
}

@media only screen and (hover: hover), not screen and (-moz-touch-enabled) {
	#content.compact > .comment-thread .comment-item:hover .comment,
	#content.compact > .comment-thread .comment-item.expanded .comment {
		background-color: #fff;
		outline: 3px solid <?php echo $hyperlink_color; ?>;
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
	outline: 2px solid #5a5;
	box-shadow:
		0 0 6px -2px #5a5 inset, 
		0 0 4px #5a5, 
		0 0 6px #5a5;
}

/*=================================*/
/* COMMENT THREAD MINIMIZE BUTTONS */
/*=================================*/

.comment-meta .comment-minimize-button {
	font-size: 1.25rem;
	top: -1px;
}
.comment-meta .comment-minimize-button::after {
	top: 23px;
}
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

/*==============*/
/* VOTE BUTTONS */
/*==============*/

.upvote,
.downvote {
	color: #c8c8c8;	
	padding: 0 5px;
	position: relative;
}
.vote::before {
	position: relative;
}
.upvote::before {
	content: '\F164';
}
.downvote::before {
	content: '\F165';
	top: 2px;
}

.vote {
	text-shadow: 
		 1px  1px 0 #999,
		-1px  1px 0 #999,
		 1px -1px 0 #999,
		-1px -1px 0 #999;
}
.upvote.selected,
.upvote:hover {
	text-shadow: 
		 1px  1px 0 #080,
		-1px  1px 0 #080,
		 1px -1px 0 #080,
		-1px -1px 0 #080;
}

.downvote.selected,
.downvote:hover {
	text-shadow: 
		 1px  1px 0 #b00,
		-1px  1px 0 #a00,
		 1px -1px 0 #b00,
		-1px -1px 0 #b00;
}
@media only screen and (min-resolution: 192dpi) {
	.vote {
		text-shadow: 
			 0.5px  0.5px 0 #999,
			-0.5px  0.5px 0 #999,
			 0.5px -0.5px 0 #999,
			-0.5px -0.5px 0 #999;
	}
	.upvote.selected,
	.upvote:hover {
		text-shadow: 
			 0.5px  0.5px 0 #060,
			-0.5px  0.5px 0 #060,
			 0.5px -0.5px 0 #060,
			-0.5px -0.5px 0 #060;
	}

	.downvote.selected,
	.downvote:hover {
		text-shadow: 
			 0.5px  0.5px 0 #900,
			-0.5px  0.5px 0 #900,
			 0.5px -0.5px 0 #900,
			-0.5px -0.5px 0 #900;
	}
}

.vote::before {
	z-index: 1;
}

.vote::after {
	position: absolute;
	color: #fff;
	visibility: hidden;
}
.vote:hover::after,
.vote.big-vote::after,
.vote:not(.big-vote).clicked-twice::after {
	visibility: visible;
}
.vote.big-vote::after,
.vote:not(.big-vote).clicked-twice::after {
	color: inherit;
}
.karma:not(.waiting) .vote.clicked-once::after {
	color: #c8c8c8;	
}

.upvote::after {
	content: "\F164";
	left: -1px;
	top: -3px;
}

.downvote::after {
	content: "\F165";
	left: -1px;
	top: 5px;
}

.comment-controls .karma {
	margin-left: -6px;
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
	max-width: 1.2em;
	overflow: hidden;
	margin-right: 0.375em;
}
.comment-controls .cancel-comment-button::before {
	font-size: 1.25em;
}
.comment-controls .cancel-comment-button:hover {
	color: #f00;
}

.new-comment-button {
	font-weight: 600;
}

.comment-controls .reply-button {
	max-width: 1.6em;
	overflow: hidden;
	margin-right: 0.5em;
}
.comment-controls .reply-button::before {
	content: "\F086";
	font-weight: 400;
	font-size: 1.125rem;
}
.comment-controls .edit-button {
	color: #0b0;
	max-width: 1.3em;
	overflow: hidden;
	margin-right: 0.25em;
}
.comment-controls .edit-button::before {
	font-size: 1rem;
}
.comment-controls .edit-button:hover {
	color: #f00;
}

h1.listing .edit-post-link,
h1.listing .edit-post-link:visited,
.post-controls .edit-post-link,
.post-controls .edit-post-link:visited {
	color: #090;
}
h1.listing .edit-post-link:hover,
.post-controls .edit-post-link:hover {
	color: #d00;
}

.posting-controls textarea {
	font-family: <?php echo $text_font; ?>;
	font-size: 1rem;
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
	top: 4px;
}

/* Markdown hints */

#markdown-hints-checkbox + label {
	color: <?php echo $hyperlink_color; ?>;
}
#markdown-hints-checkbox + label:hover {
	color: #e00;
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
	top: 2px;
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
#edit-post-form #markdown-hints-checkbox + label {
	padding: 3px 0 0 14px;
}

/*=======*/
/* LINKS */
/*=======*/

a {
	text-decoration: none;
	color: <?php echo $hyperlink_color; ?>;
}
a:visited {
	color: #8a8a8b;
}
a:hover {
	color: #3d3d3e;
}

/*=========*/
/* BUTTONS */
/*=========*/

button,
input[type='submit'] {
	color: <?php echo $hyperlink_color; ?>;
}

button:hover,
input[type='submit']:hover,
button:focus,
input[type='submit']:focus {
	color: #d00;
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
	color: #f00;
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
	font-weight: 600;
}
.post-body h6,
.comment-body h6 {
	color: #555;
}
.post-body h1,
.comment-body h1 {
	border-bottom: 1px solid #aaa;
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
	border: 1px solid #bbb;
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
	padding: 2px 12px;
	white-space: nowrap;
}
.qualified-linking-toolbar a:visited {
	color: <?php echo $hyperlink_color; ?>;
}
.qualified-linking-toolbar a:hover {
	text-decoration: none;
	background-color: #ddd;
}
.qualified-linking label::after {
	background-color: #d8d8d8;
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
@media only screen and (max-width: 1160px) {
	#theme-selector:hover::after {
		background-color: #999;
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
	#anti-kibitzer-toggle {
		background-color: #d8d8d8;
		box-shadow: 
			0 0 0 1px #999,
			0 0 0 2px transparent;
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
		background-color: #d8d8d8;
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
		color: #000;
		font-weight: normal;
	}
	#theme-selector button {
		background-color: #e6e6e6;
		border-radius: 10px;
	}
	#theme-selector button::after {
		color: #444;
		max-width: calc(100% - 3.5em);
		overflow: hidden;
		text-overflow: ellipsis;
		padding-bottom: 1px;
	}
	#theme-selector button.selected::after {
		color: #000;
		text-shadow: 
			0 -1px 0 #fff,
			0 0.5px 0.5px #000;
	}

	#quick-nav-ui {
		background-color: #fff;
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
		font-weight: bold;
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
		background-color: #e4e4e4;
		border: 1px solid #999;
	}
	#new-comment-nav-ui::before {
		color: #777;
		font-weight: bold;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button {
		box-shadow: 0 0 0 1px #999;
		color: #538d4d;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button:disabled {
		color: #bbb;
	}
	#new-comment-nav-ui .new-comments-count {
		background-color: inherit;
		box-shadow: 0 -1px 0 0 #999;
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
	#hns-date-picker {
		background-color: #e4e4e4;
		border: 1px solid #999;
	}

	/*****************************************/
	@media only screen and (max-width: 900px) {
	/*****************************************/
		h1.listing + .post-meta .post-section {
			overflow: visible;
			order: -2;
			width: unset;
		}
		h1.listing + .post-meta .post-section::before {
			position: unset;
		}

		#primary-bar .nav-inner {
			font-size: 1.25em;
		}
		#secondary-bar .nav-inner {
			font-size: 1.125em;
		}
		#secondary-bar .nav-item:not(#nav-item-search) .nav-inner {
			padding: 6px 10px;
		}

		.archive-nav *[class^='archive-nav-item-'] {
			border-width: 1px !important;
		}
		.archive-nav > *[class^='archive-nav-'] + *[class^='archive-nav-']::before {
			background-color: #aaa;
		}

		.comment-item .comment-item {
			margin: 0.75em 0 3px 6px;
		}
		.comment-item .comment-item + .comment-item {
			margin: 1.5em 0 3px 6px;
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
		.post-meta .comment-count::before {
			font-family: inherit;
			font-size: inherit;
			margin: 0;
		}
	/*******************************************/
	} @media only screen and (max-width: 520px) {
	/*******************************************/
		#primary-bar.inactive-bar .nav-inner {
			padding: 6px 10px;
			font-size: 1.375em;
		}

		h1.listing {
			margin: 18px 6px 4px 6px;
			max-width: calc(100% - 12px);
			padding: 0;
		}
		h1.listing a[href^='http'] {
			top: 2px;
		}
		h1.listing + .post-meta {
			margin: 4px 6px;
		}
		h1.listing + .post-meta > *:not(.karma) {
			line-height: 1.5;
			width: unset;
		}
		h1.listing + .post-meta .karma-value {
			top: 0;
			right: calc(100% - 2.25em);
		}

		#content.compact > .comment-thread .comment-item {
			max-height: 104px;
		}

		.textarea-container:focus-within textarea {
			background-color: #fff;
		}
		.textarea-container:focus-within .guiedit-mobile-auxiliary-button {
			padding: 6px;
			font-weight: bold;
		}
		.textarea-container:focus-within .guiedit-mobile-help-button.active {
			box-shadow:
				0 0 0 2px #c00;
			color: #c00;
		}
		.textarea-container:focus-within .guiedit-buttons-container {
			background-color: #fff;
			border-top: 1px solid #ddf;
		}
		.posting-controls .textarea-container:focus-within .guiedit-buttons-container {
			box-shadow: none;
		}
		#content.conversation-page .textarea-container:focus-within::after {
			background-color: #fff;
		}
		.textarea-container:focus-within button.guiedit {
			border: 1px solid #6a8a6b;
			border-radius: 6px;
		}
		
		#edit-post-form textarea {
			min-height: calc(100vh - 345px);
		}
	}
}
