<?php
	$UI_font = "'Mundo Sans', 'Helvetica', sans-serif";
	$headings_font = "'Caecilia', 'Helvetica', sans-serif";
	$text_font = "'Source Serif Pro', 'Helvetica', sans-serif";
	$hyperlink_color = "#92c396";
	$white_glow = "0 0 1px #fff, 0 0 3px #fff, 0 0 5px #fff";
?>

/**************/
/* THEME LESS */
/**************/

body {
	color: #000;
	font-family: <?php echo $UI_font; ?>;
	font-weight: 300;
	background-color: #fff;
}
#content {
	line-height: 1.55;
	overflow: visible;
	padding: 30px 20px 0 90px;
	transform: scale(1);
}
#content.post-page {
	padding: 0 0 0 60px;
}
#ui-elements-container {
	visibility: hidden;
}

/* Compensating for Linux/Windows being terrible. */

.post-meta > *:not(.post-section),
#primary-bar a,
#secondary-bar a,
#nav-item-search > *,
.page-toolbar > *,
#top-nav-bar > * {
	text-shadow: <?php global $platform; echo ($platform == 'Mac' ? 'none' : '0 0 0 #aaa'); ?>;
}

/*=========*/
/* NAV BAR */
/*=========*/

.nav-bar {
	background-color: #fff;
}
.nav-inner {
	font-size: 1.125em;
	padding: 0.5rem 0.75rem;
	text-align: right;
}
#secondary-bar .nav-inner {
	font-size: 0.875em;
	padding: 0.5rem 0.75rem;
}
#primary-bar,
#secondary-bar {
	position: absolute;
	right: calc(100% + 30px);
	margin: 0;
	flex-flow: column;
	line-height: 1;
}
#content.post-page #primary-bar,
#content.post-page #secondary-bar {
	right: 100%;
}

#bottom-bar .nav-inner {
	text-align: center;
    padding: 0.75em;
}

.nav-bar a,
.nav-bar a:visited {
	color: #acd2af;
}
.nav-bar a:hover {
	color: #79a97e;
}

/* Accesskey hints */

.nav-inner::after {
	display: none;
}

/* "Tabs" */

.nav-current {
	font-weight: bold;
}

#nav-item-recent-comments .nav-inner span {
	display: none;
}
#nav-item-home {
	padding-top: 0.5em;
}
#nav-item-login {
	position: fixed;
	top: 0;
	right: 0;
	padding-right: 1.5em;
}

/* Search tab */

#nav-item-search {
	position: fixed;
	top: 0;
	left: 5em;
	width: 400px;
}
#nav-item-search .nav-inner {
	padding: 1px;
	display: flex;
}
#nav-item-search form::before {
	font-size: 1.125em;
	color: #e6e6e6;
	padding: 5px;
	transition: color 0.15s ease;
}
#nav-item-search form:focus-within::before {
	color: #92c396;
}
#nav-item-search button {
	border: none;
	font-weight: inherit;
	padding: 6px;
	height: 23px;
}
#nav-item-search form:not(:focus-within) button:not(:hover) {
	color: #ddd;
}
#nav-item-search input {
	width: unset;
	flex: 1 0 auto;
	font-family: Inconsolata, monospace;
	padding: 2px 1px;
	margin: 0 0 0 2px;
}

/* Inbox indicator */

#inbox-indicator::before {
	color: #eaeaea;
	top: 3px;
	font-size: 1.125em;
}

/*==============*/
/* PAGE TOOLBAR */
/*==============*/

#content > .page-toolbar {
	padding: 0 0 0 0;
	margin: 0;
	white-space: nowrap;
	position: absolute;
	right: calc(100% + 30px);
	grid-column: 1;
}

.page-toolbar > * {
	display: block;
	text-align: right;
	line-height: 1;
	padding: 0.5rem 0.75rem;
}

.page-toolbar button {
	padding: 0;
}

.page-toolbar .button::before {
	font-size: 0.875em;
}

.new-private-message {
	white-space: normal;
	line-height: 1.15;
}
.new-private-message::before {
	opacity: 0.7;
}

.logout-button {
	color: #d33;
}

/*===================*/
/* TOP PAGINATION UI */
/*===================*/

#top-nav-bar {
	justify-content: flex-start;
	padding: 1em 0 0.25em 0;
	font-size: 1em;
	margin: 0 0 0 -5px;
	grid-column: 1;
}
#top-nav-bar .page-number {
	line-height: 1.5;
}
#top-nav-bar .page-number span {
	display: none;
}
#top-nav-bar a.disabled {
	visibility: visible;
	opacity: 0.4;
}

.sublevel-nav + #top-nav-bar {
	justify-content: center;
	grid-column: 2;
	padding: 0;
}
.archive-nav + #top-nav-bar {
	margin: 0.5em 0 0 -4px;
	padding: 0;
}

/*==============*/
/* SUBLEVEL NAV */
/*==============*/

.sublevel-nav .sublevel-item {
	border-color: #c4dbc4;
	border-style: solid;
	border-width: 1px 1px 1px 0;
	color: #92c396;
	padding: 3px 9px 0 9px;
}
.sublevel-nav .sublevel-item:first-child {
	border-radius: 8px 0 0 8px;
	border-width: 1px;
}
.sublevel-nav .sublevel-item:last-child {
	border-radius: 0 8px 8px 0;
}
.sublevel-nav a.sublevel-item:hover,
.sublevel-nav a.sublevel-item:active,
.sublevel-nav span.sublevel-item {
	background-color: #c4dbc4;
	color: #fff;
	text-decoration: none;
}
.sublevel-nav a.sublevel-item:active {
	background-color: #92c396;
	border-color: #92c396;
}

/*=====================*/
/* SORT ORDER SELECTOR */
/*=====================*/

.sublevel-nav.sort {
	padding: 18px 0 0 0;
	border-radius: 8px;
}
.sublevel-nav.sort::before {
	text-transform: uppercase;
	color: #444;
	text-shadow: 0.5px 0.5px 0 #fff;
}
.sublevel-nav.sort .sublevel-item {
	border-radius: 0;
	padding: 5px 6px 2px 6px;
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

/*============*/
/* UI TOGGLES */
/*============*/

@media not screen and (hover: none), not screen and (-moz-touch-enabled) {
	#site-nav-ui-toggle,
	#post-nav-ui-toggle {
		visibility: visible;
		position: absolute;
		display: inline-block;
		border-radius: 50%;
		z-index: 1;
	}
	#site-nav-ui-toggle button,
	#post-nav-ui-toggle button {
		font-family: Font Awesome;
		font-weight: 900;
		font-size: 32px;
		padding: 10px;
		opacity: 0.4;
		-webkit-tap-highlight-color: transparent;
		transition:
			transform 0.2s ease,
			opacity 0.15s ease;
	}
	#site-nav-ui-toggle button:hover,
	#post-nav-ui-toggle button:hover {
		opacity: 1.0;
	}
	#site-nav-ui-toggle button::selection,
	#post-nav-ui-toggle button::selection {
		background-color: transparent;
	}
	#site-nav-ui-toggle button::-moz-focus-inner,
	#post-nav-ui-toggle button::-moz-focus-inner {
		border: none;
	}
	
	#site-nav-ui-toggle {
		top: 0;
		left: 12px;
		pointer-events: none;
	}
	#site-nav-ui-toggle button {
		position: relative;
		left: 0;
		transition:
			left 0.2s ease,
			opacity 0.2s ease,
			width 0.2s ease;
		pointer-events: auto;
	}
	#site-nav-ui-toggle button:active {
		transform: none;
	}
	#site-nav-ui-toggle button.engaged {
		left: -92px;
		width: 1.125em;
		overflow: hidden;
	}
	#site-nav-ui-toggle button.engaged::before {
		content: "\F00D";
		padding: 0 0.25em 0 0;
	}
	
	#primary-bar,
	#secondary-bar,
	.page-toolbar {
		visibility: hidden;
		top: 0;
		max-height: 0px;
	}
	#primary-bar,
	#secondary-bar #nav-item-archive,
	#secondary-bar #nav-item-about,
	.page-toolbar {
		opacity: 0.0;
	}
	#primary-bar,
	#secondary-bar,
	#secondary-bar #nav-item-archive,
	#secondary-bar #nav-item-about,
	.page-toolbar {
		transition:
			top 0.2s ease,
			max-height 0.2s ease,
			visibility 0.2s ease,
			opacity 0.2s ease;
	}
	#nav-item-search,
	#nav-item-login {
		visibility: visible;
	}
	#primary-bar.engaged,
	#secondary-bar.engaged,
	.page-toolbar.engaged {
		visibility: visible;
		max-height: 1000px;
	}
	#primary-bar.engaged,
	#secondary-bar.engaged #nav-item-archive,
	#secondary-bar.engaged #nav-item-about,
	.page-toolbar.engaged {
		opacity: 1.0;
	}
	#primary-bar.engaged {
		top: 0;
	}
	#secondary-bar.engaged {
		top: 200px;
	}
	.page-toolbar.engaged {	
		top: 280px;
	}

	#post-nav-ui-toggle {
		bottom: 10px;
		right: -26px;
	}
	#content.post-page ~ #ui-elements-container #post-nav-ui-toggle {
		right: -50px;
	}
	#post-nav-ui-toggle button.engaged {
		transform: rotate(-90deg);
	}
	
	#quick-nav-ui,
	#new-comment-nav-ui,
	#hns-date-picker {
		bottom: 0;
		max-height: 0px;
		opacity: 0.0;
		transition:
			bottom 0.2s ease,
			max-height 0.2s ease,
			opacity 0.2s ease,
			visibility 0.2s ease;
	}
	#quick-nav-ui.engaged,
	#new-comment-nav-ui.engaged,
	#hns-date-picker.engaged {
		visibility: visible;
		max-height: 1000px;
		opacity: 1.0;
	}	
	
	#quick-nav-ui {
		right: -20px;
	}
	#content.post-page ~ #ui-elements-container #quick-nav-ui {
		right: -44px;
	}
	#quick-nav-ui.engaged {
		bottom: 64px;
	}
	
	#new-comment-nav-ui {
		right: -45px;
	}
	#new-comment-nav-ui.engaged {
		bottom: 216px;
	}

	#hns-date-picker {
		right: -182px;
	}
	#hns-date-picker.engaged {
		bottom: 247px;
	}
	@media only screen and (max-width: 1440px) {
		#hns-date-picker {
			background-color: rgba(255,255,255,0.95);
			right: -14px;
		}
		#hns-date-picker::before {
			display: none;
		}
		#hns-date-picker input {
			background-color: #fff;
		}
		#hns-date-picker span {
			text-shadow:
				0 0 1px #fff,
				0 0 3px #fff,
				0 0 5px #fff,
				0 0 8px #fff,
				0 0 13px #fff,
				0 0 21px #fff;
		}
		#hns-date-picker.engaged {
			bottom: 238px;
		}
	}
}

/*================*/
/* WIDTH SELECTOR */
/*================*/

#width-selector {
	opacity: 0.3;
}
#width-selector:hover {
	opacity: 1.0;
}
#width-selector button {
	box-shadow:
		0 0 0 4px #fff inset,
		0 0 0 5px #aaa inset;
}
#width-selector button:hover,
#width-selector button.selected {
	box-shadow:
		0 0 0 1px #fff inset,
		0 0 0 2px #aaa inset,
		0 0 0 4px #fff inset,
		0 0 0 5px #aaa inset;
}

/*================*/
/* THEME SELECTOR */
/*================*/

#theme-selector {
	opacity: 0.2;
}
#theme-selector button {
	box-shadow:
		0 0 0 4px #fff inset,
		0 0 0 5px #999 inset;
}
#theme-selector button:hover,
#theme-selector button.selected {
	box-shadow:
		0 0 0 1px #fff inset,
		0 0 0 2px #999 inset,
		0 0 0 4px #fff inset,
		0 0 0 5px #999 inset;
}

/*======================*/
/* THEME TWEAKER TOGGLE */
/*======================*/

#theme-tweaker-toggle button {
	opacity: 0.2;
	color: #777;
}

/*=================*/
/* QUICKNAV WIDGET */
/*=================*/

#quick-nav-ui a {
	color: #acd2af;
	border-radius: 4px;
	text-decoration: none;
}
#quick-nav-ui a:hover {
	color: #79a97e;
}
#quick-nav-ui a[href='#bottom-bar'] {
	line-height: 1.8;
}
#quick-nav-ui a:active {
	transform: scale(0.9);
}
#quick-nav-ui a[href='#comments'].no-comments {
	opacity: 0.4;
	color: #ddd;
}
@media only screen and (hover: hover), not screen and (-moz-touch-enabled) {
	#quick-nav-ui a:focus:not(:hover) {
		transform: none;
		text-shadow: none;
	}
}

/*======================*/
/* NEW COMMENT QUICKNAV */
/*======================*/

#new-comment-nav-ui .new-comments-count {
	color: #888;
	text-shadow: 0.5px 0.5px 0 #fff;
	top: 2px;
}
#new-comment-nav-ui .new-comments-count::after {
	color: #777;
}
#new-comment-nav-ui .new-comment-sequential-nav-button:disabled {
	color: #e6e6e6;
}

/*=================*/
/* HNS DATE PICKER */
/*=================*/

#hns-date-picker span {
	color: #999;
	font-weight: 400;
}
#hns-date-picker input {
	border: 1px solid #ddd;
	color: #999;
	padding: 3px 3px 0 3px;
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
	margin: 1.25em 0 0 0;
	padding: 0;
}
.archive-nav *[class^='archive-nav-item'] {
	color: <?php echo $hyperlink_color; ?>;
	border-style: solid;
	border-color: #c4dbc4;
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
.archive-nav a:hover,
.archive-nav span[class^='archive-nav-item'] {
	background-color: #c4dbc4;
	color: #fff;
}

.archive-nav a:active {
	background-color: <?php echo $hyperlink_color; ?>;
}

/*==========*/
/* LISTINGS */
/*==========*/

h1.listing {
	margin: 0.7em 20px 0.1em 0;
	font-family: <?php echo $headings_font; ?>, 'Font Awesome';
	font-size: 1.25rem;
	line-height: 1.2;
}

h1.listing a[href^="http"] {
	color: #bbb;
	font-size: 0.8125em;
	top: 3px;
}
h1.listing a[href^="/"] {
	font-weight: 300;
	text-shadow: <?php global $platform; echo ($platform == 'Mac' ? 'none' : '0 0 0 #444'); ?>;
	color: <?php global $platform; echo ($platform == 'Mac' ? '#444' : '#000'); ?>;
}
@-moz-document url-prefix() {
	h1.listing a[href^="/"] {
		text-shadow: none;
	}
}

@media only screen and (hover: hover), not screen and (-moz-touch-enabled) {
	h1.listing a:hover,
	h1.listing a:focus {
		color: #92c396;
		background-color: rgba(255,255,255,0.85);
	}	
	#content.user-page h1.listing:focus-within::before {
		left: -1em;
	}
	h1.listing:focus-within::before {
		color: #79a97e;
		left: 3.625em;
	}
	h1.listing a[href^="http"]:hover {
		color: #79a97e;
	}
}

/*===================*/
/* LISTING POST-META */
/*===================*/

h1.listing + .post-meta {
	font-size: 0.875rem;
	margin: 0 20px 0 1px;
}

h1.listing + .post-meta > * {
	color: #bbb;
	margin: 0 1.25em 0 0;
}
h1.listing + .post-meta a {
	color: #92c396;
}
h1.listing + .post-meta a:hover {
	color: #79a97e;
}
h1.listing + .post-meta .karma-value {
	cursor: default;
}
h1.listing + .post-meta .lw2-link {
	display: none;
}
h1.listing + .post-meta .post-section {
	overflow: visible;
	order: 1;
}
h1.listing + .post-meta .post-section::before {
	position: relative;
	left: unset;
	top: -1px;
}

/*============*/
/* USER PAGES */
/*============*/

#content.user-page h1.page-main-heading,
#content.user-page .user-stats {
	grid-row: 1;
}
#content.user-page #comments-list-mode-selector,
#content.user-page .sublevel-nav.sort {
	grid-row: 2 / span 2;
}
#content.user-page .sublevel-nav {
	grid-row: 2;
}
#content.user-page #top-nav-bar {
	grid-row: 3;
}

#content.user-page h1.page-main-heading,
#content.conversation-page h1.page-main-heading {
	font-family: <?php echo $headings_font; ?>;
	font-weight: normal;
	margin: 0.5em 0 0 0;
}
#content.user-page h1.page-main-heading {
	border-bottom: 1px solid #e6e6e6;
	line-height: 1;
}

#content.user-page h1.listing,
#content.user-page h1.listing + .post-meta {
	border-color: #ddd;
	border-style: solid;
}
#content.user-page h1.listing {
	padding: 6px 8px 0 8px;
	position: relative;
	border-width: 1px 1px 0 1px;
}
#content.user-page h1.listing + .post-meta {
	margin: 0;
	padding: 12px 8px 3px 8px;
	border-width: 0 1px 1px 1px;
	line-height: 1;
}

.user-stats .karma-total {
	font-weight: 500;
}

/*===============*/
/* CONVERSATIONS */
/*===============*/

/*============*/
/* LOGIN PAGE */
/*============*/

.login-container form h1 {
	font-family: <?php echo $headings_font; ?>;
	font-weight: 300;
}

.login-container form label {
	color: #aaa;
}

/* “Create account” form */

#signup-form {
	border: 1px solid #e4e4e4;
}
#signup-form input[type='submit'] {
	padding: 8px 12px 6px 12px;
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
	border: 1px solid #c4dbc4;
}
.post-body .contents ul {
	font-size: 0.8125em;
}
.post-body .contents li::before {
	color: #bbb;
}

/*==================*/
/* POSTS & COMMENTS */
/*==================*/

.post-body,
.comment-body {
	font-family: <?php echo $text_font; ?>;
	text-shadow: <?php global $platform; echo ($platform == 'Mac' ? '0 0 0 #000' : 'none'); ?>;
	font-weight: <?php global $platform; echo ($platform == 'Mac' ? '300' : '400'); ?>;
}

/*=======*/
/* POSTS */
/*=======*/

.post {
	overflow: auto;
	padding: 2em 0 0 0;
}

.post-body {
	font-size: 1.25rem;
}

.post > h1:first-child {
	font-size: 2rem;
	font-family: <?php echo $headings_font; ?>;
	font-weight: 300;
	line-height: 1.1;
	margin: 1em 0 0.25em 0;
}

/*===========*/
/* POST-META */
/*===========*/

.post-meta .post-section::before {
	color: #dfdfdf;
}
.post .post-meta .post-section::before {
	position: relative;
	top: -3px;
}
.post-meta > * {
	color: #bbb;
}
.post-meta a,
.post-meta a:visited {
	color: #92c396;
}
.post-meta a:hover {
	color: #79a97e;
}
.post-meta .lw2-link:hover {
	opacity: 1;
}

.post .bottom-post-meta {
	padding: unset;
	margin: 0.5em 0 1.5em 0;
}

/*============*/
/* LINK POSTS */
/*============*/

.post.link-post a.link-post-link {
	font-family: <?php echo $UI_font; ?>;
}
.post.link-post a.link-post-link::before {
	opacity: 0.6;
}
.post.link-post a.link-post-link:hover::before {
	opacity: 1;
}

/*==========*/
/* COMMENTS */
/*==========*/

#comments {
	border-top: 1px solid transparent;
}
.comment-item {
	border: 1px solid #ddd;
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
	background-color: #f6f6f6;
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
		0 28px 16px -16px #f6f6f6 inset,
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

.comment-body {
	font-size: 1.1875rem;
}
#content.user-page .comment-body,
#content.index-page .comment-body {
	font-size: 1.125rem;
}

/*================================*/
/* DEEP COMMENT THREAD COLLAPSING */
/*================================*/

.comment-item input[id^="expand"] + label::after {
	color: <?php echo $hyperlink_color; ?>;
	font-weight: 400;
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
	padding-top: 4px;
}
.comment-meta > * {
	color: #bbb;
}
.comment-meta a,
.comment-meta a:visited {
	color: #92c396;
}
.comment-meta a:hover {
	color: #79a97e;
}
.comment-meta .author {
	font-size: 1.125em;
	font-weight: normal;
}

.comment-controls .karma {
	color: #bbb;
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

/*====================*/
/* COMMENT PERMALINKS */
/*====================*/

.comment-meta .permalink,
.comment-meta .lw2-link,
.individual-thread-page .comment-parent-link:empty {
	filter: hue-rotate(270deg);
	opacity: 0.4;
}
.comment-meta .permalink:hover,
.comment-meta .lw2-link:hover,
.individual-thread-page .comment-parent-link:empty:hover {
	opacity: 1.0;
}

/*=======================*/
/* COMMENTS COMPACT VIEW */
/*=======================*/

#comments-list-mode-selector {
	display: inline-block;
	opacity: 0.4;
	transition: opacity 0.15s ease;
}
#content.index-page #comments-list-mode-selector {
	grid-column: 3;
	justify-self: end;
}
#comments-list-mode-selector:hover {
	opacity: 1.0;
}

#comments-list-mode-selector button {
	border: none;
	background-color: transparent;
	box-shadow:
		0 0 0 4px #fff inset,
		0 0 0 5px #aaa inset;
}
#comments-list-mode-selector button:hover,
#comments-list-mode-selector button.selected {
	box-shadow:
		0 0 0 1px #fff inset,
		0 0 0 2px #aaa inset,
		0 0 0 4px #fff inset,
		0 0 0 5px #aaa inset;
}

#content.compact > .comment-thread .comment-item {
	max-height: 58px;
}
#content.compact > .comment-thread .comment-item::after {
	color: <?php echo $hyperlink_color; ?>;
	background: linear-gradient(to right, transparent 0%, #fff 50%, #fff 100%);
}
#content.compact > .comment-thread .comment-item:hover .comment {
	background-color: #fff;
	outline: 1px solid #92c396;
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
	outline: 1px solid #5a5;
	box-shadow:
		0 0 6px -2px #5a5 inset, 
		0 0 4px #5a5;
}

/*=================================*/
/* COMMENT THREAD MINIMIZE BUTTONS */
/*=================================*/

.comment-minimize-button {
	color: #ddd;
}
.comment-minimize-button:hover {
	color: #bbb;
	text-shadow: <?php echo $white_glow; ?>;
}
.comment-minimize-button::after {
	font-family: <?php echo $UI_font; ?>;
	color: #999;
}
.comment-minimize-button.maximized::after {
	color: #ccc;
}

/*==============*/
/* VOTE BUTTONS */
/*==============*/

.upvote,
.downvote {
	color: #ddd;	
}
.upvote::before {
	content: "\F077";
}
.downvote::before {
	content: "\F078";
	position: relative;
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

.posting-controls input[type='submit'] {
	padding: 6px 12px 3px 12px;
}

.comment-controls {
	margin: 0 4px 4px 16px;
}
.comment + .comment-controls .action-button {
	font-weight: 300;
}

.new-comment-button {
	margin: 0;
	padding: 0.125em;
}

.comment-controls .cancel-comment-button {
	color: #c00;
	text-shadow: 
		0 0 1px #fff,
		0 0 2px #fff;
}
.comment-controls .cancel-comment-button:hover {
	color: #f00;
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
	font-weight: 300;
	color: #000;
	text-shadow: 0 0 0 #000;
	border-color: #eee;
	transition: border-color 0.15s ease;
}
.posting-controls textarea:focus {
	border-width: 29px 1px 1px 1px;
	border-color: #92c396;
}
.posting-controls.edit-existing-post textarea:focus,
.posting-controls form.edit-existing-comment textarea:focus {
	border-color: #090;
	box-shadow: 0 0 0 1px #090;
}

/* GUIEdit buttons */

.guiedit-buttons-container {
	background-color: #fff;
}

.posting-controls.edit-existing-post .guiedit-buttons-container button,
.posting-controls form.edit-existing-comment .guiedit-buttons-container button {
    color: #050;
}
.guiedit-buttons-container button {
	font-family: Font Awesome, <?php echo $text_font; ?>;
	border: 1px solid transparent;
}

.guiedit::after {
	font-family: <?php echo $UI_font; ?>;
	color: #999;
	font-weight: 300;
	text-shadow: 0 0 0 #999;
	top: 3px;
}

.posting-controls .markdown-reference-link a {
	background-position: right 70%;
}
.markdown-reference-link {
	color: #999;
}

/* Markdown hints */

#markdown-hints-checkbox + label {
	color: <?php echo $hyperlink_color; ?>;
}
#markdown-hints-checkbox + label:hover {
	color: #79a97e;
}
.markdown-hints {
	border: 1px solid #c00;
	background-color: #ffa;
}

/*================*/
/* EDIT POST FORM */
/*================*/

#edit-post-form .link-post-checkbox + label {
	top: 2px;
	color: #acd2af;
	transition: color 0.15s ease;
}
#edit-post-form .link-post-checkbox + label:hover {
	color: #79a97e;
}
#edit-post-form .link-post-checkbox + label::before {
	top: 2px;
	border: 1px solid #eee;
	color: #bbb;
	transition: 
		box-shadow 0.3s ease,
		border-color 0.15s ease;
}
#edit-post-form .link-post-checkbox + label:hover::before {
	border-color: #c4dbc4;
}
#edit-post-form .link-post-checkbox:checked + label {
	font-weight: normal;
}
#edit-post-form .link-post-checkbox:checked + label::before {
	border-color: #c4dbc4;
	box-shadow: 
		0 0 0 4px #fff inset,
		0 0 0 1em #c4dbc4 inset;
}

#edit-post-form label[for='url'],
#edit-post-form input[name='url'] {
	display: block;
	transition:
		max-height 0.15s ease,
		overflow 0.15s ease,
		margin-top 0.15s ease,
		margin-bottom 0.15s ease,
		padding 0.15s ease,
		border-color 0.15s ease;
		
}
#edit-post-form .link-post-checkbox:not(:checked) ~ label[for='url'],
#edit-post-form .link-post-checkbox:not(:checked) ~ input[name='url'] {
	max-height: 0;
	overflow: hidden;
	margin-top: 0;
	margin-bottom: 0;
	padding: 0;
	border-color: transparent;
}

#edit-post-form label[for='title'],
#edit-post-form label[for='url'],
#edit-post-form label[for='section'] {
	color: #aaa;
	text-shadow: 0 0 0 #aaa;
}

#edit-post-form input[type='radio'] + label {
	color: #92c396;
	border-color: #c4dbc4;
	padding: 6px 12px 3px 12px;
	position: relative;
	top: -2px;
	transition:
		background-color 0.15s ease,
		color 0.15s ease,
		border-color 0.15s ease;
}
#edit-post-form input[type='radio'][value='all'] + label {
	border-radius: 8px 0 0 8px;
	border-width: 1px;
}
#edit-post-form input[type='radio'][value='drafts'] + label {
	border-radius: 0 8px 8px 0;
	padding-right: 13px;
}
#edit-post-form input[type='radio'] + label:hover,
#edit-post-form input[type='radio']:focus + label,
#edit-post-form input[type='radio']:checked + label {
	background-color: #c4dbc4;
	color: #fff;
}
#edit-post-form input[type='radio']:active + label {
	border-color: #92c396;
	background-color: #92c396;
}

#edit-post-form input[type='submit'] {
	padding: 7px 14px 4px 14px;
}

/*=======*/
/* LINKS */
/*=======*/

a {
	text-decoration: none;
	color: <?php echo $hyperlink_color; ?>;
	transition: color 0.15s ease;
}
a:visited {
	color: #bebb84;
}
a:hover {
	color: #bbb;
}

/*=========*/
/* BUTTONS */
/*=========*/

button,
input[type='submit'] {
	color: #92c396;
}
input[type='submit'] {
	color: #92c396;
	background-color: #fff;
	border: 1px solid #c4dbc4;
	transition:
		color 0.15s ease,
		background-color 0.15s ease,
		border-color 0.15s ease;
}

input[type='submit']:hover,
input[type='submit']:focus {
	background-color: #c4dbc4;
	color: #fff;
}
input[type='submit']:active {
	background-color: #92c396;
	border-color: #92c396;
}
.button:visited {
	color: #92c396;
}
button:hover,
.button:hover {
	color: #79a97e;
	text-decoration: none;
}
button:active,
.button:active {
	transform: scale(0.9);
}
button:focus:not(:hover),
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
	font-family: <?php echo $headings_font; ?>;
	font-weight: 300;
}
.post-body h1,
.comment-body h1 {
	border-bottom: 1px solid #eee;
	line-height: 0.7;
	margin-top: 1.25em;
}
.post-body h6,
.comment-body h6 {
	color: #555;
}

/*========*/
/* QUOTES */
/*========*/

blockquote {
	border-left: 5px solid #e6e6e6;
}

/*========*/
/* IMAGES */
/*========*/

.post-body img,
.comment-body img {
	border: 1px solid #ccc;
}
.post-body img[src$='.svg'],
.comment-body img[src$='.svg'] {
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
	border: 1px solid #999;
	color: #000;
	background-color: transparent;
	border-color: transparent;
	border-bottom-color: #eee;
	transition: border-color 0.15s ease;
}
input[type='text']:focus,
input[type='search']:focus,
input[type='password']:focus {
	border-bottom-color: #c4dbc4;
}

select {
	color: #000;
}

strong, b {
	font-weight: 600;
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
	text-shadow: 0 0 1px #fff, 0 0 3px #fff, 0 0 5px #fff;
}

.qualified-linking label {
	color: #ccc;
}
.qualified-linking label:hover {
	color: #92c396;
}

.qualified-linking-toolbar {
	border: 1px solid #ccc;
	background-color: #fff;
}
.qualified-linking-toolbar a {
	padding: 3px 6px 0 6px;
}
.qualified-linking-toolbar a,
.qualified-linking-toolbar a:visited {
	color: #acd2af;
}
.qualified-linking-toolbar a:hover {
	color: #92c396;
	text-decoration: none;
	background-color: #e4f1e5;
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
		opacity: 1.0;
	}
	#hns-date-picker::before {
		border: 1px solid #999;
		border-width: 1px 0 1px 1px;
	}
}
@media only screen and (max-width: 1200px) {
	#hns-date-picker::before {
		display: none;
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

@media only screen and (hover: none), only screen and (-moz-touch-enabled) {
	#appearance-adjust-ui-toggle button,
	#post-nav-ui-toggle button,
	#theme-selector .theme-selector-close-button  {
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
	#theme-selector::before {
		color: #777;
		font-weight: normal;
		text-shadow: 0.5px 0.5px 0 #aaa;
	}
	#theme-selector button {
		background-color: #e6e6e6;
		border-radius: 10px;
	}
	#theme-selector button::after {
		color: #000;
		max-width: calc(100% - 3.5em);
		overflow: hidden;
		text-overflow: ellipsis;
		padding: 0 0 2px 0;
	}
	#theme-selector button.selected::after {
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
		background-color: #fff;
		border: 1px solid #999;
	}
	#new-comment-nav-ui::before {
		color: #777;
		font-weight: bold;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button {
		box-shadow: 0 0 0 1px #999;
		color: #00c;
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
	#new-comment-nav-ui .new-comment-sequential-nav-button.new-comment-next {
		border-radius: 0 7px 7px 0;
	}
	#new-comment-nav-ui button::after {
		font-family: <?php echo $UI_font; ?>;
	}
	#hns-date-picker {
		background-color: #fff;
		border: 1px solid #999;
	}

	/*****************************************/
	@media only screen and (max-width: 900px) {
	/*****************************************/
		#primary-bar,
		#secondary-bar,
		.page-toolbar {
			display: none !important;
		}
		#content {
			padding: 0 4px;
		}

		h1.listing + .post-meta > * {
			line-height: 1.5;
		}
		h1.listing + .post-meta .post-section {
			overflow: visible;
			order: 1;
		}
		h1.listing + .post-meta .post-section::before {
			position: unset;
		}

		#primary-bar .nav-inner {
			font-size: 1.375em;
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
	/*******************************************/
	} @media only screen and (max-width: 720px) {
	/*******************************************/
	/*******************************************/
	} @media only screen and (max-width: 520px) {
	/*******************************************/
		h1.listing {
			font-size: 1.25rem;
			margin: 18px 6px 4px 6px;
		}
		h1.listing + .post-meta {
			margin: 4px 6px;
		}
		h1.listing a[href^='http'] {
			top: 2px;
		}
		#content.conversations-user-page h1.listing::after {
			height: calc(100% + 2.25em);
		}
		#content.conversations-user-page h1.listing + .post-meta .date {
			margin: 0 0 0 1em;
		}
		
		.comment-body {
			font-size: 1.125rem;
		}
		
		#content.compact > .comment-thread .comment-item {
			max-height: 105px;
		}
		
		.comment-controls form:focus-within textarea,
		.textarea-container:focus-within textarea {
			background-color: #fff;
		}
		.comment-controls form:focus-within .guiedit-mobile-auxiliary-button,
		.textarea-container:focus-within .guiedit-mobile-auxiliary-button {
			padding: 5px 6px 6px 6px;
			font-weight: bold;
		}
		.comment-controls form:focus-within .guiedit-mobile-help-button.active,
		.textarea-container:focus-within .guiedit-mobile-help-button.active {
			box-shadow:
				0 0 0 2px #c00;
			color: #c00;
		}
		.comment-controls form:focus-within .guiedit-buttons-container,
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
		.comment-controls form:focus-within button.guiedit,
		.textarea-container:focus-within button.guiedit {
			border: 1px solid #00c;
			border-radius: 6px;
		}
		.markdown-hints::after {
			color: #090;
		}
	}
}

