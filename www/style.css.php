<?php
	header ('Content-type: text/css; charset=utf-8');
	
	$platform = @$argv[1] ?: 'Mac';
	$UI_font = ($platform == 'Windows') ? "'Whitney', 'a_Avante'" : "'Concourse', 'a_Avante'";
?>

<?php echo file_get_contents('fa-custom.css'); ?>

/***************/
/* BASE LAYOUT */
/***************/

html {
	box-sizing: border-box;
	font-size: 16px;
}
*, *::before, *::after {
	box-sizing: inherit;
}
:focus {
	outline: none;
}
body {
	padding: 0;
	margin: 0;
}
body::before {
	background-color: inherit;
	position: fixed;
	width: 100%;
	height: 100%;
}
input {
	font-family: inherit;
	font-size: inherit;
	font-weight: inherit;
}
#content,
#ui-elements-container,
#images-overlay {
	width: calc(100% - 300px);
	min-width: 900px;
	max-width: 900px;
}
#content {
	margin: 0 auto;
	padding: 0 30px;
	overflow: auto;
	position: relative;
}
#ui-elements-container {
	position: fixed;
	height: 100vh;
	top: 0;
	left: 0;
	right: 0;
	margin: auto;
	z-index: 10000;
	pointer-events: none;
}
#ui-elements-container > * {
	pointer-events: auto;
}
#images-overlay {
	position: absolute;
	z-index: 1;
	left: 0;
	right: 0;
	margin: auto;
}

/***********/
/* NAV BAR */
/***********/

.nav-bar {
	margin: 0 -30px;
}
.nav-bar {
	display: flex;
	justify-content: flex-end;
}
.nav-item {
	flex: 1 1 auto;
}
.nav-item * {
	text-overflow: ellipsis;
	white-space: nowrap;
	overflow: hidden;
}
.nav-inner {
	padding: 12px 30px;
	text-align: center;
	display: block;
	position: relative;
}
#primary-bar.inactive-bar .nav-inner {
	padding-top: 11px;
	padding-bottom: 13px;
}
.nav-inner::after {
	content: attr(accesskey);
	display: none;
}
#secondary-bar .nav-inner {
	padding: 4px 0;
}
#bottom-bar .nav-item {
	flex: 1 1 0;
}

#bottom-bar .nav-item a::before,
#top-nav-bar a::before {
	font-family: Font Awesome;
	font-weight: 900;
	font-size: 0.8em;
	position: relative;
	bottom: 1px;
	margin-right: 0.5em;
}
#bottom-bar #nav-item-first a::before,
#top-nav-bar a.nav-item-first::before {
	content: "\F100";
}
#bottom-bar #nav-item-top a::before {
	content: "\F062";
}
#bottom-bar #nav-item-prev a::before,
#top-nav-bar a.nav-item-prev::before {
	content: "\F060";
}
#bottom-bar #nav-item-next a::before,
#top-nav-bar a.nav-item-next::before {
	content: "\F061";
}
#bottom-bar #nav-item-next a::before {
	margin-left: -2em;
	margin-right: 0;
	left: 3.8em;
}

/* Search tab */

#nav-item-search {
	flex: 4 1 auto;
}
#nav-item-search form::before {
	content: "\F002";
	font-family: Font Awesome;
	font-weight: 900;
	display: inline-block;
	vertical-align: top;
	height: 23px;
	width: 23px;
}
#nav-item-search input {
	height: 23px;
	width: calc(95% - 80px);
	padding: 1px 4px;
}
#nav-item-search button {
	height: 21px;
}

/* Login tab */

#nav-item-login {
	position: relative;
	padding-right: 0.5em;
}

/*******************/
/* INBOX INDICATOR */
/*******************/

#inbox-indicator {
	position: absolute;
	top: 1px;
	right: 0;
	height: 100%;
	visibility: hidden;
}
#inbox-indicator::before {
	content: "\F0E0";
	font-family: "Font Awesome";
	color: #bbb;
	font-size: 1.1875rem;
	position: absolute;
	height: 100%;
	right: 0;
	top: 0;
	padding: 0 0.45em;
	visibility: visible;
/* 	z-index: 1000; */
	font-weight: 900;
}
#inbox-indicator.new-messages::before {
	color: #f00;
	text-shadow: 
		0 0 1px #777,
		0.5px 0.5px 1px #777;
}
a#inbox-indicator:hover::before {
	color: #fff;
	text-shadow: 
		0 0 1px #000,
		0 0 2px #000,
		0 0 4px #000,
		0 0 1px #777,
		0.5px 0.5px 1px #777;
}
a#inbox-indicator.new-messages:hover::before {
	text-shadow: 
		0 0 1px #f00,
		0 0 2px #f00,
		0 0 4px #f00,
		0 0 1px #777,
		0.5px 0.5px 1px #777;
}

/****************/
/* PAGE TOOLBAR */
/****************/

.page-toolbar {
	position: absolute;
	font-size: 0.9em;
	right: 0.4em;
	line-height: 1.8;
}
.page-toolbar > * {
	display: inline-block;
	margin-left: 1.5em;
}
.page-toolbar .button::before {
	font-family: "Font Awesome";
	font-size: 0.9em;
	padding-right: 0.3em;
}
.new-post::before {
	content: '\F067';
	font-weight: 900;
}
.new-private-message::before {
	content: '\F075';
	font-weight: 400;
}
.logout-button::before {
	content: '\F2F5';
	font-weight: 900;
}
.rss::before {
	content: url('data:image/svg+xml;base64,<?php echo base64_encode(file_get_contents("assets/rss.svg")) ?>');
	display: inline-block;
	width: 1em;
	padding-right: 0.2em;
	position: relative;
	top: 1px;
}

/*********************/
/* TOP PAGINATION UI */
/*********************/

#top-nav-bar {
	margin: 0.25em 0 0 0;
	padding: 0.75em 0 0 0;
	text-align: center;
	font-size: 1.25em;
	display: flex;
	justify-content: center;
}
.sublevel-nav + #top-nav-bar {
	margin-top: 0.25em;
}
.sublevel-nav + #top-nav-bar + h1.listing {
	margin-top: 1.5em;
}
.archive-nav + #top-nav-bar {
	margin-top: 1em;
}
.archive-nav + #top-nav-bar + h1.listing {
	margin-top: 1em;
}
#top-nav-bar a {
	line-height: 1;
}
#top-nav-bar a.disabled {
	pointer-events: none;
	visibility: hidden;
}
#top-nav-bar a.nav-item-last::before {
	content: "\2002";
}
#top-nav-bar .page-number {
	position: relative;
	display: inline-block;
	width: 1.5em;
}
#top-nav-bar .page-number-label {
	position: absolute;
	font-size: 0.5em;
	text-transform: uppercase;
	width: 100%;
	bottom: 90%;
	left: 0;
}
#top-nav-bar a::before {
	margin: 0.5em;
	display: inline-block;
}

/****************/
/* SUBLEVEL NAV */
/****************/

.sublevel-nav {
	text-align: center;
	display: flex;
	justify-content: center;
}
.sublevel-nav .sublevel-item {
	flex: 0 0 6em;
	padding: 0.125em 0.5em;
	font-size: 1.125rem;
}
.sublevel-nav span.sublevel-item {
	cursor: default;
}

/***********************/
/* SORT ORDER SELECTOR */
/***********************/

.sublevel-nav.sort {
	position: absolute;
	top: 167px;
	right: 30px;
	font-size: 0.75em;
	flex-flow: column;
}
.sublevel-nav.sort::before {
	content: "Sort";
	position: absolute;
	top: 0;
	left: 0;
	width: 100%;
}
.sublevel-nav.sort .sublevel-item {
	line-height: 1;
	font-size: 0.875rem;
	flex-basis: unset;
}

/******************/
/* WIDTH SELECTOR */
/******************/

#width-selector {
	position: absolute;
	top: 4px;
	right: -78px;
}
#width-selector button {
	width: 22px;
	height: 22px;
	padding: 6px;
	margin: 1px;
	overflow: hidden;
	background-repeat: no-repeat;
	background-size: 100%;
	background-origin: content-box;
}
#width-selector button,
#width-selector button:active,
#width-selector button:focus {
	text-shadow: none;
	color: transparent;
}	
#width-selector button:disabled {
	cursor: auto;
}
#width-selector button.select-width-normal {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/normal.gif")) ?>');
}
#width-selector button.select-width-wide {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/wide.gif")) ?>');
}
#width-selector button.select-width-fluid {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/fluid.gif")) ?>');
}

/******************/
/* THEME SELECTOR */
/******************/

#theme-selector {
	position: absolute;
	top: 3px;
	left: -41px;
	opacity: 0.4;
	display: table;
	max-width: 40px;
}
#theme-selector:hover {
	opacity: 1.0;
}

.theme-selector button {
	display: table-cell;
	width: 26px;
	height: 26px;
	padding: 5px;
	margin: 1px 7px 0 7px;
	color: transparent;
	background-size: 16px 16px;
	background-origin: content-box;
}
.theme-selector button,
.theme-selector button:hover,
.theme-selector button:active,
.theme-selector button:focus {
	text-shadow: none;
	color: transparent;
}	
.theme-selector button:nth-of-type(1) {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/theme_A.gif")) ?>');
}
.theme-selector button:nth-of-type(2) {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/theme_B.gif")) ?>');
}
.theme-selector button:nth-of-type(3) {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/theme_C.gif")) ?>');
}
.theme-selector button:nth-of-type(4) {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/theme_D.gif")) ?>');
}
.theme-selector button:nth-of-type(5) {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/theme_E.gif")) ?>');
}
.theme-selector button:nth-of-type(6) {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/theme_F.gif")) ?>');
}
.theme-selector button:nth-of-type(7) {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/theme_G.gif")) ?>');
}
.theme-selector button:nth-of-type(8) {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/theme_H.gif")) ?>');
}
.theme-selector button:disabled {
	cursor: auto;
}

/************************/
/* THEME TWEAKER TOGGLE */
/************************/

#theme-tweaker-toggle {
	position: absolute;
	top: 7px;
	left: -75px;
}
#theme-tweaker-toggle button {
	font-family: Font Awesome;
	font-weight: 900;
	font-size: 1.25rem;
	opacity: 0.4;
	z-index: 1;
}
#theme-tweaker-toggle button:hover {
	opacity: 1.0;
}

/*******************/
/* QUICKNAV WIDGET */
/*******************/

#quick-nav-ui {
	position: absolute;
	right: -67px;
	bottom: 20px;
}
#quick-nav-ui a {
	font-family: 'Font Awesome';
	font-weight: 900;
	font-size: 1.5rem;
	line-height: 1.7;
	text-align: center;
	display: block;
	width: 40px;
	height: 40px;
	margin: 10px 0 0 0;
}
#quick-nav-ui a[href='#comments'].no-comments {
	pointer-events: none;
}

/************************/
/* NEW COMMENT QUICKNAV */
/************************/

#new-comment-nav-ui {
	position: absolute;
	right: -112px;
	bottom: 42px;
}
#new-comment-nav-ui > * {
	display: block;
	position: relative;
}
#new-comment-nav-ui .new-comments-count {
	width: 2em;
	font-size: 1.25rem;
	line-height: 1.1;
	text-align: center;
	left: 1px;
	cursor: pointer;
}
#new-comment-nav-ui .new-comments-count::selection {
	background-color: transparent;
}
#new-comment-nav-ui .new-comments-count::after {
	content: "NEW";
	display: block;
	font-size: 0.625rem;
}
#new-comment-nav-ui .new-comment-sequential-nav-button {
	font-size: 1.75rem;
	font-family: 'Font Awesome';
	font-weight: 900;
	width: 1.5em;
	z-index: 5001;
}
#new-comment-nav-ui .new-comment-previous {
	top: 8px;
}
#new-comment-nav-ui .new-comment-next {
	bottom: 6px;
}
#new-comment-nav-ui .new-comment-sequential-nav-button:disabled {
	cursor: auto;
}

/*******************/
/* HNS DATE PICKER */
/*******************/

#hns-date-picker {
	position: absolute;
	bottom: 72px;
	right: -253px;
	opacity: 0.6;
}
#hns-date-picker:hover, 
#hns-date-picker:focus-within {
	opacity: 1.0;
}
#hns-date-picker span {
	display: block;
	font-size: 0.75rem;
	text-transform: uppercase;
}
#hns-date-picker input {
	margin-top: 1px;
	padding: 1px 3px;
	width: 140px;
	text-align: center;
}

/************************/
/* TEXT SIZE ADJUSTMENT */
/************************/

#text-size-adjustment-ui {
	position: absolute;
	top: 30px;
	right: -78px;
	opacity: 0.4;
}
#text-size-adjustment-ui:hover {
	opacity: 1.0;
}
#text-size-adjustment-ui button {
	font-weight: 900;
	font-family: Font Awesome;
	font-size: 0.75rem;
	width: 24px;
	height: 24px;
	padding: 0;
}
#text-size-adjustment-ui button.default {
	font-family: inherit;
	font-size: 1.125rem;
	position: relative;
	top: 1px;
}
#text-size-adjustment-ui button:disabled {
	opacity: 0.5;
}
#text-size-adjustment-ui button:disabled:hover {
	cursor: default;
}
/* This doesn't work in Mozilla browsers, so hide it */
@-moz-document url-prefix() {
	#text-size-adjustment-ui {
		display: none;
	}
}

/*******************************/
/* COMMENTS VIEW MODE SELECTOR */
/*******************************/

#comments-view-mode-selector {
	position: absolute;
	bottom: 30px;
	left: -40px;
	opacity: 0.6;
}
#comments-view-mode-selector:hover {
	opacity: 1.0;
}
#comments-view-mode-selector a {
	display: block;
	font-family: Font Awesome;
	font-size: 1.25rem;
	text-align: center;
	opacity: 0.4;
	padding: 0.25em;
	z-index: 1;
}
#comments-view-mode-selector a.threaded {
	transform: scaleY(-1);
	font-weight: 900;
}
#comments-view-mode-selector a.chrono {
	font-weight: normal;
}
#comments-view-mode-selector a.selected,
#comments-view-mode-selector a:hover {
	opacity: 1.0;
	text-decoration: none;
}
#comments-view-mode-selector a.selected {
	cursor: default;
}

/************/
/* ARCHIVES */
/************/

.archive-nav {
	margin: 1.25em 0.5em -1.25em;
	padding: 0.25em;
}
.archive-nav > * {
	display: flex;
}
.archive-nav *[class^='archive-nav-item'] {
	line-height: 1;
	flex: 1 1 5%;
	text-align: center;
	padding: 6px 4px 4px 4px;
	max-width: 8%;
}
@-moz-document url-prefix() {
	.archive-nav *[class^='archive-nav-item'] {
		padding: 5px 4px;
	}
}
.archive-nav-days .archive-nav-item-day {
	font-size: 0.8em;
	padding: 7px 0 5px 0;
	max-width: 4%;
}
.archive-nav-days .archive-nav-item-day:first-child {
	flex-basis: 10%;
}

/************/
/* LISTINGS */
/************/

h1.listing {
	font-size: 1.875rem;
	line-height: 1.15;
	max-height: 1.15em;
}

h1.listing a {
	position: relative;
}
h1.listing a:nth-of-type(2) {
	margin-left: 0.25em;
}
h1.listing a[href^="http"] {
	font-size: 0.8em;
	display: inline;
	vertical-align: top;
	position: relative;
	top: 4px;
}
@media only screen and (hover: hover), not screen and (-moz-touch-enabled) {
	h1.listing a {
		max-width: 100%;
		display: inline-block;
		white-space: nowrap;
		text-overflow: ellipsis;
		overflow: hidden;
		border-bottom: 1px solid transparent;
		-webkit-hyphens: auto;
		-moz-hyphens: auto;
		-ms-hyphens: auto;
		hyphens: auto;
		z-index: 1;
		padding: 0 0 1px 1px;
	}
	h1.listing a:nth-of-type(2) {
		max-width: calc(100% - 33px);
	}
	h1.listing a:hover,
	h1.listing a:focus {
		text-decoration: dotted underline;
		white-space: initial;
		z-index: 2;
	}	
	h1.listing:focus-within::before {
		content: ">";
		display: block;
		position: absolute;
		left: 1em;
	}

	<?php $margin_of_hover_error = '10px'; ?>
	h1.listing a:hover::before {
		content: "";
		position: absolute;
		top: -<?php echo $margin_of_hover_error; ?>;
		right: -<?php echo $margin_of_hover_error; ?>;
		bottom: -<?php echo $margin_of_hover_error; ?>;
		left: -<?php echo $margin_of_hover_error; ?>;
		z-index: -1;
	}
	h1.listing a[href^="http"]:hover {
		text-decoration: none;
	}
}

.listing-message {
	width: 100%;
	text-align: center;
	padding: 1.25em 0 1.25em 0;
	font-size: 1.375em;
}
.archive-nav + .listing-message {
	padding: 1.75em 0 1.25em 0;
}

/*********************/
/* LISTING POST-META */
/*********************/

h1.listing + .post-meta {
	position: relative;
	justify-content: flex-start;
	margin: 0 20px 0 21px;
}
h1.listing:last-of-type + .post-meta {
	margin-bottom: 1.25em;
}

h1.listing + .post-meta > * {
	margin: 0 1em 0 0;
}
h1.listing + .post-meta .post-section {
	width: 0;
	margin: 0;
	overflow: hidden;
}
h1.listing + .post-meta .post-section::before {
	position: absolute;
	left: -36px;
}

h1.listing + .post-meta .read-time {
	cursor: default;
}

/**************/
/* USER PAGES */
/**************/

#content.user-page h1.page-main-heading {
	margin: 1em 0 0.75em 0;
	line-height: 1.2;
}
#content.user-page .user-stats {
	float: right;
	margin-top: -3.1em;
}

/*****************/
/* CONVERSATIONS */
/*****************/

#content.conversation-page .conversation-participants {
	margin: 3em 0 0;
}

.conversation-participants ul,
.conversation-participants li {
	list-style-type: none;
	display: inline-block;
	margin: 0;
	padding: 0;
}
.conversation-participants li {
	margin-left: 1em;
}
#content.conversation-page .posting-controls {
	margin: 0.5em 0 4em;
	padding-bottom: 1em;
}
#content.conversation-page .post-meta-fields {
	overflow: auto;
}
#content.conversation-page textarea {
	border-top-width: 1px;
	margin-top: 0.25em;
}
#content.conversation-page h1.page-main-heading {
	text-align: center;
	margin: 0.5em 0 0 0;
}
#conversation-form input[type='text'],
#conversation-form label {
	margin: 0.25em 0;
}
#conversation-form label {
	width: 6em;
}
#conversation-form input[type='text'] {
	width: calc(100% - 6em);
}
#conversation-form input[type='submit'] {
	float: right;
}

/******************/
/* SEARCH RESULTS */
/******************/

#content.search-results-page h1.listing {
	font-size: 1.625em;
}
#content.search-results-page #secondary-bar + * {
	margin-top: 3.5rem;
}

/**************/
/* LOGIN PAGE */
/**************/

.login-container {
	margin: 3em 0 4em;
	display: flex;
	flex-flow: row wrap;
}

.login-container form {
	width: 50%;
	display: flex;
	flex-flow: row wrap;
	align-items: baseline;
	align-content: flex-start;
}
.login-container form label {
	text-align: right;
	padding: 0.25em 0.5em;
	white-space: nowrap;
	margin: 0 0 0.25em 0;
}
.login-container form input {
	width: calc(100% - 11em);
	padding: 0.25em;
}
.login-container form input[type='submit'],
.login-container form a {
	text-align: center;
}
.login-container form input[type='submit'] {
	width: 11em;
	padding: 0.35em;
	margin: 0.5em auto;
	line-height: 1;
}
.login-container form a {
	width: 100%;
}
.login-container form h1 {
	text-align: center;
	margin: 0.5em 0;
	width: 100%;
}

/* “Log in” form */

#login-form h1 {
	padding-left: 2rem;
}
#login-form label {
	width: 7em;
}
#login-form input[type='submit'],
#login-form a {
	position: relative;
	left: 1.375em;
}

/* “Create account” form */

#signup-form {
	font-size: 0.9em;
	width: calc(50% - 1em);
	margin-right: 1em;
}
#signup-form h1 {
	font-size: 1.7em;
}
#signup-form label {
	width: 9em;
}
#signup-form input[type='submit'] {
	margin: 0.75em auto 0.5em auto;
	padding: 0.4em 0.5em 0.5em 0.5em;
	position: relative;
	left: 3.5em;
}

/* Log in tip */

.login-container .login-tip {
	padding: 0.5em 0.5em 0.5em 3em;
	margin: 2em 4em 0 4em;
	text-indent: -2em;
	line-height: 1.4;
}
.login-container .login-tip span {
	font-weight: bold;
}

/* Message box */

#content.login-page .error-box {
	margin: 1.5em 0.875em -1.5em 0.875em;
}
.error-box, .success-box {
	padding: 0.25em;
	text-align: center;
}

/***********************/
/* PASSWORD RESET PAGE */
/***********************/

.reset-password-container {
	margin-bottom: 2em;
}
.reset-password-container input[type='submit'] {
	padding: 0.2em 0.5em;
	width: unset;
}
.reset-password-container input {
	margin-left: 0.5em;
	width: 12em;
}
.reset-password-container label {
	display: inline-block;
	width: 9em;
}
.reset-password-container form > div {
	margin: 0.2em;
}
.reset-password-container .action-container {
	padding-left: 11em;
	padding-top: 0.2em;
}
.reset-password-container .error-box {
	margin: unset;
}

/*********************/
/* TABLE OF CONTENTS */
/*********************/

.contents {
	float: right;
	min-width: 12em;
	max-width: 40%;
	margin: 1.25em 0 0.75em 1.25em;
	padding: 0.35em 0.35em 0.4em 0.35em;
	-webkit-hyphens: none;
	-moz-hyphens: none;
	-ms-hyphens: none;
	hyphens: none;
}

.contents-head {
	text-align: center;
	margin-bottom: 0.25em;
}

.post-body .contents ul {
	list-style-type: none;
	margin: 0 0 0 0.5em;
	counter-reset: toc-item-1 toc-item-2 toc-item-3;
	padding-left: 1em;
	font-size: 0.75em;
}
.post-body .contents li {
	margin: 0.15em 0 0.3em 1em;
	text-align: left;
	text-indent: -1em;
	line-height: 1.2;
	position: relative;
}
.post-body .contents li::before {
	position: absolute;
	width: 3em;
	display: block;
	text-align: right;
	left: -4.5em;
}
.contents .toc-item-1 {
	counter-increment: toc-item-1;
	counter-reset: toc-item-2 toc-item-3;
}
.contents .toc-item-1::before {
	content: counter(toc-item-1);
}
.contents .toc-item-1 ~ .toc-item-2 {
	margin-left: 2.9em;
	font-size: 0.95em;
}
.contents .toc-item-2 {
	counter-increment: toc-item-2;
	counter-reset: toc-item-3;
}
.contents .toc-item-1 ~ .toc-item-2::before {
	content: counter(toc-item-1) "." counter(toc-item-2);
}
.contents .toc-item-2::before {
	content: counter(toc-item-2);
}
.contents .toc-item-1 + .toc-item-3 {
	counter-increment: toc-item-2 toc-item-3;
}
.contents .toc-item-2 ~ .toc-item-3,
.contents .toc-item-1 ~ .toc-item-3 {
	margin-left: 2.9em;
	font-size: 0.95em;
}
.contents .toc-item-1 ~ .toc-item-2 ~ .toc-item-3 {
	margin-left: 5.7em;
	font-size: 0.9em;
}
.contents .toc-item-3 {
	counter-increment: toc-item-3;
}
.contents .toc-item-1 ~ .toc-item-2 ~ .toc-item-3::before {
	content: counter(toc-item-1) "." counter(toc-item-2) "." counter(toc-item-3);
}
.contents .toc-item-1 ~ .toc-item-3::before {
	content: counter(toc-item-1) "." counter(toc-item-3);
}
.contents .toc-item-2 ~ .toc-item-3::before {
	content: counter(toc-item-2) "." counter(toc-item-3);
}
.contents .toc-item-3::before {
	content: counter(toc-item-3);
}
.contents .toc-item-4,
.contents .toc-item-5,
.contents .toc-item-6 {
	display: none;
}

/********************/
/* POSTS & COMMENTS */
/********************/

.post-meta > *,
.comment-meta > * {
	display: inline-block;
	margin-right: 1em;
	font-size: 1.0625em;
	white-space: nowrap;
}
@media all and (-ms-high-contrast: none), (-ms-high-contrast: active) {
	.post-body, .comment-body {
		-ms-hyphens: auto;
		text-align: justify;
	}
}
@supports (hyphens: auto) or (-moz-hyphens: auto) or (-webkit-hyphens: auto) {
	.post-body, .comment-body {
		text-align: justify;
		-webkit-hyphens: auto;
		-moz-hyphens: auto;
		hyphens: auto;
	}
}
.post-body p, .comment-body p {
	margin: 1em 0;
}

/*************/
/* POST-META */
/*************/

.post-meta {
	display: flex;
	flex-flow: row wrap;
	justify-content: center;
	line-height: 1.9;
}
.post-meta .lw2-link {
	opacity: 0.5;
	order: 1;
}
.post-meta > *,
.post-meta .post-section::before {
	margin: 0 0.5em;
}
.post-meta .post-section {
	order: -1;
	margin: 0;
	visibility: hidden;
}
.post-meta .post-section::before {
	visibility: visible;
	font-family: "Font Awesome";
	font-weight: 900;
}
.post-section.frontpage::before {
	content: "\F015";
}
.post-section.featured::before {
	content: "\F005";
}
.post-section.meta::before {
	content: "\F077";
}
.post-section.personal::before {
	content: "\F007";
}
.post-section.draft::before {
	content: "\F15B";
}

/*********/
/* POSTS */
/*********/

.post-body {
	min-height: 8em;
	padding: 0 30px;
	line-height: 1.5;
	font-size: 1.3rem;
	overflow: auto;
	margin: 0.5em 0 0 0;
}
.post > h1:first-child {
	margin: 1.1em 0 0.35em 0;
	padding: 0 30px;
	text-align: center;
	font-size: 2.5em;
	line-height: 1;
}
.post .post-meta {
	text-align: center;
}
.post .top-post-meta:last-child {
	margin-bottom: 40px;
}
.post .bottom-post-meta {
	margin: 0;
	padding: 20px 0 22px 0;
}

/**************/
/* LINK POSTS */
/**************/

.post.link-post > .post-body > p:first-child {
	text-align: center;
	font-size: 1.125em;
	margin: 0.5em 0 0 0;
}
.post.link-post > .post-body > p:only-child {
	font-size: 1.5em;
	margin: 1em 0;
}
.post.link-post a.link-post-link::before {
	content: "\F0C1";
	font-family: Font Awesome;
	font-weight: 900;
	font-size: 0.75em;
	position: relative;
	top: -2px;
	margin-right: 0.25em;
}

/************/
/* COMMENTS */
/************/

#comments {
	padding: 0 0 1px 0;
}
ul.comment-thread {
	list-style-type: none;
	padding: 0;
}
#comments .comment-thread > li {
	position: relative;
}

.comment-item {
	margin: 2em 0 0 0;
}
.comment-item .comment-item {
	margin: 1em 8px 8px 16px;
}
.comment-item .comment-item + .comment-item {
	margin: 2em 8px 8px 16px;
}

.comment-body {
	line-height: 1.45;
	font-size: 1.2rem;
	padding: 10px;
}
.comment-body ul {
	list-style-type: circle;
}
.comment-body > *:first-child {
	margin-top: 0;
}
.comment-body > *:last-child {
	margin-bottom: 0;
}

#comments:empty::before,
#comments > .comment-controls:last-child::after {
	content: "No comments.";
	display: block;
	width: 100%;
	text-align: center;
	padding: 0.75em 0 0.9em 0;
	font-size: 1.375em;
}

/**********************************/
/* DEEP COMMENT THREAD COLLAPSING */
/**********************************/

.comment-item input[id^="expand"] {
	display: none;
}
.comment-item input[id^="expand"] + label {
	display: block;
	visibility: hidden;
	position: relative;
	margin: 8px 9px;
}
.comment-item input[id^="expand"] + label::after {
	content: "(Expand " attr(data-child-count) "	below)";
	visibility: visible;
	position: absolute;
	left: 0;
	white-space: nowrap;
	cursor: pointer;
}
.comment-item input[id^="expand"]:checked + label::after {
	content: "(Collapse " attr(data-child-count) "	below)";
}
.comment-item input[id^="expand"] ~ .comment-thread {
	max-height: 34px;
	overflow: hidden;
}
.comment-item input[id^="expand"] ~ .comment-thread > li:first-child {
	margin-top: 0;
}
.comment-item input[id^="expand"]:checked ~ .comment-thread {
	max-height: 1000000px;
}

.comment-item input[id^="expand"]:checked ~ .comment-thread .comment-thread .comment-item {
	margin: 0;
}
.comment-item input[id^="expand"]:checked ~ .comment-thread .comment-thread .comment-item a.comment-parent-link:hover::after {
	display: none;
}

/****************/
/* COMMENT-META */
/****************/

.comment-meta {
	padding: 2px 80px 2px 10px;
	margin: 0 -1px;
	border: none;
	display: flex;
	flex-flow: row wrap;
	align-items: baseline;
}
.comment-meta .comment-post-title {
	flex-basis: 100%;
	overflow: hidden;
	text-overflow: ellipsis;
}
.user-page .comment-meta {
	padding-right: 10px;
}

/*****************************/
/* COMMENT THREAD NAVIGATION */
/*****************************/

a.comment-parent-link {
	opacity: 0.5;
}
a.comment-parent-link:hover {
	opacity: 1.0;
}
a.comment-parent-link::before {
	content: "\F062";
	font-family: "Font Awesome";
	font-weight: 900;
	font-size: 0.75rem;
	line-height: 1;
	position: absolute;
	z-index: 1;
	display: block;
	padding: 3px 3px 0 3px;
	width: 16px;
	height: calc(100% + 2px);
	top: -1px;
	left: -17px;
}
a.comment-parent-link::after {
	content: "";
	position: absolute;
	z-index: 0;
	display: block;
	width: calc(100% + 26px);
	height: calc(100% + 38px);
	top: -29px;
	left: -17px;
	pointer-events: none;
	overflow: hidden;
	visibility: hidden;
}
a.comment-parent-link:hover::after {
	visibility: visible;
}

div.comment-child-links {
	display: block;
}
div.comment-child-links a {
	margin: 0 0.2em;
	display: inline-block;
}
div.comment-child-links a::first-letter {
	margin: 0 1px 0 0;
}

.comment-popup {
	position: fixed;
	top: 10%;
	right: 10%;
	max-width: 700px;
	z-index: 10000;
	font-size: 1rem;
	white-space: unset;
	pointer-events: none;
}
.comment-popup .comment-parent-link {
	display: none;
}
.comment-popup .comment-body {
	font-size: 1.0625rem;
}

/**********************/
/* COMMENT PERMALINKS */
/**********************/
/********************/
/* COMMENT LW LINKS */
/********************/

.comment-meta .permalink::before,
.comment-meta .lw2-link::before,
.individual-thread-page a.comment-parent-link:empty::before {
	content: "";
	display: inline-block;
	width: 1rem;
	height: 1rem;
	border-radius: 3px;
	box-shadow: 
		0 0 0 1px #fff,
		0 0 0 2px #00e,
		0 0 0 3px transparent;
	padding: 0 0 0 2px;
	background-size: 100%;
	position: relative;
	top: 2px;
	opacity: 0.5;
}
.comment-meta .permalink::before {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/anchor-white-on-blue.gif")) ?>');
}
.comment-meta .lw2-link::before {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/lw-white-on-blue.gif")) ?>');
}
.individual-thread-page a.comment-parent-link:empty::before {
	left: unset;
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/up-arrow-white-on-blue.gif")) ?>');
}
.comment-meta .permalink:hover::before {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/anchor-blue-on-white.gif")) ?>');
}
.comment-meta .lw2-link:hover::before {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/lw-blue-on-white.gif")) ?>');
}
.individual-thread-page a.comment-parent-link:empty:hover::before {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/up-arrow-blue-on-white.gif")) ?>');
}
.comment-meta .permalink:hover::before,
.comment-meta .lw2-link:hover::before,
.individual-thread-page a.comment-parent-link:empty:hover::before {
	box-shadow: 
		0 0 0 2px #00e,
		0 0 0 3px transparent;
	opacity: 1.0;
	filter: unset;
}
.comment-meta .permalink:active::before,
.comment-meta .lw2-link:active::before,
.individual-thread-page a.comment-parent-link:empty:active::before {
	transform: scale(0.9);
}

.comment-meta .permalink,
.comment-meta .lw2-link,
.individual-thread-page .comment-parent-link:empty {
	position: relative;
	opacity: 1.0;
}
.comment-meta .permalink::after,
.comment-meta .lw2-link::after,
.individual-thread-page .comment-parent-link:empty::after {
	content: "";
	width: 30px;
	height: 30px;
	display: block;
	position: absolute;
	top: -2px;
	left: -7px;
	box-shadow: none;
	pointer-events: auto;
	visibility: visible;
}

/*************************/
/* COMMENTS COMPACT VIEW */
/*************************/

#comments-list-mode-selector {
	position: absolute;
	top: 108px;
	left: 29px;
}
#content.user-page #comments-list-mode-selector,
#content.conversation-page #comments-list-mode-selector {
	top: 165px;
}
#content.search-results-page #comments-list-mode-selector {
	top: 95px;
}
#comments-list-mode-selector button {
	color: transparent;
	width: 32px;
	height: 32px;
	padding: 6px;
	margin: 1px;
	overflow: hidden;
	background-repeat: no-repeat;
	background-size: 100%;
	background-origin: content-box;
}
#comments-list-mode-selector button:disabled {
	cursor: auto;
}
#comments-list-mode-selector button.expanded {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/expanded.gif")) ?>');
}
#comments-list-mode-selector button.compact {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/compact.gif")) ?>');
}

#content.compact > .comment-thread {
	font-size: 0.9375rem;
}
#content.compact > .comment-thread .comment-body {
	font-size: 1.0625rem;
}
#content.compact > .comment-thread .comment-item {
	max-height: 71px;
	margin-top: 1em;
	overflow: hidden;
	position: relative;
	pointer-events: none;
}
#content.compact > .comment-thread .comment-item::after {
	content: "…";
	position: absolute;
	right: 0;
	bottom: 0;
	font-size: 2rem;
	line-height: 1;
	padding: 0 16px 10px 64px;
	pointer-events: auto;
}
#content.compact > #top-nav-bar + .comment-thread .comment-item {
	margin-top: 2.25em;
}
#content.compact > .comment-thread .comment-item:hover {
	overflow: visible;
	pointer-events: auto;
	z-index: 10;
}
#content.compact > .comment-thread .comment-item .comment-meta {
	white-space: nowrap;
	overflow: hidden;
	text-overflow: ellipsis;
	padding: 2px 10px;
}
#content.compact > .comment-thread .comment-item:hover .comment-meta {
	white-space: unset;
}
#content.compact > .comment-thread .comment-item .comment-meta a {
	pointer-events: auto;
}
#content.compact > .comment-thread .comment-item .comment-meta .comment-post-title {
	display: inline;
}
#content.compact > .comment-thread .comment-item .comment-meta .karma + .comment-post-title {
	margin-left: 0.75em;
}
#content.compact > .comment-thread:last-of-type .comment-item:hover {
	max-height: unset;
}
#content.compact > .comment-thread .comment-item:hover .comment {
	position: relative;
	z-index: 1;
	margin-bottom: 2em;
	bottom: 0;
}
#content.compact > .comment-thread .comment-item:hover .comment::before {
	content: "";
	position: absolute;
	display: block;
	width: calc(100% + 20px);
	height: calc(100% + 20px);
	z-index: -1;
	top: -10px;
	left: -10px;
}
#content.compact > .comment-thread:last-of-type .comment-item:hover .comment {
	margin: 0;
}

/*****************************/
/* HIGHLIGHTING NEW COMMENTS */
/*****************************/

.new-comment::before {
	content: "";
	position: absolute;
	width: 100%;
	height: 100%;
	z-index: 5000;
	pointer-events: none;
}

/***********************************/
/* COMMENT THREAD MINIMIZE BUTTONS */
/***********************************/

.comment-minimize-button {
	font-family: Font Awesome;
	font-weight: 900;
	font-size: 1.25rem;
	line-height: 1;
	position: absolute;
	right: 1px;
	top: 1px;
	width: 18px;
	margin: 0;
	cursor: pointer;
}
.comment-minimize-button:active {
	transform: scale(0.9);
}
.comment-minimize-button::after {
	content: attr(data-child-count);
	font-weight: normal;
	font-size: 0.8125rem;
	position: absolute;
	left: 0;
	width: 100%;
	text-align: center;
	top: 21px;
}
#content.individual-thread-page .comment-minimize-button {
	display: none;
}

/***********************************/
/* INDIVIDUAL COMMENT THREAD PAGES */
/***********************************/

.individual-thread-page > h1 {
	line-height: 1;
}
.individual-thread-page .comment-controls .edit-button {
	right: 4px;
}
.individual-thread-page #comments {
	border: none;
}

/****************/
/* VOTE BUTTONS */
/****************/

.vote {
	margin: 0;
}
.vote {
	font-family: Font Awesome;
	font-weight: 900;
	border: none;
}

/*****************************/
/* COMMENTING AND POSTING UI */
/*****************************/

.comment-controls {
	text-align: right;
	margin: 0 8px 8px 16px;
}
.comment-thread .comment-controls + .comment-thread > li:first-child {
	margin-top: 8px;
}
#comments > .comment-controls {
	margin: 8px 0 0 0;
}
#comments > .comment-controls:last-child {
	margin: 8px 0 16px 0;
}

.posting-controls input[type='submit'] {
	margin: 6px;
	padding: 4px 10px;
	font-size: 1.125rem;
}

.comment-controls .cancel-comment-button {
	position: absolute;
	right: 8px;
	margin: 0;
	height: 27px;
	font-size: inherit;
	padding: 4px 8px 2px 4px;
	z-index: 1;
}
#comments > .comment-controls .cancel-comment-button {
	right: 30px;
}
.comment-controls .cancel-comment-button::before {
	font-family: Font Awesome;
	margin-right: 3px;
	content: '\F00D';
	font-weight: 900;
	font-size: 0.9em;
	opacity: 0.7;
}

.comment + .comment-controls .action-button {
	font-weight: normal;
	font-size: 1.0625em;
	padding: 1px 6px;
}
.comment-controls .action-button::before {
	font-family: Font Awesome;
	margin-right: 3px;
}
.new-comment-button {
	font-size: 1.5rem;
	margin: 0 0.25em;
}
.comment-controls .edit-button {
	position: absolute;
	right: 24px;
	top: 7px;
}
.comment-thread .comment-thread .edit-button {
	right: 8px;
}
.comment-controls .reply-button::before {
	content: '\F3E5';
	font-weight: 900;
	font-size: 0.9em;
	opacity: 0.6;
}

.post-controls {
	text-align: right;
}
.edit-post-link {
	display: inline-block;
	margin-bottom: 0.25em;
	font-size: 1.125rem;
}
.edit-post-link::before {
	margin-right: 0.3em;
}
.comment-controls .edit-button::before,
.edit-post-link::before {
	content: '\F303';
	font-family: "Font Awesome";
	font-weight: 900;
	font-size: 0.75em;
	position: relative;
	top: -1px;
}

.comment-controls form {
	position: relative;
}
.textarea-container {
	position: relative;
}
.posting-controls textarea {
	display: block;
	width: 100%;
	height: 15em;
	min-height: 15em;
	max-height: calc(100vh - 6em);
	margin: 2px 0;
	padding: 4px 5px;
	font-size: 1.2rem;
	border-style: solid;
	border-width: 29px 1px 1px 1px;
	resize: none;
}

/* GUIEdit buttons */

.guiedit-buttons-container {
	position: absolute;
	left: 1px;
	top: 1px;
	width: calc(100% - 2px);
	height: 28px;
	text-align: left;
	padding: 1px 4px 0 4px;
	overflow: hidden;
}
.post-page .guiedit-buttons-container {
	padding-right: 60px;
}
.guiedit-buttons-container button {
	height: 26px;
	padding: 0 7px;
	font-weight: 900;
	font-size: 0.875rem;
	line-height: 1;
	position: static;
}
.guiedit-buttons-container button:active {
	transform: none;
}
.guiedit-buttons-container button:active div {
	transform: scale(0.9);
}
.guiedit-buttons-container button sup {
	font-weight: bold;
}
.guiedit::after {
	content: attr(data-tooltip);
	position: absolute;
	font-weight: normal;
	font-size: 1rem;
	top: 2px;
	left: 440px;
	height: 25px;
	padding: 4px 0;
	white-space: nowrap;
	visibility: hidden;
}
.guiedit:hover::after {
	visibility: visible;
}

/* Markdown hints */

.posting-controls .markdown-reference-link {
	float: left;
	padding: 5px 0 0 6px;
}
.posting-controls .markdown-reference-link a {
	padding-right: 1.5em;
	margin-right: 0.15em;
	background-image: url('data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAJAAAACQCAQAAABNTyozAAAEDklEQVR4Ae3cY3QDjRaG0V1e27Zt27Zt27Ztf7Zt27Zt10jOdZtZzbSN2q41533+tsFO4zRi0TKRJVACJdDiJVACJVACJVACZQmUQAmUQAmUQAmUQFkCJVACJVACJVACJVDWuD7P8icnGRcVbdyJ/uRZ+jTZYxwq/lN2qMcozvtMibmySe/TsPeqi0JZ3XsAHm1SZAua9CjgoMQo6UB4uiim5gbXV7Ab1EQxT+P3RRw/dHtV3e39UL3g8XuOEw39QNX3g4LHcYwU/n5uo+q7beGKNqLwJ3U1cteKuepEQ1cid03BJIESKIESKIESKIESaIkl0I3dv7Q7a293c//ShrWym7l/abdbGaCnidJGPFzre6opUdqDtLJXitJ+svpA4Uy30dru6hJRHaCws37L37CDRbWAwvctf38S1QOqe43l7f2iikDheg+x9J5ksqpA4TS3svju5CJRXaCwvX7lG3KAqDZQ+Jby/U4kUM0rNN+7hAQSrvNAC/c4Ewn0/052C8Xd0fkigebbRp/5DdpHJFCxr5nfr4QEUqzmJYC3iQRq1jXuj8cYT6CyTnAv54oEKm9EJFBnJVAC7eoS0XJn2r8qQP/wNFOipUY8wvbVAeIjooXq3ki1gPhHC0A/oWWgQZ/37ZI2FaUdVPpb33LHlQS6scPFstrDQBtAvEpNdLEfsZJA3N3lYsnOcTvaAuKzomttqW+lgXimabFoYx5N20D8SXSlw9yElQfiE0J5dW+lI6BBu4uOO8+dWB0g1hel/YIOgbiVE0VHXefhrB7QTRwtmra3gS4AcW+Xibab8SJWE4h7uaLpn/Ud6AoQTzIu2uzDrDYQzzUjCo17HF0D4g3qoo1+yWoCld8hv5OuAvFl0XLb6V8rQGws5votXQfqs45oqaPdjLUDdNO5f7Xa32APgBhu6b2SC92VtQTEfVwlXOhO9ASI2zhNLKsRj2atAfFCo55Iz4C4nyvFks16OWsRiPvQUyCeblIs0adYq0B6DsTb1EV5fk+1gfiWKG0XAwnUZyPRtOPdggTiRg4UC7rEPUkg4PbOFIXGPIEEmt+DCmeu5rUkUHHPaXj76Qsk0MK9R/ynv5FAzfdDYS9Da+n/xe6ovd2lS/8vVlyfH7o1vQLKJVACJVACJVACJVACIYGW/A6z/A6zG8RcNbdT9d1eTcx1A8eKhn6s6vtxweNYfisaqvupu+jXV8H63cXP1Asev+Wpopi6aVMVbFpdFPMUlP6jdrY/8AgTYkHZhEcAvFNdFMpq3qFh78y/okIT3qk4j8zborn290hN91S/c6zrzapVsFnXO9bvPFXjYtEykSVQAnVUAiVQAiVQAiVQAiVQlkAJlEAJlEAJlEAJlCVQAiVQAiVQAiVQAmX/BMHb3CdNrgcrAAAAAElFTkSuQmCC');
	background-size: 1.25em;
	background-repeat: no-repeat;
	background-position: right center;
}

#markdown-hints-checkbox + label {
	float: left;
	padding: 4px 0 0 0;
	margin: 0 0 0 1em;
	cursor: pointer;
}
#edit-post-form #markdown-hints-checkbox + label {
	padding: 2px 0 0 0;
}
#markdown-hints-checkbox {
	visibility: hidden;
	float: left;
}
#markdown-hints-checkbox + label::after {
	content: "(Show Markdown help)";
}
#markdown-hints-checkbox:checked + label::after {
	content: "(Hide Markdown help)";
}
#markdown-hints-checkbox + label::before {
	content: '\F059';
	font-family: Font Awesome;
	font-weight: 900;
	margin-right: 3px;
}
#markdown-hints-checkbox:checked + label::before {
	font-weight: normal;
}
.markdown-hints {
	margin: 4px 0 0 4px;
	padding: 4px 8px;
	position: absolute;
	text-align: left;
	top: calc(100% - 1em);
	z-index: 1;
	display: none;
}
#markdown-hints-checkbox:checked ~ .markdown-hints {
	display: table;
}
.markdown-hints-row {
	display: table-row;
}
.markdown-hints .markdown-hints-row span,
.markdown-hints .markdown-hints-row code {
	float: none;
	display: table-cell;
	border: none;
	background-color: inherit;
	padding: 0 12px 0 0;
}

/******************/
/* EDIT POST FORM */
/******************/

#edit-post-form {
	padding: 1em 1em 4em 1em;
}
#edit-post-form .post-meta-fields {
	overflow: auto;
	display: flex;
	flex-flow: row wrap;
}

#edit-post-form input {
	width: auto;
}
#edit-post-form input[type='text'] {
	width: calc(100% - 6em);
	margin: 0.25em 0;
	padding: 0.25em;
}
#edit-post-form input[type='submit'] {
	padding: 6px 12px;
	float: right;
}
#edit-post-form .link-post-checkbox {
	height: 0;
	opacity: 0;
	pointer-events: none;
}
#edit-post-form .link-post-checkbox + label {
	padding-left: 6px;
	margin-left: 0.5em;
	white-space: nowrap;
	position: relative;
	cursor: pointer;
}
#edit-post-form .link-post-checkbox + label::before {
	content: "";
	font-family: Font Awesome;
	font-size: 1.375rem;
	line-height: 0.7;
	text-indent: 1px;
	font-weight: 900;
	position: absolute;
	width: 20px;
	height: 20px;
	left: -20px;
	top: 4px;
}
#edit-post-form label[for='url'],
#edit-post-form input[name='url'] {
	display: none;
}
#edit-post-form .link-post-checkbox:checked ~ label[for='url'],
#edit-post-form .link-post-checkbox:checked ~ input[name='url'] {
	display: block;
}
#edit-post-form label {
	margin: 0.25em 0;
	line-height: normal;
	border: 1px solid transparent;
	text-align: right;
    padding: 0.25em 0.5em;
    white-space: nowrap;
}
#edit-post-form label[for='title'],
#edit-post-form label[for='url'],
#edit-post-form label[for='section'] {
	width: 6em;
}
#edit-post-form input[name='title'] {
	max-width: calc(100% - 12.5em);
}
#edit-post-form label[for='link-post'] {
	width: 4em;
}
#edit-post-form label[for='section'] {
	margin: 0.35em 0;
}
#edit-post-form input[type='radio'] {
	width: 0;
	margin: 0;
	opacity: 0;
	pointer-events: none;
}
#edit-post-form input[type='radio'] + label {
	margin: 0.35em 0;
	width: auto;
	padding: 0.25em 0.75em;
	text-align: center;
	border-style: solid;
	border-width: 1px 1px 1px 0;
	cursor: pointer;
}
#edit-post-form input[type='radio']:focus + label {
	position: relative;
}
#edit-post-form input[type='radio']:checked + label {
	cursor: default;
}
#edit-post-form textarea {
	min-height: 24em;
	margin-top: 4px;
}
#edit-post-form .markdown-hints {
	top: calc(100% + 2em);
}
#content.edit-post-page {
	overflow: visible;
}

.guiedit-mobile-auxiliary-button {
	display: none;
}

/***********/
/* BUTTONS */
/***********/

button,
input[type='submit'] {
	font-family: inherit;
	font-size: inherit;
	background-color: inherit;
	cursor: pointer;
	border: none;
	border-radius: 0;
}

/************/
/* HEADINGS */
/************/

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
	line-height: 1.1;
	margin: 1em 0 0.75em 0;
	text-align: left;
}

.post-body h5,
.post-body h6,
.comment-body h5,
.comment-body h6 {
	font-size: 1em;
}
.post-body h4,
.comment-body h4 {
	font-size: 1.2em;
}
.post-body h3,
.comment-body h3 {
	font-size: 1.4em;
}
.post-body h2,
.comment-body h2 {
	font-size: 1.75em;
}
.post-body h1,
.comment-body h1 {
	font-size: 2.1em;
}

/**********/
/* QUOTES */
/**********/

blockquote,
.post-body .comment-box .comment-body {
	font-size: 0.9em;
	margin: 1em 0;
	padding-left: 0.5em;
	margin-left: 1px;
	padding-bottom: 3px;
}
blockquote *:first-child {
	margin-top: 0;
}
blockquote *:last-child {
	margin-bottom: 0;
}
blockquote blockquote {
	font-size: 0.95em;
}

/* Pseudo-blockquotes that LW sometimes uses for some reason */

.post-body .comment-box .user-name {
	font-style: italic;
}
.post-body .comment-box .user-name::after {
	content: ":";
}
.post-body .comment-box {
	zoom: 1.25;
}

/**********/
/* IMAGES */
/**********/

#content img {
	max-width: 100%;
}

img.inline-latex {
	position: relative;
	top: 2.5px;
	margin: 0 2px;
}

#content figure {
	text-align: center;
	margin: 1.5em auto;
}

p.imgonly,
div.imgonly {
	text-align: center;
}

/********/
/* MISC */
/********/

sup, sub {
	vertical-align: baseline;
	position: relative;
	top: -0.5em;
	left: 0.05em;
	font-size: 0.8em;
}
sub {
	top: 0.3em;
	-webkit-hyphens: none;
	-moz-hyphens: none;
	-ms-hyphens: none;
	hyphens: none;
}

hr {
	border: none;
}

pre {
	white-space: pre-wrap;
}
code {
	font-family: Inconsolata, Menlo, monospace;
	font-size: 0.95em;
	display: inline-block;
	padding: 0 4px 1px 5px;
}
pre > code {
	display: block;
	border-radius: 0;
	padding: 3px 4px 5px 8px;
}

input[type='text'],
input[type='search'],
input[type='password'],
textarea {
	-webkit-appearance: none;
}

.frac::after {
	content: "\200B";
}

/*************/
/* FOOTNOTES */
/*************/

ol {
	counter-reset: ordered-list;
}
.footnote-definition {
	font-size: 0.9em;
	list-style-type: none;
	counter-increment: ordered-list;
	position: relative;
}
.footnote-definition p {
	font-size: inherit !important;
}
.footnote-definition::before {
	content: counter(ordered-list) ".";
	position: absolute;
	left: -2.5em;
	font-weight: bold;
	text-align: right;
	width: 2em;
}

/*********/
/* LISTS */
/*********/

li {
	margin-bottom: 0.5em;
}

.post-body ol p,
.post-body ul p,
.comment-body ol p,
.comment-body ul p {
	margin: 0.5em 0;
}

.post-body ol {
	list-style: none;
	padding: 0;
	counter-reset: ol;
}
.post-body ol > li {
	position: relative;
	counter-increment: ol;
	padding: 0 0 0 2.5em;
	margin: 0.25em 0 0 0;
}
.post-body ol > li::before {
	content: counter(ol) ".";
	position: absolute;
	width: 2em;
	text-align: right;
	left: 0;
}
.post-body ul {
	list-style: none;
	padding: 0;
}
.post-body ul:not(.contents-list) li {
	position: relative;
	padding: 0 0 0 2.5em;
	margin: 0.25em 0 0 0;
}
.post-body ul:not(.contents-list) li::before {
	content: "•";
	position: absolute;
	width: 2em;
	text-align: right;
	left: 0;
}
.post-body li > ul:first-child > li {
	padding-left: 0;
}
.post-body li > ul:first-child > li::before {
	content: none;
}

/**************/
/* ABOUT PAGE */
/**************/

#content.about-page .contents {
	margin-top: 0.25em;
}
#content.about-page .accesskey-table {
	border-collapse: collapse;
	margin: auto;
}
#content.about-page .accesskey-table th,
#content.about-page .accesskey-table td {
	padding: 2px 6px;
}
#content.about-page .accesskey-table td:first-child {
	padding-right: 1.5em;
}
#content.about-page .accesskey-table td:last-child {
	text-align: center;
	font-family: Inconsolata, Menlo, monospace;
}
#content.about-page h3:nth-of-type(n+2) {
	clear: both;
}

/******************/
/* IMAGES OVERLAY */
/******************/

#images-overlay img {
	position: absolute;
}
#images-overlay + #content .post-body img {
	visibility: hidden;
}

/**************************/
/* QUALIFIED HYPERLINKING */
/**************************/

#content.no-comments #comments, 
#content.no-comments .post-meta .comment-count,
#content.no-comments .post-meta .karma,
#content.no-comments + #ui-elements-container #new-comment-nav-ui,
#content.no-comments + #ui-elements-container #hns-date-picker,
#content.no-comments + #ui-elements-container #quick-nav-ui {
	display: none;
}

#content.no-nav-bars #primary-bar,
#content.no-nav-bars #secondary-bar {
	display: none;
}
#content.no-nav-bars {
	margin: 8px auto;
}
#content.no-nav-bars + #ui-elements-container > * {
	padding-top: 8px;
}

#aux-about-link {
	position: fixed;
	top: 40px;
	left: calc((100% - 900px) / 2 - 69px);
	width: 1.5em;
	height: 1.5em;
	text-align: center;
	display: table;
}
#aux-about-link a {
	display: table-cell;
	width: 100%;
	vertical-align: middle;
	font-family: Font Awesome;
	font-weight: 900;
	font-size: 1.25rem;
	opacity: 0.4;
	z-index: 1;
}

.qualified-linking {
	margin: 0;
	position: relative;
}
.qualified-linking input[type='checkbox'] {
	visibility: hidden;
	width: 0;
	height: 0;
	margin: 0;
}
.qualified-linking label {
	font-family: Font Awesome;
	font-weight: 900;
	font-size: 1rem;
	padding: 0 0.5em;
	display: inline-block;
	margin-left: 0.25em;
}
.qualified-linking label:hover {
	cursor: pointer;
}
.qualified-linking label:active span {
	display: inline-block;
	transform: scale(0.9);
}
.qualified-linking label::selection {
	background-color: transparent;
}

.qualified-linking label::after {
	content: "";
	width: 100vw;
	height: 0;
	left: 0;
	top: 0;
	position: fixed;
	z-index: 1;
	cursor: default;
}
.qualified-linking input[type='checkbox']:checked + label::after {
	height: 100vh;
}

.qualified-linking-toolbar {
	position: absolute;
	right: 0.25em;
	top: 110%;
	z-index: 1;
}
.qualified-linking input[type='checkbox'] ~ .qualified-linking-toolbar {
	display: none;
}
.qualified-linking input[type='checkbox']:checked ~ .qualified-linking-toolbar {
	display: block;
}
#qualified-linking-toolbar-toggle-checkbox-bottom ~ .qualified-linking-toolbar {
	top: unset;
	bottom: 125%;
}

.qualified-linking-toolbar a {
	display: block;
	padding: 0 6px;
	margin: 4px;
}
.qualified-linking-toolbar a::selection {
	background-color: transparent;
}

/********/
/* MATH */
/********/

div > .MJXc-display {
	max-width: 100%;
	overflow-y: hidden;
}
.mjx-chtml {
	clear: both;
}

/**********************/
/* FOR NARROW SCREENS */
/**********************/

@media only screen and (max-width: 1440px) {
	#hns-date-picker {
		right: -81px;
		padding: 8px 10px 10px 10px;
		bottom: 62px;
		display: none;
	}
	#hns-date-picker::before {
		content: "";
		position: absolute;
		display: block;
		z-index: -1;
		height: calc(100% + 2px);
		top: -1px;
		left: -1px;
		width: 50%;
	}
}
@media only screen and (max-width: 1160px) {
	#new-comment-nav-ui {
		bottom: 180px;
		right: -68px;
	}
	#hns-date-picker {
		bottom: 200px;
		right: -36px;
	}
	#hns-date-picker::before {
		width: calc(100% - 35px);
	}
}
@media only screen and (max-width: 1080px) {
	#width-selector {
		right: -30px;
	}
	#width-selector button {
		display: block;
	}
	#text-size-adjustment-ui {
		top: 90px;
		right: -30px;
	}
	#text-size-adjustment-ui button {
		display: block;
		position: relative;
	}
	#text-size-adjustment-ui button.increase {
		bottom: 48px;
	}
	#text-size-adjustment-ui button.decrease {
		top: 50px;
	}
	#theme-selector {
		top: 54px;
	}
	#theme-tweaker-toggle {
		left: -41px;
	}
	#theme-tweaker-toggle button {
		height: 2em;
		width: 2em;
		padding: 7px;
	}
	#quick-nav-ui {
		right: -54px;
	}
	#new-comment-nav-ui {
		right: -55px;
	}
	#hns-date-picker {
		right: -23px;
	}
	#hns-date-picker::before {
		width: calc(100% - 22px);
	}
}
@media only screen and (max-width: 1040px) {
	#quick-nav-ui {
		right: -49px;
	}
	#new-comment-nav-ui {
		right: -50px;
	}
	#hns-date-picker {
		right: -18px;
	}
	#hns-date-picker::before {
		width: calc(100% - 17px);
	}
}
@media only screen and (max-width: 1020px) {
	#quick-nav-ui {
		right: -20px;
	}
	#new-comment-nav-ui {
		right: -21px;
	}
	#new-comment-nav-ui .new-comments-count::before {
		content: "";
		position: absolute;
		width: 100%;
		height: calc(100% + 45px);
		z-index: -1;
		left: 0;
		top: -22px;
	}
	#hns-date-picker {
		right: 19px;
	}
	#hns-date-picker::before {
		width: 100%;
	}
}
@media only screen and (max-width: 1000px) {
	#theme-selector {
		left: -17px;
		top: 120px;
		padding: 3px 0;
		max-width: 32px;
	}
	#theme-selector button {
		margin: 1px 4px;
	}
	#text-size-adjustment-ui {
		top: 100px;
		right: -12px;
	}
	#quick-nav-ui,
	#new-comment-nav-ui,
	#new-comment-nav-ui + #hns-date-picker {
		opacity: 0.4;
	}
	#quick-nav-ui:hover,
	#new-comment-nav-ui:hover,
	#new-comment-nav-ui + #hns-date-picker:hover,
	#new-comment-nav-ui + #hns-date-picker:focus-within,
	#new-comment-nav-ui:hover + #hns-date-picker {
		opacity: 1.0;
	}
	#theme-tweaker-toggle {
		top: 70px;
		left: -21px;
	}
}

<?php include("style_mobile_additions.css.php"); ?>

<?php if (isset($argv[2]) && preg_match("/\\.css(.php)?$/", $argv[2])) include($argv[2]); ?>
