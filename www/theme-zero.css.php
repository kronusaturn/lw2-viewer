/**************/
/* THEME ZERO */
/**************/

/*===========*/
/* VARIABLES */
/*===========*/

/*	Typography.
	*/
:root {
	--GW-UI-font: 'Trade Gothic', 'Clear Sans', 'Helvetica', sans-serif;
	--GW-UI-font-weight-light: 400;
	--GW-UI-font-weight-heavy: 400;

	--GW-body-text-font: 'News Gothic BT', 'Clear Sans', 'Helvetica', sans-serif;

	--GW-post-listings-font-weight: 700;
}

/*	Layout.
	*/
:root {
	--GW-content-side-padding: 50px;
	--GW-sequence-page-content-side-padding: 50px;
	--GW-user-page-content-side-padding: 30px;
	--GW-recent-comments-page-content-side-padding: 30px;
	--GW-conversation-page-content-side-padding: 30px;
	--GW-post-page-content-side-padding: 30px;
	--GW-post-side-padding: 30px;
	--GW-edit-post-page-content-side-padding: 30px;
	--GW-individual-thread-page-content-side-padding: 30px;

	--GW-comment-compact-height: 55px;
	--GW-comment-compact-height-mobile: 105px;
	--GW-comment-minimized-height: 38px;
	--GW-comment-minimized-height-mobile: 64px;

	--GW-HNS-date-picker-text-field-width: 154px;
}
@media only screen and (max-width: 900px) {
	:root {
		--GW-content-side-padding: calc(100% / 45);
		--GW-sequence-page-content-side-padding: calc(100% / 36);
		--GW-user-page-content-side-padding: calc(100% / 30);
		--GW-recent-comments-page-content-side-padding: calc(100% / 30);
		--GW-conversation-page-content-side-padding: calc(100% / 30);
		--GW-post-page-content-side-padding: calc(100% / 30);
		--GW-post-side-padding: 0px;
		--GW-edit-post-page-content-side-padding: calc(100% / 30);
		--GW-individual-thread-page-content-side-padding: calc(100% / 30);
	}
}

/*	Color scheme.
	*/
:root {
	--GW-body-background-color: #eee;

	--GW-hyperlink-color: #00e;
	--GW-hyperlink-visited-color: #551a8b;
	--GW-hyperlink-hover-color: #c00;
	--GW-hyperlink-active-color: #e00;

	--GW-shadow-white-glow:
		0 0 1px #fff,
		0 0 3px #fff,
		0 0 5px #fff;

	--GW-comment-background-color-odd: #fff;
	--GW-comment-background-color-even: #eee;
	--GW-comment-background-color-target: #ffc;

	--GW-comment-item-expanded-background-color: #fff;
	--GW-comment-item-expanded-box-shadow:
		0 0  3px #fff,
		0 0  5px #fff,
		0 0  7px #fff,
		0 0 10px #fff,
		0 0 20px #fff,
		0 0 30px #fff,
		0 0 40px #fff;
	--GW-comment-item-expanded-outline-color: #00e;

	--GW-new-comment-item-outline-color: #e00;

	--GW-comment-highlight-color: #00f;
	--GW-comment-highlight-color-faint: #0090ff;

	--GW-theme-selector-outline-color: transparent;

	--GW-submit-button-background-color: #e4e4e4;
}

/*======*/
/* BASE */
/*======*/

body {
	color: #000;
}
#content {
	line-height: 1.55;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

#ui-elements-container > div[id$='-ui-toggle'] button,
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

/*==========*/
/* NAV BARS */
/*==========*/

#primary-bar .nav-inner,
#bottom-bar .nav-inner {
	font-size: 1.125em;
}
#secondary-bar .nav-inner {
	font-size: 0.9375em;
}

.nav-bar {
	background-color: #e4e4e4;
}
.active-bar {
	border-top: 2px solid var(--GW-body-background-color);
}

.nav-inner:hover {
	text-shadow: var(--GW-shadow-white-glow);
}

/*= Decorative bottom bar =*/

#bottom-bar.decorative {
	background-color: transparent;
}
#bottom-bar.decorative::after {
	color: #d8d8d8;
}

/* Accesskey hints */

.nav-inner::after {
	left: 5px;
	top: -1px;
	font-size: 0.7em;
	color: #d8d8d8;
}
.inactive-bar .nav-inner::after {
	color: #ccc;
}
.nav-inner:hover::after {
	color: #bbb;
}

/*= This makes the navbar items look like tabs: =*/

.nav-current {
	background-color: var(--GW-body-background-color);
	box-shadow: 1px 0 0 0 var(--GW-body-background-color);
	text-shadow: 0 0 0 currentColor;
}

/* Search tab */

#nav-item-search form::before {
	opacity: 0.3;
}
#nav-item-search form:focus-within::before {
	opacity: 1.0;
}
#nav-item-search input:not(:focus) {
	background-color: #f4f4f4;
}
#nav-item-search input::placeholder {
	color: #d00;
}
#nav-item-search .nav-inner:hover {
	text-shadow: none;
}

/*= Recent Comments tab =*/

#nav-item-recent-comments span {
	margin: 0 5px 0 0;
}

/*= About tab =*/

#nav-item-about {
	margin-right: 0.5em;
}

/*= Keyboard help button =*/

@media only screen and (min-width: 961px) {
	#nav-item-about button.open-keyboard-help {
		padding: 7px 8px;
		font-weight: 400;
	}
}

/*===============*/
/* PAGINATION UI */
/*===============*/

#top-nav-bar a::after,
#bottom-bar a::after {
	color: #000;
}

#bottom-bar .nav-item a::before,
#top-nav-bar a::before {
	font-weight: 400;
}

#bottom-bar .nav-item a::before {
	font-size: 0.875em;
	bottom: 0;
}

/*==============*/
/* PAGE TOOLBAR */
/*==============*/

.page-toolbar .button,
.page-toolbar .button:visited {
	color: #090;
}
.page-toolbar .button:hover {
	color: var(--GW-hyperlink-hover-color);
}
.page-toolbar .button:active,
.page-toolbar .button:focus {
	color: var(--GW-hyperlink-active-color);
}

.page-toolbar .button::before {
	font-weight: 400;
}

.button.logout-button,
.button.ignore-button {
	color: #d33;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 640px) {
	#content.user-page .page-toolbar {
		margin-top: 1em;
	}
	#content.user-page .page-toolbar > * {
		padding: 0.5em 1em;
		white-space: nowrap;
		margin: 0 2px 0.25em 2px;
	}
	#content.user-page .page-toolbar > * {
		background-color: #e4e4e4;
	}
	.button.logout-button,
	.button.ignore-button {
		color: #c00;
	}
	#content.user-page .page-toolbar > .rss {
		padding-top: calc(0.5em + 1px);
		padding-bottom: calc(0.5em - 1px);
	}
}

/*==============*/
/* SUBLEVEL NAV */
/*==============*/

.sublevel-nav .sublevel-item {
	background-color: #e4e4e4;
	outline: 1px solid #e4e4e4;
}
.sublevel-nav .sublevel-item:hover,
.sublevel-nav .sublevel-item.selected {
	background-color: transparent;
	box-shadow: 0 0 0 1px #e4e4e4 inset;
}
.sublevel-nav .sublevel-item.selected {
	color: #000;
	text-shadow: 0 0 0 currentColor;
}
.sublevel-nav a:link,
.sublevel-nav a:visited {
	color: var(--GW-hyperlink-color);
}
.sublevel-nav a:hover,
.sublevel-nav a:focus {
	color: var(--GW-hyperlink-hover-color);
}
.sublevel-nav a:active {
	color: var(--GW-hyperlink-active-color);
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	.sublevel-nav:not(.sort) .sublevel-item,
	.sublevel-nav:not(.sort) .sublevel-item:first-child,
	.sublevel-nav:not(.sort) .sublevel-item:last-child {
		margin: 3px;
	}
}

/*=====================*/
/* SORT ORDER SELECTOR */
/*=====================*/

.sublevel-nav.sort {
	padding: 18px 0 0 0;
}
.sublevel-nav.sort::before {
	text-transform: uppercase;
	top: -1px;
}
.sublevel-nav.sort .sublevel-item {
	padding: 7px 7px 6px 7px;
	text-transform: uppercase;
}

/*================*/
/* WIDTH SELECTOR */
/*================*/

#width-selector button {
	background-color: #e4e4e4;
}
#width-selector button.selected,
#width-selector button:hover {
	background-color: var(--GW-body-background-color);
	box-shadow:
		0 0 0 2px #e4e4e4 inset;
}
#width-selector button::after {
	color: #aaa;
}

/*================*/
/* THEME SELECTOR */
/*================*/

#theme-selector {
	opacity: 1.0;
}
#theme-selector button {
	background-color: #e4e4e4;
	background-repeat: no-repeat;
}
#theme-selector button.selected,
#theme-selector button:hover {
	background-color: var(--GW-body-background-color);
	box-shadow:
		0 0 0 1px #e4e4e4 inset,
		0 0 0 4px var(--GW-body-background-color) inset;
}

#theme-selector button::before {
	color: #aaa;
	background-color: var(--GW-body-background-color);
}
#theme-selector button:hover::before,
#theme-selector button.selected::before {
	color: #000;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 1000px) {
	#theme-selector {
		background-color: #e4e4e4;
	}
	#theme-selector:hover::after {
		background-color: #e4e4e4;
	}
	#theme-selector button::before {
		background-color: #e4e4e4;
	}
	#theme-selector button.selected,
	#theme-selector button:hover {
		outline: 1px solid #ddd;
	}
}
@media only screen and (max-width: 960px) {
	#theme-selector {
		background-color: #eee;
		box-shadow:
			0 0 0 1px #ddd,
			0 0 1px 3px #fff,
			0 0 3px 3px #fff,
			0 0 5px 3px #fff;
	}
	#theme-selector::before {
		color: #777;
	}
	#theme-selector button:hover,
	#theme-selector button.selected {
		outline: none;
		box-shadow:
			0 0 0 2px #e4e4e4 inset;
	}
	#theme-selector button::after {
		color: #000;
	}
	#theme-selector button.selected::after {
		font-weight: bold;
	}
}

/*======================*/
/* THEME TWEAKER TOGGLE */
/*======================*/

#theme-tweaker-toggle button {
	color: #777;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 1080px) {
	#theme-tweaker-toggle button {
		transform: scale(0.8);
		background-color: #e4e4e4;
		opacity: 1.0;
		color: #aaa;
	}
	#theme-tweaker-toggle button:active {
		box-shadow:
			0 0 10px #ccc inset;
	}
}
@media only screen and (max-width: 1000px) {
	#theme-tweaker-toggle button {
		border: 1px solid #ddd;
	}
}

/*=================*/
/* QUICKNAV WIDGET */
/*=================*/

#quick-nav-ui {
	box-shadow: 0 0 0 6px var(--GW-body-background-color);
}
#quick-nav-ui a {
	color: var(--GW-hyperlink-color);
	background-color: #e4e4e4;
}
#quick-nav-ui a[href='#bottom-bar'] {
	line-height: 1.8;
}
#quick-nav-ui a:active {
	color: var(--GW-hyperlink-active-color);
	transform: scale(0.9);
}
#quick-nav-ui a[href='#comments'].no-comments,
#quick-nav-ui a[href='#answers'].no-answers {
	opacity: 0.4;
	color: #bbb;
}

@media only screen and (hover: hover) and (pointer: fine) {
	#quick-nav-ui a:hover,
	#quick-nav-ui a:focus,
	#quick-nav-ui a.highlighted {
		color: var(--GW-hyperlink-hover-color);
		background-color: var(--GW-body-background-color);
		box-shadow: 0 0 0 3px #e4e4e4 inset;
	}
	#quick-nav-ui a.highlighted {
		transform: scale(0.9);
	}
	#quick-nav-ui a {
		transition:
			color 0.1s ease,
			background-color 0.1s ease,
			box-shadow 0.1s ease,
			transform 0.1s ease;
	}
	#quick-nav-ui a:hover {
		transition: none;
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
	color: #666;
	text-shadow: 0.5px 0.5px 0 #fff;
}
#new-comment-nav-ui .new-comments-count::after {
	color: #777;
}
#new-comment-nav-ui .new-comment-sequential-nav-button:disabled {
	color: #ddd;
	text-shadow: none;
}

@media only screen and (hover: hover) and (pointer: fine) {
	#new-comment-nav-ui .new-comments-count:hover {
		text-shadow:
			0 0 1px #fff,
			0 0 3px #fff,
			0 0 5px #fff,
			0 0 8px #fff,
			0.5px 0.5px 0 #fff;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button:focus {
		text-shadow: var(--GW-shadow-white-glow);
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button.highlighted {
		color: var(--GW-hyperlink-active-color);
		text-shadow: var(--GW-shadow-white-glow);
		transform: scale(0.9);
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button {
		transition:
			color 0.1s ease,
			text-shadow 0.1s ease,
			transform 0.1s ease;
	}
}

/*=================*/
/* HNS DATE PICKER */
/*=================*/

#hns-date-picker span {
	color: #777;
}
#hns-date-picker input {
	border: 1px solid transparent;
	background-color: #fff;
	color: #888;
}
#hns-date-picker input:focus {
	color: #000;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

#hns-date-picker.flipped {
	background-color: #e4e4e4;
	left: calc(100% - 10px);
}

/*================================*
	MOBILE VERSIONS OF QUICKNAV,
	NEW COMMENT NAV, AND HNS
	DATE PICKER
 *================================*/

@media only screen and (max-width: 960px) {
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
		box-shadow:
			0 0 1px 0 #fff,
			0 0 3px 0 #fff,
			0 0 5px 0 #fff;
		background-color: #fff;
		border-radius: 4px;
	}

	#quick-nav-ui {
		background-color: #fff;
	}
	#quick-nav-ui a::after {
		text-shadow: 0 0 0 currentColor;
	}

	#new-comment-nav-ui {
		background-color: #fff;
		border: 1px solid transparent;
	}
	#new-comment-nav-ui::before {
		color: #777;
		font-weight: bold;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button {
		color: #00c;
		background-color: #e4e4e4;
		outline: 2px solid #fff;
	}
	#new-comment-nav-ui .new-comments-count {
		background-color: #e4e4e4;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button:disabled {
		color: #ccc;
	}

	#hns-date-picker {
		right: 54px;
	}
	#hns-date-picker,
	#hns-date-picker::before {
		background-color: #e4e4e4;
	}
}

/*======================*/
/* ANTI-KIBITZER TOGGLE */
/*======================*/

#anti-kibitzer-toggle button::before,
#anti-kibitzer-toggle button::after {
	background-color: #aaa;
	-webkit-background-clip: text;
	color: transparent;
	text-shadow: rgba(255, 255, 255, 0.5) 0px 1px 1px;
}
#anti-kibitzer-toggle button:hover::before,
#anti-kibitzer-toggle button:hover::after {
	background-color: #777;
}

/*======================*/
/* TEXT SIZE ADJUSTMENT */
/*======================*/

#text-size-adjustment-ui {
	opacity: 1.0;
}
#text-size-adjustment-ui button {
	color: #777;
	font-weight: 400;
	background-color: #e4e4e4;
	box-shadow: 0 0 0 1px var(--GW-body-background-color) inset;
}
#text-size-adjustment-ui button.decrease,
#text-size-adjustment-ui button.increase {
	padding-top: 1px;
}
#text-size-adjustment-ui button.default {
	font-size: 1em;
}
#text-size-adjustment-ui button:hover {
	text-shadow: none;
	background-color: var(--GW-body-background-color);
	box-shadow: 
		0 0 0 1px var(--GW-body-background-color) inset,
		0 0 0 3px #e4e4e4 inset;
}
#text-size-adjustment-ui button:disabled {
	box-shadow: none;
	background-color: var(--GW-body-background-color);
}
#text-size-adjustment-ui::after {
	color: #999;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 1080px) {
	#text-size-adjustment-ui button {
		padding-left: 1px;
		background-color: #ddd;
	}
}

/*=============================*/
/* COMMENTS LIST MODE SELECTOR */
/*=============================*/

#comments-list-mode-selector button {
	background-color: #e4e4e4;
}
#comments-list-mode-selector button:hover,
#comments-list-mode-selector button.selected {
	background-color: var(--GW-body-background-color);
	box-shadow: 0 0 0 2px #e4e4e4 inset;
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
	outline: 2px solid #e4e4e4;
}

.archive-nav *[class^='archive-nav-item'] {
	background-color: #e4e4e4;
	outline: 1px solid #e4e4e4;
}
.archive-nav a:hover,
.archive-nav span[class^='archive-nav-item'] {
	background-color: var(--GW-body-background-color);
	box-shadow: 0 0 0 1px #e4e4e4 inset;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	.archive-nav > *[class^='archive-nav-'] + *[class^='archive-nav-']::before {
		background-color: #e4e4e4;
	}
}

/*==========*/
/* LISTINGS */
/*==========*/

h1.listing {
	font-size: 1.375rem;
	margin: 7px 0 0 0;
}

h1.listing a[href^="http"] {
	color: #00c;
}
h1.listing a[href^="/posts"] {
	color: #000;
}

/*	Listings hover reveal.
	*/
@media only screen and (hover: hover) and (pointer: fine) {
	h1.listing a:hover,
	h1.listing a:focus {
		color: #777;
		background-color: rgba(238, 238, 238, 0.85);
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

h1.listing:not(:focus-within) a:not(:hover) .post-type-prefix {
	opacity: 0.4;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 520px) {
	h1.listing {
		font-size: 1.25rem;
	}
}

/*======*/
/* SPAM */
/*======*/

h1.listing:not(:focus-within).spam {
	opacity: 0.1;
}
h1.listing:not(:focus-within).spam + .post-meta {
	opacity: 0.25;
}
h1.listing.spam:hover,
h1.listing.spam + .post-meta:hover,
h1.listing.spam:hover + .post-meta {
	opacity: 1.0;
}

/*===================*/
/* LISTING POST-META */
/*===================*/

h1.listing + .post-meta {
	font-size: 0.875rem;
	margin: 0 0 1em 0;
	padding: 0 0 0 1px;
}

h1.listing + .post-meta > * {
	margin: 0 1em 0 0;
}

/*	Comment count.
	*/
h1.listing + .post-meta .comment-count:not(.new-comments) {
	color: #000;
}
h1.listing + .post-meta .comment-count span:nth-of-type(2) {
	margin: 0 0 0 2px;
}
h1.listing + .post-meta .comment-count:not(.new-comments) span:nth-of-type(2) {
	display: none;
}

/*	Post section.
	*/
h1.listing + .post-meta .post-section::before {
	width: 1.25em;
	text-align: center;
}
h1.listing + .post-meta .post-section.alignment-forum::before {
	padding-top: 1px;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	h1.listing + .post-meta .post-section {
		overflow: visible;
		order: 1;
	}
	h1.listing + .post-meta .post-section::before {
		position: unset;
	}
}

/*============*/
/* USER PAGES */
/*============*/
/*======================*/
/* SEARCH RESULTS PAGES */
/*======================*/

#content.user-page h1.page-main-heading {
	border-bottom: 1px solid #ddd;
}

#content.user-page h1.listing,
#content.user-page h1.listing + .post-meta,
#content.search-results-page h1.listing,
#content.search-results-page h1.listing + .post-meta {
	background-color: #fff;
}
#content.user-page h1.listing,
#content.search-results-page h1.listing {
	padding: 6px 6px 0 6px;
	margin: 1rem 0 0 0;
}
#content.own-user-page h1.listing,
h1.listing.own-post-listing {
	padding-right: 36px;
}

/*	Listings hover reveal.
	*/
@media only screen and (hover: hover) and (pointer: fine) {
	#content.user-page h1.listing a:hover,
	#content.user-page h1.listing a:focus,
	#content.search-results-page h1.listing a:hover,
	#content.search-results-page h1.listing a:focus {
		background-color: rgba(255, 255, 255, 0.85);
	}
}

#content.user-page h1.listing + .post-meta,
#content.search-results-page h1.listing + .post-meta {
	padding: 8px 8px 3px 36px;
}
#content.user-page h1.listing + .post-meta .post-section::before,
#content.search-results-page h1.listing + .post-meta .post-section::before {
	left: 0.375em;
}

/*=--------------------=*/
/*= Conversations list =*/
/*=--------------------=*/

#content.conversations-user-page h1.listing + .post-meta {
	padding: 8px 8px 3px 8px;
}
#content.conversations-user-page h1.listing + .post-meta > * {
	margin: 0 1em 0 0;
}

/*===============*/
/* CONVERSATIONS */
/*===============*/

/*============*/
/* LOGIN PAGE */
/*============*/

/* “Create account” form */

#signup-form {
	background-color: #e4e4e4;
	--GW-submit-button-background-color: #eee;
}
#signup-form input[type='text']:not(:focus),
#signup-form input[type='password']:not(:focus) {
	background-color: #f4f4f4;
}

/* Log in tip */

.login-container .login-tip {
	background-color: #e4e4e4;
}

/* Message box */

.error-box {
	border: 1px solid #e00;
	background-color: #faa;
}
.success-box {
	border: 1px solid #0c0;
	background-color: #afa;
}
/*============*/
/* ERROR PAGE */
/*============*/

/*=------------=*/
/*= Retry form =*/
/*=------------=*/

.reassurance {
	border-top: 1px solid #ddd;
}
.reassurance .saved-comment-content {
	border: 1px solid #ddd;
	background-color: #ffd;
}

/*===================*/
/* TABLE OF CONTENTS */
/*===================*/

.contents {
	background-color: #eee;
}
.contents li::before {
	color: #999;
}
.post-body .contents a,
.post-body .contents a:hover {
	border: none;
}
.post-body .contents a:hover {
	background-color: #e4e4e4;
	box-shadow: 5px 0 0 0 #ddd;
}
.post-body .contents li:hover::before {
	color: #000;
}

/*=================*/
/* POST NAVIGATION */
/*=================*/

.post-nav-links a,
.post-nav-links a:visited {
	color: var(--GW-hyperlink-color);
	font-weight: normal;
}
.post-nav-links a:hover,
.post-nav-links a:focus {
	color: var(--GW-hyperlink-color-hover);
}
.post-nav-links a:active {
	color: var(--GW-hyperlink-color-active);
}

.post-nav-label {
	opacity: 0.75;
}

@media only screen and (max-width: 900px) {
	.post-nav-item > * {
		background-color: #e4e4e4;
	}
	.sequence-title {
		border-top: 1px solid #fff;
	}
	.post-nav.prev {
		border-right: 1px solid #fff;
	}
	.post-nav.next {
		border-left: 1px solid #fff;
	}
}

/*==================*/
/* POSTS & COMMENTS */
/*==================*/

.body-text a {
	border-bottom: 1px dotted #bbb;
}
.body-text a:hover {
	border-bottom: 1px solid currentColor;
}

.post-body {
	font-size: 1.25rem;
}

.comment-body {
	font-size: 1.1875rem;
}

/*=======*/
/* POSTS */
/*=======*/

h1.post-title {
	font-size: 2em;
	margin: 1.5em 0 0.5em 0;
}

h1.post-title .post-type-prefix {
	opacity: 0.35;
	font-weight: normal;
}

/*===========*/
/* POST-META */
/*===========*/

.bottom-post-meta {
	border-color: #ddd;
}

article > .post-meta > *,
.post .post-meta > * {
	margin: 0 0.5em;
}

.post-meta a:hover {
	text-decoration: underline;
}

/*	Author.
	*/
.post-meta .author,
.post-meta .author:visited {
	color: #090;
}

/*	Date.
	*/
.post-meta .date {
	color: #888;
}

/*	Post section.
	*/
.post-meta .post-section::before,
.comment-meta .alignment-forum {
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

/*============*/
/* LINK POSTS */
/*============*/

.post.link-post a.link-post-link::before {
	font-weight: 400;
}
.post.link-post a.link-post-link:hover::before {
	color: #4879ec;
}
.post.link-post a.link-post-link:focus {
	color: #999;
	border-bottom: 2px dotted currentColor;
}

/*==========*/
/* COMMENTS */
/*==========*/

.comments::before {
	border-top: 1px solid #ddd;
}

.comment-item {
	border: 1px solid var(--GW-comment-parent-background-color);
	background-color: var(--GW-comment-background-color);
}
.comment-parent-link::after {
	box-shadow:
		0 28px 16px -16px var(--GW-comment-parent-background-color) inset,
		4px 16px 0 12px var(--GW-comment-background-color-target) inset,
		4px	4px 0 12px var(--GW-comment-background-color-target) inset;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	.comment-item .comment-item,
	.comment-item .comment-item + .comment-item {
		border-width: 1px 2px 0 1px;
	}
}
@media only screen and (max-width: 520px) {
	.comment-body {
		font-size: 1.125rem;
	}
}

/*==================*/
/* COMMENT LISTINGS */
/*==================*/

.listings .comment-thread .comment-meta a.date:focus,
.listings .comment-thread .comment-meta a.permalink:focus {
	color: #888;
	outline: 2px dotted #999;
	background-color: #fff;
	padding: 0 5px;
	left: -5px;
}
#content.compact .listings .comment-thread .comment-meta a.date:focus,
#content.compact .listings .comment-thread .comment-meta a.permalink:focus {
	padding: 2px 5px 0 5px;
}
.listings .comment-thread .comment-meta a.date:focus + *,
.listings .comment-thread .comment-meta a.permalink:focus + *:not(.comment-post-title) {
	margin-left: -10px;
}

/*===========================*/
/* COMMENT THREAD NAVIGATION */
/*===========================*/

a.comment-parent-link::before {
	color: #ccc;
}
a.comment-parent-link:hover::before {
	color: #999;
}

.comment-child-link::before {
	color: #aaa;
}

.comment-item-highlight,
.comment-item-highlight-faint {
	outline: 2px solid var(--GW-comment-highlight-color);
	border: 1px solid var(--GW-comment-highlight-color);
}

/*================================*/
/* DEEP COMMENT THREAD COLLAPSING */
/*================================*/

.comment-item input[id^="expand"]:checked ~ .comment-thread .comment-thread .comment-item {
	border-width: 1px 0 0 0;
}

/*==============*/
/* COMMENT-META */
/*==============*/

.comment-meta .author {
	font-size: 1.125em;
	color: #3e9535;
	text-shadow: 0 0 0 currentColor;
}
.comment-item .author:not(.redacted).original-poster::after {
	filter: brightness(60%) opacity(60%);
}

.comment-meta .date {
	color: #666;
}

.comment-meta a:hover {
	text-decoration: underline;
}

/*	Karma controls.
	*/
.comment-item .karma.active-controls::after,
.comment-item .karma .karma-value::after,
.post .karma.active-controls::after,
.post .karma .karma-value::after,
.author::before {
	background-color: #e4e4e4;
	color: #444;
	box-shadow: 0 0 0 1px #fff inset;
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
	min-width: 60px;
}

/*====================*/
/* ANTI-KIBITZER MODE */
/*====================*/

.author.redacted,
.inline-author.redacted {
	opacity: 0.6;
}

.karma-value.redacted {
	opacity: 0.4;
}

.link-post-domain.redacted {
	opacity: 0.4;
}

/*===========================*/
/* HIGHLIGHTING NEW COMMENTS */
/*===========================*/

.new-comment::before {
	outline: 2px solid var(--GW-comment-item-outline-color);
}

/*====================*/
/* COMMENT PERMALINKS */
/*====================*/
/*==================*/
/* COMMENT LW LINKS */
/*==================*/

.comment-meta .permalink::before,
.comment-meta .lw2-link::before,
.comment-meta .comment-parent-link span::before,
.post .post-meta .lw2-link::before {
	filter: saturate(25%) opacity(0.25);
	opacity: 1.0;
}

/*=================================*/
/* COMMENT THREAD MINIMIZE BUTTONS */
/*=================================*/

.comment-item.depth-odd > .comment > .comment-meta .comment-minimize-button {
	color: #ddd;
}
.comment-item.depth-even > .comment > .comment-meta .comment-minimize-button {
	color: #ccc;
}
.comment-item > .comment > .comment-meta .comment-minimize-button:hover,
.comment-item > .comment > .comment-meta .comment-minimize-button:hover::after {
	color: #aaa;
}
.comment-minimize-button::after {
	color: #bbb;
}

/*==============*/
/* VOTE BUTTONS */
/*==============*/

.vote::before {
	content: "";
	border-radius: 50%;
	background-size: 17px 17px;
	width: 17px;
	height: 17px;
	display: inline-block;
	position: relative;
	top: 2.5px;
}
.vote:active {
	transform: none;
}
.vote:hover::before,
.vote.selected::before,
.vote.clicked-once::before,
.vote.clicked-twice::before {
	filter: drop-shadow(0 0 1px #fff);
}

@media only screen and (hover: hover) and (pointer: fine) {
	.vote {
		filter: saturate(50%) opacity(50%);
	}
	.comment:hover .vote,
	.comment:hover + .comment-controls .vote,
	.karma.active-controls:hover .vote {
		filter: none;
	}
}

.upvote::before,
.waiting .upvote.big-vote.clicked-twice::before {
	background-image: url('data:image/svg+xml;base64,<?php echo base64_encode(file_get_contents("assets/upvote-green-circle-plus.svg")) ?>');
	filter: grayscale(100%) brightness(128%);
}
.downvote::before,
.waiting .downvote.big-vote.clicked-twice::before {
	background-image: url('data:image/svg+xml;base64,<?php echo base64_encode(file_get_contents("assets/downvote-red-circle-minus.svg")) ?>');
	filter: grayscale(100%) brightness(188%);
}

.vote.clicked-once::before,
.vote.big-vote.clicked-once::before {
	box-shadow:
		0 0 0 1px #fff,
		0 0 0 4px #c8c8c8,
		0 0 0 5px transparent;
}

.vote.big-vote.clicked-twice::before,
.waiting .vote.big-vote:not(.clicked-twice)::before,
.waiting .vote:not(.big-vote).clicked-once::before {
	box-shadow: none;
}

.upvote.clicked-twice::before,
.upvote.big-vote::before {
	box-shadow:
		0 0 0 1px #fff,
		0 0 0 4px #00d800,
		0 0 0 5px transparent;
}

.downvote.clicked-twice::before,
.downvote.big-vote::before {
	box-shadow:
		0 0 0 1px #fff,
		0 0 0 4px #eb4c2a,
		0 0 0 5px transparent;
}

/*===========================*/
/* COMMENTING AND POSTING UI */
/*===========================*/

.posting-controls input[type='submit'] {
	padding: 6px 12px;
}

@media only screen and (hover: hover) and (pointer: fine) {
	.comment-item .comment-controls .action-button {
		filter: saturate(50%) opacity(50%);
	}
	.comment-item .comment:hover + .comment-controls .action-button,
	.comment-item .comment-controls:hover .action-button {
		filter: none;
	}
}

.comment-controls .cancel-comment-button {
	color: #c00;
	text-shadow: 0 0 1px #fff;
}
.comment-controls .cancel-comment-button::before {
	font-weight: 400;
}
.comment-controls .cancel-comment-button:hover {
	color: #f00;
	text-shadow: var(--GW-shadow-white-glow);
}

.new-comment-button {
	background-color: #e4e4e4;
	padding: 10px 14px 11px 14px;
}
.new-comment-button:hover {
	box-shadow: 0 0 0 2px #e4e4e4 inset;
}

.comment-controls .delete-button,
.comment-controls .retract-button {
	color: #c00;
}
.comment-controls .delete-button::before,
.comment-controls .retract-button::before, 
.comment-controls .unretract-button::before {
	font-size: 0.9375em;
	font-weight: 400;
	opacity: 0.65;
}
.comment-controls .edit-button,
.comment-controls .unretract-button {
	color: #090;
}
.comment-controls .action-button:hover {
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
	color: #000;
	border-color: #fff;
}
.comment-item.depth-odd > .posting-controls textarea {
	border-color: #ddd;
}
.posting-controls textarea:focus {
	border-color: #00e;
	box-shadow: 0 0 0 1px #00e;
}
.posting-controls.edit-existing-post textarea:focus,
.posting-controls form.edit-existing-comment textarea:focus {
	border-color: #090;
	box-shadow: 0 0 0 1px #090;
}

/*= Scroll bars =*/

.posting-controls textarea::-webkit-scrollbar {
	width: 16px;
	background-color: transparent;
}
.posting-controls textarea::-webkit-scrollbar-track {
	border-left: 1px solid #e4e4e4;
}
.posting-controls textarea:focus::-webkit-scrollbar-track {
	box-shadow: -2px 0 0 0 #0040ff;
}
.posting-controls textarea::-webkit-scrollbar-thumb {
	background-color: #acacff;
	box-shadow:
		1px 0 0 0 #acacff,
		1px 0 0 0 #e4e4e4 inset,
		2px 0 0 0 #fff inset;
}
.posting-controls textarea:focus::-webkit-scrollbar-thumb {
	background-color: #0040ff;
	box-shadow:
		1px 0 0 0 #fff,
		1px 0 0 0 #fff inset,
		-2px 0 0 0 #0040ff;
}

.posting-controls.edit-existing-post textarea:focus::-webkit-scrollbar-track,
.posting-controls form.edit-existing-comment textarea:focus::-webkit-scrollbar-track {
	box-shadow: -2px 0 0 0 #090;
}
.posting-controls.edit-existing-post textarea:focus::-webkit-scrollbar-thumb,
.posting-controls form.edit-existing-comment textarea:focus::-webkit-scrollbar-thumb {
	background-color: #090;
	box-shadow:
		1px 0 0 0 #fff,
		1px 0 0 0 #fff inset,
		-2px 0 0 0 #090;
}

/* GUIEdit buttons */

.guiedit-buttons-container {
	background-color: #e4e4e4;
}

.posting-controls.edit-existing-post .guiedit-buttons-container button,
.posting-controls form.edit-existing-comment .guiedit-buttons-container button {
	color: #050;
}

button.guiedit sup {
	font-weight: bold;
}
.guiedit::after {
	color: #777;
	text-shadow: none;
}

/* Markdown hints */

#markdown-hints-checkbox + label:hover {
	text-shadow: var(--GW-shadow-white-glow);
}
#markdown-hints {
	border: 1px solid #c00;
	background-color: #ffa;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 520px) {
	.textarea-container:focus-within textarea {
		box-shadow:
			0 0 0 1px #00e,
			0 0 0 2px #fff;
	}
	.textarea-container:focus-within .guiedit-mobile-auxiliary-button {
		padding: 5px 6px 7px 6px;
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
	.textarea-container:focus-within button.guiedit {
		padding: 9px 1px 9px 0;
		border: 1px solid #00c;
	}
	#markdown-hints::after {
		color: #090;
	}
}

/*================*/
/* EDIT POST FORM */
/*================*/

/*	Checkboxes.
	*/
#edit-post-form .post-meta-fields input[type='checkbox'] + label {
	border: none;
	padding-top: 5px;
}
#edit-post-form .post-meta-fields input[type='checkbox'] + label::before {
	background-color: #fff;
	color: #777;
	border-color: var(--GW-body-background-color);
	border-style: solid;
	border-width: 0 1px 1px 0;
}
@media only screen and (hover: hover) and (pointer: fine) {
	#edit-post-form .post-meta-fields input[type='checkbox'] + label:hover,
	#edit-post-form .post-meta-fields input[type='checkbox']:focus + label {
		box-shadow: 0 0 0 2px #fff inset;
	}
	#edit-post-form .post-meta-fields input[type='checkbox'] + label:hover::before,
	#edit-post-form .post-meta-fields input[type='checkbox']:focus + label::before {
		border-color: #aaa;
	}
}
#edit-post-form .post-meta-fields input[type='checkbox']:checked + label::before {
	background-color: #999;
	box-shadow: 0 0 0 3px #fff inset;
}

/*	Section selector.
	*/
#edit-post-form input[type='radio'] + label {
	color: var(--GW-hyperlink-color);
	background-color: #e4e4e4;
	border: 1px solid #e4e4e4;
}
#edit-post-form input[type='radio']#meta + label,
#edit-post-form input[type='radio']#drafts + label {
	border-left-width: 0;
}
#edit-post-form input[type='radio'] + label:hover,
#edit-post-form input[type='radio']:focus + label,
#edit-post-form input[type='radio']:checked + label {
	background-color: transparent;
	box-shadow: 0 0 0 1px #e4e4e4 inset;
}
#edit-post-form input[type='radio']:checked + label {
	color: #000;
	text-shadow: 0 0 0 currentColor;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 520px) {
	.post-controls .edit-post-link {
		text-shadow:
			0 0 1px #fff,
			0 0 3px #0f0;
	}
	.post-controls .edit-post-link:hover,
	.post-controls .edit-post-link:active {
		text-shadow:
			0 0 1px #fff,
			0 0 3px #f88;
	}
}

/*===========*/
/* SEQUENCES */
/*===========*/

.sequence-text {
	font-size: 1.125rem;
}

h1.sequence-chapter {
	font-size: 2rem;
}

#content.sequences-page::after {
	font-weight: bold;
	background-color: #444;
	-webkit-background-clip: text;
	color: transparent;
	text-shadow: 
		rgba(255, 255, 255, 0.5) 0px 3px 3px;
}

/*=======*/
/* LINKS */
/*=======*/

a {
	text-decoration: none;
}

/*=========*/
/* BUTTONS */
/*=========*/

button:active,
input[type='submit']:active, 
.button:active {
	transform: scale(0.9);
}
.button:focus:not(:hover) {
	transform: none;
}

@media only screen and (hover: hover) and (pointer: fine) {
	button:hover,
	button:focus,
	input[type='submit']:hover,
	input[type='submit']:focus,
	.button:hover,
	.button:focus {
		text-shadow: var(--GW-shadow-white-glow);
	}
}

input[type='submit'] {
	background-color: var(--GW-submit-button-background-color);
}
input[type='submit']:hover,
input[type='submit']:focus {
	background-color: inherit;
	box-shadow: 0 0 0 2px var(--GW-submit-button-background-color) inset;
}

/*==========*/
/* HEADINGS */
/*==========*/

.body-text h5,
.body-text h6 {
	font-size: 1em;
}
.body-text h4 {
	font-size: 1.15em;
}
.body-text h3 {
	font-size: 1.3em;
}
.body-text h2 {
	font-size: 1.5em;
}
.body-text h1 {
	font-size: 1.75em;
}

.body-text h6 {
	color: #555;
}

/*========*/
/* QUOTES */
/*========*/

blockquote {
	border-left: 5px solid #ddd;
}

/*========*/
/* IMAGES */
/*========*/

#content img,
#content figure.image img {
	border: 1px solid #fff;
}
#content figure img {
	border: 1px solid #fff;
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
	border: 1px solid #ddd;
}

/*======*/
/* MISC */
/*======*/

hr {
	border-bottom: 2px solid #ddd;
}

code {
	background-color: #eee;
	border: 1px solid #eee;
	box-shadow: 0 0 0 1px #e4e4e4;
	text-shadow:
		0 0 1px #fff,
		0 0 2px #fff;
}

input[type='text'],
input[type='search'],
input[type='password'] {
	border: 1px solid transparent;
	color: #000;
	background-color: #fff;
}
input[type='text']:focus,
input[type='search']:focus,
input[type='password']:focus {
	outline: 2px solid #00e;
}

select {
	color: #000;
}

u {
	text-decoration: none;
	border-bottom: 0.05em solid currentColor;
}

/*============*/
/* ABOUT PAGE */
/*============*/

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
	text-shadow: var(--GW-shadow-white-glow);
}

.qualified-linking label:hover {
	text-shadow:
		0 0 1px #fff,
		0 0 3px #fff,
		0 0 5px #00e;
}

.qualified-linking-toolbar {
	border: 1px solid #fff;
	background-color: #fff;
}
.qualified-linking-toolbar a,
.qualified-linking-toolbar button {
	background-color: #eee;
	border: 1px solid #eee;
}
.qualified-linking-toolbar a:hover,
.qualified-linking-toolbar button:hover {
	text-decoration: none;
	background-color: #fff;
	box-shadow: 0 0 0 1px #eee inset;
}
.qualified-linking label::after {
	background-color: #eee;
	opacity: 0.8;
}

/*======*/
/* MATH */
/*======*/

.mathjax-block-container .mjx-chtml::-webkit-scrollbar {
	height: 12px;
}

.mjx-chtml::-webkit-scrollbar {
	background-color: #fff;
	border-top: 1px solid #ccc;
	border-bottom: 1px solid #ccc;
	box-shadow:
		-1px 0 0 0 #ccc,
		1px 0 0 0 #ccc;
}
.mjx-chtml::-webkit-scrollbar-thumb {
	background-color: #ccc;
	border-top: 1px solid #ccc;
	border-bottom: 1px solid #ccc;
	box-shadow:
		 0  -1px 0 0 #fff inset,
		 0   1px 0 0 #fff inset,
		 1px 0   0 0 #fff inset,
		-1px 0   0 0 #fff inset;
}

.mathjax-inline-container .mjx-chtml::-webkit-scrollbar {
	height: 8px;
}

#keyboard-help-overlay .keyboard-help-container {
	background-color: #eee;
}

/*=-------------=*/
/*= Scroll bars =*/
/*=-------------=*/

#keyboard-help-overlay .keyboard-help-container::-webkit-scrollbar {
	width: 14px;
	background-color: transparent;
}
#keyboard-help-overlay .keyboard-help-container::-webkit-scrollbar-track {
	border-left: 1px solid #ddd;
}
#keyboard-help-overlay .keyboard-help-container::-webkit-scrollbar-thumb {
	background-color: #ccc;
	box-shadow:
		1px 0 0 0 #eee inset,
		-1px 0 0 0 #eee inset,
		0 1px 0 0 #eee inset,
		0 -1px 0 0 #eee inset;
	border-left: 1px solid #ddd;
}
#keyboard-help-overlay .keyboard-help-container::-webkit-scrollbar-thumb:hover {
	background-color: #aaa;
}

/*=--------------------=*/
/*= Dividers & heading =*/
/*=--------------------=*/

#keyboard-help-overlay .keyboard-help-container h1 {
	font-size: 1.25em;
}
#keyboard-help-overlay .keyboard-help-container h1,
#keyboard-help-overlay .keyboard-help-container .keyboard-shortcuts-lists {
	border-color: #ddd;
}

/*=------=*/
/*= Keys =*/
/*=------=*/

#keyboard-help-overlay .keyboard-help-container code {
	background-color: #fff;
	border: 1px solid #ccc;
}
#keyboard-help-overlay .keyboard-help-container code.ak {
	background-color: #fff289;
	border-color: #d4a500;
}

/*=--------=*/
/*= Labels =*/
/*=--------=*/
#keyboard-help-overlay .keyboard-help-container ul li.section {
	font-weight: bold;
}

/*=--------------=*/
/*= Close button =*/
/*=--------------=*/

#keyboard-help-overlay button.close-keyboard-help:hover {
	background-color: #eee;
	box-shadow:
		0 1px 0 0 #eee,
		0 1px 0 1px #ddd;
}
#keyboard-help-overlay button.close-keyboard-help:active {
	box-shadow:
		0 0 0 1px #ddd;
}
/*=================*/
/* ALIGNMENT FORUM */
/*=================*/

#content.alignment-forum-index-page {
	--GW-content-background-color: #eaedff;
}
#content.alignment-forum-index-page::after {
	background-color: #7f85b2;
	-webkit-background-clip: text;
	color: transparent;
	text-shadow:
		rgba(255, 255, 255, 0.5) 0px 3px 3px;
}

/*	Hover styles.
	*/
@media only screen and (hover: hover) and (pointer: fine) {
	#content.alignment-forum-index-page h1.listing a:hover,
	#content.alignment-forum-index-page h1.listing a:focus {
		background-color: rgba(234, 237, 255, 0.85);
	}
}
