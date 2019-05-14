/******************************/
/* READTHESEQUENCES.COM THEME */
/******************************/

/*===========*/
/* VARIABLES */
/*===========*/

/*	Typography.
	*/
:root {
	--GW-UI-font: 'Proxima Nova', var(--GW-sans-serif-fallback-font-stack);
	--GW-UI-font-weight-light: 300;
	--GW-UI-font-weight-heavy: 600;

	--GW-body-text-font: 'Garamond Premier Pro', var(--GW-serif-fallback-font-stack);
	--GW-body-text-font-weight: 500;

	--GW-monospaced-font: 'Triplicate', var(--GW-monospaced-fallback-font-stack);

	--GW-nav-item-font-weight: <?php echo ($platform == 'Mac') ? '200' : '300'; ?>;

	--GW-post-listings-font: 'Garamond Premier Pro Subhead', var(--GW-serif-fallback-font-stack);
	--GW-post-listings-font-weight: 400;

	--GW-listings-post-meta-font-weight: 400;

	--GW-post-title-font-weight: 500;
}
@media only screen and (max-width: 900px) {
	:root {
		--GW-nav-item-font-weight: <?php echo ($platform == 'Mac') ? '300' : '400'; ?>;
	}
}

/*	Layout.
	*/
:root {
	--GW-comment-compact-height: 52px;
	--GW-comment-compact-height-mobile: 103px;
	--GW-comment-minimized-height: 37px;
	--GW-comment-minimized-height-mobile: 68px;
}

/*	Color scheme.
	*/
:root {
	--GW-body-background-color: #fffffa;

	--GW-hyperlink-color: #999;
	--GW-hyperlink-hover-color: #333;
	--GW-hyperlink-active-color: #999;

	--GW-shadow-link-glow: 0px 0px 0.5px #333;

	--GW-search-field-placeholder-color: #d00;

	--GW-theme-selector-outline-color: #ddd;
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

#ui-elements-container > div[id$='-ui-toggle'] button  {
	color: #bbb;
	text-shadow:
		0 0 1px var(--GW-body-background-color),
		0 0 3px var(--GW-body-background-color),
		0 0 5px var(--GW-body-background-color),
		0 0 10px var(--GW-body-background-color),
		0 0 20px var(--GW-body-background-color),
		0 0 30px var(--GW-body-background-color);
}

/*==========*/
/* NAV BARS */
/*==========*/

#primary-bar .nav-inner,
#bottom-bar .nav-inner {
	font-size: 1.375em;
}
#secondary-bar .nav-inner {
	font-size: 1em;
}

#primary-bar .nav-inner {
	line-height: 1.5;
}

/*= Decorative bottom bar =*/

#bottom-bar.decorative {
	border: none;
}
#bottom-bar.decorative {
	color: #d8d8d8;
}

/* Accesskey hints */

.nav-inner::after {
	left: 5px;
	top: -2px;
	font-size: 0.7em;
	color: #ddd;
}
#secondary-bar .nav-inner::after {
	color: #d8d8d8;
}
.nav-inner:hover::after {
	color: #bbb;
}
.nav-inner:hover::after {
	color: #bbb;
}

/* This makes the navbar items look like tabs: */

#bottom-bar {
	border-top: 1px solid #ddd;
}
.nav-bar .nav-item:not(:last-child) {
	border-right: 1px solid #ddd;
}

.active-bar {
	border-style: solid;
	border-color: #ddd;
	border-width: 1px 0;
}

.nav-bar .nav-item:not(.nav-current):not(#nav-item-search):hover,
#nav-item-search:focus-within {
	background-color: #f0f0eb;
}

.nav-current .nav-inner {
	font-weight: var(--GW-UI-font-weight-heavy);
}
.nav-bar a:hover,
.nav-bar a:focus,
.nav-bar button:hover,
.nav-bar button:focus {
	text-shadow: var(--GW-shadow-link-glow);
}

/*= Search tab =*/

#nav-item-search input[type='search'] {
	margin: 0;
}
#nav-item-search form::before {
	color: #bbb;
	font-weight: 300;
}
#nav-item-search button {
	font-weight: var(--GW-UI-font-weight-light);
}

/*= Inbox indicator =*/

#inbox-indicator::before {
	color: #e4e4e4;
	padding-top: 1px;
	text-shadow:
		0 0 1px var(--GW-content-background-color),
		0 0 2px var(--GW-content-background-color),
		0 0 3px var(--GW-content-background-color);
}

/*= Recent Comments tab =*/

#nav-item-recent-comments span {
	margin: 0 5px 0 0;
}

/*= Keyboard help button =*/

@media only screen and (min-width: 961px) {
	#nav-item-about button.open-keyboard-help {
		font-weight: 300;
		opacity: 0.75;
	}
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	#nav-item-search button::before {
		font-weight: 400;
	}
	.nav-bar .nav-inner::before {
		font-weight: 300;
	}
	.nav-current .nav-inner::before {
		font-weight: 900;
	}
}

/*===============*/
/* PAGINATION UI */
/*===============*/

#bottom-bar .nav-item a::before,
#top-nav-bar a::before {
	font-size: 1em;
	color: #bbb;
	font-weight: 900;
}
#top-nav-bar a::before {
	bottom: -2px;
}
#top-nav-bar a:hover {
	background-color: transparent;
}
#bottom-bar .nav-item a:hover::before,
#top-nav-bar a:hover::before {
	color: var(--GW-hyperlink-hover-color);
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
#bottom-bar #nav-item-last a::before,
#top-nav-bar a.nav-item-last::before {
	content: "\F0DA\F0DA";
}
#top-nav-bar a::after,
#bottom-bar a::after {
	color: #222;
	text-shadow: none;
}
#top-nav-bar a::after {
	bottom: calc(100% - 11px);
}

/*=-------------------=*/
/*= Top pagination UI =*/
/*=-------------------=*/

#top-nav-bar {
	margin: 0;
	flex-wrap: wrap;
}
#top-nav-bar .page-number {
	font-family: var(--GW-body-text-font);
	padding-top: 7px;
	color: #777;
}
#top-nav-bar .page-number-label {
	font-family: var(--GW-UI-font);
	bottom: 70%;
	color: #222;
}
#top-nav-bar::after {
	content: "";
	background-image:
		linear-gradient(
			to right,
			rgba(0, 0, 0, 0),
			rgba(0, 0, 0, 0.25),
			rgba(0, 0, 0, 0)
		);
	background-origin: content-box;
	background-repeat: no-repeat;
	margin: 0.125em auto 0 auto;
	padding: 0 12.5%;
	height: 1px;
	flex: 1 0 100%;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	#top-nav-bar .page-number {
		padding-top: 11px;
	}
	#top-nav-bar::after {
		margin: 0 auto;
	}
}

/*==============*/
/* PAGE TOOLBAR */
/*==============*/

.page-toolbar a:hover,
.page-toolbar button:hover {
	text-shadow: var(--GW-shadow-link-glow);
	background-color: transparent;
}
.page-toolbar .button::before,
.page-toolbar .rss::before {
	font-weight: 300;
}
.page-toolbar .rss::before {
	opacity: 0.85;
}
.page-toolbar .rss:hover::before {
	font-weight: 900;
	opacity: 1.0;
}

/*==============*/
/* SUBLEVEL NAV */
/*==============*/

.sublevel-nav .sublevel-item {
	background-color: var(--GW-body-background-color);
}

.sublevel-nav:not(.sort) .sublevel-item {
	border-style: solid;
	border-color: #ddd;
	border-width: 1px 1px 1px 0;
	font-weight: var(--GW-UI-font-weight-light);
}
.sublevel-nav:not(.sort) .sublevel-item:last-child {
	border-width: 1px 0;
}

.sublevel-nav .sublevel-item:not(.selected):hover {
	background-color: #f0f0eb;
	color: #000;
	text-shadow: var(--GW-shadow-link-glow);
}
.sublevel-nav .sublevel-item.selected {
	font-weight: var(--GW-UI-font-weight-heavy);
	color: #000;
}

/*=====================*/
/* SORT ORDER SELECTOR */
/*=====================*/

.sublevel-nav.sort {
	padding: 18px 0 0 0;
}
.sublevel-nav.sort .sublevel-item {
	letter-spacing: 0.5px;
	padding: 7px 6px 5px 7px;
	text-transform: uppercase;
	box-shadow:
		0 0 0 1px #ddd;
}
.sublevel-nav.sort .sublevel-item.selected {
	background-color: transparent;
}
.sublevel-nav.sort::before {
	text-transform: uppercase;
	color: #aaa;
	text-shadow:
		0.5px 0.5px 0 #fff;
}
.sublevel-nav.sort::after {
	content: "";
	position: absolute;
	top: 17px;
	left: 0;
	width: 100%;
	height: calc(100% - 16px);
	box-shadow:
		0 0 0 1px var(--GW-content-background-color);
}

/*================*/
/* WIDTH SELECTOR */
/*================*/

#width-selector button {
	box-shadow: 
		0 0 0 4px var(--GW-body-background-color) inset, 
		0 0 0 5px #ddd inset;
}
#width-selector button:hover,
#width-selector button.selected {
	box-shadow: 
		0 0 0 1px #ddd inset,
		0 0 0 4px var(--GW-body-background-color) inset,
		0 0 0 5px #ddd inset;
	background-color: transparent;
}
#width-selector button {
	color: #ccc;
}

/*================*/
/* THEME SELECTOR */
/*================*/

#theme-selector button {
	box-shadow: 
		0 0 0 4px var(--GW-body-background-color) inset, 
		0 0 0 5px #ccc inset;
}
#theme-selector button:hover,
#theme-selector button.selected {
	box-shadow: 
		0 0 0 1px #ccc inset,
		0 0 0 4px var(--GW-body-background-color) inset,
		0 0 0 5px #ccc inset;
}

#theme-selector button::before {
	color: #bbb;
}
#theme-selector button:hover::before,
#theme-selector button.selected::before {
	color: #444;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 960px) {
	#theme-selector {
		background-color: #fffffa;
		box-shadow: 
			0 0 0 1px #ddd,
			0 0 1px 3px #fffffa,
			0 0 3px 3px #fffffa,
			0 0 5px 3px #fffffa,
			0 0 10px 3px #fffffa,
			0 0 20px 3px #fffffa;
	}
	#theme-selector::before {
		color: #999;
		font-weight: 200;
	}
	#theme-selector button::after {
		color: #888;
		max-width: calc(100% - 3.5em);
		overflow: hidden;
		text-overflow: ellipsis;
	}
	#theme-selector button:hover::after,
	#theme-selector button.selected::after {
		color: #000;
	}
	#theme-selector .theme-selector-close-button {
		color: var(--GW-content-background-color);
		text-shadow:
			1px 1px 0 #ccc,
			0 0 8px #ccc;
	}
}

/*======================*/
/* THEME TWEAKER TOGGLE */
/*======================*/

#theme-tweaker-toggle button {
	color: #aaa;
}
#theme-tweaker-toggle button:hover {
	color: #888;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 1000px) {
	#theme-tweaker-toggle button {
		background-color: var(--GW-content-background-color);
	}
}

/*=================*/
/* QUICKNAV WIDGET */
/*=================*/

#quick-nav-ui a {
	box-shadow: 0 0 0 1px #ccc inset;
	color: #bbb;
}
#quick-nav-ui a[href='#bottom-bar'] {
	line-height: 1.9;
}
#quick-nav-ui a:active {
	transform: scale(0.9);
}
#quick-nav-ui a[href='#top'],
#quick-nav-ui a[href='#bottom-bar'] {
	font-weight: 400;
}
#quick-nav-ui a[href='#comments'],
#quick-nav-ui a[href='#answers'] {
	font-weight: 300;
}
#quick-nav-ui a[href='#comments'].no-comments,
#quick-nav-ui a[href='#answers'].no-answers {
	opacity: 0.5;
	color: #d8d8d8;
}
@media only screen and (hover: hover) and (pointer: fine) {
	#quick-nav-ui a:hover,
	#quick-nav-ui a:focus,
	#quick-nav-ui a.highlighted {
		background-color: #f0f0eb;
		color: #333;
		text-shadow: 0px 0px 0.5px #333;
	}
	#quick-nav-ui a.highlighted {
		transform: scale(0.9);
	}
	#quick-nav-ui a {
		transition:
			color 0.1s ease,
			background-color 0.1s ease,
			text-shadow 0.1s ease,
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

#new-comment-nav-ui .new-comments-count,
#new-comment-nav-ui .new-comments-count::after {
	font-weight: var(--GW-UI-font-weight-light);
}
#new-comment-nav-ui .new-comments-count {
	color: #444;
}
#new-comment-nav-ui .new-comments-count::after {
	color: #000;
}
#new-comment-nav-ui .new-comment-sequential-nav-button:disabled {
	color: #e6e6e6;
}

@media only screen and (hover: hover) and (pointer: fine) {
	#new-comment-nav-ui .new-comment-sequential-nav-button.highlighted {
		color: var(--GW-hyperlink-hover-color);
		transform: scale(0.9);
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button {
		transition:
			color 0.1s ease,
			transform 0.1s ease;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button:hover {
		transition: none;
	}
}

/*=================*/
/* HNS DATE PICKER */
/*=================*/

#hns-date-picker input {
	color: #777;
	font-weight: var(--GW-UI-font-weight-light);
}
#hns-date-picker input:hover,
#hns-date-picker input:focus {
	color: #000;
}
#hns-date-picker span {
	color: #888;
}

/*================================*
	MOBILE VERSIONS OF QUICKNAV,
	NEW COMMENT NAV, AND HNS
	DATE PICKER
 *================================*/

@media only screen and (max-width: 960px) {
	#quick-nav-ui {
		background-color: #fffffa;
	}
	#quick-nav-ui,
	#new-comment-nav-ui {
		box-shadow:
			0 0 1px 3px #fffffa,
			0 0 3px 3px #fffffa,
			0 0 5px 3px #fffffa,
			0 0 10px 3px #fffffa,
			0 0 20px 3px #fffffa;
	}
	#quick-nav-ui a::after,
	#new-comment-nav-ui::before {
		font-weight: bold;
		box-shadow:
			0 0 1px 0 #fffffa,
			0 0 3px 0 #fffffa,
			0 0 5px 0 #fffffa;
		background-color: #fffffa;
	}
	#quick-nav-ui a::after {
		color: #888;
	}
	#new-comment-nav-ui {
		background-color: #fffffa;
		border: 1px solid #ccc;
	}
	#new-comment-nav-ui::before {
		color: #777;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button {
		box-shadow: 0 0 0 1px #ccc;
		color: #777;
	}
	#new-comment-nav-ui .new-comments-count {
		box-shadow: 0 -1px 0 0 #ccc;
	}
	#hns-date-picker {
		box-shadow: 0 0 0 2px #ccc;
	}
}

/*======================*/
/* ANTI-KIBITZER TOGGLE */
/*======================*/

#anti-kibitzer-toggle button {
	background-color: #ccc;
	-webkit-background-clip: text;
	color: transparent;
	text-shadow: rgba(255, 255, 255, 0.5) 0px 1px 1px;
}
#anti-kibitzer-toggle button:hover {
	background-color: #777;
}
#anti-kibitzer-toggle button:active {
	background-color: #222;
}

/*======================*/
/* TEXT SIZE ADJUSTMENT */
/*======================*/

#text-size-adjustment-ui button {
	color: #777;
	font-weight: 400;
}
#text-size-adjustment-ui button.default {
	font-weight: var(--GW-UI-font-weight-light);
}
#text-size-adjustment-ui button:disabled:hover {
	text-shadow: none;
}
#text-size-adjustment-ui::after {
	color: #aaa;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 1080px) {
	#text-size-adjustment-ui {
		border-style: solid;
		border-color: #ccc;
		border-width: 0 1px;
	}
	#text-size-adjustment-ui button {
		padding: 1px 0 0 1px;
	}
	#text-size-adjustment-ui button.default {
		padding: 0 0 0 1px;
	}
	#text-size-adjustment-ui button:nth-of-type(n+2) {
		box-shadow:
			0 1px 0 0 #ccc;
	}
}

/*=============================*/
/* COMMENTS LIST MODE SELECTOR */
/*=============================*/

#comments-list-mode-selector button {
	color: #ccc;
	box-shadow:
		0 0 0 4px var(--GW-body-background-color) inset,
		0 0 0 5px #ddd inset;
}
#comments-list-mode-selector button:hover,
#comments-list-mode-selector button.selected {
	box-shadow: 
		0 0 0 1px #ddd inset,
		0 0 0 4px var(--GW-body-background-color) inset,
		0 0 0 5px #ddd inset;
	background-color: transparent;
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
	outline: 1px solid transparent;
}
.archive-nav *[class^='archive-nav-item'] {
	background-color: var(--GW-content-background-color);
	outline: 1px solid #ddd;
	font-weight: var(--GW-UI-font-weight-light);
}
.archive-nav span[class^='archive-nav-item'] {
	font-weight: var(--GW-UI-font-weight-heavy);
}

.archive-nav a:hover {
	background-color: #f0f0eb;
	text-shadow: var(--GW-shadow-link-glow);
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	.archive-nav > *[class^='archive-nav-'] + *[class^='archive-nav-']::before {
		background-color: #ddd;
	}
}

/*==========*/
/* LISTINGS */
/*==========*/

h1.listing {
	margin: 0.7em 0 0 0;
	justify-content: center;
	text-align: center;
}

h1.listing a[href^="http"] {
	color: #888;
	line-height: 1.2;
}
h1.listing a[href^='/'] {
	color: #690010;
	text-shadow: 0.5px 0.5px 0.5px #ffaca5;
}

@media only screen and (hover: hover) and (pointer: fine) {
	h1.listing a:hover,
	h1.listing a:focus {
		text-shadow: 
			0px 0px 0.5px #ffaca5, 
			0px 0px 1.5px #de7069,
			0.5px 0.5px 0.5px #ff988f;
		background-color: rgba(255, 255, 250, 0.85);
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

/*=------------------=*/
/*= Post type prefix =*/
/*=------------------=*/

h1.listing .post-type-prefix {
	width: 1.5em;
}
h1.listing:not(:focus-within) a:not(:hover) .post-type-prefix {
	opacity: 0.5;
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
	justify-content: center;
	text-align: center;
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

/*	Comment count.
	*/
h1.listing + .post-meta .comment-count:not(.new-comments) {
	opacity: 0.6;
}
h1.listing + .post-meta .comment-count span:nth-of-type(2) {
	margin: 0 0 0 2px;
}
h1.listing + .post-meta .comment-count:not(.new-comments) span:nth-of-type(2) {
	display: none;
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

#content.user-page #top-nav-bar {
	margin: 0 0 0.25em 0;
}

#content.user-page h1.listing,
#content.search-results-page h1.listing {
	padding: 7px 8px 0 8px;
	margin: 1rem 0 0 0;
}
#content.own-user-page h1.listing,
h1.listing.own-post-listing {
	padding-right: 36px;
}

#content.user-page h1.listing::after,
#content.search-results-page h1.listing::after{
	content: "";
	position: absolute;
	top: 0;
	left: 0;
	width: 100%;
	height: calc(100% + 1.25em);
	background-color: var(--GW-content-background-color);
	filter: drop-shadow(0 0 3px #777);
}
#content.user-page h1.listing.link-post-listing::after,
#content.search-results-page h1.listing.link-post-listing::after{
	height: calc(100% + 2em);
}

#content.user-page h1.listing + .post-meta,
#content.search-results-page h1.listing + .post-meta {
	padding: 8px;
}

/*=--------------------=*/
/*= Conversations list =*/
/*=--------------------=*/

#content.conversations-user-page h1.listing {
	padding: 8px 8px 0 8px;
	font-size: 1.75rem;
}

/*===============*/
/* CONVERSATIONS */
/*===============*/

#content.conversation-page h1.page-main-heading {
	font-weight: var(--GW-post-listings-font-weight);
	text-shadow: 
			  0px   0px 0.5px #ddd, 
			  0px   0px 1.0px #aaa,
			0.5px 0.5px 0.5px #ddd;
}

/*============*/
/* LOGIN PAGE */
/*============*/

/*=-----------------------=*/
/*= “Create account” form =*/
/*=-----------------------=*/

#signup-form {
	border: 1px solid #ddd;
}

/*=------------=*/
/*= Log in tip =*/
/*=------------=*/

.login-container .login-tip {
	border: 1px solid #eee;
}

/*=-------------=*/
/*= Message box =*/
/*=-------------=*/

.error-box {
	border: 1px solid red;
	background-color: #faa;
}
.success-box {
	border: 1px solid green;
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
	font-family: var(--GW-body-text-font);
}
.contents-head::after {
	content: "";
	background-image:
		linear-gradient(
			to right,
			rgba(0, 0, 0, 0),
			rgba(0, 0, 0, 0.25),
			rgba(0, 0, 0, 0)
		);
	display: block;
	margin: 0 auto 0.5em auto;
	height: 1px;
}
.contents a::after {
	display: none;
}
.post-body .contents ul {
	padding: 0 1em;
}
.post-body .contents a,
.post-body .contents a:visited,
.post-body .contents a:hover {
	color: #690010;
	font-weight: var(--GW-UI-font-weight-heavy);
}
.post-body .contents a:hover {
	border: none;
	background-color: #f0f0eb;
	box-shadow: -1px -1px 0 0 #ddd inset;
	text-shadow: 
		0px 0px 0.5px #ffaca5;
}
.post-body .contents li::before {
	color: #999;
	font-feature-settings: 'onum';
}
.post-body .contents li:hover::before {
	color: #000;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	.contents {
		margin-left: auto;
	}
}

/*=================*/
/* POST NAVIGATION */
/*=================*/

.post-nav-links a,
.post-nav-links a:visited {
	font-weight: var(--GW-UI-font-weight-light);
}
.post-nav-links a:hover {
	text-shadow: var(--GW-shadow-link-glow);
	background-color: #f0f0eb;
}

.post-nav-label {
	opacity: 0.85;
	font-weight: 300;
}

.sequence-title {
	border-color: #ddd;
	border-style: solid;
	border-width: 0 1px;
}

.post-nav.prev {
	padding-left: 2px;
}
.post-nav.next {
	padding-right: 2px;
}
.post-nav.prev .post-nav-title::before,
.post-nav.next .post-nav-title::after {
	opacity: 0.6;
}

@media only screen and (max-width: 900px) {
	.post-nav.prev,
	.post-nav.next {
		padding: 0.75em 0.5em 0.5em 0.5em;
	}
	.sequence-title {
		border-width: 1px 0 0 0;
	}
	.post-nav.prev {
		border-right: 1px solid #ddd;
	}
	.post-nav.next {
		border-left: 1px solid #ddd;
	}
}

/*==================*/
/* POSTS & COMMENTS */
/*==================*/

.body-text {
	line-height: 1.45;
}

.body-text a,
.body-text a:visited {
	color: inherit;
}
.body-text a:link::after,
.body-text a:visited::after {
	position: relative;
	content: "﻿°";
	margin-left: 2px;
	margin-right: 1px;
	color: #933;
}
.body-text a:hover {
	color: #999;
	border-bottom: 1px dotted #999;
}

.post-body {
	font-size: 1.375rem;
}
.comment-body {
	font-size: 1.25rem;
}

/*=======*/
/* POSTS */
/*=======*/

.post-page .top-post-meta::after {
	content: "❦";
	display: block;
	margin: 0.625em 0 -0.625em 0;
	font-size: 1.5rem;
	flex-basis: 100%;
	order: 2;
	background-color: #ccc;
	-webkit-background-clip: text;
	color: transparent;
	text-shadow: rgba(255, 255, 255, 0.5) 0px 1px 1px;
}

h1.post-title {
	text-shadow:
		0px 0px 0.5px #eee, 
		0px 0px 1.5px #ddd,
		0.5px 0.5px 0.5px #ccc;
}

h1.post-title .post-type-prefix {
	opacity: 0.35;
}

/*===========*/
/* POST-META */
/*===========*/

.post .bottom-post-meta {
	border-color: #ddd;
	padding-bottom: 18px;
}

.post-meta {
	text-align: center;
}

.post-meta > * {
	color: #999;
	opacity: 0.7;
	margin: 0 1em 0 0;
}
.post-meta a {
	opacity: 1.0;
}
.post .post-meta .karma {
	opacity: 1.0;
}

/*	Post section.
	*/
.post-meta .post-section::before,
.comment-meta .alignment-forum {
	position: relative;
	top: -1px;
	margin: 0 0 0 0.5em;
	background-color: #999;
	-webkit-background-clip: text;
	color: transparent;
	text-shadow: rgba(255, 255, 255, 0.5) 0px 1px 1px;
}
.post-meta .post-section:hover::before {
	background-color: #444;
}
.comment-meta .alignment-forum {
	margin: 0 1em 0 0;
}
.post-meta .post-section.alignment-forum::before {
	background-color: #b9bbff;
}
.post-meta .post-section.alignment-forum:hover::before {
	background-color: #e7e8ff;
}

/*============*/
/* LINK POSTS */
/*============*/

.post.link-post a.link-post-link {
	text-decoration: none;
	color: #77121a;
}
.post.link-post a.link-post-link:hover,
.post.link-post a.link-post-link:focus {
	text-shadow: 
		0px 0px 0.5px #ffaca5, 
		0px 0px 1.5px #de7069,
		0.5px 0.5px 0.5px #ff988f;
	text-decoration: dotted underline;
	border: none;
}
.post.link-post a.link-post-link::before {
	position: relative;
	top: 1px;
	color: #999;
}
.post.link-post a.link-post-link:hover::before,
.post.link-post a.link-post-link:focus::before {
	color: #690010;
	text-shadow: 
		 0.5px 0.5px 0 #fff,
		 -0.5px -0.5px 0 #fff,
		 0 0 2px #fff,
		 0 0 3px #00c;
}

/*==========*/
/* COMMENTS */
/*==========*/

.comments::before {
	border-top: 1px solid #ddd;
}

.comment-item {
	border: 1px solid transparent;
}

.comment-item {
	filter: drop-shadow(0 0 3px #777);
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 520px) {
	.comment-body {
		line-height: 1.3;
	}
}

/*=========*/
/* ANSWERS */
/*=========*/

.answer-item::after {
	text-transform: uppercase;
	color: #ccc;
}

/*==================*/
/* COMMENT LISTINGS */
/*==================*/

.listings .comment-thread .comment-meta a.date:focus,
.listings .comment-thread .comment-meta a.permalink:focus {
	color: #000;
	text-shadow: 0 0 0 currentColor;
	outline: 2px dotted #000;
	background-color: #fff;
	padding: 2px 6px 0 6px;
	left: -6px;
	margin-top: -1px;
}
#content.compact .listings .comment-thread .comment-meta a.date:focus,
#content.compact .listings .comment-thread .comment-meta a.permalink:focus {
	padding: 1px 5px 0 5px;
	left: -5px;
	margin-top: 0;
}
.listings .comment-thread .comment-meta a.date:focus + *,
.listings .comment-thread .comment-meta a.permalink:focus + *:not(.comment-post-title) {
	margin-left: -12px;
}
#content.compact .listings .comment-thread .comment-meta a.date:focus + *,
#content.compact .listings .comment-thread .comment-meta a.permalink:focus + *:not(.comment-post-title) {
	margin-left: -10px;
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

/*================================*/
/* DEEP COMMENT THREAD COLLAPSING */
/*================================*/

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
.post .karma .karma-value::after,
.author::before {
	background-color: #fff;
	color: #777;
	box-shadow: 0 0 0 1px #ccc inset;
	text-shadow: none;
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

#content.compact > .comment-thread .comment-item {
	max-height: 56px;
}

@media only screen and (hover: hover) and (pointer: fine) {
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
@media only screen and (hover: hover) and (pointer: fine) {
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

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 520px) {
	#content.compact > .comment-thread .comment-item {
		max-height: 104px;
	}
	#content.compact.user-page h1.listing {
		margin-top: 0.5rem;
	}
	#content.compact.user-page h1.listing + .post-meta {
		margin-bottom: 0.75rem;
	}
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
<?php echo $firefox_exclusive; ?> {
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

.comment-controls .delete-button,
.comment-controls .retract-button {
	color: #d00;
}
.comment-controls .delete-button::before {
	opacity: 0.6;
	font-weight: 400;
}
.comment-controls .retract-button::before {
	opacity: 0.5;
	font-weight: 400;
}
.comment-controls .edit-button,
.comment-controls .unretract-button {
	color: #0a0;
}
.comment-controls .edit-button::before,
.comment-controls .unretract-button::before {
	font-weight: 400;
}
.comment-controls .action-button:hover {
	color: #f00;
	text-shadow:
		0px 0px 0.5px #fff,
		0px 0px 1.5px #800,
}

.post-controls {
	margin: 0.5em -0.75em 0 0;
}
.edit-post-link,
.edit-post-link:visited {
	color: #090;
}

.posting-controls textarea {
	font-size: 1.25rem;
	font-weight: 500;
	color: #000;
	background-color: #fff;
	border-color: #aaa;
	box-shadow: 
		0 0 0 1px #eee inset;
	font-feature-settings: 'ss01';
}
.posting-controls textarea:focus {
	background-color: #ffd;
	border-color: var(--GW-hyperlink-color);
	box-shadow: 
		0 0 0 1px #ddf inset,
		0 0 0 1px #fff,
		0 0 0 2px var(--GW-hyperlink-color);
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

.guiedit::after {
	color: #777;
	text-shadow: none;
}

/* Markdown hints */

#markdown-hints {
	border: 1px solid #c00;
	background-color: #ffa;
}

/*================*/
/* EDIT POST FORM */
/*================*/

#edit-post-form .post-meta-fields input[type='checkbox'] + label::before {
	border-radius: 3px;
	border: 1px solid #ddd;
	color: #777;
}
@media only screen and (hover: hover) and (pointer: fine) {
	#edit-post-form .post-meta-fields input[type='checkbox'] + label:hover,
	#edit-post-form .post-meta-fields input[type='checkbox']:focus + label {
		text-shadow: 
			0 0 1px #fff,
			0 0 2px #fff,
			0 0 2.5px #aaa;
	}
	#edit-post-form .post-meta-fields input[type='checkbox'] + label:hover::before,
	#edit-post-form .post-meta-fields input[type='checkbox']:focus + label::before {
		border-color: #aaa;
	}
}
#edit-post-form .post-meta-fields input[type='checkbox']:checked + label::before {
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
}

/*=========*/
/* BUTTONS */
/*=========*/

input[type='submit'] {
	border: 1px solid #ddd;
}

button:hover,
button:focus,
input[type='submit']:hover,
input[type='submit']:focus,
.button:hover,
.button:focus {
	text-shadow: var(--GW-shadow-link-glow);
	background-color: #f0f0eb;
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
	margin: 1.5em 0 0.25em 0;
}
.body-text h4 {
	font-size: 1.15em;
}
.body-text h3 {
	font-variant: small-caps;
	font-size: 1.3em;
}
.body-text h2 {
	font-style: italic;
	font-size: 1.5em;
}
.body-text h1 {
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

#content img,
#content figure.image img {
	border: 1px solid #ccc;
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
	font-feature-settings: 'ss02';
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

#content.about-page img {
	border: 1px solid #000;
}

/*========================*/
/* QUALIFIED HYPERLINKING */
/*========================*/

#content.no-nav-bars {
	margin: auto;
}


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

.mathjax-block-container .mjx-chtml::-webkit-scrollbar {
	height: 12px;
	background-color: #f6f6ff;
	border-radius: 6px;
	border: 1px solid #ddf;
}
.mathjax-block-container .mjx-chtml::-webkit-scrollbar-thumb {
	background-color: #dde;
	border-radius: 6px;
	border: 1px solid #cce;
}
.mathjax-inline-container .mjx-chtml::-webkit-scrollbar {
	height: 8px;
	background-color: #f6f6ff;
	border-radius: 4px;
	border: 1px solid #ddf;
}
.mathjax-inline-container .mjx-chtml::-webkit-scrollbar-thumb {
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
	background-color: #7f85b2;
	color: transparent;
	-webkit-background-clip: text;
	text-shadow: 
		rgba(255, 255, 255, 0.5) 0px 3px 3px;
}
@media only screen and (hover: hover) and (pointer: fine) {
	#content.alignment-forum-index-page h1.listing a:hover,
	#content.alignment-forum-index-page h1.listing a:focus {
		background-color: rgba(244, 245, 255, 0.85);
	}	
}

/*========*/
/* MOBILE */
/*========*/

@media only screen and (max-width: 520px) {
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

	#edit-post-form .post-meta-fields input[type='checkbox'] + label {
		top: 2px;
	}
	#edit-post-form .post-meta-fields input[type='checkbox'] + label::before {
		top: 1px;
	}
	#edit-post-form textarea {
		min-height: calc(100vh - 440px);
	}
}

/*===============*/
/* KEYBOARD HELP */
/*===============*/
