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

	--GW-listings-post-meta-font-weight: 300;

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

	--GW-comment-listings-date-focused-adjust-y: -1px;
}
:root #content.compact {
	--GW-comment-listings-date-focused-adjust-y: -2px;
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

	--GW-comment-item-outline-width: 1px;
	--GW-comment-item-default-outline-color: #999;
	--GW-comment-item-new-comment-outline-color: #00a91f;
	--GW-comment-item-focused-outline-color: #00c;
	--GW-comment-item-higlight-color: #8c6400;
	--GW-comment-item-highlight-faint-color: #c79700;

	--GW-vote-button-color: #bbb;
	--GW-upvote-button-color: #0f0;
	--GW-downvote-button-color: #f00;
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
#ui-elements-container > div[id$='-ui-toggle'] button:hover {
	background-color: transparent;
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
#nav-item-search button:hover,
#nav-item-search button:focus {
	box-shadow:
		-1px 0   0 0 #ddd inset,
		 0   1px 0 0 #ddd inset,
		 0  -1px 0 0 #ddd inset;
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
	background-color: transparent;
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
		background-color: transparent;
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
	font-size: 2rem;
	margin: 0.7em 0 0 0;
	justify-content: center;
	text-align: center;
}

h1.listing a[href^="http"] {
	color: #888;
	line-height: 1;
	top: -2px;
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

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 520px) {
	h1.listing {
		font-size: 1.75rem;
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
	color: #bbb;
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
	font-weight: var(--GW-UI-font-weight-heavy);
	border-bottom: 1px solid #ddd;
}
#content.user-page .user-stats {
	font-weight: var(--GW-UI-font-weight-light);
	text-shadow: 0 0 0 #999;
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
	filter: drop-shadow(0 0 3px var(--GW-comment-item-outline-color));
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

.comment-popup .comment-body {
	font-size: 1.1875rem;
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

.post .post-meta {
	font-weight: var(--GW-listings-post-meta-font-weight);
}

.post .bottom-post-meta {
	border-color: #ddd;
	padding-bottom: 18px;
}

.post-meta {
	text-align: center;
}

.post-meta > * {
	color: #999;
	margin: 0 1em 0 0;
	text-shadow: 0 0 0 currentColor;
}
.post-meta *:not(a) {
	color: #bbb;
}
.post-meta a * {
	color: inherit;
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
	background-color: #4c528d;
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
	filter: drop-shadow(0 0 3px var(--GW-comment-item-outline-color));
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
	left: -1px;
}

/*==================*/
/* COMMENT LISTINGS */
/*==================*/

@media only screen and (hover: hover) and (pointer: fine) {
	#content.compact .comment-item:hover .comment,
	#content.compact .comment-item.expanded .comment {
		outline: none;
	}
}
@media not screen and (hover: hover) and (pointer: fine) {
	#content.compact .comment-thread.expanded .comment {
		outline: none;
	}
}

.listings .comment-thread .comment-meta a.date:focus::after,
.listings .comment-thread .comment-meta .nav-links-container a:focus::after {
	outline: 2px dotted #000;
}
.listings .comment-thread .comment-meta a.date:focus {
	color: #000;
	text-shadow: 0 0 0 currentColor;
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
}

.comment-child-link::before {
	color: #ccc;
}

/*==============*/
/* COMMENT-META */
/*==============*/

.comment-meta {
	font-weight: 300;
	text-shadow: 0 0 0 currentColor;
	padding-top: 5px;
	padding-bottom: 4px;
	border-bottom: 1px dotted #bbb;
}

.comment-meta .comment-post-title a:focus {
	text-decoration: underline;
}

/*	Author.
	*/
.comment-meta .author {
	font-size: 1.125em;
	color: #444;
	text-shadow: none;
}
.comment-meta .author:focus {
	border-bottom: 2px dotted currentColor;
	line-height: 1;
}
.comment-item .author:not(.redacted).original-poster::after {
	opacity: 0.8;
}

/*	Karma controls.
	*/
.comment-item .karma.active-controls::after,
.comment-item .karma .karma-value::after,
.post .karma.active-controls::after,
.post .karma .karma-value::after,
.author::before {
	background-color: var(--GW-content-background-color);
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
}

/*====================*/
/* ANTI-KIBITZER MODE */
/*====================*/

.author.redacted,
.inline-author.redacted {
	opacity: 0.8;
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
/*==================*/
/* COMMENT LW LINKS */
/*==================*/

.comment-meta .permalink,
.comment-meta .lw2-link,
.comment-meta .comment-parent-link span,
.post .post-meta .lw2-link {
	filter: grayscale(75%) opacity(75%);
}
.comment-meta .permalink:hover,
.comment-meta .lw2-link:hover,
.comment-meta .comment-parent-link:hover span,
.post .post-meta .lw2-link:hover {
	filter: unset;
}

/*=================================*/
/* COMMENT THREAD MINIMIZE BUTTONS */
/*=================================*/

.comment-minimize-button {
	color: #d8d8d8;
	top: 1px;
	right: 1px;
}
.comment-minimize-button:hover {
	color: #999;
}
.comment-minimize-button::after {
	color: #777;
}
.comment-minimize-button.maximized::after {
	color: #ccc;
}

/*==============*/
/* VOTE BUTTONS */
/*==============*/

.vote:hover,
.vote:focus {
	background-color: transparent;
}

.upvote,
.downvote {
	position: relative;	
	text-shadow: none;
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
	display: none;
}
.vote.selected:hover::after,
.vote.big-vote::after {
	display: initial;
}
.karma.waiting .vote.big-vote::after {
	color: transparent;
}
.vote.big-vote::after,
.vote:not(.big-vote).clicked-twice::after {
	color: inherit;
}
.karma:not(.waiting) .vote.clicked-once::after {
	color: var(--GW-vote-button-color);
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
@supports (-moz-user-focus: normal) {
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

.posting-controls .new-comment-button {
	border: 1px solid #ddd;
	padding: 0.5rem 1rem;
	margin: -0.5rem 0 0 0;
	font-weight: var(--GW-UI-font-weight-light);
}

.posting-controls input[type='submit'],
.posting-controls .cancel-post-editing-button {
	border: 1px solid #bbb;
}
.posting-controls input[type='submit']:hover,
.posting-controls input[type='submit']:focus,
.posting-controls .cancel-post-editing-button:hover,
.posting-controls .cancel-post-editing-button:focus {
	background-color: #ddd;
	border: 1px solid #aaa;
}

.comment-controls .cancel-comment-button,
.posting-controls .cancel-post-editing-button {
	color: #c00;
	text-shadow: 
		0 0 1px #fff,
		0 0 2px #fff;
}
.comment-controls .cancel-comment-button:hover {
	color: #f00;
	background-color: transparent;
}
.posting-controls .cancel-post-editing-button:hover {
	color: #900;
	text-shadow: var(--GW-shadow-white-glow);
}

.comment-item .comment-controls .action-button:hover {
	background-color: transparent;
}

.comment-controls .delete-button,
.comment-controls .retract-button {
	color: #ff694a;
}
.comment-controls .delete-button::before {
	font-weight: 300;
}
.comment-controls .retract-button::before {
	font-weight: 400;
}
.comment-controls .edit-button,
.comment-controls .unretract-button {
	color: #1fb414;
}
.comment-controls .edit-button::before,
.comment-controls .unretract-button::before {
	font-weight: 300;
}
.comment-controls .action-button:hover {
	color: #f00;
	text-shadow:
		0px 0px 0.5px #fff,
		0px 0px 1.0px #800;
}

.post-controls .edit-post-link,
.post-controls .edit-post-link:visited {
	color: #090;
	box-shadow: 0 0 0 1px #ddd;
	padding: 6px 12px 4px 13px;
	font-weight: var(--GW-UI-font-weight-light);
}
.post-controls .edit-post-link::before {
	font-weight: 300;
}

.posting-controls textarea {
	border-color: #ccc;
	box-shadow: 
		0 0 0 1px #eee inset;
}
.posting-controls textarea:focus {
	background-color: #ffd;
	border-color: var(--GW-hyperlink-color);
	box-shadow: 
		0 0 0 1px #ddf inset,
		0 0 0 1px #fff,
		0 0 0 2px var(--GW-hyperlink-color);
}

/* GUIEdit buttons */

.guiedit-buttons-container {
	background-image:
		linear-gradient(
			to bottom,
			#fff 0%,
			#ddf 50%,
			#ccf 75%,
			#aaf 100%
		);
}

.guiedit-buttons-container button {
	color: #888;
}
.guiedit-buttons-container button:hover {
	background-color: transparent;
	color: #000;
	text-shadow: none;
}

.guiedit::after {
	color: #444;
	text-shadow: none;
}

/* Markdown hints */

.comment-item.open-for-editing {
	z-index: 1;
}

#markdown-hints-checkbox + label::before {
	font-size: 1.1em;
	line-height: 1.4;
}
#markdown-hints-checkbox:checked + label::before {
	font-weight: 300;
}

#markdown-hints {
	border: 1px solid #c00;
	background-color: #ffa;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 520px) {
	.comment-item.open-for-editing:focus-within {
		filter: none;
		z-index: unset;
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
}

/*================*/
/* EDIT POST FORM */
/*================*/

#edit-post-form .post-section-selector label {
	padding: 5px 12px;
	color: #777;
	box-shadow: 0 0 0 1px #ddd;
}
#edit-post-form .post-section-selector:focus-within label {
	background-color: #ffd;
}
#edit-post-form .post-section-selector label:nth-of-type(n+2) {
	margin-left: 1px;
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
	z-index: 1;
}
#edit-post-form input[type='radio']:active + label,
#edit-post-form input[type='radio']:checked + label {
	background-color: #ddd;
	color: #000;
	font-weight: var(--GW-UI-font-weight-heavy);
}

/*===========*/
/* SEQUENCES */
/*===========*/

.sequence-text {
	font-size: 1.375rem;
}

h1.sequence-chapter {
	text-align: center;
}

#content.sequences-page::after {
	text-align: center;
	font-weight: var(--GW-UI-font-weight-light);
	color: #000;
	margin-bottom: -0.25em;
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

.body-text h3 {
	font-variant: small-caps;
	font-size: 1.3em;
}
.body-text h2 {
	font-style: italic;
}
.post-body h1 {
	text-align: center;
}
.post-body h1::before {
	content: "❦";
	margin: 0 auto 1em auto;
	font-size: 0.625em;
}
.post-body h2 {
	margin: 1em 0 0 0;
}
.post-body h2::before {
	content: "☙";
	text-align: center;
	margin: 0 auto 0.5em auto;
}
.post-body h1::before,
.post-body h2::before {
	display: block;
	font-weight: normal;
	font-style: normal;
	background-color: #ccc;
	-webkit-background-clip: text;
	color: transparent;
	text-shadow: rgba(255, 255, 255, 0.5) 0px 1px 1px;
}
.post-body h1:first-child::before,
.post-body .contents + h1::before,
.post-body h2:first-child::before,
.post-body .contents + h2::before {
	content: "";
}

.body-text h5 {
	font-weight: bold;
}
.body-text h6 {
	font-style: italic;
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

sup {
	top: -0.4em;
}

hr {
	height: 1px;
	background-image: linear-gradient(to right, rgba(0, 0, 0, 0), rgba(0, 0, 0, 0.75), rgba(0, 0, 0, 0));
}

/*=------=*/
/*= Code =*/
/*=------=*/

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

/*=---------------=*/
/*= Form elements =*/
/*=---------------=*/

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

/*=------------=*/
/*= Checkboxes =*/
/*=------------=*/

input[type='checkbox'] + label::before {
	border-radius: 3px;
	border: 1px solid #ddd;
	color: #777;
}
input[type='checkbox']:checked + label::before {
	content: "\F00C";
}
input[type='checkbox'] + label:hover,
input[type='checkbox']:focus + label {
	text-shadow: 
		0 0 1px #fff,
		0 0 2px #fff,
		0 0 2.5px #aaa;
}
input[type='checkbox'] + label:hover::before,
input[type='checkbox']:focus + label::before {
	background-color: #ffd;
	border-color: #aaa;
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
	text-shadow: none;
}
.qualified-linking label span {
	color: inherit;
}
.qualified-linking label:hover {
	color: #333;
	text-shadow: var(--GW-shadow-link-glow);
}

.qualified-linking-toolbar {
	border: 1px solid #aaa;
	background-color: var(--GW-content-background-color);
}
.post-meta .qualified-linking-toolbar a{
	padding-top: 3px;
}
.qualified-linking-toolbar a,
.qualified-linking-toolbar button {
	border: 1px solid #ddd;
}
.qualified-linking-toolbar button {
	font-weight: 300;
}
.qualified-linking-toolbar a:hover,
.qualified-linking-toolbar button:hover {
	color: #333;
	background-color: #f0f0eb;
}

/*======*/
/* MATH */
/*======*/

.mathjax-block-container .mjx-chtml::-webkit-scrollbar {
	height: 12px;
}

.mjx-chtml::-webkit-scrollbar {
	background-color: #fff;
	border-top: 1px solid #ddd;
	border-bottom: 1px solid #ddd;
	box-shadow:
		-1px 0 0 0 #ddd,
		1px 0 0 0 #ddd;
}
.mjx-chtml::-webkit-scrollbar-thumb {
	background-color: #ddd;
	border-top: 1px solid #ddd;
	border-bottom: 1px solid #ddd;
	box-shadow:
		 0  -1px 0 0 #fff inset,
		 0   1px 0 0 #fff inset,
		 1px 0   0 0 #fff inset,
		-1px 0   0 0 #fff inset;
}

.mathjax-inline-container .mjx-chtml::-webkit-scrollbar {
	height: 8px;
}

/*===============*/
/* KEYBOARD HELP */
/*===============*/

#keyboard-help-overlay .keyboard-help-container {
	background-color: var(--GW-content-background-color);
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
	background-color: #ddd;
	box-shadow:
		1px 0 0 0 #fff inset,
		-1px 0 0 0 #fff inset,
		0 1px 0 0 #fff inset,
		0 -1px 0 0 #fff inset;
	border-left: 1px solid #ddd;
}
#keyboard-help-overlay .keyboard-help-container::-webkit-scrollbar-thumb:hover {
	background-color: #bbb;
}

/*=--------------------=*/
/*= Dividers & heading =*/
/*=--------------------=*/

#keyboard-help-overlay .keyboard-help-container h1,
#keyboard-help-overlay .keyboard-help-container .keyboard-shortcuts-lists {
	border-color: #ddd;
}

/*=------=*/
/*= Keys =*/
/*=------=*/

#keyboard-help-overlay .keyboard-help-container code {
	background-color: #eee;
	border: 1px solid #ccc;
	padding: 4px 8px;
}
#keyboard-help-overlay .keyboard-help-container code.ak {
	background-color: #ffeb83;
	border-color: #d4a500;
}

/*=--------------=*/
/*= Close button =*/
/*=--------------=*/

#keyboard-help-overlay button.close-keyboard-help:hover {
	background-color: #eee;
	box-shadow:
		1px -1px 0 0 #ddd inset;
}
#keyboard-help-overlay button.close-keyboard-help:active {
	box-shadow:
		0 0 0 1px #ddd;
}

/*=================*/
/* ALIGNMENT FORUM */
/*=================*/

#content.alignment-forum-index-page::before {
	background-color: #fbfcff;
	border-color: #ddd;
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
		background-color: rgba(251, 252, 255, 0.85);
	}	
}
