/*********************/
/* ULTRAMODERN THEME */
/*********************/

/*===========*/
/* VARIABLES */
/*===========*/

/*	Typography.
	*/
:root {
	--GW-UI-font: 'Proxima Nova', sans-serif;
	--GW-UI-font-weight-light: 300;
	--GW-UI-font-weight-heavy: 400;

	--GW-post-listings-font: 'Raleway', 'Helvetica', 'Arial', 'Verdana', sans-serif;
	--GW-post-listings-font-weight: 200;

	--GW-body-text-font: 'Raleway', 'Helvetica', 'Arial', 'Verdana', sans-serif;
	--GW-body-text-font-weight: <?php echo ($platform == 'Mac' ? '300' : '400'); ?>;
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

	--GW-comment-compact-height: 54px;
	--GW-comment-compact-height-mobile: 105px;
	--GW-comment-minimized-height: 34px;
	--GW-comment-minimized-height-mobile: 64px;
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
	}
}

/*	Color scheme.
	*/
:root {
	--GW-body-background-color: #888;

	--GW-hyperlink-color: #222;
	
	--GW-shadow-white-glow:
		0 0 1px #fff,
		0 0 3px #fff,
		0 0 5px #fff;
	--GW-shadow-link-glow:
		0.0px 0.0px 1.0px #777,
		0.5px 0.5px 1.0px #ccc,
		0.5px 0.5px 1.0px #bbb;
}

/*======*/
/* BASE */
/*======*/

body {
	color: #444;
	text-shadow:
		0.0px 0.0px 1.0px #777,
		0.5px 0.5px 1.0px #aaa,
		0.5px 0.5px 1.0px #bbb;
}
#content {
	line-height: 1.55;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 960px) {
	#ui-elements-container > div[id$='-ui-toggle'] button,
	#theme-selector .theme-selector-close-button  {
		color: #555;
		text-shadow:
			0 0 1px #aaa,
			0 0 3px #aaa,
			0 0 5px #aaa,
			0 0 10px #aaa;
	}
}

/*==========*/
/* NAV BARS */
/*==========*/

#primary-bar .nav-inner,
#bottom-bar .nav-inner {
	font-size: 1.1875em;
}
#secondary-bar .nav-inner {
	font-size: 0.875em;
}

#primary-bar,
#secondary-bar {
	border: 1px solid transparent;
	border-bottom-color: #666;
	box-shadow:
		0 1.5px 1.5px -1.5px #bbb,
		0 1px 1px -1px #777;
}

.nav-inner {
	font-weight: var(--GW-UI-font-weight-light);
}
.nav-current .nav-inner {
	filter: invert(100%) contrast(175%);
	font-weight: var(--GW-UI-font-weight-heavy);
}

#bottom-bar.decorative::before {
	filter: brightness(50%) opacity(0.6);
}
#bottom-bar.decorative::after {
	color: #777;
	background-color: #888;
}

/* Accesskey hints */

.nav-inner::after {
	display: none;
}

/* Search tab */

#nav-item-search form::before {
	opacity: 0.6;
}
#nav-item-search input::placeholder {
	color: #faa;
	font-weight: var(--GW-UI-font-weight-heavy);
}

/*= Recent Comments tab =*/

#nav-item-recent-comments span {
	margin: 0 5px 0 0;
}

/*= Keyboard help button =*/

@media only screen and (min-width: 961px) {
	#nav-item-about button.open-keyboard-help {
		padding: 7px 8px 9px 7px;
		font-weight: 300;
	}
}

/*= Inbox indicator =*/

#inbox-indicator {
	text-shadow: none;
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

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	#secondary-bar .nav-item:not(#nav-item-search) .nav-inner {
		padding: 6px 10px;
	}
	#primary-bar .nav-inner::before, 
	#secondary-bar .nav-inner::before {
		opacity: 0.8;
		font-weight: 400;
	}
	#inbox-indicator::before {
		padding-top: 2px;
	}
}

/*==============*/
/* PAGE TOOLBAR */
/*==============*/

.new-post::before,
.logout-button::before {
	font-weight: 400;
}
.page-toolbar .rss::before {
	filter: grayscale(100%) 
}
.page-toolbar .rss:hover::before {
	filter: none;
}

/*==============*/
/* SUBLEVEL NAV */
/*==============*/

.sublevel-nav .sublevel-item {
	color: #222;
	font-weight: var(--GW-UI-font-weight-light);
}
.sublevel-nav .sublevel-item.selected {
	color: #000;
	font-weight: var(--GW-UI-font-weight-heavy);
}
.sublevel-nav .sublevel-item:not(.selected):active,
.sublevel-nav .sublevel-item.selected {
	background-color: #777;
}

.sublevel-nav:not(.sort) .sublevel-item {
	border-style: solid;
	border-color: #666;
	border-width: 1px 0 1px 1px;
}
.sublevel-nav:not(.sort) .sublevel-item:first-child {
	border-radius: 8px 0 0 8px;
}
.sublevel-nav:not(.sort) .sublevel-item:last-child {
	border-width: 1px;
	border-radius: 0 8px 8px 0;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	.sublevel-nav:not(.sort) .sublevel-item,
	.sublevel-nav:not(.sort) .sublevel-item:first-child,
	.sublevel-nav:not(.sort) .sublevel-item:last-child {
		border-radius: 8px;
		border-width: 1px;
		margin: 2px;
	}
}

/*=====================*/
/* SORT ORDER SELECTOR */
/*=====================*/

.sublevel-nav.sort .sublevel-item {
	padding: 9px 8px 6px 8px;
	text-transform: uppercase;
	box-shadow: 1px 1px 0 0 #666 inset;
}
.sublevel-nav.sort {
	border: 2px solid transparent;
	padding: 18px 0 0 0;
	border-radius: 8px;
}
.sublevel-nav.sort::before {
	text-transform: uppercase;
	color: #222;
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
		0 1px 0.5px 0 #888 inset,
		0 1px 0.5px 0 #888 inset,
		0 1px 0.5px 0 #888 inset,
		0 18px 0 0 #888 inset,
		0 0 0 1px #666 inset,
		0 18px 0 1px #666 inset,
		0 0 0 2px #888;
}

/*================*/
/* WIDTH SELECTOR */
/*================*/

#width-selector button {
	box-shadow:
		0 0 0 4px #888 inset,
		0 0 0 5px #bbb inset;
}
#width-selector button:hover,
#width-selector button.selected {
	box-shadow:
		0 0 0 1px #888 inset,
		0 0 0 2px #bbb inset,
		0 0 0 4px #888 inset,
		0 0 0 5px #bbb inset;
}
#width-selector button::after {
	color: #bbb;
	font-weight: var(--GW-UI-font-weight-light);
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
	color: #bbb;
	background-color: #888;
	font-weight: var(--GW-UI-font-weight-light);
}
#theme-selector button:hover::before,
#theme-selector button.selected::before {
	color: #ddd;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 960px) {
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
		font-weight: var(--GW-UI-font-weight-light);
		text-shadow: 
			0px 0px 1px #777, 
			0.5px 0.5px 1px #aaa, 
			0.5px 0.5px 1px #bbb;
	}
	#theme-selector button {
		border-radius: 10px;
	}
	#theme-selector button::after {
		color: #222;
		text-shadow: 
			0px 0px 0.5px #777, 
			0.5px 0.5px 0.5px #aaa, 
			0.5px 0.5px 0.5px #bbb;
	}
	#theme-selector button:hover,
	#theme-selector button.selected {
		box-shadow:
			0 0 4px 1px #ccc inset;
	}
	#theme-selector button.selected::after {
		text-shadow: 
			0px 0px 1px #777, 
			0.5px 0.5px 1px #aaa, 
			0.5px 0.5px 1px #bbb;
		filter: invert(100%) contrast(150%);
	}
}

/*======================*/
/* THEME TWEAKER TOGGLE */
/*======================*/

#theme-tweaker-toggle button {
	font-weight: 400;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 1000px) {
	#theme-tweaker-toggle button {
		border: 1px solid #aaa;
		box-shadow: 
			0 0 10px #999 inset,
			0 0 0 1px transparent;
		border-radius: 50%;
		transform: scale(0.8);
		background-color: #888;
		opacity: 1.0;
	}
}

/*=================*/
/* QUICKNAV WIDGET */
/*=================*/

#quick-nav-ui a {
	color: #666;
	border-radius: 4px;
	box-shadow: 0 0 0 1px #aaa;
}
#quick-nav-ui a[href='#bottom-bar'],
#quick-nav-ui a[href='#comments'] {
	line-height: 1.8;
}
#quick-nav-ui a:active {
	transform: scale(0.9);
}
#quick-nav-ui a[href='#comments'].no-comments {
	opacity: 0.4;
	color: #777;
}
@media only screen and (hover: hover) and (pointer: fine) {
	#quick-nav-ui a:hover  {
		color: #444;
		box-shadow: 0 0 0 1px #ccc;
	}
}

/*======================*/
/* NEW COMMENT QUICKNAV */
/*======================*/

#new-comment-nav-ui .new-comments-count {
	color: #555;
}
#new-comment-nav-ui .new-comments-count::after {
	color: #333;
}
#new-comment-nav-ui .new-comment-sequential-nav-button {
	color: #bbb;
}
#new-comment-nav-ui .new-comment-sequential-nav-button:disabled {
	color: #929292;
}
@media only screen and (hover: hover) and (pointer: fine) {
	#new-comment-nav-ui .new-comments-count:hover {
		text-shadow: var(--GW-shadow-white-glow);
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button:hover {
		color: #444;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button:focus {
		text-shadow: var(--GW-shadow-white-glow);
	}
}

/*=================*/
/* HNS DATE PICKER */
/*=================*/

#hns-date-picker span {
	color: #444;
}
#hns-date-picker input {
	border: 1px solid #aaa;
	color: #555;
	font-size: 0.9375em;
}
#hns-date-picker input:focus {
	color: #000;
	border: 1px solid #ccc;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

#hns-date-picker.flipped {
	background-color: var(--GW-body-background-color);
	opacity: 1.0;
}

/*================================*
	MOBILE VERSIONS OF QUICKNAV,
	NEW COMMENT NAV, AND HNS
	DATE PICKER
 *================================*/

@media only screen and (max-width: 960px) {
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

	#hns-date-picker.engaged {
		border: 1px solid #444;
	}
	#hns-date-picker span,
	#hns-date-picker input {
		color: #444;
	}
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
	color: #222;
}
#text-size-adjustment-ui button:hover {
	color: #ccc;
}
#text-size-adjustment-ui::after {
	color: #ccc;
	text-shadow: none;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 1080px) {
	#text-size-adjustment-ui {
		margin-left: 4px;
		box-shadow: 
			0 0 6px #999 inset,
			0 0 0 1px transparent;
	}
}

/*=============================*/
/* COMMENTS LIST MODE SELECTOR */
/*=============================*/

#comments-list-mode-selector button {
	box-shadow:
		0 0 0 4px #888 inset,
		0 0 0 5px #bbb inset;
}
#comments-list-mode-selector button:hover,
#comments-list-mode-selector button.selected {
	box-shadow:
		0 0 0 1px #888 inset,
		0 0 0 2px #bbb inset,
		0 0 0 4px #888 inset,
		0 0 0 5px #bbb inset;
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
	outline: 1px solid #ccc;
	border-color: #888;
}

.archive-nav *[class^="archive-nav-item"] {
	text-shadow: none;
	font-weight: var(--GW-UI-font-weight-light);
	background-color: #888;
}
.archive-nav span[class^="archive-nav-item"],
.archive-nav a:hover {
	color: #fff;
	z-index: 1;
	outline: 1px solid #ccc;
	transition: box-shadow 0.05s ease;
	font-weight: var(--GW-UI-font-weight-light);
}
.archive-nav span[class^="archive-nav-item"],
.archive-nav a[class^="archive-nav-item"]:active {
	box-shadow: 
		0 0 0 2px #888 inset,
		0 0 0 3px #ccc inset,
		0 0 0 4px #888 inset;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	.archive-nav > *[class^='archive-nav-'] + *[class^='archive-nav-']::before {
		background-color: #ccc;
	}
}

/*==========*/
/* LISTINGS */
/*==========*/

h1.listing {
	font-size: 1.5rem;
	margin: 7px 0 3px 0;
}

h1.listing a {
	color: #a2f2f1;
	text-shadow: 
		0.0px 0.0px 2.0px #777,
		0.5px 0.5px 2.0px #444,
		0.5px 0.5px 2.0px #333;
}
h1.listing a[href^="http"] {
	color: #a2cbf2;
}

@media only screen and (hover: hover) and (pointer: fine) {
	h1.listing a:hover,
	h1.listing a:focus {
		background-color: rgba(136,136,136,0.85);
		text-shadow: 
			0 0 1px #a2cbf2,
			0 0 2px #a2cbf2,
			0 0 3px #a2cbf2,
			0.0px 0.0px 2.0px #777,
			0.5px 0.5px 2.0px #444,
			0.5px 0.5px 2.0px #333;
	}	
	h1.listing:focus-within::before {
		color: #a2f2f1;
		font-weight: 400;
	}
	h1.listing a[href^="http"]:hover {
		color: #a2f2c9;
	}
}

h1.listing .edit-post-link {
	padding: 10px 3px 30px 0.5em;
	top: 0;
	right: -1.5em;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 720px) {
	h1.listing {
		font-size: 1.375rem;
	}
}
@media only screen and (max-width: 520px) {
	h1.listing {
		font-size: 1.25rem;
	}
}

/*======*/
/* SPAM */
/*======*/

h1.listing.spam {
	opacity: 0.35;
}
h1.listing.spam + .post-meta {
	opacity: 0.35;
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
	margin: 0 0 16px 0;
}

h1.listing + .post-meta > * {
	margin: 0 0.25em 0 0;
}

h1.listing + .post-meta .comment-count,
h1.listing + .post-meta .read-time,
h1.listing + .post-meta .word-count,
h1.listing + .post-meta .lw2-link {
	margin: 0 0.75em 0 0;
}

/*	Karma value.
	*/
h1.listing + .post-meta .karma {
	order: -1;
}
h1.listing + .post-meta .karma::after {
	content: " by";
}

/*	Comment count.
	*/
h1.listing + .post-meta .comment-count.new-comments::before {
	color: #00f;
	opacity: 0.5;
}
h1.listing + .post-meta .comment-count.new-comments:hover::before {
	opacity: 1.0;
}

/*	LW2 link.
	*/
h1.listing + .post-meta .lw2-link {
	opacity: 0.75;
}
h1.listing + .post-meta .lw2-link::before {
	content: "\F0C1";
	font-weight: 400;
}
h1.listing + .post-meta .lw2-link:hover::before {
	color: inherit;
}

/*	Read time.
	*/
h1.listing + .post-meta .read-time span {
	display: none;
}
h1.listing + .post-meta .read-time::after {
	content: " min";
}
h1.listing + .post-meta .read-time:hover::before {
	color: inherit;
}
h1.listing + .post-meta .read-time::before {
	content: "\F017";
}

/*	Word count.
	*/
h1.listing + .post-meta .word-count::after {
	content: "";
}
h1.listing + .post-meta .word-count::before {
	content: "\F15C";
}

/*	Date.
	*/
h1.listing + .post-meta .date::before {
	content: "on ";
}
h1.listing + .post-meta .date::after {
	content: " — ";
	opacity: 0.5;
	margin: 0 0 0 0.25em;
}

/*	Link post domain.
	*/
h1.listing + .post-meta .link-post-domain {
	order: 1;
	margin: 0 0 0 0.5em;
}

/*	Post section.
	*/
h1.listing + .post-meta .post-section::before {
	font-size: 0.9375em;
	left: -2em;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	h1.listing + .post-meta .post-section {
		width: auto;
		overflow: visible;
		order: 3;
		margin: 0 0 0 0.5em;
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
	border-bottom: 1px solid #777;
}

#content.user-page h1.listing,
#content.user-page h1.listing + .post-meta,
#content.search-results-page h1.listing,
#content.search-results-page h1.listing + .post-meta {
	border-style: solid;
	border-color: #666;
	border-width: 0 0 0 1px;
	box-shadow:
		1.5px 0 1.5px -1.5px #bbb inset,
		1px 0 1px -1px #777 inset;
}
#content.user-page h1.listing,
#content.search-results-page h1.listing {
	padding: 3px 6px 0 6px;
	margin: 1rem 0 0 0;
}
#content.user-page.all-user-page h1.listing {
	margin: 2rem 0 0 0;
}
#content.own-user-page h1.listing,
h1.listing.own-post-listing {
	padding-right: 36px;
}

#content.user-page h1.listing + .post-meta,
#content.search-results-page h1.listing + .post-meta {
	padding: 0.5em 6px 6px 34px;
}
#content.user-page h1.listing + .post-meta .post-section::before,
#content.search-results-page h1.listing + .post-meta .post-section::before {
	left: 10px;
}

/*=--------------------=*/
/*= Conversations list =*/
/*=--------------------=*/

#content.conversations-user-page h1.listing + .post-meta {
	padding: 0.5em 6px 2px 8px;
}

/*===============*/
/* CONVERSATIONS */
/*===============*/

/*============*/
/* LOGIN PAGE */
/*============*/

.login-container h1 {
	font-weight: var(--GW-UI-font-weight-heavy);
}

/* “Create account” form */

#signup-form {
	border: 1px solid #aaa;
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

/*=================*/
/* POST NAVIGATION */
/*=================*/

.post-nav-links a,
.post-nav-links a:visited {
	color: #444;
	font-weight: 300;
}
.post-nav-links a:hover {
	color: #ccc;
}

.post-nav-label {
	opacity: 0.75;
}

@media only screen and (max-width: 900px) {
	.sequence-title {
		border-top: 1px solid #777;
	}
	.post-nav.prev {
		border-right: 1px solid #777;
	}
	.post-nav.next {
		border-left: 1px solid #777;
	}
}

/*==================*/
/* POSTS & COMMENTS */
/*==================*/

.body-text {
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

/*=======*/
/* POSTS */
/*=======*/

article > .post-meta > *,
.post .post-meta > * {
	margin: 0 0.25em;
}

article > .post-meta a,
article > .post-meta a:visited,
.post .post-meta a {
	color: #222;
}

article > .post-meta .author,
.post .post-meta .author,
.post .post-meta .lw2-link {
	margin: 0 0.5em;
}

.bottom-post-meta {
	border-color: #777;
}

/*===========*/
/* POST-META */
/*===========*/

.post-meta > * {
	font-size: 1em;
	color: #444;
}

.post-meta .comment-count span,
.post-meta .lw2-link span {
	display: none;
}

.post-meta .comment-count::before,
.post-meta .read-time::before,
.post-meta .word-count::before,
.post-meta .lw2-link::before {
	font-family: var(--GW-Font-Awesome);
	margin: 0 0.25em 0 0;
	font-size: 0.875em;
	color: #666;
}
.post-meta a:hover {
	color: #000;
}
.post-meta .comment-count::before {
	content: "\F086";
}
.post-meta .comment-count:hover::before {
	color: #444;
	font-weight: 900;
}

/*	Post section.
	*/
.post-meta .post-section::before,
.comment-meta .alignment-forum {
	color: #888;
}
.post-meta .post-section::before {
	text-shadow: 
		1px 1px 0 #ccc, 
		0 1px 0 #ccc, 
		0 0 5px #ccc;
}
a.post-section:hover::before {
	color: #999;
}
.post-meta .post-section.alignment-forum::before,
.comment-meta .alignment-forum {
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

.comments::before {
	border-top: 1px solid #666;
	box-shadow:
		0 1.5px 1.5px -1.5px #bbb inset,
		0 1px 1px -1px #777 inset;
}

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
<?php echo $firefox_exclusive; ?> {
	.comment-item {
		box-shadow:
			1.5px 0 1.5px -1px #bbb inset, 
			1px 0 1px -1px #777 inset;
	}
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	.comment-item .comment-item {
		margin: 0.75em 0 4px 6px;
	}
	.comment-item .comment-item + .comment-item {
		margin: 1.5em 0 4px 6px;
	}
}

/*===========================*/
/* COMMENT THREAD NAVIGATION */
/*===========================*/

a.comment-parent-link::after {
	display: none;
}
a.comment-parent-link::before {
	padding: 2px 3px 0 4px;
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

#content.compact > .comment-thread .comment-item::after {
	color: #ccc;
	background: linear-gradient(to right, transparent 0%, #888 50%, #888 100%);
}

@media only screen and (hover: hover) and (pointer: fine) {
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
@media not screen and (hover: hover) and (pointer: fine) {
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
	text-shadow: var(--GW-shadow-white-glow)
}
.comment-minimize-button::after {
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
<?php echo $firefox_exclusive; ?> {
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
	text-shadow: var(--GW-shadow-white-glow);
}

.posting-controls .action-button,
.posting-controls input[type='submit'] {
	font-weight: normal;
}
.posting-controls .action-button:hover,
.posting-controls input[type='submit']:hover {
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
}

.edit-post-link,
.edit-post-link:visited {
	color: #090;
}

.posting-controls textarea {
	color: #000;
	background-color: transparent;
	border-color: #999;
	text-shadow: 
		0px 0px 1px #777,
		0.5px 0.5px 1px #aaa,
		0.5px 0.5px 1px #bbb;
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
#markdown-hints {
	background-color: #888;
	border: 1px solid #ccc;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	.comment-controls .cancel-comment-button::before {
		text-shadow:
			0 0 1px #fff,
			0 0 3px #fff;
	}
}
@media only screen and (max-width: 520px) {
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
}

/*================*/
/* EDIT POST FORM */
/*================*/

#edit-post-form .post-meta-fields input[type='checkbox'] + label::before {
	border-radius: 3px;
	border: 1px solid #999;
	color: #aaa;
}
@media only screen and (hover: hover) and (pointer: fine) {
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

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 520px) {
	#edit-post-form .post-meta-fields input[type='checkbox'] + label {
		top: 2px;
	}
	#edit-post-form .post-meta-fields input[type='checkbox'] + label::before {
		top: 1px;
	}
}

/*=======*/
/* LINKS */
/*=======*/

a {
	text-decoration: none;
}
a:hover {
	text-shadow: var(--GW-shadow-link-glow);
}

/*=========*/
/* BUTTONS */
/*=========*/

button,
input[type='submit'],
.button {
	text-shadow: inherit;
}
button:hover,
input[type='submit']:hover,
.button:hover {
	text-shadow:  var(--GW-shadow-link-glow);
}
button:active,
input[type='submit']:active,
.button:active {
	transform: scale(0.9);
}
button:focus:not(:hover),
input[type='submit']:focus:not(:hover),
.button:focus:not(:hover) {
	transform: none;
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
	text-shadow: var(--GW-shadow-white-glow);
}

.qualified-linking label:hover {
	text-shadow: var(--GW-shadow-white-glow);
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
	text-shadow: var(--GW-shadow-white-glow);
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
@media only screen and (hover: hover) and (pointer: fine) {
	#content.alignment-forum-index-page h1.listing a:hover,
	#content.alignment-forum-index-page h1.listing a:focus {
		background-color: rgba(135,138,159,0.85);
	}	
}
