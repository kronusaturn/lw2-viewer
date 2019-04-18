/*****************/
/* DEFAULT THEME */
/*****************/

/*===========*/
/* VARIABLES */
/*===========*/

/*	Typography.
	*/
:root {
	--GW-UI-font: <?php echo (($platform == 'Mac') ? "'Concourse', 'a_Avante'" : "'Whitney', 'a_Avante'"); ?>, 'Open Sans', 'Arial', sans-serif;
	--GW-UI-font-smallcaps: <?php echo (($platform == 'Mac') ? "'Concourse Smallcaps', 'a_Avante'" : "'Whitney Smallcaps', 'a_Avante'"); ?>, 'Open Sans', 'Arial', sans-serif;
	--GW-UI-font-weight-light: 400;
	--GW-UI-font-weight-heavy: 600;

	--GW-post-listings-font: 'Concourse', 'a_Avante', 'Open Sans', 'Arial', sans-serif;
	--GW-post-listings-font-weight: 700;

	--GW-body-text-font: 'Charter', 'PT Serif', 'Georgia', serif;

	--GW-comment-meta-author-font-weight: 700;
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

	--GW-comment-compact-height: 60px;
	--GW-comment-minimized-height: 38px;
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
	--GW-body-background-color: #d8d8d8;
	--GW-content-background-color: #fff;

	--GW-hyperlink-color: #00e;
	--GW-hyperlink-visited-color: #551a8b;
	--GW-hyperlink-hover-color: #c00;
	--GW-hyperlink-active-color: #e00;
	
	--GW-shadow-white-glow:
		0 0 1px #fff,
		0 0 3px #fff,
		0 0 5px #fff;
}

/*=============*/
/* BASE LAYOUT */
/*=============*/

body {
	color: #000;
	font-feature-settings: 'ss07';
}

#content {
	line-height: 1.55;
}
#content::before {
	box-shadow: 0px 0px 10px #555;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 960px) {
	#ui-elements-container > div[id$='-ui-toggle'] button  {
		color: #888;
		text-shadow:
			0 0 1px #fff,
			0 0 3px #fff,
			0 0 5px #fff,
			0 0 10px #fff,
			0 0 20px #fff,
			0 0 30px #fff;
	}
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

.nav-bar .nav-item:not(.nav-current):not(#nav-item-search):hover,
#bottom-bar a:hover,
#nav-item-search:not(.nav-current):focus-within {
	background-color: #ddd;
}
.inactive-bar .nav-item:not(.nav-current):not(#nav-item-search):hover,
.inactive-bar #nav-item-search:not(.nav-current):focus-within {
	background-color: #d8d8d8;
}

.nav-bar a:hover,
.nav-bar a:focus {
	text-decoration: none;
	text-shadow: var(--GW-shadow-white-glow);
}

#bottom-bar.decorative::after {
	color: #d8d8d8;
}

/*= Accesskey hints =*/

.nav-inner::after {
	display: block;
	position: absolute;
	left: 5px;
	top: -2px;
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
.active-bar {
	position: relative;
	box-shadow: 0 -3px 4px -2px #ccc;
}
.active-bar .nav-inactive {
	background-color: #eee;
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

/*= Search tab =*/

#nav-item-search button {
	border: none;
	font-weight: inherit;
}
#nav-item-search input::placeholder {
	color: #d00;
	font-weight: normal;
}

/*= Inbox indicator =*/

#inbox-indicator::before {
	padding-top: 1px;
}

/*	Viewport width adjustments.
	*/
@media only screen and (max-width: 900px) {
	#nav-item-search button::before {
		color: #00e;
	}
}

/*= Recent Comments tab =*/

#nav-item-recent-comments span {
	margin: 0 5px 0 0;
}

/*= Top pagination UI hover tooltips =*/

#top-nav-bar a::after,
#bottom-bar a::after {
	color: #000;
}

/*==============*/
/* PAGE TOOLBAR */
/*==============*/

.page-toolbar .button {
	color: #090;
}
.page-toolbar .button:hover {
	color: var(--GW-hyperlink-hover-color);
}
.page-toolbar .button:active,
.page-toolbar .button:focus {
	color: var(--GW-hyperlink-active-color);
}

.button.logout-button,
.button.ignore-button {
	color: #d33;
}

/*==============*/
/* SUBLEVEL NAV */
/*==============*/

.sublevel-nav .sublevel-item {
	color: #777;
	background-color: #fff;
}
.sublevel-nav .sublevel-item:not(.selected):hover {
	background-color: #ddd;
	color: #000;
	text-decoration: none;
	text-shadow: none;
}
.sublevel-nav .sublevel-item:not(.selected):active,
.sublevel-nav .sublevel-item.selected {
	background-color: #ddd;
	color: #000;
	text-shadow: 
		0 -1px 0 #fff,
		0 0.5px 0.5px #000;
}

.sublevel-nav:not(.sort) .sublevel-item {
	border-style: solid;
	border-color: #ddd;
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
	letter-spacing: 0.5px;
	padding: <?php echo ($platform == 'Mac') ? "7px 7px 5px 7px" : "6px 7px"; ?>;
	text-transform: uppercase;
	pointer-events: auto;
	box-shadow: 1px 1px 0 0 #aaa inset;
}
.sublevel-nav.sort {
	border: 2px solid transparent;
	padding: 18px 0 0 0;
	border-radius: 8px;
	pointer-events: none;
	background-color: #bbb;
}
.sublevel-nav.sort::before {
	text-transform: uppercase;
	color: #444;
	text-shadow: 0.5px 0.5px 0 #fff;
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
		0 18px 0 0 #bbb inset,
		0 0 0 1px #aaa inset,
		0 18px 0 1px #aaa inset,
		0 0 0 2px #bbb;
}

/*================*/
/* WIDTH SELECTOR */
/*================*/
/* THEME SELECTOR */
/*================*/

#width-selector button,
#theme-selector button {
	box-shadow:
		0 0 0 4px var(--GW-body-background-color) inset,
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
	color: #999;
	background-color: var(--GW-body-background-color);
}
#theme-selector button:hover::before,
#theme-selector button.selected::before {
	color: #666;
}
#width-selector button::after {
	color: #999;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 1160px) {
	#theme-selector:hover::after {
		background-color: #999;
	}
}
@media only screen and (max-width: 1000px) {
	#theme-selector {
		background-color: var(--GW-body-background-color);
		box-shadow: 
			0 0 0 1px #999,
			0 0 0 2px transparent;
	}
}
@media only screen and (max-width: 960px) {
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
		color: #666;
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
		border: 1px solid #999;
		box-shadow: 
			0 0 10px #999 inset,
			0 0 0 1px transparent;
		border-radius: 50%;
		transform: scale(0.8);
	}
}
@media only screen and (max-width: 1000px) {
	#theme-tweaker-toggle button {
		background-color: #ddd;
	}
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

@media only screen and (hover: hover) and (pointer: fine) {
	#quick-nav-ui a:hover {
		color: #000;
		background-color: #eee;
	}
	#quick-nav-ui a:focus:not(:hover) {
		transform: none;
		text-shadow: none;
	}
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 1020px) {
	#quick-nav-ui a {
		box-shadow: 
			0 0 0 1px #999,
			0 0 0 2px transparent;
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
	color: #bbb;
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
		color: #d00;
		text-shadow: var(--GW-shadow-white-glow);
	}
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 1020px) {
	#new-comment-nav-ui .new-comments-count::before {
		background-color: var(--GW-body-background-color);
		box-shadow: 
			0 0 0 1px #999,
			0 0 0 2px transparent;
		border-radius: 8px;
	}
}

/*=================*/
/* HNS DATE PICKER */
/*=================*/

#hns-date-picker span {
	color: #777;
	text-shadow: 0.5px 0.5px 0 #fff;
}
#hns-date-picker input {
	border: 1px solid #777;
	background-color: transparent;
	color: #666;
	box-shadow: 0 0 0 1px transparent;
}
#hns-date-picker input:focus {
	color: #000;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

#hns-date-picker.flipped {
	background-color: var(--GW-body-background-color);
	opacity: 1.0;
}
#hns-date-picker.flipped::before {
	border: 1px solid #999;
	border-width: 1px 0 1px 1px;
}

/*--------------------------------*
	MOBILE VERSIONS OF QUICKNAV,
	NEW COMMENT NAV, AND HNS
	DATE PICKER
 *--------------------------------*/
	
@media only screen and (max-width: 960px) {
	#quick-nav-ui {
		background-color: #fff;
	}
	#quick-nav-ui,
	#new-comment-nav-ui,
	#hns-date-picker {
		box-shadow:
			0 0 1px 3px #fff;
		filter:
			drop-shadow(0 0 1px #fff)
			drop-shadow(0 0 1px #fff);
	}
	#quick-nav-ui a::after,
	#new-comment-nav-ui::before {
		background-color: #fff;
		border-radius: 4px;
	}
	#quick-nav-ui,
	#new-comment-nav-ui {
		border-radius: 8px;
	}
	#new-comment-nav-ui {
		background-color: #d8d8d8;
		border: 1px solid #999;
	}
	#new-comment-nav-ui::before {
		color: #777;
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
	#new-comment-nav-ui .new-comment-sequential-nav-button.new-comment-next {
		border-radius: 0 7px 7px 0;
	}
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

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 1020px) {
	#anti-kibitzer-toggle {
		box-shadow: 
			0 0 0 1px #999,
			0 0 0 2px transparent;
		background-color: var(--GW-body-background-color);
		border-radius: 6px;
		overflow: hidden;
	}
}

/*======================*/
/* TEXT SIZE ADJUSTMENT */
/*======================*/

#text-size-adjustment-ui button {
	color: #777;
}
#text-size-adjustment-ui button:disabled:hover {
	text-shadow: none;
}
#text-size-adjustment-ui::after {
	color: #999;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 1080px) {
	#text-size-adjustment-ui button {
		border: 1px solid #999;
		border-radius: 50%;
		box-shadow: 
			0 0 6px #999 inset,
			0 0 0 1px transparent;
	}
	#text-size-adjustment-ui button.decrease,
	#text-size-adjustment-ui button.increase {
		padding: 1px 0 0 0;
	}
}
@media only screen and (max-width: 1000px) {
	#text-size-adjustment-ui button {
		background-color: #ddd;
	}
	#text-size-adjustment-ui button:hover {
		background-color: #eee;
	}
}

/*=============================*/
/* COMMENTS LIST MODE SELECTOR */
/*=============================*/

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
	text-shadow: var(--GW-shadow-white-glow);
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

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	.archive-nav > *[class^='archive-nav-'] + *[class^='archive-nav-']::before {
		background-color: #aaa;
	}
}

/*==========*/
/* LISTINGS */
/*==========*/

h1.listing {
	font-size: 1.875em;
	margin: 7px 0 0 0;
}

h1.listing a[href^='/'] {
	color: #000;
}
h1.listing a[href^="http"] {
	color: #00c;
}

@media only screen and (hover: hover) and (pointer: fine) {
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

h1.listing .edit-post-link {
	padding: 5px 3px 12px 0.5em;
	top: 0;
	right: 0;
}
h1.listing .edit-post-link:hover {
	text-decoration: none;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	h1.listing {
		font-size: 1.75rem;
	}
}
@media only screen and (max-width: 840px) {
	h1.listing {
		font-size: 1.625rem;
	}
}
@media only screen and (max-width: 720px) {
	h1.listing {
		font-size: 1.5rem;
		margin-bottom: 2px;
	}
}
@media only screen and (max-width: 320px) {
	h1.listing {
		font-size: 1.25rem;
	}
}

/*======*/
/* SPAM */
/*======*/

h1.listing.spam {
	opacity: 0.15;
}
h1.listing.spam + .post-meta {
	opacity: 0.4;
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
	padding: 0 330px 14px 1px;
	box-shadow: 0 -1px 0 0 #ddd inset;
}
article h1.listing + .post-meta {
	padding-bottom: 8px;
}
h1.listing + .post-meta .karma-value,
h1.listing + .post-meta .comment-count,
h1.listing + .post-meta .lw2-link,
h1.listing + .post-meta .read-time {
	text-shadow: 0.5px 0.5px 0.5px #999;
	position: absolute;
	bottom: 2px;
	margin: 0;
}
h1.listing + .post-meta .karma-value span,
h1.listing + .post-meta .comment-count span,
h1.listing + .post-meta .lw2-link span,
h1.listing + .post-meta .read-time span {
	display: none;
}
h1.listing + .post-meta .karma-value::before,
h1.listing + .post-meta .comment-count::before,
h1.listing + .post-meta .lw2-link::before,
h1.listing + .post-meta .read-time::before {
	color: #fff;
	font-family: var(--GW-Font-Awesome);
	font-weight: 900;
	margin: 0 8px 0 0;
	text-shadow:
		0.5px 0.5px 0.5px #999,
		0     0     2px #999;
}
_::-webkit-full-page-media, _:future, :root h1.listing + .post-meta .karma-value::before,
_::-webkit-full-page-media, _:future, :root h1.listing + .post-meta .comment-count::before,
_::-webkit-full-page-media, _:future, :root h1.listing + .post-meta .lw2-link::before,
_::-webkit-full-page-media, _:future, :root h1.listing + .post-meta .read-time::before {
	text-shadow: 0 0 3px #999;
}

/*	Karma value.
	*/
h1.listing + .post-meta .karma {
	margin: 0;
}
h1.listing + .post-meta .karma-value {
	cursor: default;
	color: #c00;
	right: 232px;
}
h1.listing + .post-meta .karma-value::before {
	content: "\F139";
	font-size: 0.9375em;
}

/*	Comment count.
	*/
h1.listing + .post-meta .comment-count {
	color: #009100;
	right: 160px;
}
h1.listing + .post-meta .comment-count::before {
	content: "\F086";
}
h1.listing + .post-meta .comment-count:hover {
	text-decoration: none;
}
h1.listing + .post-meta .comment-count:hover::before {
	color: #009100;
	text-shadow: 0.5px 0.5px 0.5px #fff;
	font-weight: 400;
}
h1.listing + .post-meta .comment-count.new-comments::before {
	color: #009100;
	text-shadow: 0.5px 0.5px 0.5px #fff;
}

/*	LW2 link.
	*/
h1.listing + .post-meta .lw2-link {
	right: 0;
}
h1.listing + .post-meta .lw2-link::before {
	content: "\F0C1";
}
h1.listing + .post-meta .lw2-link:hover {
	text-decoration: none;
}
h1.listing + .post-meta .lw2-link:hover::before {
	color: #00f;
}

/*	Read time.
	*/
h1.listing + .post-meta .read-time {
	right: 70px;
}
h1.listing + .post-meta .read-time::before {
	content: "\F2F2";
	cursor: pointer;
}
h1.listing + .post-meta .read-time::after {
	content: " min";
}
h1.listing + .post-meta .read-time:hover::before {
	color: #777;
}

/*	Word count.
	*/
h1.listing + .post-meta .word-count::before {
	content: "\F15C";
}
h1.listing + .post-meta .read-time.word-count::after {
	content: none;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	h1.listing + .post-meta .post-section {
		width: auto;
		overflow: visible;
		order: 5;
	}
	h1.listing + .post-meta .post-section::before {
		position: static;
	}
}
@media only screen and (max-width: 720px) {
	h1.listing + .post-meta {
		padding-right: 254px;
	}
	h1.listing + .post-meta .karma-value {
		right: 192px;
	}
	h1.listing + .post-meta .comment-count {
		right: 132px;
	}
	h1.listing + .post-meta .read-time {
		right: 56px;
	}
}
@media only screen and (max-width: 520px) {
	h1.listing + .post-meta {
		padding: 0 192px 6px 1px;
	}
	article h1.listing + .post-meta {
		padding-bottom: 6px;
	}
	h1.listing + .post-meta .karma-value {
		right: 136px;
	}
	h1.listing + .post-meta .comment-count {
		right: 76px;
	}
	h1.listing + .post-meta .read-time {
		right: 0;
	}
	h1.listing + .post-meta .lw2-link {
		display: none;
	}
	h1.listing + .post-meta .post-section[href^='/'] {
		order: -1;
		margin-right: 0.75em;
		position: relative;
		top: 1px;
	}
}

/*============*/
/* USER PAGES */
/*============*/
/*======================*/
/* SEARCH RESULTS PAGES */
/*======================*/

#content.user-page h1.page-main-heading {
	border-bottom: 1px solid #ccc;
}

#content.user-page h1.listing,
#content.user-page h1.listing + .post-meta,
#content.search-results-page h1.listing,
#content.search-results-page h1.listing + .post-meta {
	background-color: #eee;
	border-style: solid;
	border-color: #ccc;
}
#content.user-page h1.listing,
#content.search-results-page h1.listing {
	padding: 3px 6px 0 6px;
	border-width: 1px 1px 0 1px;
	margin: 1rem 0 0 0;
	max-width: 100%;
}
#content.user-page.all-user-page h1.listing {
	margin: 2rem 0 0 0;
}
#content.user-page.all-user-page.compact h1.listing,
#content.search-results-page.compact h1.listing {
	margin: 1rem 0 0 0;
}
#content.own-user-page h1.listing,
h1.listing.own-post-listing {
	padding-right: 36px;
}
@media only screen and (hover: hover) and (pointer: fine) {
	#content.user-page h1.listing a:hover,
	#content.user-page h1.listing a:focus,
	#content.search-results-page h1.listing a:hover,
	#content.search-results-page h1.listing a:focus {
		background-color: #eee;
	}
	#content.user-page h1.listing:focus-within::before,
	#content.search-results-page h1.listing:focus-within::before {
		left: -0.625em;
	}
}
#content.user-page h1.listing + .post-meta,
#content.search-results-page h1.listing + .post-meta {
	padding-left: 8px;
	padding-top: 3px;
	border-width: 0 1px 1px 1px;
	margin: 0 0 1rem 0;
	box-shadow: none;
}
#content.user-page h1.listing + .post-meta .post-section,
#content.search-results-page h1.listing + .post-meta .post-section {
	width: auto;
	overflow: visible;
	order: 5;
}
#content.user-page h1.listing + .post-meta .post-section::before,
#content.search-results-page h1.listing + .post-meta .post-section::before {
	position: static;
}
#content.user-page h1.listing + .post-meta .lw2-link,
#content.search-results-page h1.listing + .post-meta .lw2-link {
	right: 6px;
}

/*=--------------------=*/
/*= Conversations list =*/
/*=--------------------=*/

#content.user-page.conversations-user-page h1.listing + .post-meta {
	padding: 5px 6px 5px 8px;
}

/*============*/
/* LOGIN PAGE */
/*============*/

.login-container form input[type='submit'] {
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
}

/*============*/
/* ERROR PAGE */
/*============*/

.error-container input[type="submit"] {
	background-color: #e4e4e4;
	border: 1px solid #ccc;
}

/*===================*/
/* TABLE OF CONTENTS */
/*===================*/

.contents {
	border: 1px solid #ddd;
	background-color: #eee;
}
.post-body .contents li::before {
	color: #999;
	font-feature-settings: "tnum";
}
.post-body .contents a,
.post-body .contents a:hover {
	border: none;
}
.post-body .contents a:hover {
	background-color: #ddd;
	box-shadow: 5px 0 0 0 #ccc;
}
.post-body .contents li:hover::before {
	color: #000;
}

/*==================*/
/* POSTS & COMMENTS */
/*==================*/

.body-text a {
	border-bottom: 1px dotted #bbb;
}
.body-text a:hover {
	text-decoration: none;
	border-bottom: 1px solid currentColor;
}

/*=======*/
/* POSTS */
/*=======*/

/*===========*/
/* POST-META */
/*===========*/

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
.post-meta .post-section.alignment-forum::before,
.comment-meta .alignment-forum {
	text-shadow:
		1px 1px 0   #626dd7, 
		0   1px 0   #626dd7, 
		0   0   5px #626dd7;
}
a.post-section.alignment-forum:hover::before {
	color: #e6e5ff;
}
.post-meta .date {
	color: #888;
}
.post-meta a.author,
.post-meta a.author:visited {
	color: #090;
}
.bottom-post-meta {
	border-top: 1px solid #ddd;
}

/*============*/
/* LINK POSTS */
/*============*/

.post.link-post a.link-post-link {
	text-decoration: none;
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

.comments::before {
	border-top: 1px solid #000;
	box-shadow: 0 3px 3px -4px #000 inset;
}
.listings .comment-thread .comment-meta a.date:focus,
.listings .comment-thread .comment-meta a.permalink:focus {
	color: #888;
	outline: 2px dotted #999;
	position: relative;
	background-color: #fff;
}
.listings .comment-thread .comment-meta a.date:focus {
	padding: 2px 5px 0 5px;
	left: -5px;
}
.listings .comment-thread .comment-meta a.date:focus + * {
	margin-left: -8px;
}
.listings .comment-thread .comment-meta a.permalink:focus {
	padding: 2px 5px 0 5px;
	left: -5px;
}
.listings .comment-thread .comment-meta a.permalink:focus + *:not(.comment-post-title) {
	margin-left: -10px;
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

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	.comment-item .comment-item {
		margin: 0.75em 2px 4px 6px;
		box-shadow: 
			0 0 2px #ccc, 
			0 0 4px #ccc, 
			0 0 7px #ccc;
	}
	.comment-item .comment-item + .comment-item {
		margin: 1.5em 2px 4px 6px;
	}
	.comment-body {
		font-size: 1.125rem;
	}
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
	font-size: 1.25em;
	color: #000;
}
.comment-meta .author:hover {
	text-decoration: none;
	color: #090;
}
.comment-item .author:not(.redacted).original-poster::after {
	opacity: 0.5;
}

.comment-item .karma.active-controls::after,
.comment-item .karma .karma-value::after,
.post .karma.active-controls::after,
.post .karma .karma-value::after,
.author::before {
	background-color: #fff;
	color: #999;
	border-radius: 4px;
	box-shadow: 0 0 0 1px #ddd inset;
}
.comment-item .karma.active-controls::after,
.post .karma.active-controls::after {
	padding: 6px 4px 4px 4px;
	bottom: -44px;
}
.comment-item .karma .karma-value::after,
.post .karma .karma-value::after {
	padding: 2px 8px 1px 8px;
	top: -25px;
	min-width: 56px;
}
.author::before {
	padding: 3px 0.5em 1px 0.5em;
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
/* COMMENT THREAD NAVIGATION */
/*===========================*/

a.comment-parent-link::before {
	color: #bbb;
}
a.comment-parent-link:hover::before {
	background-color: #ffd;
	color: #999;
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

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	a.comment-parent-link:hover::before {
		background-color: unset;
	}
}

/*=======================*/
/* COMMENTS COMPACT VIEW */
/*=======================*/

@media only screen and (hover: hover) and (pointer: fine) {
	#content.compact .comment-item:hover .comment,
	#content.compact .comment-item.expanded .comment {
		background-color: #fff;
		outline: 3px solid #00c;
	}
	#content.compact .comment-item:hover .comment::before,
	#content.compact .comment-item.expanded .comment::before {
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
@media not screen and (hover: hover) and (pointer: fine) {
	#content.compact .comment-thread.expanded .comment {
		background-color: #fff;
		outline: 3px solid #00c;
	}
	#content.compact .comment-thread.expanded .comment::before {
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

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 520px) {
	#content.compact > .comment-thread .comment-item {
		max-height: 110px;
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
	text-shadow: var(--GW-shadow-white-glow);
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
	background-color: #fff;
	border: 1px solid #aaa;
}
.posting-controls input[type='submit']:hover,
.posting-controls input[type='submit']:focus {
	background-color: #ddd;
	border: 1px solid #999;
}

.comment-controls .cancel-comment-button {
	color: #c00;
	text-shadow: 
		0 0 1px #fff,
		0 0 2px #fff;
}
.comment-controls .cancel-comment-button:hover {
	color: #f00;
	text-shadow: var(--GW-shadow-white-glow);
}

.comment-controls .delete-button,
.comment-controls .retract-button {
	color: #a00;
	opacity: 0.85;
}
.comment-controls .edit-button,
.comment-controls .unretract-button {
	color: #090;
}
.comment-controls .action-button:hover {
	color: #f00;
	opacity: 1.0;
}

.button.edit-post-link:not(:hover) {
	color: #090;
}

.posting-controls textarea {
	color: #000;
	background-color: #fff;
	border-color: #aaa;
	box-shadow: 
		0 0 0 1px #eee inset;
}
.posting-controls textarea:focus {
	background-color: #ffd;
	border-color: #00e;
	box-shadow: 
		0 0 0 1px #ddf inset,
		0 0 0 1px #fff,
		0 0 0 2px #00e;
}
.posting-controls.edit-existing-post textarea:focus,
.posting-controls form.edit-existing-comment textarea:focus {
	border-color: #090;
	box-shadow: 
		0 0 0 1px #81ff7f inset,
		0 0 0 1px #fff,
		0 0 0 2px #090;
}

/*= Scroll bars =*/

.posting-controls textarea::-webkit-scrollbar {
	width: 16px;
	background-color: transparent;
}
.posting-controls textarea::-webkit-scrollbar-track {
	background-color: #eee;
	border-left: 1px solid #aaa;
	border-top: 1px solid #eee;
}
.posting-controls textarea:focus::-webkit-scrollbar-track {
	border-left: 1px solid #00e;
	border-top: 1px solid #ddf;
}
.posting-controls textarea::-webkit-scrollbar-thumb {
	background-color: #aaa;
	box-shadow: 0 0 0 1px #eee inset;
	border-left: 1px solid #aaa;
}
.posting-controls textarea:focus::-webkit-scrollbar-thumb {
	border-left: 1px solid #00e;
	background-color: #0040ff;
	box-shadow: 
		0 1px 0 0 #ddf inset,
		0 0 0 1px #eee inset;
}

.posting-controls.edit-existing-post textarea:focus::-webkit-scrollbar-track,
.posting-controls form.edit-existing-comment textarea:focus::-webkit-scrollbar-track {
	border-left: 1px solid #090;
}
.posting-controls.edit-existing-post textarea:focus::-webkit-scrollbar-thumb,
.posting-controls form.edit-existing-comment textarea:focus::-webkit-scrollbar-thumb {
	border-left: 1px solid #090;
	background-color: #28a708;
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
	}
	.textarea-container:focus-within .guiedit-buttons-container {
		background-color: white;
		border-top: 1px solid #ddf;
	}
	.textarea-container:focus-within button.guiedit {
		background-color: #eee;
		border: 1px solid #ddd;
		border-radius: 6px;
	}
	#markdown-hints::after {
		color: #090;
	}
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
	padding: 5px 12px;
}
#edit-post-form label[for='section'] {
	padding-top: 5px;
	padding-bottom: 5px;
}
#edit-post-form input[type='radio'][value='all'] + label {
	border-radius: 8px 0 0 8px;
	border-width: 1px;
}
#edit-post-form input[type='radio'][value='drafts'] + label {
	border-radius: 0 8px 8px 0;
}
@media only screen and (hover: hover) and (pointer: fine) {
	#edit-post-form input[type='radio'] + label:hover,
	#edit-post-form input[type='radio']:focus + label {
		background-color: #ddd;
		color: #000;
	}
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

/*=============*/
/*= SEQUENCES =*/
/*=============*/

.sequence-text {
	font-size: 1.25rem;
}

h1.sequence-chapter {
	font-size: 2.25rem;
	margin: 0;
}

#content.sequences-page::after {
	background-color: #333;
	-webkit-background-clip: text;
	color: transparent;
	text-shadow: 
		rgba(255,255,255,0.5) 0px 3px 3px;
}

/*=======*/
/* LINKS */
/*=======*/

a {
	text-decoration: none;
}
a:hover {
	text-decoration: underline;
}

/*=========*/
/* BUTTONS */
/*=========*/

button:active,
input[type='submit']:active {
	transform: scale(0.9);
}
.button:active {
	transform: scale(0.9);
}
<?php echo $firefox_exclusive; ?> {
	.button:active {
		transform: none;
	}
}

@media only screen and (hover: hover) and (pointer: fine) {
	button:hover,
	input[type='submit']:hover,
	button:focus,
	input[type='submit']:focus {
		text-shadow: var(--GW-shadow-white-glow);
	}

	.button:hover {
		text-shadow: var(--GW-shadow-white-glow);
		text-decoration: none;
	}
	.button:focus:not(:hover) {
		transform: none;
	}
}

/*==========*/
/* HEADINGS */
/*==========*/

.body-text h1,
.body-text h2,
.body-text h4 {
	font-family: var(--GW-UI-font);
}
.body-text h3,
.body-text h5,
.body-text h6 {
	font-weight: 600;
	font-family: var(--GW-UI-font-smallcaps);
	letter-spacing: -0.02em;
}
.body-text h6 {
	color: #555;
}
.body-text h1 {
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
	border: 1px solid #ccc;
}

/*======*/
/* MISC */
/*======*/

hr {
	border-bottom: 1px solid #999;
}

code {
	background-color: #f6f6ff;
	border: 1px solid #f6f6ff;
	border-radius: 4px;
	box-shadow: 
		0 0 0 1px #f6f6ff,
		0 1px 0 1px #f6f6ff,
		0 1px 0 2px #ddf,
		0 0 0 2px #ddf;
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

<?php if ($platform != 'Mac')
echo "{$firefox_exclusive} {";
echo <<<EOT
h1.listing s,
	.post > h1:first-of-type s {
	  position: relative;
	  text-decoration: none;
	}
	h1.listing s::after,
	.post > h1:first-of-type s::after {
	  position: absolute;
	  border-bottom: 3px solid #000;
	  content: "";
	  top: 0;
	  left: 0;
	  width: 100%;
	  height: calc(50% + 6px)
	}
}
EOT;
?>

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
		0 0 5px currentColor;
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
	text-shadow: var(--GW-shadow-white-glow);
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
	background-color: #eef0ff;
}
#content.alignment-forum-index-page::after {
	background-color: #626dd7;
	-webkit-background-clip: text;
	color: transparent;
	text-shadow: 
		rgba(255,255,255,0.5) 0px 3px 3px;
}

@media only screen and (hover: hover) and (pointer: fine) {
	#content.alignment-forum-index-page h1.listing a:hover,
	#content.alignment-forum-index-page h1.listing a:focus {
		background-color: rgba(238,240,255,0.85);
	}	
}
