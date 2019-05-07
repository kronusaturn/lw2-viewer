/*****************/
/* DEFAULT THEME */
/*****************/

/*===========*/
/* VARIABLES */
/*===========*/

/*	Typography.
	*/
:root {
	--GW-UI-font: <?php echo (($platform == 'Mac') ? "'Concourse'" : "'Whitney'"); ?>, var(--GW-sans-serif-fallback-font-stack);
	--GW-UI-font-smallcaps: <?php echo (($platform == 'Mac') ? "'Concourse Smallcaps'" : "'Whitney Smallcaps'"); ?>, var(--GW-sans-serif-fallback-font-stack);
	--GW-UI-font-weight-light: 400;
	--GW-UI-font-weight-heavy: 600;

	--GW-monospaced-font: 'Consolas', var(--GW-monospaced-fallback-font-stack);

	--GW-nav-item-font-weight: 500;

	--GW-post-listings-font: 'Concourse', var(--GW-sans-serif-fallback-font-stack);
	--GW-post-listings-font-weight: 700;

	--GW-body-text-font: 'Charter', var(--GW-serif-fallback-font-stack);

	--GW-comment-meta-author-font-weight: 700;
}
@media only screen and (max-width: 900px) {
	:root {
		--GW-nav-item-font-weight: 600;
	}
}

/*	Layout.
	*/
:root {
	--GW-comment-compact-height: 59px;
	--GW-comment-compact-height-mobile: 108px;
	--GW-comment-minimized-height: 38px;
	--GW-comment-minimized-height-mobile: 68px;
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

	--GW-archive-nav-item-color: rgba(0, 0, 238, 0.75);
	
	--GW-shadow-white-glow:
		0 0 1px #fff,
		0 0 3px #fff,
		0 0 5px #fff;

	--GW-comment-background-color-odd: #eee;
	--GW-comment-background-color-even: #fff;
	--GW-comment-background-color-target: #ffd;

	--GW-comment-item-expanded-background-color: #fff;
	--GW-comment-item-expanded-box-shadow:
		0 0  3px #fff,
		0 0  5px #fff,
		0 0  7px #fff,
		0 0 10px #fff,
		0 0 20px #fff,
		0 0 30px #fff,
		0 0 40px #fff;
	--GW-comment-item-expanded-outline-color: #00c;

	--GW-new-comment-item-outline-color: #5a5;

	--GW-comment-highlight-color: #c79700;
	--GW-comment-highlight-color-faint: #e7b200;

	--GW-vote-button-color: #bbb;
	--GW-upvote-button-color: #0b0;
	--GW-downvote-button-color: #f00;

	--GW-search-field-placeholder-color: #d00;

	--GW-theme-selector-outline-color: #999;
}

/*======*/
/* BASE */
/*======*/

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

.nav-current .nav-inner {
	font-weight: var(--GW-UI-font-weight-heavy);
}

.nav-bar a:hover,
.nav-bar a:focus {
	text-decoration: none;
	text-shadow: var(--GW-shadow-white-glow);
}

/*= Decorative bottom bar =*/

#bottom-bar.decorative {
	color: #d8d8d8;
}

/*= Accesskey hints =*/

.nav-inner::after {
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

/*= Inbox indicator =*/

#inbox-indicator::before {
	padding-top: 1px;
}

/*= Recent Comments tab =*/

#nav-item-recent-comments span {
	margin: 0 5px 0 0;
}

/*===============*/
/* PAGINATION UI */
/*===============*/

#top-nav-bar a::after,
#bottom-bar a::after {
	color: #000;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 960px) {
	#bottom-bar .nav-inner {
		text-shadow: none;
	}
	#bottom-bar .nav-inner:hover::before,
	#bottom-bar .nav-inner:hover::after {
		text-shadow: var(--GW-shadow-white-glow);
	}
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

.rss::before {
	opacity: 0.5;
}
.rss:hover::before {
	opacity: 1.0;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 640px) {
	#content.user-page .page-toolbar .button,
	#content.user-page .page-toolbar .rss {
		border: 1px solid #ccc;
		border-radius: 8px;
	}
	#content.user-page .page-toolbar .button:hover,
	#content.user-page .page-toolbar .rss:hover {
		background-color: #eee;
		border-color: #bbb;
	}
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
	line-height: 1.6;
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
	box-shadow:
		1px 1px 0 0 #aaa inset;
}
.sublevel-nav.sort {
	border: 2px solid transparent;
	padding: 18px 0 0 0;
	border-radius: 8px;
}
.sublevel-nav.sort::before {
	text-transform: uppercase;
	color: #777;
	text-shadow:
		0.5px 0.5px 0 #fff;
}
.sublevel-nav.sort::after {
	content: "";
	position: absolute;
	top: 18px;
	left: 0;
	width: 100%;
	height: calc(100% - 18px);
	border-radius: 6px;
	box-shadow:
		0 0 0 1px #aaa inset,
		0 0 0 2px #fff;
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
}
#theme-selector button:hover::before,
#theme-selector button.selected::before {
	color: #000;
}
#width-selector button {
	color: #999;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 960px) {
	#theme-selector {
		box-shadow: 
			0 0 0 1px var(--GW-theme-selector-outline-color),
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
	}
	#theme-selector button.selected {
		box-shadow:
			0 0 0 5px #aaa inset;
		background-color: #fff;
	}
	#theme-selector button.selected::after {
		text-shadow: 
			0 -1px 0 #fff,
			0 0.5px 0.5px #000;
	}
	@media (-webkit-max-device-pixel-ratio: 1), (max-resolution: 191dpi) {
		#theme-selector button.selected::after {
			text-shadow: 
				0 -1px 0 #fff,
				0 0.5px 0.5px #bbb;
		}
	}
}

/*======================*/
/* THEME TWEAKER TOGGLE */
/*======================*/

#theme-tweaker-toggle button {
	color: #999;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 1000px) {
	#theme-tweaker-toggle {
		background-color: #ddd;
		box-shadow:
			0   0 0 1px #999 inset;
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
#quick-nav-ui a:active {
	transform: scale(0.9);
}
#quick-nav-ui a[href='#comments'].no-comments,
#quick-nav-ui a[href='#answers'].no-answers {
	opacity: 0.4;
	color: #bbb;
}

@media only screen and (hover: hover) and (pointer: fine) {
	#quick-nav-ui a:hover,
	#quick-nav-ui a.highlighted {
		color: #000;
		background-color: #eee;
	}
	#quick-nav-ui a.highlighted {
		transform: scale(0.9);
	}
	#quick-nav-ui a {
		transition:
			color 0.1s ease,
			background-color 0.1s ease,
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

#hns-date-picker::before {
	border: 1px solid #999;
}

@media only screen and (max-width: 1020px) {
	#hns-date-picker,
	#hns-date-picker::before {
		border-radius: 8px 0 0 8px;
	}
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

	#quick-nav-ui {
		background-color: #fff;
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
		box-shadow: 0 -1px 0 0 #999;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button.new-comment-previous {
		border-radius: 7px 0 0 7px;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button.new-comment-next {
		border-radius: 0 7px 7px 0;
	}

	#hns-date-picker,
	#hns-date-picker::before {
		border-radius: 0;
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
	text-shadow: rgba(255, 255, 255, 0.5) 0px 1px 1px;
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
			0  2px 0 1px var(--GW-body-background-color),
			0  0   0 1px var(--GW-body-background-color),
			0  2px 0 2px #999,
			0  0   0 2px #999,
			0  0   0 3px transparent;
		background-color: var(--GW-body-background-color);
		border-radius: 8px;
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
		background-color: #ddd;
	}
	#text-size-adjustment-ui button:hover {
		background-color: #eee;
	}
	#text-size-adjustment-ui button.decrease,
	#text-size-adjustment-ui button.increase {
		padding: 1px 0 0 0;
	}
}

/*=============================*/
/* COMMENTS LIST MODE SELECTOR */
/*=============================*/

#comments-list-mode-selector button {
	color: #aaa;
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
	outline: 1px solid #aaa;
}

.archive-nav *[class^='archive-nav-item'] {
	background-color: #eee;
	outline: 1px solid #ddd;
}

.archive-nav a:hover,
.archive-nav span[class^='archive-nav-item'] {
	background-color: #ddd;
}

.archive-nav a:hover {
	text-decoration: none;
	text-shadow: var(--GW-shadow-white-glow);
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
	font-size: 1.875rem;
	margin: 7px 0 0 0;
}

h1.listing a[href^='/'] {
	color: #000;
}
h1.listing a[href^="http"] {
	color: #19a2e6;
	width: 1.375em;
}

@media only screen and (hover: hover) and (pointer: fine) {
	h1.listing a:hover,
	h1.listing a:focus {
		color: #777;
		background-color: rgba(255, 255, 255, 0.85);
	}	
	h1.listing:focus-within::before {
		color: #00f;
	}
	h1.listing a[href^="http"]:hover {
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
	width: 1.375em;
	display: inline-block;
}

h1.listing:not(:focus-within) a:not(:hover) .post-type-prefix {
	opacity: 0.35;
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

h1.listing:not(:focus-within).spam {
	opacity: 0.15;
}
h1.listing:not(:focus-within).spam + .post-meta {
	opacity: 0.4;
}
h1.listing.spam:hover,
h1.listing.spam + .post-meta:hover,
h1.listing.spam:hover + .post-meta {
	opacity: 1.0;
}
h1.listing.spam .post-type-prefix {
	opacity: 1.0;
}

/*===================*/
/* LISTING POST-META */
/*===================*/

h1.listing + .post-meta {
	padding: 0 330px 14px 1px;
	margin: 0;
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
}
#content.user-page h1.listing + .post-meta,
#content.search-results-page h1.listing + .post-meta {
	padding-left: 8px;
	padding-top: 3px;
	border-width: 0 1px 1px 1px;
	box-shadow: none;
}
#content.user-page h1.listing + .post-meta .post-section,
#content.search-results-page h1.listing + .post-meta .post-section {
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
	border: 1px solid #ddd;
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

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	.comment-body {
		font-size: 1.125rem;
	}
}

/*=======*/
/* POSTS */
/*=======*/

h1.post-title .post-type-prefix {
	opacity: 0.35;
}

/*=================*/
/* POST NAVIGATION */
/*=================*/

.post-nav-links a,
.post-nav-links a:visited {
	color: #000;
}
.post-nav-links a:hover {
	text-decoration: none;
}
.post-nav-title {
	font-weight: var(--GW-UI-font-weight-heavy);
}

.post-nav-label {
	color: #777;
}
.post-nav-links a:hover .post-nav-label {
	color: #888;
}
.post-nav-links a:hover .post-nav-title {
	color: #777;
}

@media only screen and (max-width: 900px) {
	.sequence-title {
		border-top: 1px dotted #777;
	}
	.post-nav.prev {
		border-right: 1px dotted #777;
	}
	.post-nav.next {
		border-left: 1px dotted #777;
	}
}

/*===========*/
/* POST-META */
/*===========*/

.bottom-post-meta {
	border-color: #ddd;
}

.post-meta > * {
	margin: 0 1em 0 0;
}

/*	Author.
	*/
.post-meta a.author,
.post-meta a.author:visited {
	color: #090;
}

/*	Date, link post domain.
	*/
.post-meta .date,
.post-meta .link-post-domain {
	color: #888;
}

/*	Post section.
	*/
.post-meta .post-section::before {
	color: #ddd;
}
.post-meta .post-section:hover::before {
	color: #090;
}
.post-meta .post-section.alignment-forum::before,
.comment-meta .alignment-forum {
	color: #cbccff;
	padding: 1px 0 0 0;
}
.post-meta .post-section.alignment-forum:hover::before,
.comment-meta .alignment-forum {
	color: #626dd7;
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

.comment-item {
	border: 1px solid #ccc;
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
	.comment-item .comment-item {
		box-shadow: 
			0 0 2px #ccc, 
			0 0 4px #ccc, 
			0 0 7px #ccc;
	}
}

/*=========*/
/* ANSWERS */
/*=========*/

.answer-item {
	border-width: 2px;
	border-color: #bbb;
}
.answer-item::after {
	left: -2px;
	text-transform: uppercase;
	color: #aaa;
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
	padding-top: 2px;
}
.listings .comment-thread .comment-meta a.date:focus + * {
	margin-left: -10px;
}
.listings .comment-thread .comment-meta a.permalink:focus + *:not(.comment-post-title) {
	margin-left: -10px;
}

/*===========================*/
/* COMMENT THREAD NAVIGATION */
/*===========================*/

a.comment-parent-link::before {
	color: #bbb;
}
a.comment-parent-link:hover::before {
	color: #999;
}

.comment-child-link::before {
	color: #aaa;
}

.comment-item-highlight,
.comment-item-highlight-faint {
	border-color: var(--GW-comment-highlight-color);
	filter:
		drop-shadow(0 0 2px var(--GW-comment-item-outline-color))
		drop-shadow(0 0 4px var(--GW-comment-highlight-color))
		drop-shadow(0 0 4px var(--GW-comment-highlight-color)) !important;
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

/*	Comment author.
	*/
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

.author:not(.redacted)::before {
	padding: 3px 0.5em 1px 0.5em;
}

/*	Karma controls.
	*/
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
	box-shadow:
		0 0 6px -2px var(--GW-comment-item-outline-color) inset, 
		0 0 4px var(--GW-comment-item-outline-color), 
		0 0 6px var(--GW-comment-item-outline-color);
}

.answer-item.new-comment::before {
	box-shadow: none;
}
.answer-item.new-comment {
	filter: drop-shadow(0 0 2px var(--GW-comment-item-outline-color));
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
	width: 17px;
	height: 17px;
	display: inline-block;
	position: relative;
}

.upvote::before {
	content: "\F055";
}
.downvote::before {
	content: "\F056";
}

.vote::before,
.vote.big-vote.clicked-once::before,
.vote.big-vote.clicked-twice::before,
.vote.selected.clicked-once::before {
	font-weight: 400;
}
.vote.big-vote::before,
.vote.clicked-twice::before,
.vote:not(.selected):hover::before  {
	font-weight: 900;
}

.vote:hover::before {
	filter: drop-shadow(0 0 1px #fff);
}

/*===========================*/
/* COMMENTING AND POSTING UI */
/*===========================*/

.posting-controls input[type='submit'],
.posting-controls .cancel-post-editing-button {
	background-color: #fff;
	border: 1px solid #aaa;
}
.posting-controls input[type='submit']:hover,
.posting-controls input[type='submit']:focus,
.posting-controls .cancel-post-editing-button:hover,
.posting-controls .cancel-post-editing-button:focus {
	background-color: #ddd;
	border: 1px solid #999;
}

.comment-controls .cancel-comment-button,
.posting-controls .cancel-post-editing-button {
	color: #c00;
	text-shadow: 
		0 0 1px #fff,
		0 0 2px #fff;
}
.comment-controls .cancel-comment-button:hover,
.posting-controls .cancel-post-editing-button:hover {
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
#markdown-hints .markdown-hints-row code {
	box-shadow: none;
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

.post-controls {
	font-weight: var(--GW-UI-font-weight-heavy);
}

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

/*===========*/
/* SEQUENCES */
/*===========*/

.sequence-text {
	font-size: 1.25rem;
}

h1.sequence-chapter {
	font-size: 2.25rem;
}

#content.sequences-page::after {
	background-color: #333;
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
a:hover {
	text-decoration: underline;
}

/*=========*/
/* BUTTONS */
/*=========*/

button:active,
input[type='submit']:active,
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

<?php if ($platform != 'Mac') {
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
}
?>

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
		0 0 5px currentColor;
}

.qualified-linking-toolbar {
	border: 1px solid #000;
	background-color: #fff;
}
.qualified-linking-toolbar a,
.qualified-linking-toolbar button {
	background-color: #eee;
	border: 1px solid #ccc;
	border-radius: 4px;
}
.qualified-linking-toolbar a:hover,
.qualified-linking-toolbar button:hover {
	text-decoration: none;
	background-color: #ddd;
	text-shadow: var(--GW-shadow-white-glow);
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

/*===============*/
/* KEYBOARD HELP */
/*===============*/

#keyboard-help-overlay .keyboard-help-container {
	background-color: #fff;
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
		1px 0 0 0 #fff inset,
		-1px 0 0 0 #fff inset,
		0 1px 0 0 #fff inset,
		0 -1px 0 0 #fff inset;
	border-left: 1px solid #ddd;
}
#keyboard-help-overlay .keyboard-help-container::-webkit-scrollbar-thumb:hover {
	background-color: #aaa;
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
	padding: 5px 8px 2px 8px;
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

#content.alignment-forum-index-page {
	--GW-content-background-color: #eef0ff;
}
#content.alignment-forum-index-page::after {
	background-color: #626dd7;
	-webkit-background-clip: text;
	color: transparent;
	text-shadow: 
		rgba(255, 255, 255, 0.5) 0px 3px 3px;
}

@media only screen and (hover: hover) and (pointer: fine) {
	#content.alignment-forum-index-page h1.listing a:hover,
	#content.alignment-forum-index-page h1.listing a:focus {
		background-color: rgba(238, 240, 255, 0.85);
	}	
}
