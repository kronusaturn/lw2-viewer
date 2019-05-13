/**************/
/* GREY THEME */
/**************/

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

	--GW-post-listings-font-weight: 400;

	--GW-body-text-font: 'Source Sans Pro', var(--GW-sans-serif-fallback-font-stack);

	--GW-link-post-link-font-weight: 600;

}
<?php echo $firefox_exclusive; ?> {
	:root {
		--GW-body-text-font-weight: <?php echo ($platform == 'Windows' ? '300' : '400'); ?>;
	}
}

/*	Layout.
	*/
:root {
	--GW-comment-compact-height: 58px;
	--GW-comment-compact-height-mobile: 108px;
	--GW-comment-minimized-height: 38px;
	--GW-comment-minimized-height-mobile: 68px;
}

/*	Color scheme.
	*/
:root {
	--GW-body-background-color: #eee;
	--GW-content-background-color: #fff;

	--GW-hyperlink-color: #f60;
	--GW-hyperlink-visited-color: #ff943b;

	--GW-nav-bar-item-color: #888;
	--GW-nav-bar-item-hover-color: #888;
	--GW-nav-bar-item-active-color: #888;

	--GW-button-color: #999;
	--GW-button-hover-color: #d00;
	--GW-button-active-color: #f00;

	--GW-archive-nav-item-color: rgba(119, 119, 119, 0.75);

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
	--GW-comment-item-expanded-outline-color: #f60;

	--GW-new-comment-item-outline-color: #9037ff;

	--GW-comment-highlight-color: #c79700;
	--GW-comment-highlight-color-faint: #e7b200;

	--GW-vote-button-color: #c8c8c8;
	--GW-upvote-button-color: #00d800;
	--GW-downvote-button-color: #eb4c2a;

	--GW-search-field-placeholder-color: #d00;

	--GW-theme-selector-outline-color: #ddd;
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
	box-shadow: 0px 0px 10px #bbb;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 960px) {
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
}

/*==========*/
/* NAV BARS */
/*==========*/

#primary-bar .nav-inner,
#bottom-bar .nav-inner {
	font-size: 1.25em;
}
#secondary-bar .nav-inner {
	font-size: 0.9375em;
}

.nav-item:not(.nav-current) .nav-inner {
	font-weight: var(--GW-UI-font-weight-light);
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

#nav-item-search form::before {
	opacity: 0.4;
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

/*= Sequences tab =*/

#nav-item-sequences .nav-inner {
	font-weight: 300;
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
	}
	#inbox-indicator::before {
		padding-top: 3px;
	}
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

.page-toolbar > * {
	color: var(--GW-button-color);
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
		border: 1px solid #ddd;
		border-radius: 8px;
	}
	#content.user-page .page-toolbar .button:hover,
	#content.user-page .page-toolbar .rss:hover {
		background-color: #f6f6f6;
		border-color: #ccc;
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
		1px 1px 0 0 #d8d8d8 inset;
}
.sublevel-nav.sort {
	border: 2px solid transparent;
	padding: 18px 0 0 0;
	border-radius: 8px;
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
	top: 18px;
	left: 0;
	width: 100%;
	height: calc(100% - 18px);
	border-radius: 6px;
	box-shadow:
		0 0 0 1px #d8d8d8 inset,
		0 0 0 2px #fff;
}

/*================*/
/* WIDTH SELECTOR */
/*================*/
/* THEME SELECTOR */
/*================*/

#theme-selector button,
#width-selector button {
	box-shadow:
		0 0 0 4px var(--GW-body-background-color) inset,
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
}
#theme-selector button:hover::before,
#theme-selector button.selected::before {
	color: #222;
}
#width-selector button {
	color: #aaa;
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
		color: #888;
		text-shadow: 0.5px 0.5px 0 #fff;
	}
	#theme-selector button {
		background-color: #e6e6e6;
		border-radius: 10px;
	}
	#theme-selector button::after {
		color: #000;
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
	color: #aaa;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 1000px) {
	#theme-tweaker-toggle {
		background-color: #eee;
		box-shadow:
			0   0 0 1px #d8d8d8 inset;
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
		background-color: #d8d8d8;
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
			0 0 0 1px #ccc,
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
		text-shadow: 0 0 1px #fff, 0 0 3px #fff, 0 0 5px #fff;
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
			0 0 0 1px #ccc,
			0 0 0 2px transparent;
		border-radius: 8px;
	}
}

/*=================*/
/* HNS DATE PICKER */
/*=================*/

#hns-date-picker span {
	color: #999;
	text-shadow: 0.5px 0.5px 0 #fff;
}
#hns-date-picker input {
	border: 1px solid #aaa;
	background-color: transparent;
	color: #777;
}
#hns-date-picker input:focus {
	color: #000;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

#hns-date-picker::before {
	border: 1px solid #ccc;
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
	#quick-nav-ui a {
		background-color: #eee;
		box-shadow: 0 0 0 1px #999;
	}

	#new-comment-nav-ui {
		background-color: #eee;
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
	background-color: #aaa;
	-webkit-background-clip: text;
	color: transparent;
	text-shadow: rgba(255, 255, 255, 0.5) 0px 1px 1px;
}
#anti-kibitzer-toggle button:hover::before,
#anti-kibitzer-toggle button:hover::after {
	background-color: #555;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 1020px) {
	#anti-kibitzer-toggle button::before,
	#anti-kibitzer-toggle button::after {
		background-color: #999;
	}
	#anti-kibitzer-toggle {
		box-shadow: 
			0  2px 0 1px #e4e4e4,
			0  0   0 1px #e4e4e4,
			0  2px 0 2px #ccc,
			0  0   0 2px #ccc,
			0  0   0 3px transparent;
		background-color: #e4e4e4;
		border-radius: 8px;
		overflow: hidden;
	}
}
@media only screen and (max-width: 960px) {
	#anti-kibitzer-toggle {
		background-color: #eee;
	}
}

/*======================*/
/* TEXT SIZE ADJUSTMENT */
/*======================*/

#text-size-adjustment-ui button {
	color: #888;
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
	#text-size-adjustment-ui button {
		border: 1px solid #aaa;
		border-radius: 50%;
		box-shadow: 
			0 0 6px #aaa inset,
			0 0 0 1px transparent;
		background-color: #ddd;
	}
	#text-size-adjustment-ui button.decrease,
	#text-size-adjustment-ui button.increase {
		padding: 1px 0 0 0;
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
		0 0 0 5px #ccc inset;
	color: #bbb;
}
#comments-list-mode-selector button:hover,
#comments-list-mode-selector button.selected {
	box-shadow:
		0 0 0 5px #ccc inset;
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
	font-size: 1.5rem;
	margin: 7px 0 0 0;
}

h1.listing a[href^="http"] {
	color: #bbb;
	min-width: 1.125em;
	margin-left: 0;
}
h1.listing a[href^="http"] + a {
	margin-left: 0.125em;
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
		color: #4879ec;
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
	width: 1.25em;
	display: inline-block;
}

h1.listing:not(:focus-within) a:not(:hover) .post-type-prefix {
	opacity: 0.6;
	text-shadow: 0 0 0 currentColor;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 720px) {
	h1.listing {
		font-size: 1.375rem;
		margin-bottom: 2px;
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

h1.listing:not(:focus-within).spam {
	opacity: 0.3;
}
h1.listing:not(:focus-within).spam + .post-meta {
	opacity: 0.15;
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
	padding: 0 0 0 1px;
}

h1.listing + .post-meta > * {
	margin: 0 0.25em 0 0;
}
h1.listing + .post-meta a,
h1.listing + .post-meta a:visited {
	color: #222;
}

h1.listing + .post-meta .comment-count,
h1.listing + .post-meta .read-time,
h1.listing + .post-meta .word-count,
h1.listing + .post-meta .lw2-link {
	margin: 0 0.75em 0 0;
}
h1.listing + .post-meta .comment-count::before,
h1.listing + .post-meta .read-time::before,
h1.listing + .post-meta .word-count::before,
h1.listing + .post-meta .lw2-link::before {
	margin: 0 0.25em 0 0;
	font-family: var(--GW-Font-Awesome);
	font-size: 0.875em;
	color: #ccc;
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
h1.listing + .post-meta .comment-count:hover::before {
	font-weight: 900;
}
h1.listing + .post-meta .comment-count.new-comments::before {
	color: #af8e00;
	text-shadow: 0.5px 0.5px 1.5px #ffdf00;
}
h1.listing + .post-meta .comment-count.new-comments:hover::before {
	text-shadow: none;
}

/*	LW2 link.
	*/
h1.listing + .post-meta .lw2-link {
	opacity: 0.75;
}
h1.listing + .post-meta .lw2-link:hover {
	text-decoration: none;
	color: #777;
}
h1.listing + .post-meta .lw2-link::before {
	content: "\F0C1";
	font-weight: 400;
}
h1.listing + .post-meta .lw2-link:hover::before {
	color: #777;
}
h1.listing + .post-meta .lw2-link span {
	display: none;
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
	color: #777;
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
	margin: 0 0.5em;
}

/*	Link post domain.
	*/
h1.listing + .post-meta .link-post-domain {
	order: 1;
}

/*	Post section.
	*/
h1.listing + .post-meta .post-section::before {
	font-size: 0.9375em;
	left: -1.875em;
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
	border-bottom: 1px solid #ddd;
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
		background-color: #eee;
	}	
}

#content.user-page h1.listing + .post-meta,
#content.search-results-page h1.listing + .post-meta {
	padding: 8px 8px 3px 8px;
	border-width: 0 1px 1px 1px;
}
#content.user-page h1.listing + .post-meta .post-section,
#content.search-results-page h1.listing + .post-meta .post-section {
	overflow: visible;
	order: 5;
	margin: 0 0 0 0.75em;
}
#content.user-page h1.listing + .post-meta .post-section::before,
#content.search-results-page h1.listing + .post-meta .post-section::before {
	position: static;
}

/*=--------------------=*/
/*= Conversations list =*/
/*=--------------------=*/

#content.conversations-user-page h1.listing + .post-meta > * {
	margin: 0 0.75em 0 0;
}
#content.conversations-user-page h1.listing + .post-meta .date::after {
	content: none;
}

/*===============*/
/* CONVERSATIONS */
/*===============*/

#content.conversation-page h1.page-main-heading {
	font-weight: var(--GW-post-listings-font-weight);
	color: #222;
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
.post-body .contents {
	font-size: 1.1875em;
}
.contents li::before {
	color: #999;
	font-feature-settings: "tnum";
}
.post-body .contents a:hover {
	background-color: #e6e6e6;
	box-shadow: 5px 0 0 0 #d8d8d8;
	text-decoration: none;
}
.post-body .contents li:hover::before {
	color: #000;
}

/*=================*/
/* POST NAVIGATION */
/*=================*/

.post-nav-links a,
.post-nav-links a:visited {
	color: #888;
}
.post-nav-links a:hover {
	text-decoration: none;
}

.post-nav-label {
	color: #999;
}

.post-nav-links a:hover .post-nav-title {
	color: #222;
}

@media only screen and (max-width: 900px) {
	.sequence-title {
		border-top: 1px dotted #aaa;
	}
	.post-nav.prev {
		border-right: 1px dotted #aaa;
	}
	.post-nav.next {
		border-left: 1px dotted #aaa;
	}
}

/*==================*/
/* POSTS & COMMENTS */
/*==================*/

.post-body {
	font-size: 1.1875rem;
	line-height: 1.6;
}
.comment-body {
	font-size: 1.125rem;
}
@media (-webkit-max-device-pixel-ratio: 1), (max-resolution: 191dpi) { 
	.post-body {
		font-size: 1.125rem;
	}
	.comment-body {
		font-size: 1.0625rem;
	}
}

/*=======*/
/* POSTS */
/*=======*/

h1.post-title {
	color: #222;
	font-size: 3em;
}

h1.post-title .post-type-prefix {
	opacity: 0.5;
}

/*===========*/
/* POST-META */
/*===========*/

.post .bottom-post-meta {
	border-color: #ddd;
}

article > .post-meta > *,
.post .post-meta > * {
	margin: 0 0.5em;
}

article > .post-meta a,
article > .post-meta a:visited,
.post .post-meta a {
	color: #222;
}

.post-meta > * {
	font-size: 1em;
	color: #666;
}

/*	Comment count.
	*/
.post-meta .comment-count span {
	display: none;
}
.post-meta .comment-count:hover {
	text-decoration: none;
	color: #777;
}
.post-meta .comment-count::before {
	content: "\F086";
	font-family: var(--GW-Font-Awesome);
	font-size: 0.875em;
	color: #ccc;
	margin: 0 0.25em 0 0;
}
.post-meta .comment-count:hover::before {
	color: #777;
	text-shadow: inherit;
}

/*	Post section.
	*/
.post-meta .post-section::before {
	color: #ddd;
}
.post-meta .post-section:hover::before {
	color: #9d7e00;
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

.post.link-post a.link-post-link:hover {
	text-decoration: none;
}
.post.link-post a.link-post-link:hover::before {
	color: #4879ec;
	text-shadow: 
		0.5px 0.5px 0 #fff,
		-0.5px -0.5px 0 #fff,
		0 0 2px #fff,
		0 0 3px #00c;
}
.post.link-post a.link-post-link:hover,
.post.link-post a.link-post-link:focus {
	border-bottom: 2px dotted currentColor;
}
.post.link-post a.link-post-link:focus {
	color: #777;
}

/*==========*/
/* COMMENTS */
/*==========*/

.comments::before {
	border-top: 1px solid #777;
	box-shadow: 0 3px 3px -4px #444 inset;
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
	color: #bbb;
}

/*==================*/
/* COMMENT LISTINGS */
/*==================*/

.listings .comment-thread .comment-meta a.date:focus,
.listings .comment-thread .comment-meta a.permalink:focus {
	color: #999;
	outline: 2px dotted #aaa;
	background-color: #fff;
	line-height: 1;
}
.listings .comment-thread .comment-meta a.date:focus {
	padding: 5px 4px 4px 4px;
	left: -4px;
}
#content.compact .listings .comment-thread .comment-meta a.date:focus {
	padding: 5px 4px 5px 4px;
}
.listings .comment-thread .comment-meta a.date:focus + * {
	margin-left: -8px;
}
.listings .comment-thread .comment-meta a.permalink:focus {
	padding: 3px 5px 4px 5px;
	left: -5px;
}
#content.compact .listings .comment-thread .comment-meta a.permalink:focus {
	padding: 3px 5px 5px 5px;
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

.comment-meta a {
	color: #222;
}

/*	Comment author.
	*/
.comment-meta .author {
	color: #999;
	font-size: 1.25em;
}
.comment-meta .author:hover {
	text-decoration: none;
	color: #090;
}
.comment-item .author:not(.redacted).original-poster::after {
	opacity: 0.8;
}

.author:not(.redacted)::before {
	padding: 2px 0.5em;
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
	opacity: 0.8;
}

.karma-value.redacted {
	opacity: 0.4;
}

.link-post-domain.redacted {
	opacity: 0.7;
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
	filter: hue-rotate(165deg);
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

.vote {
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
	visibility: hidden;
}
.vote.big-vote::after,
.karma:not(.waiting) .vote.clicked-once::after,
.karma:not(.waiting) .vote.clicked-twice::after {
	visibility: visible;
}
.vote.big-vote.clicked-twice::after {
	visibility: hidden;
}
.vote.clicked-once::after {
	color: #c8c8c8;
	text-shadow: none;
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

.post-controls {
	font-weight: var(--GW-UI-font-weight-light);
}

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
	color: #d00;
}
.comment-controls .edit-button,
.comment-controls .unretract-button {
	color: #0b0;
}
.comment-controls .action-button:hover {
	color: #f00;
}

.button.edit-post-link:not(:hover) {
	color: #090;
}
.comment-controls .edit-button::before,
.post-controls .edit-post-link::before {
	font-weight: 400;
}

.posting-controls textarea {
	color: #000;
	background-color: #fff;
	border-color: #aaa;
	box-shadow: 
		0 0 0 1px #eee inset;
}
<?php echo $firefox_exclusive; ?> {
	.posting-controls textarea {
		font-weight: <?php global $platform; echo ($platform == 'Windows' ? '300' : '400'); ?>;
	}
}
.posting-controls textarea:focus {
	background-color: #ffe;
	border-color: var(--GW-hyperlink-color);
	box-shadow: 
		0 0 0 1px #ddf inset,
		0 0 0 1px #fff,
		0 0 0 2px var(--GW-hyperlink-color);
}

/*= Scroll bars =*/

.posting-controls textarea::-webkit-scrollbar {
	width: 16px;
	background-color: transparent;
}
.posting-controls textarea::-webkit-scrollbar-track {
	background-color: #eee;
	border-left: 1px solid #bbb;
	border-top: 1px solid #eee;
}
.posting-controls textarea:focus::-webkit-scrollbar-track {
	border-left: 1px solid #f60;
	border-top: 1px solid #ddf;
}
.posting-controls textarea::-webkit-scrollbar-thumb {
	background-color: #bbb;
	box-shadow: 0 0 0 1px #eee inset;
	border-left: 1px solid #bbb;
}
.posting-controls textarea:focus::-webkit-scrollbar-thumb {
	border-left: 1px solid #f60;
	background-color: #f60;
	box-shadow: 
		0 1px 0 0 #ddf inset,
		0 0 0 1px #eee inset;
}

/* GUIEdit buttons */

.guiedit-buttons-container {
	background-image: linear-gradient(to bottom, #fff 0%, #ddd 50%, #ccc 75%, #aaa 100%);
}

button.guiedit::after {
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
		background-color: #fff;
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

#edit-post-form .post-section-selector label {
	padding: 5px 12px;
	color: #777;
	box-shadow: 0 0 0 1px #ddd;
}
#edit-post-form .post-section-selector:focus-within label {
	background-color: #ffe;
}
#edit-post-form .post-section-selector label:nth-of-type(n+2) {
	margin-left: 1px;
}
#edit-post-form .post-section-selector label:first-of-type {
	border-radius: 8px 0 0 8px;
}
#edit-post-form .post-section-selector label:last-of-type {
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
	z-index: 1;
}
#edit-post-form input[type='radio']:active + label,
#edit-post-form input[type='radio']:checked + label {
	background-color: #ddd;
	color: #000;
	text-shadow: 
		0 -1px 0 #fff,
		0 0.5px 0.5px #000;
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
	background-color: #777;
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
.button:focus:not(:hover) {
	transform: none;
}

@media only screen and (hover: hover) and (pointer: fine) {
	button:hover,
	input[type='submit']:hover,
	.button:hover,
	button:focus,
	input[type='submit']:focus,
	.button:focus {
		text-shadow: var(--GW-shadow-white-glow);
	}

	.button:hover {
		text-decoration: none;
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
	font-family: var(--GW-UI-font-smallcaps);
	letter-spacing: -0.02em;
}
.body-text h6 {
	color: #555;
}
.body-text h1 {
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
	background-color: #ffe;
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
	background-color: #ffe;
	border-color: #aaa;
}

/*=-----------=*/
/*= Fractions =*/
/*=-----------=*/

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
	--GW-content-background-color: #f1f3ff;
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
		background-color: rgba(241, 243, 255, 0.85);
	}	
}
