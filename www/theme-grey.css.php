/**************/
/* GREY THEME */
/**************/

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

	--GW-post-listings-font-weight: 400;

	--GW-body-text-font: 'Source Sans Pro', 'Trebuchet MS', 'Helvetica', 'Arial', 'Verdana', sans-serif;

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
	--GW-content-side-padding: 50px;
	--GW-sequence-page-content-side-padding: 50px;
	--GW-user-page-content-side-padding: 30px;
	--GW-recent-comments-page-content-side-padding: 30px;
	--GW-conversation-page-content-side-padding: 30px;
	--GW-post-page-content-side-padding: 30px;
	--GW-post-side-padding: 30px;
	--GW-edit-post-page-content-side-padding: 30px;

	--GW-comment-compact-height: 59px;
	--GW-comment-compact-height-mobile: 108px;
	--GW-comment-minimized-height: 38px;
	--GW-comment-minimized-height-mobile: 68px;
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

#bottom-bar.decorative::after {
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
#nav-item-search button {
	color: #999;
}
#nav-item-search input::placeholder {
	color: #d00;
	font-weight: normal;
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

/*= Top pagination UI hover tooltips =*/

#top-nav-bar a::after,
#bottom-bar a::after {
	color: #000;
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

#bottom-bar .nav-item a::before,
#top-nav-bar a::before {
	font-weight: 400;
}

/*==============*/
/* PAGE TOOLBAR */
/*==============*/

.page-toolbar > * {
	color: var(--GW-button-color);
}
.page-toolbar .button::before {
	font-weight: 400;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 640px) {
	#content.user-page .page-toolbar > * {
		border: 1px solid #ddd;
		border-radius: 8px;
	}
	#content.user-page .page-toolbar > .rss {
		padding-top: calc(0.5em + 1px);
		padding-bottom: calc(0.5em - 1px);
	}
	#content.user-page .page-toolbar > *:hover {
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
	box-shadow: 1px 1px 0 0 #ccc inset;
}
.sublevel-nav.sort {
	border: 2px solid transparent;
	padding: 18px 0 0 0;
	border-radius: 8px;
}
.sublevel-nav.sort::before {
	text-transform: uppercase;
	color: #777;
	text-shadow: 0.5px 0.5px 0 #fff;
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
		0 1px 0.5px 0 #fff inset,
		0 1px 0.5px 0 #fff inset,
		0 1px 0.5px 0 #fff inset,
		0 18px 0 0 #fff inset,
		0 0 0 1px #ccc inset,
		0 18px 0 1px #ccc inset,
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
	background-color: var(--GW-body-background-color);
}
#theme-selector button:hover::before,
#theme-selector button.selected::before {
	color: #777;
}
#width-selector button::after {
	color: #aaa;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 1160px) {
	#theme-selector:hover::after {
		background-color: #ddd;
	}
}
@media only screen and (max-width: 1000px) {
	#theme-selector {
		background-color: var(--GW-body-background-color);
		box-shadow: 
			0 0 0 1px #bbb,
			0 0 0 2px transparent;
	}
	#theme-selector:hover::after {
		background-color: #bbb;
	}	
}
@media only screen and (max-width: 960px) {
	#theme-selector {
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
	color: #888;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 1080px) {
	#theme-tweaker-toggle button {
		border: 1px solid #888;
		box-shadow: 
			0 0 10px #888 inset,
			0 0 0 1px transparent;
		border-radius: 50%;
		transform: scale(0.8);
	}
}
@media only screen and (max-width: 1000px) {
	#theme-tweaker-toggle button {
		background-color: #eee;
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
#quick-nav-ui a[href='#comments'].no-comments {
	opacity: 0.4;
	color: #bbb;
}
@media only screen and (hover: hover) and (pointer: fine) {
	#quick-nav-ui a:hover  {
		color: #000;
		background-color: #d8d8d8;
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
	color: #ccc;
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
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 1020px) {
	#new-comment-nav-ui .new-comments-count::before {
		background-color: #e4e4e4;
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
	font-weight: 600;
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

#hns-date-picker.flipped {
	background-color: var(--GW-body-background-color);
	opacity: 1.0;
}
#hns-date-picker.flipped::before {
	border: 1px solid #ccc;
	border-width: 1px 0 1px 1px;
}

@media only screen and (max-width: 1020px) and (min-width: 961px) {
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
	#quick-nav-ui {
		background-color: #fff;
	}
	#quick-nav-ui a {
		background-color: #eee;
		box-shadow: 0 0 0 1px #999;
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
		0 0 0 5px #bbb inset;
	opacity: 0.8;
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
	border-color: #fff;
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

/*===============*/
/* KEYBOARD HELP */
/*===============*/

#nav-item-about button.open-keyboard-help {
	font-weight: 400;
	top: -1px;
	color: #888;
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
}

@media only screen and (hover: hover) and (pointer: fine) {
	h1.listing a:hover,
	h1.listing a:focus {
		color: #777;
		background-color: rgba(255, 255, 255, 0.85);
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

h1.listing.spam {
	opacity: 0.3;
}
h1.listing.spam + .post-meta {
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
	margin: 0 0 0.875em 0;
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
	color: #0c0;
	text-shadow: 0.5px 0.5px 1.5px #7fff64;
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
	color: #777;
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

h1.post-title {
	color: #222;
	font-size: 3em;
}

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

.post .bottom-post-meta {
	border-color: #ddd;
}

/*===========*/
/* POST-META */
/*===========*/

.post-meta > * {
	font-size: 1em;
	color: #666;
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
	color: #ccc;
}
.post-meta .comment-count:hover,
.post-meta .lw2-link:hover {
	text-decoration: none;
	text-shadow: 
		0 0 0.5px #fff,
		0 0 1px #fff,
		0 0 8px #777;
	color: #777;
}
.post-meta .comment-count::before {
	content: "\F086";
}
.post-meta .comment-count:hover::before {
	color: #777;
	text-shadow: inherit;
}

/*	Post section.
	*/
.post-meta .post-section::before,
.comment-meta .alignment-forum {
	color: #fff;
}
.post-meta .post-section::before {
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
	.comment-item .comment-item {
		margin: 0.75em 2px 4px 6px;
	}
	.comment-item .comment-item + .comment-item {
		margin: 1.5em 2px 4px 6px;
	}
}

/*==================*/
/* COMMENT LISTINGS */
/*==================*/

.listings .comment-thread .comment-meta a.date:focus,
.listings .comment-thread .comment-meta a.permalink:focus {
	color: #999;
	outline: 2px dotted #aaa;
	position: relative;
	background-color: #fff;
}
.listings .comment-thread .comment-meta a.date:focus {
	padding: 0 4px;
	left: -4px;
}
.listings .comment-thread .comment-meta a.date:focus + * {
	margin-left: -8px;
}
.listings .comment-thread .comment-meta a.permalink:focus {
	padding: 0 5px;
	left: -5px;
}
.listings .comment-thread .comment-meta a.permalink:focus + *:not(.comment-post-title) {
	margin-left: -10px;
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

@media only screen and (hover: hover) and (pointer: fine) {
	#content.compact > .comment-thread .comment-item:hover .comment,
	#content.compact > .comment-thread .comment-item.expanded .comment {
		background-color: #fff;
		outline: 3px solid var(--GW-hyperlink-color);
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
@media not screen and (hover: hover) and (pointer: fine) {
	#content.compact > .comment-thread.expanded .comment-item .comment {
		background-color: #fff;
		outline: 3px solid var(--GW-hyperlink-color);
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

/*===========================*/
/* HIGHLIGHTING NEW COMMENTS */
/*===========================*/

.new-comment::before {
	outline: 2px solid #9037ff;
	box-shadow:
		0 0 6px -2px #9037ff inset, 
		0 0 4px #9037ff, 
		0 0 6px #9037ff;
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

.upvote,
.downvote {
	color: #c8c8c8;
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
	background-image: linear-gradient(to bottom, #fff 0%, #ddd 50%, #ccc 75%, #aaa 100%);
}

.posting-controls.edit-existing-post .guiedit-buttons-container button,
.posting-controls form.edit-existing-comment .guiedit-buttons-container button {
	color: #080;
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

.post-controls {
	font-weight: var(--GW-UI-font-weight-light);
}

#edit-post-form .post-meta-fields label[for='link-post'] {
	margin: 0 0 0.5em 0.125em;
	border: none;
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
	font-size: 1.125rem;
}

h1.sequence-chapter {
	font-size: 2rem;
	margin: 0;
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
	background-color: #f1f3ff;
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
