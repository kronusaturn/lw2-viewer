/*********************/
/* ULTRAMODERN THEME */
/*********************/

/*===========*/
/* VARIABLES */
/*===========*/

/*	Typography.
	*/
:root {
	--GW-UI-font: 'Proxima Nova', var(--GW-sans-serif-fallback-font-stack);
	--GW-UI-font-weight-light: 300;
	--GW-UI-font-weight-heavy: 400;

	--GW-monospaced-font: 'Tired of Courier', var(--GW-monospaced-fallback-font-stack);

	--GW-post-listings-font: 'Raleway', var(--GW-sans-serif-fallback-font-stack);
	--GW-post-listings-font-weight: 200;

	--GW-body-text-font: 'Raleway', var(--GW-sans-serif-fallback-font-stack);
	--GW-body-text-font-weight: <?php echo ($platform == 'Mac' ? '300' : '400'); ?>;

	--GW-content-headings-font-weight: 400;
}

/*	Layout.
	*/
:root {
	--GW-comment-compact-height: 55px;
	--GW-comment-compact-height-mobile: 104px;
	--GW-comment-minimized-height: 36px;
	--GW-comment-minimized-height-mobile: 61px;

	--GW-comment-listings-date-focused-adjust-y: -1px;
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
	--GW-shadow-text-glow:
		0.0px 0.0px 1.0px #777,
		0.5px 0.5px 1.0px #aaa,
		0.5px 0.5px 1.0px #bbb;
	--GW-shadow-link-glow:
		0.0px 0.0px 1.0px #777,
		0.5px 0.5px 1.0px #ccc,
		0.5px 0.5px 1.0px #bbb;

	--GW-comment-item-outline-width: 2px;

	--GW-comment-item-new-comment-outline-color: #f00;
	--GW-comment-item-focused-outline-color: #ccc;
	--GW-comment-item-higlight-color: #e7b200;
	--GW-comment-item-highlight-faint-color: #f8e7b5;

	--GW-comment-item-expanded-background-color: #999;
	--GW-comment-item-expanded-box-shadow:
		0 0  3px #999,
		0 0  5px #999,
		0 0  7px #999,
		0 0 10px #999,
		0 0 20px #999,
		0 0 30px #999,
		0 0 40px #999;

	--GW-vote-button-color: #666;
	--GW-upvote-button-color: #00d800;
	--GW-downvote-button-color: #eb4c2a;

	--GW-search-field-placeholder-color: #faa;
}

/*======*/
/* BASE */
/*======*/

body {
	color: #444;
	text-shadow: var(--GW-shadow-text-glow);
}
#content {
	line-height: 1.55;
}

p {
	font-family: var(--GW-body-text-font);
	font-weight: var(--GW-body-text-font-weight);
	color: var(--GW-body-text-color);
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 960px) {
	#ui-elements-container > div[id$='-ui-toggle'] button,
	#theme-selector .theme-selector-close-button  {
		color: #666;
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
		0 1.5px 1.5px -1.5px #777;
}

.nav-inner {
	font-weight: var(--GW-UI-font-weight-light);
}
.nav-current .nav-inner {
	filter: invert(100%) contrast(175%);
	font-weight: var(--GW-UI-font-weight-heavy);
}

/*= Decorative bottom bar =*/

#bottom-bar.decorative::before {
	background: none;
	box-shadow:
		0 calc(1em - 1px) 0 0 #888 inset,
		0 calc(1em + 0px) 0 0 #666 inset,
		0 calc(1em + 1.5px) 1.5px -1.5px #bbb inset,
		0 calc(1em + 1.5px) 1.5px -1.5px #777 inset;
}
#bottom-bar.decorative::after {
	color: #555;
	font-weight: 200;
}

/* Accesskey hints */

.nav-inner::after {
	display: none;
}

/* Search tab */

#nav-item-search form::before {
	font-weight: 400;
}
#nav-item-search button {
	padding-top: 1px;
}
#nav-item-search input::placeholder {
	font-weight: var(--GW-UI-font-weight-heavy);
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
		font-weight: 300;
	}
}

/*= Inbox indicator =*/

#inbox-indicator {
	text-shadow: none;
}
#inbox-indicator::before {
	color: #666;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 960px) {
	#nav-item-about {
		margin-right: unset;
	}
}
@media only screen and (max-width: 900px) {
	#secondary-bar .nav-inner::before {
		line-height: 1;
	}
	#secondary-bar .nav-item:not(#nav-item-search) .nav-inner {
		padding: 6px 10px;
	}
	#nav-item-search button::before {
		font-weight: 400;
	}
	#primary-bar .nav-inner::before, 
	#secondary-bar .nav-inner::before {
		opacity: 0.8;
		font-weight: 300;
	}
	@media (-webkit-max-device-pixel-ratio: 1), (max-resolution: 191dpi) { 
		#secondary-bar .nav-inner::before {
			font-weight: 400;
		}
	}
	#primary-bar .nav-current .nav-inner::before, 
	#secondary-bar .nav-current .nav-inner::before {
		font-weight: 400;
		opacity: 1.0;
	}
	#inbox-indicator::before {
		padding-top: 2px;
	}
}

/*===============*/
/* PAGINATION UI */
/*===============*/

#top-nav-bar a::before {
	font-weight: 300;
}

#top-nav-bar a::after,
#bottom-bar a::after {
	color: #222;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 960px) {
	#bottom-bar .nav-inner {
		text-shadow: none;
	}
	#bottom-bar .nav-inner::before,
	#bottom-bar .nav-inner::after {
		text-shadow: var(--GW-shadow-text-glow);
	}
	#bottom-bar .nav-inner:hover::before,
	#bottom-bar .nav-inner:hover::after {
		text-shadow: var(--GW-shadow-link-glow);
	}
}

/*==============*/
/* PAGE TOOLBAR */
/*==============*/

.new-post::before,
.new-private-message::before,
.logout-button::before {
	font-weight: 300;
}

.rss::before {
	opacity: 0.65;
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
		border: 1px solid #aaa;
		border-radius: 8px;
	}
	#content.user-page .page-toolbar .button:hover,
	#content.user-page .page-toolbar .rss:hover {
		border-color: #ccc;
	}
}

/*==============*/
/* SUBLEVEL NAV */
/*==============*/

.sublevel-nav .sublevel-item {
	color: #222;
	font-weight: var(--GW-UI-font-weight-light);
	line-height: 1.6;
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
	color: #555;
	box-shadow:
		0 0 0 4px #888 inset,
		0 0 0 5px #555 inset;
}
#width-selector button.selected {
	box-shadow:
		0 0 0 1px #888 inset,
		0 0 0 2px #555 inset,
		0 0 0 4px #888 inset,
		0 0 0 5px #555 inset;
}
#width-selector button:not(.selected):hover {
	box-shadow:
		0 0 0 1px #888 inset,
		0 0 0 2px #bbb inset,
		0 0 0 4px #888 inset,
		0 0 0 5px #bbb inset;
	color: #bbb;
}

#width-selector button::after {
	color: #ccc;
	font-weight: var(--GW-UI-font-weight-light);
}

/*================*/
/* THEME SELECTOR */
/*================*/

#theme-selector button {
	box-shadow:
		0 0 0 5px #888 inset;
	filter: contrast(50%);
}
#theme-selector:hover button {
	filter: none;
}
#theme-selector button.selected {
	box-shadow:
		0 0 0 2px #888 inset,
		0 0 0 3px #444 inset,
		0 0 0 5px #888 inset;
}
#theme-selector button:not(.selected):hover {
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
#theme-selector button:hover::before {
	color: #fff;
}
#theme-selector button.selected::before {
	color: #000;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 960px) {
	#theme-selector {
		background-color: #888;
		box-shadow: 
			0 0 0 1px #666,
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
		filter: none;
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

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 1000px) {
	#theme-tweaker-toggle button {
		color: #555;
	}
	#theme-tweaker-toggle {
		background-color: var(--GW-body-background-color);
	}
}

/*=================*/
/* QUICKNAV WIDGET */
/*=================*/

#quick-nav-ui a {
	color: #555;
	font-weight: 400;
	border-radius: 4px;
	box-shadow: 0 0 0 1px #666;
}
#quick-nav-ui a[href='#bottom-bar'],
#quick-nav-ui a[href='#comments'] {
	line-height: 1.8;
}
#quick-nav-ui a:active {
	transform: scale(0.9);
}
#quick-nav-ui a[href='#comments'].no-comments,
#quick-nav-ui a[href='#answers'].no-answers {
	opacity: 0.4;
	color: #777;
}
@media only screen and (hover: hover) and (pointer: fine) {
	#quick-nav-ui a:hover,
	#quick-nav-ui a.highlighted   {
		color: #444;
		box-shadow: 0 0 0 1px #ccc;
	}
	#quick-nav-ui a.highlighted {
		transform: scale(0.9);
	}
	#quick-nav-ui a {
		transition:
			color 0.1s ease,
			background-color 0.1s ease,
			box-shadow 0.1s ease;
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
	color: #555;
}
#new-comment-nav-ui .new-comments-count::after {
	color: #333;
}
#new-comment-nav-ui .new-comment-sequential-nav-button {
	color: #444;
}
#new-comment-nav-ui .new-comment-sequential-nav-button:not(:hover) {
	text-shadow: none;
}
#new-comment-nav-ui .new-comment-sequential-nav-button:disabled {
	color: #808080;
}
@media only screen and (hover: hover) and (pointer: fine) {
	#new-comment-nav-ui .new-comments-count:hover {
		text-shadow: var(--GW-shadow-white-glow);
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button:hover,
	#new-comment-nav-ui .new-comment-sequential-nav-button.highlighted {
		color: #aaa;
		text-shadow: none;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button.highlighted {
		text-shadow: var(--GW-shadow-white-glow);
		transform: scale(0.9);
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button {
		transition:
			color 0.1s ease,
			text-shadow 0.1s ease,
			transform 0.1s ease;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button:focus {
		text-shadow: var(--GW-shadow-white-glow);
	}
}

/*=================*/
/* HNS DATE PICKER */
/*=================*/

#hns-date-picker input {
	border: 1px solid #666;
	color: #555;
	font-size: 0.9375em;
}
#hns-date-picker input:focus {
	color: #000;
	border: 1px solid #ccc;
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

	#quick-nav-ui {
		background-color: #999;
	}
	#quick-nav-ui a {
		background-color: #888;
		box-shadow: 0 0 0 1px #444;
		color: #444;
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
		margin-bottom: 2px;
	}
	#hns-date-picker::before {
		border: 1px solid #444;
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
	text-shadow: rgba(255, 255, 255, 0.35) 0px 0.5px 0.5px;
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
	color: #555;
	box-shadow:
		0 0 0 4px #888 inset,
		0 0 0 5px #555 inset;
}
#comments-list-mode-selector button.selected {
	box-shadow:
		0 0 0 1px #888 inset,
		0 0 0 2px #555 inset,
		0 0 0 4px #888 inset,
		0 0 0 5px #555 inset;
}
#comments-list-mode-selector button:not(.selected):hover {
	color: #bbb;
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
	min-width: 1.125em;
	left: 1px;
}

@media only screen and (hover: hover) and (pointer: fine) and (min-width: 961px) {
	h1.listing a[href^='/posts/'],
	h1.listing a[href^='/s/'],
	h1.listing a[href^='/conversation?id'] {
		padding-bottom: 2px;
	}
}

@media only screen and (hover: hover) and (pointer: fine) {
	h1.listing a:hover,
	h1.listing a:focus {
		background-color: rgba(136, 136, 136, 0.85);
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

/*=------------------=*/
/*= Post type prefix =*/
/*=------------------=*/

h1.listing .post-type-prefix {
	width: 1.125em;
}

h1.listing:not(:focus-within) a:not(:hover) .post-type-prefix {
	color: #8fd2ff;
	text-shadow: 
		0.0px 0.0px 2.0px #777,
		0.5px 0.5px 2.0px #444,
		0.5px 0.5px 2.0px #333,
		0.5px 0.5px 2.0px #a2f2f1;
}

/*=-----------------------=*/
/*= In-listing edit links =*/
/*=-----------------------=*/

h1.listing .edit-post-link {
	right: -1.5em;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 960px) {
	h1.listing .edit-post-link {
		right: 0;
	}
}
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

h1.listing:not(:focus-within).spam {
	opacity: 0.35;
}
h1.listing:not(:focus-within).spam + .post-meta {
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
	padding: 0 0 0 1px;
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
h1.listing + .post-meta .comment-count::before,
h1.listing + .post-meta .read-time::before,
h1.listing + .post-meta .word-count::before,
h1.listing + .post-meta .lw2-link::before {
	margin: 0 0.25em 0 0;
	font-family: var(--GW-Font-Awesome);
	font-size: 0.875em;
	color: #666;
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
	color: #004abe;
	opacity: 0.75;
}
h1.listing + .post-meta .comment-count.new-comments:hover::before {
	color: #00f;
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
	font-weight: var(--GW-UI-font-weight-heavy);
	color: #333;
	border-bottom: 1px solid #666;
	box-shadow:
		0 1.5px 1.5px -1.5px #bbb,
		0 1.5px 1.5px -1.5px #777;
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
		1.5px 0 1.5px -1.5px #777 inset;
}
#content.user-page h1.listing,
#content.search-results-page h1.listing {
	padding: 3px 6px 0 6px;
}
#content.own-user-page h1.listing,
h1.listing.own-post-listing {
	padding-right: 12px;
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

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 960px) {
	#content.own-user-page h1.listing,
	h1.listing.own-post-listing {
		padding-right: 12px;
	}
}

/*===============*/
/* CONVERSATIONS */
/*===============*/

#content.conversation-page h1.page-main-heading {
	color: #222;
}

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

/*============*/
/* ERROR PAGE */
/*============*/

/*=------------=*/
/*= Retry form =*/
/*=------------=*/

.reassurance {
	border-top: 1px solid #666;
	box-shadow:
		0 1.5px 1.5px -1.5px #bbb inset,
		0 1.5px 1.5px -1.5px #777 inset;
}
.reassurance .saved-comment-content {
	border: 1px solid #ddd;
}

/*===================*/
/* TABLE OF CONTENTS */
/*===================*/

.post-body .contents {
	background-color: var(--GW-body-background-color);
}
.post-body .contents ul {
	font-size: 0.85em;
}
.post-body .contents li::before {
	font-feature-settings: "tnum";
}
.post-body .contents a:hover {
	box-shadow:
		0 0 0 1px #666,
		4px 0 0 1px #666,
		1.5px 1.5px 1.5px -1.5px #bbb inset,
		1.5px 1.5px 1.5px -1.5px #777 inset,
		2.5px 2.5px 1.5px -1.5px #bbb,
		2.5px 2.5px 1.5px -1.5px #777,
		6.5px 2.5px 1.5px -1.5px #bbb,
		6.5px 2.5px 1.5px -1.5px #777
}
.post-body .contents li:hover::before {
	text-shadow: var(--GW-shadow-link-glow);
}

/*=================*/
/* POST NAVIGATION */
/*=================*/

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
	text-shadow: var(--GW-shadow-text-glow);
}
.body-text strong,
.body-text b {
	font-weight: 600;
}

.body-text a:link,
p a:link {
	color: inherit;
	text-shadow: 
		0px 0px 1px #bd5984, 
		0.5px 0.5px 1px #f68a84, 
		0.5px 0.5px 1px #ff9b8c;
}
.body-text a:visited,
p a:visited {
	color: inherit;
	text-shadow:
		0px 0px 1px #a766dd, 
		0.5px 0.5px 1px #d9f, 
		0.5px 0.5px 1px #efa9ff;
}
.body-text a:hover,
.body-text a:focus,
p a:hover,
p a:focus {
	color: #444;
	text-shadow:
		0px 0px 1px #bd5984, 
		0.5px 0.5px 1px #f68a84, 
		0.5px 0.5px 1px #ff9b8c,
		0px 0px 5px #f60;
}

.post-body {
	font-size: 1.1875rem;
}
.comment-body {
	font-size: 1.125rem;
}

/*=======*/
/* POSTS */
/*=======*/

h1.post-title {
	color: #000;
	font-size: 3em;
}

h1.post-title .post-type-prefix {
	font-variant: small-caps;
	font-size: 0.625em;
}

/*===========*/
/* POST-META */
/*===========*/

.bottom-post-meta {
	border-color: #666;
	box-shadow:
		0 1.5px 1.5px -1.5px #bbb,
		0 1.5px 1.5px -1.5px #777,
		0 1.5px 1.5px -1.5px #bbb inset,
		0 1.5px 1.5px -1.5px #777 inset;
}
#content:not(.in-sequence) .bottom-post-meta {
	box-shadow:
		0 1.5px 1.5px -1.5px #bbb inset,
		0 1.5px 1.5px -1.5px #777 inset;
}

article > .post-meta > *,
.post .post-meta > * {
	margin: 0 0.5em;
}

.post-meta > * {
	font-size: 1em;
}

.post-meta a:hover {
	color: #000;
}

/*	Comment count.
	*/
.post-meta .comment-count span {
	display: none;
}
.post-meta .comment-count::before {
	font-family: var(--GW-Font-Awesome);
	margin: 0 0.25em 0 0;
	font-size: 0.875em;
	color: #666;
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
.post-meta .post-section::before {
	color: #777;
}
.post-meta .post-section:hover::before {
	color: #050;
}
.post-meta .post-section.alignment-forum::before,
.comment-meta .alignment-forum {
	color: #6f749f;
	padding: 1px 0 0 0;
}
.post-meta .post-section.alignment-forum:hover::before,
.comment-meta .alignment-forum {
	color: #1f3e9f;
}

/*============*/
/* LINK POSTS */
/*============*/

.post.link-post a.link-post-link:focus {
	text-decoration: dotted underline;
}

/*==========*/
/* COMMENTS */
/*==========*/

.comments::before {
	border-top: 1px solid #666;
	box-shadow:
		0 1.5px 1.5px -1.5px #bbb inset,
		0 1.5px 1.5px -1.5px #777 inset;
}
.comment-item {
	border: 1px solid transparent;
	border-left-color: #666;
	box-shadow:
		1.5px 0 1.5px -1.5px #bbb inset, 
		1.5px 0 1.5px -1.5px #777 inset;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	.comment-item .comment-item,
	.comment-item .comment-item + .comment-item {
		margin-right: 0;
	}
}

/*=========*/
/* ANSWERS */
/*=========*/

.answer-item::after {
	left: 0;
	text-transform: uppercase;
	border-width: 0 0 1px 0;
	border-color: #666;
	box-shadow:
		0 1.5px 1.5px -1.5px #bbb, 
		0 1.5px 1.5px -1.5px #777;
	font-weight: 300;
	color: #555;
}

/*==================*/
/* COMMENT LISTINGS */
/*==================*/

.listings .comment-thread .comment-meta a.date:focus,
.listings .comment-thread .comment-meta .nav-links-container a:focus {
	filter: invert(100%) contrast(150%);
}
.listings .comment-thread .comment-meta a.date:focus::after,
.listings .comment-thread .comment-meta .nav-links-container a:focus::after {
	outline: 2px dotted #222;
}

.listings .comment-thread .comment-meta .nav-links-container a:focus::before {
	filter: invert(100%);
}

/*===========================*/
/* COMMENT THREAD NAVIGATION */
/*===========================*/

a.comment-parent-link::after {
	display: none;
}
a.comment-parent-link::before {
	padding: 2px 3px 0 4px;
	opacity: 0.3;
}
a.comment-parent-link:hover::before {
	opacity: 1.0;
}

.comment-child-link::before {
	opacity: 0.4;
}

/*=--------------=*/
/*= Highlighting =*/
/*=--------------=*/

.comment-item.focused,
.comment-item.new-comment,
.comment-item.highlight,
.comment-item.highlight-faint,
.comment-popup {
	filter: drop-shadow(0 0 var(--GW-comment-item-outline-width) var(--GW-comment-item-outline-color));
}

.comment-item.answer-item.focused::after,
.comment-item.answer-item.new-comment::after,
.comment-item.answer-item.highlight::after,
.comment-item.answer-item.highlight-faint::after {
	box-shadow: none;
}

/*==============*/
/* COMMENT-META */
/*==============*/

.comment-meta .comment-post-title a:focus {
	border-bottom: 1px dotted currentColor;
}

/*	Comment author.
	*/
.comment-meta .author {
	font-size: 1.125em;
}
.comment-item .author:not(.redacted).original-poster::after {
	opacity: 0.8;
}
.comment-meta .author:focus {
	border-bottom: 2px dotted currentColor;
	line-height: 1;
}

/*	Karma controls.
	*/
.comment-item .karma.active-controls::after,
.comment-item .karma .karma-value::after,
.post .karma.active-controls::after,
.post .karma .karma-value::after,
.author::before {
	border-radius: 4px;
	box-shadow: 0 0 0 1px #bbb inset;
	background-color: var(--GW-body-background-color);
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
}

/*====================*/
/* ANTI-KIBITZER MODE */
/*====================*/

.author.redacted,
.inline-author.redacted {
	opacity: 0.8;
}

.karma-value.redacted {
	opacity: 0.6;
}

.link-post-domain.redacted {
	opacity: 0.6;
}

/*=================================*/
/* COMMENT THREAD MINIMIZE BUTTONS */
/*=================================*/

.comment-minimize-button {
	font-weight: 300;
}
.comment-minimize-button:hover {
	font-weight: 900;
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
	filter: grayscale(100%) brightness(55%) contrast(120%);
}

/*=================================*/
/* INDIVIDUAL COMMENT THREAD PAGES */
/*=================================*/

.individual-thread-page h1.post-title {
	color: unset;
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

.comment-controls .cancel-comment-button,
.posting-controls .cancel-post-editing-button {
	color: #700;
}
.comment-controls .cancel-comment-button:hover,
.posting-controls .cancel-post-editing-button:hover {
	color: #c00;
	text-shadow: var(--GW-shadow-white-glow);
}
.comment-controls .cancel-comment-button::before {
	font-weight: 400;
}

.comment-controls .delete-button,
.comment-controls .retract-button {
	color: #700;
}
.comment-controls .edit-button,
.comment-controls .unretract-button {
	color: #004a00;
}

.edit-post-link::before {
	color: #050;
	font-weight: 400;
	text-shadow: none;
}
.edit-post-link:hover::before {
	text-shadow: var(--GW-shadow-white-glow);
}

.comment-controls .action-button::before {
	font-weight: 300;
	opacity: 1;
}

.posting-controls textarea {
	background-color: transparent;
	border-color: #aaa;
	text-shadow: var(--GW-UI-shadow-text-glow);
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
	box-shadow: 0 -1px 0 0 #aaa inset;
}
.textarea-container:focus-within .guiedit-buttons-container {
	box-shadow: 0 -1px 0 0 #ccc inset;
}

button.guiedit {
	color: #444;
}

/* Markdown hints */

#markdown-hints {
	background-color: var(--GW-body-background-color);
	border: 1px solid #ccc;
	color: #000;
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

	.textarea-container:focus-within textarea {
		background-color: var(--GW-body-background-color);
	}
	.textarea-container:focus-within .guiedit-mobile-auxiliary-button {
		border: 1px solid transparent;
		padding: 6px;
	}
	.textarea-container:focus-within .guiedit-mobile-help-button.active {
		box-shadow:
			0 0 0 1px #ccc,
			0 0 0 3px var(--GW-body-background-color),
			0 0 0 4px #ccc;
	}
	.textarea-container:focus-within .guiedit-buttons-container {
		border-top: 1px solid #ddf;
	}
	.textarea-container:focus-within button.guiedit {
		border: 1px solid transparent;
	}
	#markdown-hints::after {
		color: #050;
	}
}

/*================*/
/* EDIT POST FORM */
/*================*/

#edit-post-form input[type='radio'] + label {
	padding: 5px 12px;
	color: #222;
	box-shadow: 0 0 0 1px #aaa;
	font-weight: var(--GW-UI-font-weight-light);
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
#edit-post-form input[type='radio']:focus + label,
#edit-post-form input[type='radio']:checked + label {
	background-color: #999;
	text-shadow: var(--GW-shadow-link-glow);
}
#edit-post-form .post-section-selector:focus-within {
	text-shadow: var(--GW-shadow-link-glow);
}
#edit-post-form .post-section-selector:focus-within label {
	box-shadow: 0 0 0 1px #ccc;
}
#edit-post-form .post-section-selector:focus-within input[type='radio']:focus + label,
#edit-post-form .post-section-selector:focus-within input[type='radio']:checked + label {
	background-color: #ccc;
}
#edit-post-form input[type='radio'] + label:hover,
#edit-post-form input[type='radio']:focus + label {
	color: #000;
}
#edit-post-form input[type='radio']:active + label,
#edit-post-form input[type='radio']:checked + label {
	background-color: #aaa;
}
#edit-post-form input[type='radio']:checked + label {
	color: #000;
	font-weight: var(--GW-UI-font-weight-heavy);
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
	font-weight: inherit;
}
button:hover,
button:focus,
input[type='submit']:hover,
input[type='submit']:focus,
.button:hover,
.button:focus {
	text-shadow: var(--GW-shadow-link-glow);
}
button:active,
input[type='submit']:active,
.button:active {
	transform: scale(0.9);
}

/*==========*/
/* HEADINGS */
/*==========*/

.body-text h1 strong,
.body-text h2 strong,
.body-text h3 strong,
.body-text h4 strong,
.body-text h5 strong,
.body-text h6 strong {
	font-weight: normal;
}
.body-text h6 {
	color: #555;
}
.body-text h1 {
	border-bottom: 1px solid #666;
	box-shadow:
		0 1.5px 1.5px -1.5px #bbb, 
		0 1.5px 1.5px -1.5px #777;
}
.body-text h2 {
	border-bottom: 1px dotted #666;
}

/*========*/
/* QUOTES */
/*========*/

blockquote {
	position: relative;
	padding-left: 1.125em;
}
blockquote::before {
	content: "";
	position: absolute;
	height: 100%;
	width: 8px;
	border: 1px solid #333;
	left: 0;
	top: 0;
	box-shadow:
		1.5px 1.5px 1.5px -1px #bbb inset,
		1.5px 1.5px 1.5px -1px #777 inset,
		1.5px 1.5px 1.5px -1px #bbb,
		1.5px 1.5px 1.5px -1px #777;
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

/*=============*/
/* IMAGE FOCUS */
/*=============*/

#images-overlay div::after {
	text-shadow: none;
	padding: 10px 16px 9px 16px;
}

#image-focus-overlay .caption,
#image-focus-overlay .help-overlay {
	text-shadow: none;
}

#content img:hover,
#images-overlay img:hover {
	filter: drop-shadow(0 0 3px #222);
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
	height: 3px;
	border-top: 1px solid #666;
	box-shadow:
		0 1.5px 1.5px -1.5px #bbb inset, 
		0 1.5px 1.5px -1.5px #777 inset;
}

code,
pre {
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

/*=------------=*/
/*= Checkboxes =*/
/*=------------=*/

input[type='checkbox'] + label::before {
	border-radius: 3px;
	border: 1px solid #aaa;
	color: #aaa;
}
input[type='checkbox']:checked + label::before {
	content: "\F00C";
}

input[type='checkbox'] + label:hover,
input[type='checkbox']:focus + label {
	text-shadow: var(--GW-shadow-link-glow);
}
input[type='checkbox'] + label:hover::before,
input[type='checkbox']:focus + label::before {
	border-color: #ccc;
}

/*=-----------=*/
/*= Fractions =*/
/*=-----------=*/

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

*::selection {
	background-color: #eee;
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
.qualified-linking-toolbar a,
.qualified-linking-toolbar button {
	border: 1px solid #888;
	border-radius: 4px;
}
.qualified-linking-toolbar a:hover,
.qualified-linking-toolbar button:hover {
	border: 1px solid #999;
	text-shadow: var(--GW-shadow-white-glow);
}

/*======*/
/* MATH */
/*======*/

.mathjax-block-container .mjx-chtml::-webkit-scrollbar {
	height: 12px;
	border-radius: 6px;
}
.mathjax-block-container .mjx-chtml::-webkit-scrollbar-thumb {
	border-radius: 6px;
}

.mjx-chtml::-webkit-scrollbar {
	background-color: #888;
	border: 1px solid #666;
	box-shadow:
		1.5px 1.5px 1.5px -1.5px #bbb inset,
		1.5px 1.5px 1.5px -1.5px #777 inset,
		1.5px 1.5px 1.5px -1.5px #bbb,
		1.5px 1.5px 1.5px -1.5px #777;
}
.mjx-chtml::-webkit-scrollbar-thumb {
	background-color: #666;
	border: 1px solid #666;
	box-shadow:
		1.5px 1.5px 1.5px -1.5px #bbb inset,
		1.5px 1.5px 1.5px -1.5px #777 inset;
}

.mathjax-inline-container .mjx-chtml::-webkit-scrollbar {
	height: 8px;
	border-radius: 4px;
}
.mathjax-inline-container .mjx-chtml::-webkit-scrollbar-thumb {
	border-radius: 4px;
}

/*===============*/
/* KEYBOARD HELP */
/*===============*/

#keyboard-help-overlay .keyboard-help-container {
	background-color: #888;
}

/*=-------------=*/
/*= Scroll bars =*/
/*=-------------=*/

#keyboard-help-overlay .keyboard-help-container::-webkit-scrollbar {
	width: 16px;
	background-color: transparent;
}
#keyboard-help-overlay .keyboard-help-container::-webkit-scrollbar-track {
	border-left: 1px solid #aaa;
	border-right: 1px solid #aaa;
}
#keyboard-help-overlay .keyboard-help-container::-webkit-scrollbar-thumb {
	background-color: #aaa;
	box-shadow:
		1px 0 0 0 #777 inset,
		-1px 0 0 0 #777 inset;
	border-left: 1px solid #aaa;
	border-right: 1px solid #aaa;
}
#keyboard-help-overlay .keyboard-help-container::-webkit-scrollbar-thumb:hover {
	background-color: #ccc;
}

/*=--------------------=*/
/*= Dividers & heading =*/
/*=--------------------=*/

#keyboard-help-overlay .keyboard-help-container h1,
#keyboard-help-overlay .keyboard-help-container .keyboard-shortcuts-lists {
	border-color: #aaa;
}
#keyboard-help-overlay .keyboard-help-container h1 {
	font-size: 1.25em;
}

/*=------=*/
/*= Keys =*/
/*=------=*/

#keyboard-help-overlay .keyboard-help-container code {
	background-color: #999;
	border: 1px solid #ccc;
	color: #000;
	text-shadow: 0 0 0 currentColor;
}
#keyboard-help-overlay .keyboard-help-container code.ak {
	background-color: #a89a35;
	border-color: #d4a500;
}

/*=--------=*/
/*= Labels =*/
/*=--------=*/

#keyboard-help-overlay .keyboard-help-container ul {
	font-weight: var(--GW-UI-font-weight-light);
	color: #222;
}
#keyboard-help-overlay .keyboard-help-container ul li.section {
	font-size: 1.25rem;
	color: #222;
}

/*=--------------=*/
/*= Close button =*/
/*=--------------=*/

#keyboard-help-overlay button.close-keyboard-help:hover {
	background-color: #999;
	box-shadow:
		0 0 0 1px #aaa,
		0 1px 0 0 #aaa inset;
}
#keyboard-help-overlay button.close-keyboard-help:active {
	box-shadow:
		0 0 0 1px #ccc;
}

/*=================*/
/* ALIGNMENT FORUM */
/*=================*/

#content.alignment-forum-index-page::after {
	background-color: #222d4b;
	color: transparent;
	-webkit-background-clip: text;
	text-shadow: 
		rgba(136, 136, 136, 0.5) 0px 3px 3px;
}
@media only screen and (hover: hover) and (pointer: fine) {
	#content.alignment-forum-index-page h1.listing a:hover,
	#content.alignment-forum-index-page h1.listing a:focus {
		background-color: rgba(135, 138, 159, 0.85);
	}	
}
