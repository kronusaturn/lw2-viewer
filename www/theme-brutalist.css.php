/*******************/
/* BRUTALIST THEME */
/*******************/

/*===========*/
/* VARIABLES */
/*===========*/

/*	Typography.
	*/
:root {
	--GW-UI-font: 'Anonymous Pro', var(--GW-monospaced-fallback-font-stack);
	--GW-UI-font-weight-light: 400;
	--GW-UI-font-weight-heavy: 600;

	--GW-width-selector-tooltip-font: 'Input Sans', var(--GW-sans-serif-fallback-font-stack);
	--GW-width-selector-tooltip-font-weight: 200;

	--GW-text-size-adjustment-tooltip-font: 'Input Sans', var(--GW-sans-serif-fallback-font-stack);
	--GW-text-size-adjustment-tooltip-font-weight: 200;

	--GW-theme-selector-tooltip-font: <?php echo (($platform == 'Mac') ? "'Concourse'" : "'Whitney'"); ?>, var(--GW-sans-serif-fallback-font-stack);

	--GW-body-text-font: 'Input Sans', var(--GW-sans-serif-fallback-font-stack);
	--GW-body-text-font-weight: 200;

	--GW-listings-post-meta-font: 'Anonymous Pro', var(--GW-monospaced-fallback-font-stack);
	--GW-listings-post-meta-font-weight: 400;

	--GW-link-post-link-font-weight: 600;

	--GW-TOC-heading-font-weight: 700;
}

/*	Layout.
	*/
:root {
	--GW-comment-compact-height: 59px;
	--GW-comment-compact-height-mobile: 108px;
	--GW-comment-minimized-height: 38px;
	--GW-comment-minimized-height-mobile: 68px;

	--GW-HNS-date-picker-text-field-width: 160px;
}
@media only screen and (max-width: 1020px) {
	:root {
		--GW-quick-nav-ui-left-margin: -19px;
	}
}

/*	Color scheme.
	*/
:root {
	--GW-body-background-color: #fff;

	--GW-primary-color: #000;

	--GW-hyperlink-color: var(--GW-primary-color);

	--GW-theme-selector-outline-color: var(--GW-primary-color);

	--GW-search-field-placeholder-color: #d00;

	--GW-C0: var(--GW-body-background-color);
	--GW-C1: var(--GW-primary-color);
	--GW-C2: #d8d8d8;
	--GW-C3: #ccc;
	--GW-C4: #bbb;
	--GW-C5: #aaa;
	--GW-C6: #999;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	:root {
		--GW-listings-post-meta-font: 'Input Sans';
		--GW-listings-post-meta-font-weight: 300;
	}
}

/*======*/
/* BASE */
/*======*/

body {
	color: var(--GW-C1);
}
#content {
	line-height: 1.55;
}
#content::before {
	box-shadow:
		0 0 0 2px var(--GW-C1);
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

.nav-bar:nth-of-type(2) {
	border-bottom: 2px solid var(--GW-C1);
}

.nav-current:not(#nav-item-search) .nav-inner,
.nav-bar a.nav-inner:hover {
	box-shadow: 
		0 0 0 4px var(--GW-C0) inset,
		0 0 0 6px var(--GW-C1) inset;
}
.nav-bar a.nav-inner:active {
	box-shadow: 
		0 0 0 8px var(--GW-C0) inset,
		0 0 0 10px var(--GW-C1) inset;
}

/*= Decorative bottom bar =*/

#bottom-bar.decorative::after {
	padding-right: 5px;
	padding-left: 6px;
	color: var(--GW-C2);
}
#bottom-bar.decorative::before {
	background: none;
	box-shadow:
		0 calc(1em - 1px) 0 0 var(--GW-C0) inset,
		0 calc(1em) 0 0 var(--GW-C2) inset;
}

/*= Accesskey hints =*/

.nav-inner::after {
	left: 8px;
	top: 3px;
	font-size: 0.7em;
	color: var(--GW-C2);
}
.nav-inner:hover::after {
	color: var(--GW-C5);
}

/*= About tag =*/

#nav-item-about .nav-inner {
	margin: 0 0.5em 0 0;
}
#nav-item-about button.open-keyboard-help {
	border: none;
	text-shadow:
		 2px  2px 0 var(--GW-C0),
		-2px -2px 0 var(--GW-C0);
	padding: 8px 9px 8px 8px;
}

/*= Search tab =*/

#nav-item-search button {
	border: none;
	font-weight: var(--GW-UI-font-weight-heavy);
}

/*= Recent Comments tab =*/

#nav-item-recent-comments span {
	margin: 0 5px 0 0;
}

/*= Inbox indicator =*/

#inbox-indicator {
	text-shadow:
		 1px  1px 0 var(--GW-C0),
		-1px -1px 0 var(--GW-C0);
}
#inbox-indicator::before {
	padding-top: 1px;
	color: var(--GW-C2);
}

/*===============*/
/* PAGINATION UI */
/*===============*/

#top-nav-bar a:hover::before {
	box-shadow: 
		0 0 0 7px var(--GW-C0),
		0 0 0 8px var(--GW-C1);
}
#top-nav-bar a:hover::after {
	background-color: var(--GW-C0);
}
#top-nav-bar a:active::before {
	box-shadow: 
		0 0 0 5px var(--GW-C0),
		0 0 0 6px var(--GW-C1);
}

#top-nav-bar a::after,
#bottom-bar a::after {
	color: var(--GW-C1);
}

#bottom-bar {
	margin: 0.75em 0 0 0;
	box-shadow:
		0 -2px 0 0 var(--GW-C1);
}
#bottom-bar a {
	line-height: 1.9;
}
#bottom-bar a::after {
	bottom: -2px;
}
#bottom-bar a:not([href='#top'])::after {
	background-color: var(--GW-C0);
	left: 0;
	right: 0;
	margin: auto;
	padding: 1px 6px;
}
<?php fit_content("#bottom-bar a:not([href='#top'])::after"); ?>

/*==============*/
/* PAGE TOOLBAR */
/*==============*/

.page-toolbar {
	border-style: solid;
	border-color: var(--GW-C1);
	border-width: 0 0 2px 2px;
}
.page-toolbar > *:nth-child(n+2) {
	margin-left: 0.5em;
}
.page-toolbar > * {
	padding: 0 4px 0 0;
}
.page-toolbar .rss {
	padding: 0 4px;
}
.page-toolbar .rss::before {
	width: 0.75em;
	margin: 0 3px 0 0;
}
.page-toolbar > *:hover {
	box-shadow:
		0 0 0 2px var(--GW-C1);
}
.page-toolbar > *:active {
	box-shadow:
		0 0 0 2px var(--GW-C1) inset;
}

.page-toolbar button,
.page-toolbar button:hover,
.page-toolbar button:focus,
.page-toolbar button:active {
	box-shadow: none;
}

.rss::before {
	filter: grayscale(100%);
}
.rss:hover::before {
	filter: grayscale(100%) brightness(65%) contrast(400%);
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 720px) {
	.page-toolbar {
		border: none;
		box-shadow:
			4px -2px 0 0 var(--GW-C0) inset,
			6px -4px 0 0 var(--GW-C1) inset;
		line-height: 1.4;
		padding-top: 0.25em;
		padding-bottom: 0.45em;
	}
}
@media only screen and (max-width: 640px) {
	#content.user-page .page-toolbar {
		box-shadow: none;
	}
	#content.user-page .page-toolbar > * {
		box-shadow:
			0 0 0 2px var(--GW-C1);
	}
	#content.user-page .page-toolbar > *:hover {
		box-shadow:
			0 0 0 2px var(--GW-C1),
			0 0 0 2px var(--GW-C0) inset,
			0 0 0 4px var(--GW-C1) inset;
	}
	#content.user-page .page-toolbar > *:active {
		box-shadow:
			0 0 0 2px var(--GW-C1),
			0 0 0 4px var(--GW-C0) inset,
			0 0 0 6px var(--GW-C1) inset;
	}
}

/*==============*/
/* SUBLEVEL NAV */
/*==============*/

.sublevel-nav .sublevel-item {
	border-color: var(--GW-C1);
	border-style: solid;
	border-width: 2px 0;
}
.sublevel-nav .sublevel-item:first-child {
	border-width: 2px 0 2px 2px;
}
.sublevel-nav .sublevel-item:last-child {
	border-width: 2px 2px 2px 0;
}
.sublevel-nav .sublevel-item:hover {
	box-shadow: 
		0 0 0 2px var(--GW-C0) inset,
		0 0 0 4px var(--GW-C1) inset;
}
.sublevel-nav .sublevel-item:active {
	box-shadow: 
		0 0 0 4px var(--GW-C0) inset,
		0 0 0 6px var(--GW-C1) inset;
}
.sublevel-nav .sublevel-item:disabled,
.sublevel-nav span.sublevel-item {
	box-shadow: 
		0 0 0 2px var(--GW-C0) inset,
		0 0 0 4px var(--GW-C1) inset;
	color: inherit;
}
.sublevel-nav span.sublevel-item {
	font-weight: var(--GW-UI-font-weight-heavy);
}
.sublevel-nav:not(.sort) .sublevel-item {
	line-height: 1.6;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	.sublevel-nav:not(.sort) .sublevel-item,
	.sublevel-nav:not(.sort) .sublevel-item:first-child,
	.sublevel-nav:not(.sort) .sublevel-item:last-child {
		border-width: 2px;
		margin: 2px;
	}
}

/*=====================*/
/* SORT ORDER SELECTOR */
/*=====================*/

.sublevel-nav.sort {
	padding: 18px 0 0 0;
	border: 2px solid var(--GW-C1);
}
.sublevel-nav.sort::before {
	text-transform: uppercase;
}
.sublevel-nav.sort .sublevel-item {
	padding: 7px 8px 6px 9px;
	text-transform: uppercase;
	border: none;
}
.sublevel-nav.sort span.sublevel-item {
	font-weight: var(--GW-UI-font-weight-heavy)
}

/*================*/
/* WIDTH SELECTOR */
/*================*/

#width-selector:hover {
	box-shadow:
		0 0 0 1px var(--GW-C1);
}
#width-selector button:hover,
#width-selector button:focus,
#width-selector button.selected {
	box-shadow:
		0 0 0 1px var(--GW-C0) inset,
		0 0 0 2px var(--GW-C1) inset;
}
#width-selector button:active {
	box-shadow:
		0 0 0 3px var(--GW-C0) inset,
		0 0 0 4px var(--GW-C1) inset;
}
#width-selector button::after {
	color: var(--GW-C1);
	font-size: 0.9em;
	top: 25px;
	background-color: var(--GW-C0);
	z-index: 1;
	font-size: 0.75em;
	font-stretch: semi-condensed;
	box-shadow: 
		0 -1px 0 0 var(--GW-C0),
		0 0 0 1px var(--GW-C1);
	padding: 4px 0 2px 0;
}

/*================*/
/* THEME SELECTOR */
/*================*/

#theme-selector button {
	box-shadow:
		0 0 0 5px var(--GW-C0) inset;
}
#theme-selector button:hover,
#theme-selector button.selected {
	box-shadow:
		0 0 0 2px var(--GW-C0) inset,
		0 0 0 3px var(--GW-C1) inset,
		0 0 0 5px var(--GW-C0) inset;
}

#theme-selector button::before {
	color: var(--GW-C5);
}
#theme-selector button:hover::before,
#theme-selector button.selected::before {
	color: var(--GW-C1);
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (min-width: 1161px) {
	#theme-selector:hover {
		box-shadow:
			0 0 0 1px var(--GW-C1);
	}
	#theme-selector:hover button::before {
		box-shadow: 
			-1px 0 0 0 var(--GW-C1);
	}
	#theme-selector:hover button:first-child::before {
		box-shadow: 
			 0  -1px 0 0 var(--GW-C1),
			-1px 0   0 0 var(--GW-C1);
	}
	#theme-selector:hover button:nth-last-child(2)::before {
		box-shadow: 
			 0   1px 0 0 var(--GW-C1),
			-1px 0   0 0 var(--GW-C1);
	}
}
@media only screen and (max-width: 1160px) {
	#theme-selector:hover::after {
		box-shadow:
			-1px  0   0 0   var(--GW-C0),
			 1px  1px 0 1px var(--GW-theme-selector-outline-color),
			 1px -1px 0 1px var(--GW-theme-selector-outline-color),
			 1px  0   0 1px var(--GW-theme-selector-outline-color);
	}
}
@media only screen and (max-width: 1000px) {
	#theme-selector {
		opacity: 1.0;
		border: 1px solid var(--GW-C1);
		box-shadow: 
			0 0 0 1px var(--GW-C1),
			0 0 0 7px var(--GW-C0);
		margin-top: 136px;
	}
}

/*======================*/
/* THEME TWEAKER TOGGLE */
/*======================*/

#theme-tweaker-toggle button:hover,
#theme-tweaker-toggle button:active {
	box-shadow: none;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 1000px) {
	#theme-tweaker-toggle {
		margin-right: -19px;
	}
	#theme-tweaker-toggle button {
		background-color: var(--GW-C0);
		opacity: 1.0;
		box-shadow:
			0 0 0 2px var(--GW-C1) inset,
			0 0 0 6px var(--GW-C0);
		padding: 1px 1px 1px 2px;
	}
	#theme-tweaker-toggle button:hover,
	#theme-tweaker-toggle button:focus {
		box-shadow:
			0 0 0 2px var(--GW-C1) inset,
			0 0 0 4px var(--GW-C0) inset,
			0 0 0 6px var(--GW-C1) inset,
			0 0 0 4px var(--GW-C0);
	}
	#theme-tweaker-toggle button:active {
		box-shadow:
			0 0 0 2px var(--GW-C1) inset,
			0 0 0 6px var(--GW-C0) inset,
			0 0 0 8px var(--GW-C1) inset,
			0 0 0 4px var(--GW-C0);
	}
}

/*======================*/
/* ANTI-KIBITZER TOGGLE */
/*======================*/

#anti-kibitzer-toggle button {
	color: var(--GW-C2);
	box-shadow: none;
}
#anti-kibitzer-toggle button::before,
#anti-kibitzer-toggle button::after {
	color: inherit;
}
#anti-kibitzer-toggle.engaged button {
	color: var(--GW-C5);
}
#anti-kibitzer-toggle button:hover {
	color: var(--GW-C6);
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 1020px) {
	#anti-kibitzer-toggle {
		background-color: var(--GW-C0);
		box-shadow: 
			0 0 0 2px var(--GW-C1),
			0 0 0 8px var(--GW-C0);
	}
	#anti-kibitzer-toggle button {
		padding: 0 0 0 1px;
		width: 41px;
	}
}

/*=========================*/
/* TEXT SIZE ADJUSTMENT UI */
/*=========================*/

#text-size-adjustment-ui button {
	font-weight: 400;
	padding: 1px 0 0 1px;
}
#text-size-adjustment-ui button.default {
	padding: 0 0 0 2px;
	font-weight: var(--GW-UI-font-weight-light);
}
#text-size-adjustment-ui button:hover,
#text-size-adjustment-ui button:focus {
	box-shadow: 
		0 0 0 2px var(--GW-C0) inset,
		0 0 0 3px var(--GW-C1) inset;
}
#text-size-adjustment-ui button:active {
	box-shadow: 
		0 0 0 4px var(--GW-C0) inset,
		0 0 0 5px var(--GW-C1) inset;
}
#text-size-adjustment-ui::after {
	color: var(--GW-C1);
	font-size: 0.75em;
	font-stretch: semi-condensed;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 1080px) {
	#text-size-adjustment-ui {
		opacity: 1.0;
	}
	#text-size-adjustment-ui {
		background-color: var(--GW-C0);
		box-shadow:
			0 0 0 2px var(--GW-C1),
			0 0 0 8px var(--GW-C0);
		margin: 142px 0 0 -11px;
	}
	#text-size-adjustment-ui button {
		font-weight: 900;
	}
	#text-size-adjustment-ui button.default {
		font-weight: var(--GW-UI-font-weight-heavy);
	}
}

/*=================*/
/* QUICKNAV WIDGET */
/*=================*/

#quick-nav-ui a {
	box-shadow: 
		0 0 0 1px var(--GW-C0),
		0 0 0 3px var(--GW-C1);
}
#quick-nav-ui a[href='#bottom-bar'] {
	line-height: 1.8;
}
@media only screen and (hover: hover) and (pointer: fine) {
	#quick-nav-ui a:hover {
		box-shadow: 
			0 0 0 1px var(--GW-C0) inset,
			0 0 0 3px var(--GW-C1) inset,
			0 0 0 1px var(--GW-C0),
			0 0 0 3px var(--GW-C1);
	}
}
#quick-nav-ui a:active {
	box-shadow: 
		0 0 0 3px var(--GW-C0) inset,
		0 0 0 5px var(--GW-C1) inset,
		0 0 0 1px var(--GW-C0),
		0 0 0 3px var(--GW-C1);
}
#quick-nav-ui a[href='#comments'].no-comments,
#quick-nav-ui a[href='#answers'].no-answers {
	opacity: 0.4;
	color: var(--GW-C4);
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 1020px) {
	#quick-nav-ui {
		background-color: var(--GW-C0);
		box-shadow:
			-1px 0 0 8px var(--GW-C0),
			 0   0 0 8px var(--GW-C0);
	}
}

/*======================*/
/* NEW COMMENT QUICKNAV */
/*======================*/

#new-comment-nav-ui .new-comments-count {
	left: 2px;
}
#new-comment-nav-ui .new-comments-count::after {
	position: relative;
	right: 1px;
}
#new-comment-nav-ui .new-comment-sequential-nav-button:disabled {
	color: var(--GW-C4);
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 1020px) {
	#new-comment-nav-ui .new-comments-count::before {
		background-color: var(--GW-C0);
		box-shadow: 
			0 0 0 2px #000,
			0 0 0 10px var(--GW-C0);
	}
}

/*=================*/
/* HNS DATE PICKER */
/*=================*/

#hns-date-picker span {
	color: #777;
}
#hns-date-picker input {
	color: #666;
}
#hns-date-picker input:focus {
	color: #000;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 1440px) {
	#hns-date-picker {
		background-color: var(--GW-C0);
		opacity: 1.0;
	}
	#hns-date-picker::before {
		border: 2px solid #000;
		box-shadow: 0 0 0 1px #000;
	}
}

/*================================*
	MOBILE VERSIONS OF QUICKNAV,
	NEW COMMENT NAV, AND HNS
	DATE PICKER
 *================================*/

@media only screen and (max-width: 960px) {
	#ui-elements-container > div[id$='-ui-toggle'] button,
	#theme-selector .theme-selector-close-button  {
		color: #000;
		opacity: 1.0;
	}
	#appearance-adjust-ui-toggle button,
	#post-nav-ui-toggle button {
		background-color: var(--GW-C0);
		box-shadow:
			0 0 0 2px var(--GW-C0),
			0 0 0 2px #000 inset;
	}

	#theme-selector {
		box-shadow: 
			0 0 0 1px #000;
	}
	#theme-selector button {
		box-shadow: 
			0 0 0 1px var(--GW-C0) inset, 
			0 0 0 3px #000 inset;
	}
	#theme-selector button:hover,
	#theme-selector button.selected {
		box-shadow: 
			0 0 0 1px var(--GW-C0) inset, 
			0 0 0 3px #000 inset, 
			0 0 0 5px var(--GW-C0) inset, 
			0 0 0 7px #000 inset;
	}
	#theme-selector button:active {
		box-shadow: 
			0 0 0  1px var(--GW-C0) inset, 
			0 0 0  3px #000 inset, 
			0 0 0  8px var(--GW-C0) inset, 
			0 0 0 10px #000 inset;
	}
	#theme-selector button::after {
		color: #000;
		padding-left: 1px;
	}
	#theme-selector .theme-selector-close-button,
	#theme-selector .theme-selector-close-button:hover {
		font-weight: 300;
		width: 54px;
		box-shadow:
			0 0 0 4px var(--GW-C0) inset,
			0 0 0 6px #000 inset;
		padding: 12px 10px 10px 12px;
	}

	#quick-nav-ui {
		background-color: var(--GW-C0);
	}
	#quick-nav-ui a {
		box-shadow: 
			0 0 0 2px #000;
	}
	#quick-nav-ui a::after,
	#new-comment-nav-ui::before {
		font-size: 0.5rem;
		background-color: var(--GW-C0);
	}
	#new-comment-nav-ui {
		background-color: var(--GW-C0);
		border: 1px solid #000;
		box-shadow: 0 0 0 1px #000;
	}
	#new-comment-nav-ui::before {
		color: #000;
		font-size: 0.6875rem;
		bottom: calc(100% + 2px);
		padding: 2px 0 0 0;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button {
		color: #000;
	}
	#new-comment-nav-ui .new-comments-count,
	#new-comment-nav-ui .new-comments-count::after {
		color: #000;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button:disabled {
		color: var(--GW-C4);
	}

	#hns-date-picker {
		bottom: 130px;
		right: 52px;
		text-indent: 0;
	}
}

/*=============================*/
/* COMMENTS VIEW MODE SELECTOR */
/*=============================*/

/*=============================*/
/* COMMENTS LIST MODE SELECTOR */
/*=============================*/

#comments-list-mode-selector {
	border: 2px solid var(--GW-C1);
}
#comments-list-mode-selector button {
	border: none;
}
#comments-list-mode-selector button:hover,
#comments-list-mode-selector button:focus,
#comments-list-mode-selector button.selected {
	box-shadow:
		0 0 0 1px var(--GW-C0) inset,
		0 0 0 3px var(--GW-C1) inset;
}
#comments-list-mode-selector button:active {
	box-shadow:
		0 0 0 3px var(--GW-C0) inset,
		0 0 0 5px var(--GW-C1) inset;
}

/*===============*/
/* KEYBOARD HELP */
/*===============*/

/*==========*/
/* ARCHIVES */
/*==========*/

.archive-nav {
	border: 2px solid #000;
}
.archive-nav span[class^='archive-nav-item'],
.archive-nav a:hover {
	box-shadow: 
		0 0 0 1px var(--GW-C0) inset,
		0 0 0 3px #000 inset;
}
.archive-nav a:active {
	transform: none;
	box-shadow: 
		0 0 0 3px var(--GW-C0) inset,
		0 0 0 5px #000 inset;
}

/*==========*/
/* LISTINGS */
/*==========*/

h1.listing {
	margin: 0.7em 20px 0.1em 20px;
	max-width: calc(100% - 40px);
	font-size: 1.625rem;
}

h1.listing a[href^="http"] {
	font-size: 0.7em;
	top: 6px;
	color: var(--GW-C0);
	text-shadow: 
		 0.5px 0.5px 0 #000, 
		-0.5px -0.5px 0 #000,
		 0 0 2px #000;
}

@media only screen and (hover: hover) and (pointer: fine) {
	h1.listing a:hover,
	h1.listing a:focus {
		color: #777;
		background-color: rgba(255, 255, 255, 0.85);
	}
	h1.listing:focus-within::before {
		color: #000;
		left: -0.625em;
	}
	h1.listing a[href^="http"]:hover {
		color: var(--GW-C0);
		text-shadow: 
			 0.5px 0.5px 0 #000, 
			-0.5px -0.5px 0 #000,
			 0 0 2px #000,
			 0 0 3px #000;
	}
}

h1.listing .edit-post-link {
	padding: 5px 3px 36px 0.5em;
	top: 0;
	right: 0;
}
h1.listing .edit-post-link:hover {
	text-decoration: none;
}
#content.user-page h1.listing .edit-post-link {
	background-color: var(--GW-C0);
}

/*======*/
/* SPAM */
/*======*/

h1.listing:not(:focus-within).spam {
	opacity: 0.1;
}
h1.listing:not(:focus-within).spam + .post-meta {
	opacity: 0.2;
}
h1.listing.spam:hover,
h1.listing.spam + .post-meta:hover,
h1.listing.spam:hover + .post-meta {
	opacity: 1.0;
}

/*===================*/
/* LISTING POST-META */
/*===================*/

h1.listing + .post-meta > * {
	line-height: 1.5;
}
h1.listing + .post-meta .read-time {
	opacity: 0.5;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	h1.listing + .post-meta {
		font-stretch: semi-condensed;
	}
}
@media only screen and (max-width: 520px) {
	h1.listing + .post-meta {
		font-stretch: condensed;
	}
}

/*============*/
/* USER PAGES */
/*============*/

#content.user-page h1.page-main-heading {
	border-bottom: 1px dotted #000;
}

#content.user-page h1.listing,
#content.user-page h1.listing + .post-meta {
	border-style: solid;
	border-color: #000;
}
#content.user-page h1.listing {
	padding: 0 6px;
	padding-top: 0.25em;
	border-width: 2px 2px 0 2px;
	margin: 1rem 0 0 0;
	max-width: 100%;
}
#content.own-user-page h1.listing,
h1.listing.own-post-listing {
	padding-right: 36px;
}
@media only screen and (hover: hover) and (pointer: fine) {
	#content.user-page h1.listing:focus-within::before {
		left: -0.625em;
	}
}
#content.user-page h1.listing + .post-meta {
	padding: 0.75em 6px 0.5em 33px;
	border-width: 0 2px 2px 2px;
	margin: 0 0 1rem 0;
}
#content.user-page h1.listing + .post-meta .post-section::before {
	left: -2px;
	top: unset;
}

#content.conversations-user-page h1.listing {
	padding: 8px 6px 8px 10px;
	font-size: 1.5rem;
}
#content.conversations-user-page h1.listing + .post-meta {
	padding: 10px 4px 6px 4px;
	margin: 0;
}

/*===============*/
/* CONVERSATIONS */
/*===============*/

#content.conversation-page textarea {
	border-top-width: 2px;
}

/*============*/
/* LOGIN PAGE */
/*============*/

/* “Log in” form */
#login-form {
    grid-template-columns: 5.75em 1fr;
}

#login-form a:hover {
	text-decoration: dotted underline;
}
#login-form a:active {
	transform: scale(0.9);
}

/* “Create account” form */

#signup-form {
    grid-template-columns: 10.5em 1fr;
	border: 2px solid #000;
}

/* Log in tip */

.login-container .login-tip {
	border: 1px solid #000;
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

.reset-password-container label {
	width: 12em;
}

/*===================*/
/* TABLE OF CONTENTS */
/*===================*/

.contents {
	border: 2px solid #000;
	background-color: var(--GW-C0);
}
.contents-head {
	font-size: 1.125em;
}
.post-body .contents ul {
	font-size: 0.9375em;
}
.contents ul a {
	border-bottom: 2px dotted #999;
}
.post-body .contents li::before {
	color: #999;
	font-feature-settings: "tnum";
}

/*==================*/
/* POSTS & COMMENTS */
/*==================*/

.body-text a {
	border-bottom: 2px dotted currentColor;
}
.body-text {
	--GW-hyperlink-hover-color: #999;
	--GW-hyperlink-active-color: #999;
}
.post-meta a:hover,
.comment-meta a:hover {
	border-bottom: 1px dotted currentColor;
}

/*===========*/
/* POST-META */
/*===========*/

.post-meta .post-section::before,
.comment-meta .alignment-forum {
	color: var(--GW-C0);
	top: -1px;
	text-shadow: 
		1px 1px 0 #777, 
		0 1px 0 #777, 
		0 0 5px #777;
}
a.post-section:hover {
	text-decoration: none;
}
a.post-section:hover::before {
	color: #eee;
}
.post-meta .date {
	color: #888;
}
.bottom-post-meta {
	border-color: #000;
	border-style: dotted;
}

/*============*/
/* LINK POSTS */
/*============*/

.post.link-post a.link-post-link {
	font-size: 1.25em;
	border: none;
}
.post.link-post a.link-post-link::before {
	color: var(--GW-C0);
	text-shadow: 
		 0.5px 0.5px 0 #000, 
		-0.5px -0.5px 0 #000,
		 0 0 2px #000;
}
.post.link-post a.link-post-link:hover {
	color: inherit;
	border-bottom: 2px dotted #000;
}
.post.link-post a.link-post-link:hover::before {
	text-shadow: 
		 0.5px 0.5px 0 #000, 
		-0.5px -0.5px 0 #000,
		 0 0 2px #000,
		 0 0 3px #000;
}
.post.link-post a.link-post-link:focus {
	color: #999;
	border-bottom: 2px dotted #999;
}

/*=======*/
/* POSTS */
/*=======*/

.post-body {
	font-size: 1.125rem;
	margin-top: 1em;
}
h1.post-title {
	font-size: 2.25rem;
	margin-top: 1.375em;
}

/*=================*/
/* POST NAVIGATION */
/*=================*/

.post-nav-links a:hover {
	box-shadow: 
		0 0 0 2px var(--GW-C0) inset,
		0 0 0 4px #000 inset;
}
.post-nav-links a:active {
	box-shadow: 
		0 0 0 6px var(--GW-C0) inset,
		0 0 0 8px #000 inset;
}

.post-nav-label {
	opacity: 0.75;
}

@media only screen and (max-width: 900px) {
	.sequence-title {
		border-top: 1px dotted #000;
	}
	.post-nav.prev {
		border-right: 1px dotted #000;
	}
	.post-nav.next {
		border-left: 1px dotted #000;
	}
}

/*==========*/
/* COMMENTS */
/*==========*/

.comments::before {
	border-top: 2px solid #000;
}
#content > .comment-thread .comment-meta a.date:focus,
#content > .comment-thread .comment-meta a.permalink:focus {
	color: #999;
	outline: 2px dotted #999;
	position: relative;
	background-color: var(--GW-C0);
	padding: 0 5px;
	left: -5px;
}
#content > .comment-thread .comment-meta a.date:focus + *,
#content > .comment-thread .comment-meta a.permalink:focus + *:not(.comment-post-title) {
	margin-left: -10px;
}
.comment-item {
	border: 2px solid #000;
}

.comment-body {
	font-size: 1rem;
	line-height: 1.5;
}

/*================================*/
/* DEEP COMMENT THREAD COLLAPSING */
/*================================*/

.comment-item input[id^="expand"] + label:hover::after {
	color: #c00;
}
.comment-item input[id^="expand"] + label:active::after,
.comment-item input[id^="expand"] + label:focus::after{
	color: #c00;
}
.comment-item input[id^="expand"]:checked ~ .comment-thread .comment-thread .comment-item {
	border-width: 1px 0 0 0;
}

/*==============*/
/* COMMENT-META */
/*==============*/

.comment-meta .author {
	font-size: 1.25em;
}
.comment-item .author:not(.redacted).original-poster::after {
	opacity: 0.6;
}

.comment-item .karma.active-controls::after,
.comment-item .karma .karma-value::after,
.post .karma.active-controls::after,
.post .karma .karma-value::after,
.author::before {
	background-color: var(--GW-C0);
	color: #000;
	box-shadow: 0 0 0 1px #000 inset;
}
.comment-item .karma.active-controls::after,
.post .karma.active-controls::after {
	padding: 6px;
	bottom: -46px;
	max-width: unset;
	transform: translateX(-2px);
}
.comment-item .karma .karma-value::after,
.post .karma .karma-value::after {
	padding: 2px 8px;
	top: -26px;
	min-width: 80px;
}

/*====================*/
/* ANTI-KIBITZER MODE */
/*====================*/

.author.redacted,
.inline-author.redacted {
	opacity: 0.5;
}

.karma-value.redacted {
	opacity: 0.5;
}

.link-post-domain.redacted {
	opacity: 0.3;
}

/*===========================*/
/* COMMENT THREAD NAVIGATION */
/*===========================*/

a.comment-parent-link::after {
	display: none;
}
a.comment-parent-link::before {
	color: var(--GW-C4);
	padding: 4px 3px 0 2px;
}
a.comment-parent-link:hover::before {
	color: #999;
}

.comment-child-link::before {
	color: #aaa;
}

.comment-item-highlight,
.comment-item-highlight-faint {
	border: 2px solid #ccc;
}

.comment-popup {
	border: 2px solid #000;
	background-color: var(--GW-C0);
}
.comment-popup .comment-body {
	font-size: 0.9375rem;
}

/*====================*/
/* COMMENT PERMALINKS */
/*====================*/

.comment-meta .permalink,
.comment-meta .lw2-link,
.individual-thread-page .comment-parent-link:empty {
	filter: grayscale(100%);
}

/*=======================*/
/* COMMENTS COMPACT VIEW */
/*=======================*/

@media only screen and (hover: hover) and (pointer: fine) {
	#content.compact > .comment-thread .comment-item:hover .comment,
	#content.compact > .comment-thread .comment-item.expanded .comment {
		background-color: var(--GW-C0);
		outline: 3px solid #000;
	}
	#content.compact > .comment-thread .comment-item:hover .comment::before,
	#content.compact > .comment-thread .comment-item.expanded .comment::before {
		background-color: var(--GW-C0);
		box-shadow: 
			0 0  3px var(--GW-C0),
			0 0  5px var(--GW-C0),
			0 0  7px var(--GW-C0),
			0 0 10px var(--GW-C0),
			0 0 20px var(--GW-C0),
			0 0 30px var(--GW-C0),
			0 0 40px var(--GW-C0);
	}
}
@media not screen and (hover: hover) and (pointer: fine) {
	#content.compact > .comment-thread.expanded .comment-item .comment {
		background-color: var(--GW-C0);
		outline: 3px solid #000;
	}
	#content.compact > .comment-thread.expanded .comment-item .comment::before {
		background-color: var(--GW-C0);
		box-shadow: 
			0 0  3px var(--GW-C0),
			0 0  5px var(--GW-C0),
			0 0  7px var(--GW-C0),
			0 0 10px var(--GW-C0),
			0 0 20px var(--GW-C0),
			0 0 30px var(--GW-C0),
			0 0 40px var(--GW-C0);
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

.new-comment {
	border: 2px dotted #000;
}
.new-comment::before {
	display: none;
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
	font-size: 0.8125rem;
	top: 20px;
	left: 0.5px;
	color: #777;
}
.comment-minimize-button.maximized::after {
	color: #ccc;
}

/*=================================*/
/* INDIVIDUAL COMMENT THREAD PAGES */
/*=================================*/

.individual-thread-page > h1 a:hover {
	color: #777;
	text-decoration: dotted underline;
}

.individual-thread-page > .comments {
	border-top: 2px solid #000;
}

/*==============*/
/* VOTE BUTTONS */
/*==============*/

.vote::before {
	content: "";
	border-radius: 2px;
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
.vote:hover,
.vote:active,
.vote:focus {
	box-shadow: none;
}
.vote:hover::before,
.vote.selected::before,
.vote.clicked-once::before,
.vote.clicked-twice::before {
	filter: drop-shadow(0 0 1px var(--GW-C0));
}
.vote::before,
.waiting .vote.big-vote.clicked-twice::before {
	filter: contrast(5%) brightness(182%);
}
.vote.clicked-once::before,
.vote.big-vote.clicked-once::before {
	box-shadow:
		0 0 0 3px #ccc,
		0 0 0 4px transparent;
}
.vote.big-vote.clicked-twice::before,
.waiting .vote.big-vote:not(.clicked-twice)::before,
.waiting .vote:not(.big-vote).clicked-once::before {
	box-shadow: none;
}
.vote.clicked-twice::before,
.vote.big-vote::before {
	box-shadow:
		0 0 0 3px #000,
		0 0 0 4px transparent;
}

.upvote::before {
	background-image: url('data:image/svg+xml;base64,<?php echo base64_encode(file_get_contents("assets/upvote-black-square-plus.svg")) ?>');
}
.downvote::before {
	background-image: url('data:image/svg+xml;base64,<?php echo base64_encode(file_get_contents("assets/downvote-black-square-minus.svg")) ?>');
}

/*===========================*/
/* COMMENTING AND POSTING UI */
/*===========================*/

.comment-controls .cancel-comment-button {
	height: 28px;
	color: var(--GW-C0);
	padding: 4px 6px 2px 6px;
}

.comment-controls .delete-button::before,
.comment-controls .retract-button::before,
.comment-controls .unretract-button::before {
	opacity: 0.8;
}

.posting-controls .action-button {
	border: none;
}
.posting-controls .action-button:hover,
.posting-controls .action-button:active,
.posting-controls .action-button:focus {
	box-shadow: none;
	text-decoration: dotted underline;
}
.posting-controls .action-button:active {
	transform: scale(0.9);
}

.posting-controls textarea {
	font-size: 1rem;
	line-height: 1.4;
	color: #000;
	background-color: var(--GW-C0);
	border-color: #000;
	border-width: 28px 2px 2px 2px;
}
.posting-controls textarea:focus {
	border-style: dotted;
	border-width: 28px 2px 2px 2px;
}
.posting-controls textarea::selection {
	background-color: #000;
	color: var(--GW-C0);
}
.posting-controls textarea::-webkit-scrollbar {
	width: 18px;
}
.posting-controls textarea::-webkit-scrollbar-track {
	background-color: var(--GW-C0);
}
.posting-controls textarea::-webkit-scrollbar-thumb {
	background-color: #000;
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/checkerboard2_1px_gray.gif")) ?>');
	background-size: 2px 2px;
	box-shadow: 
		0 2px 0 1px var(--GW-C0) inset,
		0 0 0 1px var(--GW-C0) inset,
		0 2px 0 1.5px #777 inset,
		0 0 0 1.5px #777 inset;
}
.posting-controls textarea::-webkit-scrollbar-thumb:active {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/checkerboard2_1px.gif")) ?>');
	box-shadow: 
		0 2px 0 1px var(--GW-C0) inset,
		0 0 0 1px var(--GW-C0) inset,
		0 2px 0 1.5px #000 inset,
		0 0 0 1.5px #000 inset;
}

/* GUIEdit buttons */

.guiedit-buttons-container {
	background-color: #000;
	box-shadow: 0 0 0 1px #000;
}

.posting-controls.edit-existing-post .guiedit-buttons-container button,
.posting-controls form.edit-existing-comment .guiedit-buttons-container button {
	color: #0c0;
}

button.guiedit {
	color: var(--GW-C0);
	border: none;
}
button.guiedit:hover,
button.guiedit:active,
button.guiedit:focus {
	box-shadow: none;
	color: #777;
}
button.guiedit::after {
	color: var(--GW-C0);
	top: 2px;
	height: 25px;
	text-shadow: none;
}

/* Markdown hints */

#markdown-hints {
	border: 2px solid #000;
	background-color: var(--GW-C0);
}

/*================*/
/* EDIT POST FORM */
/*================*/

#edit-post-form .post-meta-fields {
	grid-template-columns: 6em auto auto auto 1fr auto;
}
#edit-post-form .post-meta-fields input[type='checkbox'] + label::before {
	color: #000;
	border: 1px solid #000;
	top: 2px;
}
@media only screen and (hover: hover) and (pointer: fine) {
	#edit-post-form .post-meta-fields input[type='checkbox'] + label:hover::before,
	#edit-post-form .post-meta-fields input[type='checkbox']:focus + label::before {
		box-shadow: 
			0 0 0 1px var(--GW-C0) inset,
			0 0 0 2px #000 inset;
	}
}
#edit-post-form .post-meta-fields input[type='checkbox']:active + label::before,
#edit-post-form .post-meta-fields input[type='checkbox']:checked:active + label::before {
	background-color: var(--GW-C0);
	box-shadow: 
		0 0 0 3px var(--GW-C0) inset,
		0 0 0 4px #000 inset;
}
#edit-post-form .post-meta-fields input[type='checkbox']:checked + label::before {
	content: "";
	background-color: #000;
	box-shadow: 
		0 0 0 4px var(--GW-C0) inset;
}
#edit-post-form input[type='radio'] + label {
	border-color: #000;
	color: #000;
}
#edit-post-form input[type='radio'][value='all'] + label {
	border-width: 1px;
}
#edit-post-form input[type='radio'][value='drafts'] + label {
}
#edit-post-form input[type='radio'] + label:hover,
#edit-post-form input[type='radio']:focus + label {
	color: #000;
	box-shadow: 
		0 0 0 1px var(--GW-C0) inset,
		0 0 0 2px #000 inset;
}
#edit-post-form input[type='radio']:active + label {
	box-shadow: 
		0 0 0 2px var(--GW-C0) inset,
		0 0 0 3px #000 inset;
}
#edit-post-form input[type='radio']:focus + label {
	box-shadow: 
		0 0 0 1px #000;
}
#edit-post-form input[type='radio']:checked + label {
	border-color: #000;
	box-shadow: 
		0 0 0 1px var(--GW-C0) inset,
		0 0 0 2px #000 inset;
}

#edit-post-form #markdown-hints-checkbox + label {
	padding: 4px 0 0 6px;
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

button,
input[type='submit'] {
	border: 2px solid var(--GW-C1);
	box-shadow: 0 0 0 1px transparent;
}
#ui-elements-container button {
	border: none;
}

button:hover,
input[type='submit']:hover,
button:focus,
input[type='submit']:focus {
	text-decoration: none;
	background-color: transparent;
	color: inherit;
	box-shadow: 
		0 0 0 2px var(--GW-C0) inset,
		0 0 0 4px var(--GW-C1) inset,
		0 0 0 1px transparent;
}
button:active,
input[type='submit']:active {
	box-shadow: 
		0 0 0 4px var(--GW-C0) inset,
		0 0 0 6px var(--GW-C1) inset,
		0 0 0 1px transparent;
}

/*==========*/
/* HEADINGS */
/*==========*/

.body-text h6 {
	color: #555;
}
.body-text h1 {
	border-bottom: 2px solid var(--GW-C1);
}

/*========*/
/* QUOTES */
/*========*/

blockquote {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/checkerboard_1px.gif")) ?>');
	background-size: 5px 2px;
	background-repeat: repeat-y;
	background-position: top left;
	padding-left: calc(0.5em + 5px);
}

/*========*/
/* IMAGES */
/*========*/

#content img,
#content figure.image img {
	border: 1px dotted var(--GW-C1);
}
#content figure img {
	border: 1px solid var(--GW-C1);
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
	border: 1px solid var(--GW-C1);
}

/*======*/
/* MISC */
/*======*/

hr {
	border-bottom: 1px solid #999;
}

code {
	border: 1px dotted var(--GW-C1);
}

input[type='text'],
input[type='search'],
input[type='password'],
textarea {
	background-color: transparent;
	border: 1px solid var(--GW-C1);
	color: var(--GW-C1);
}
input[type='text']:focus,
input[type='search']:focus,
input[type='password']:focus,
textarea:focus {
	border-style: dashed;
}

select {
	color: var(--GW-C1);
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

.about-page mark {
	background-color: #e6e6e6;
	text-decoration: none;
	box-shadow: 
		0 -1px 0 0 #000 inset, 
		0 -3px 1px -2px #000 inset;
	padding: 0 1px;
}

#content.about-page .accesskey-table {
	border-color: #ddd;
}

#content.about-page img {
	border: 1px solid #000;
}

/*========================*/
/* QUALIFIED HYPERLINKING */
/*========================*/

#aux-about-link a:hover {
	color: #777;
}

.qualified-linking label:hover {
	color: #777;
}

.qualified-linking-toolbar {
	border: 2px solid #000;
	background-color: var(--GW-C0);
}
.qualified-linking-toolbar a:hover {
	box-shadow: 0 0 0 2px #000;
}
.qualified-linking-toolbar a:active {
	box-shadow: 0 0 0 2px #000 inset;
}
.qualified-linking label::after {
	background-color: var(--GW-C0);
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
	background-color: #f2f6ff;
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
		background-color: rgba(242, 246, 255, 0.85);
	}
}

/*========*/
/* MOBILE */
/*========*/

@media only screen and (max-width: 900px) {
	h1.listing {
		line-height: 1;
		margin-bottom: 6px;
	}
	h1.listing + .post-meta .post-section {
		overflow: visible;
		order: 1;
	}
	h1.listing + .post-meta .post-section::before {
		position: unset;
	}

	.archive-nav > *[class^='archive-nav-'] + *[class^='archive-nav-']::before {
		background-color: #000;
		height: 2px;
	}

	.comment-item .comment-item {
		margin: 0.75em 2px 3px 6px;
	}
	.comment-item .comment-item + .comment-item {
		margin: 1.5em 2px 3px 6px;
	}

	a.comment-parent-link::before {
		line-height: 1;
	}

	#edit-post-form textarea {
		min-height: calc(100vh - 400px)
	}
	#edit-post-form #markdown-hints {
		position: fixed;
		top: 74px;
		left: 0;
		right: 0;
		max-width: 330px;
		margin: auto;
	}
	#edit-post-form input[type='submit'] {
		background-color: var(--GW-C0);
	}

	.comment-controls .cancel-comment-button {
		max-width: 1.4em;
	}
	.comment-controls .cancel-comment-button::before {
		opacity: 1.0;
		position: relative;
		top: -1px;
	}
} @media only screen and (max-width: 720px) {
	#content.conversations-user-page h1.listing + .post-meta .date {
		margin: 0 0 0 1em;
	}
} @media only screen and (max-width: 520px) {
	h1.listing {
		font-size: 1.375rem;
		margin: 18px 6px 4px 6px;
		max-width: calc(100% - 12px);
	}
	h1.listing a[href^='http'] {
		top: 4px;
	}
	h1.listing + .post-meta {
		margin: 4px 6px;
	}
	h1.listing + .post-meta .post-section {
		order: 1;
		overflow: visible;
	}
	h1.listing + .post-meta .post-section::before {
		position: relative;
		top: -1px;
		left: 0;
	}
	#content.conversations-user-page h1.listing {
		font-size: 1.375rem;
	}
	#content.conversations-user-page h1.listing + .post-meta .conversation-participants {
		margin: 0;
	}
	#content.conversations-user-page h1.listing + .post-meta .messages-count {
		margin: 0 0 0 1em;
	}

	#content.compact > .comment-thread .comment-item {
		max-height: 110px;
	}

	.textarea-container:focus-within textarea {
		background-color: var(--GW-C0);
		border: none;
		box-shadow:
			0 0 0 2px #000;
	}
	.textarea-container:focus-within .guiedit-mobile-auxiliary-button {
		padding: 5px 6px 6px 6px;
		color: var(--GW-C0);
		box-shadow: none;
	}
	.textarea-container:focus-within .guiedit-mobile-help-button.active {
		box-shadow:
			0 0 0 1px #000 inset,
			0 0 0 3px var(--GW-C0) inset,
			0 0 0 2px var(--GW-C0);
	}
	.textarea-container:focus-within .guiedit-buttons-container {
		border-top: 2px solid #000;
	}
	.posting-controls .textarea-container:focus-within .guiedit-buttons-container {
		padding-bottom: 5px;
	}
	#content.conversation-page .textarea-container:focus-within::after {
		background-color: #000;
	}
	.textarea-container:focus-within button.guiedit {
		border: 1px solid transparent;
	}
	#markdown-hints,
	#edit-post-form #markdown-hints {
		border: 2px solid #000;
		box-shadow:
			0 0 0 2px var(--GW-C0),
			0 0 0 4px #000;
	}
	#markdown-hints::after {
		color: #000;
	}

	#edit-post-form .post-meta-fields input[type='checkbox'] + label {
		top: 0;
	}
	#edit-post-form .post-meta-fields input[type='checkbox'] + label::before {
		top: 1px;
	}
}
