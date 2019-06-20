/**************/
/* THEME LESS */
/**************/

/*===========*/
/* VARIABLES */
/*===========*/

/*	Typography.
	*/
:root {
	--GW-UI-font: 'Open Sans', var(--GW-sans-serif-fallback-font-stack);
	--GW-UI-font-weight-light: 300;
	--GW-UI-font-weight-heavy: 400;

	--GW-nav-item-font-weight: var(--GW-UI-font-weight-light);
	--GW-nav-item-current-font-weight: 700;

	--GW-monospaced-font: 'Source Code Pro', var(--GW-monospaced-fallback-font-stack);

	--GW-post-listings-font: 'Caecilia', var(--GW-sans-serif-fallback-font-stack);

	--GW-body-text-font: 'Source Serif Pro', var(--GW-serif-fallback-font-stack);
}

/*	Layout.
	*/
:root {
	--GW-content-side-padding: 0px;
	--GW-sequence-page-content-side-padding: 0px;
	--GW-user-page-content-side-padding: 0px;
	--GW-recent-comments-page-content-side-padding: 0px;
	--GW-conversation-page-content-side-padding: 0px;
	--GW-post-page-content-side-padding: 0px;
	--GW-edit-post-page-content-side-padding: 0px;
	--GW-individual-thread-page-content-side-padding: 0px;

	--GW-less-theme-nav-bar-width: 112px;
}

/*	Color scheme.
	*/
:root {
	--GW-body-background-color: #fff;

	--GW-hyperlink-color: #92c396;

	--GW-shadow-white-glow:
		0 0 1px #fff,
		0 0 3px #fff,
		0 0 5px #fff;

	--GW-comment-background-color-odd: #f6f6f6;
	--GW-comment-background-color-even: #fff;
	--GW-comment-background-color-target: #ffd;
}

/*======*/
/* BASE */
/*======*/

body {
	color: #000;
}
#content {
	line-height: 1.55;
	padding: 30px 20px 0 90px;
}
#content.comment-thread-page {
	padding: 0 0 0 60px;
}

/* Compensating for Linux/Windows being terrible. */

.post-meta > *:not(.post-section),
.comment-meta > *,
#primary-bar a,
#bottom-bar a,
#secondary-bar a,
#secondary-bar button,
#nav-item-search > *,
.page-toolbar > *,
#top-nav-bar > *,
.body-text a {
	text-shadow: <?php echo ($platform == 'Mac' ? 'none' : '0 0 0 #aaa'); ?>;
}

/* Compensating for Safari being terrible. */

@media not all and (min-resolution:.001dpcm) { @media {
	.post-meta > *:not(.post-section),
	.comment-meta > *,
	#primary-bar a,
	#secondary-bar a,
    #secondary-bar button,
	#bottom-bar a,
	#nav-item-search > *,
	.page-toolbar > *,
	#top-nav-bar > *,
	.body-text a {
		text-shadow: 0 0 1px #888;
	}
}}

/*==========*/
/* NAV BARS */
/*==========*/

/*= Layout (desktop) =*/

@media only screen and (min-width: 901px) {
	#primary-bar .nav-inner,
	#bottom-bar .nav-inner {
		font-size: 1em;
	}
	#primary-bar .nav-inner,
	#secondary-bar .nav-inner {
		padding: 0.5rem 0.75rem;
	}
	#primary-bar .nav-inner {
		justify-content: flex-end;
	}
	#secondary-bar .nav-inner {
		font-size: 0.8125em;
	}

	#primary-bar,
	#secondary-bar {
		flex-flow: column;
		line-height: 1;
	}
	#primary-bar {
		position: fixed;
		max-width: inherit;
		left: 0;
		right: 0;
		margin: auto;
		pointer-events: none;
	}
	#primary-bar > * {
		right: calc(var(--GW-less-theme-nav-bar-width) - 65px);
		width: var(--GW-less-theme-nav-bar-width);
		pointer-events: auto;
	}
	#nav-item-sequences .nav-inner::before {
		content: unset;
	}
	#secondary-bar #nav-item-about {
		padding-right: unset;
	}
	#nav-item-archive,
	#nav-item-sequences,
	#nav-item-about {
		position: fixed;
		transform: translateX(calc(-1 * (var(--GW-less-theme-nav-bar-width) + 25px)));
		width: var(--GW-less-theme-nav-bar-width);
	}
	#nav-item-archive .nav-inner,
	#nav-item-sequences .nav-inner,
	#nav-item-about .nav-inner {
		justify-content: flex-end;
	}
	#content.comment-thread-page #nav-item-archive,
	#content.comment-thread-page #nav-item-sequences,
	#content.comment-thread-page #nav-item-about {
		transform: translateX(calc(-1 * (var(--GW-less-theme-nav-bar-width) - 30px)));
	}

	#nav-item-home {
		padding-top: 0.5em;
	}

	#bottom-bar .nav-inner {
		text-align: center;
		padding: 0.75em;
	}
}

/*= Styling =*/

#bottom-bar .nav-item a::before {
	font-weight: 300;
}

.nav-bar a,
.nav-bar a:visited {
	color: #acd2af;
}
.nav-bar a:hover {
	color: #79a97e;
}

/*= Accesskey hints =*/

.nav-inner::after {
	display: none;
}

/*= ‘Tabs’ =*/

#nav-item-recent-comments .nav-inner span {
	display: none;
}
#nav-item-login {
	position: absolute;
	top: 0;
	right: 0;
	padding-right: 1.5em;
}

/*= Search tab =*/

#nav-item-search {
	position: absolute;
	top: 0;
	left: 4.75em;
	width: 400px;
}
#nav-item-search .nav-inner {
	padding: 1px;
	display: flex;
}
#nav-item-search form::before {
	font-size: 1.125em;
	color: #e6e6e6;
	font-weight: 400;
	padding: 5px;
	transition: color 0.15s ease;
}
#nav-item-search form:focus-within::before {
	color: #92c396;
}
#nav-item-search button {
	border: none;
	font-weight: 300;
	padding: 6px;
	height: 23px;
}
#nav-item-search form:not(:focus-within) button:not(:hover) {
	color: #ddd;
}
#nav-item-search input {
	width: unset;
	flex: 1 0 auto;
	padding: 2px 1px;
	margin: 0 0 0 2px;
}
#nav-item-search input::placeholder {
	color: #d00;
	font-weight: normal;
}

/*= Inbox indicator =*/

#inbox-indicator::before {
	color: #eaeaea;
	top: 3px;
	font-size: 1.125em;
}

/*= Decorative bottom bar =*/

#bottom-bar.decorative {
	margin-top: 0.25em;
}
#bottom-bar.decorative::before {
	filter: opacity(0.5);
}
#bottom-bar.decorative::after {
	color: #eee;
	background-color: #fff;
}

/*= Top pagination UI hover tooltips =*/

#top-nav-bar a::after,
#bottom-bar a::after {
	color: #000;
}

/*==============*/
/* PAGE TOOLBAR */
/*==============*/

.page-toolbar {
	padding: 0 0 0 0;
	margin: 0;
	white-space: nowrap;
	position: fixed;
	width: 120px;
}
.page-toolbar > * {
	display: block;
	text-align: right;
	line-height: 1;
	padding: 0.5rem 0.75rem;
	position: relative;
	right: 56px;
}
@media not screen and (hover: none) {
	.page-toolbar {
		width: inherit;
		min-width: inherit;
		max-width: inherit;
		left: 0;
		right: 0;
		margin-right: unset;
		margin: auto;
		align-items: flex-start;
		pointer-events: none;
	}
	.page-toolbar > * {
		right: 47px;
		width: 112px;
		margin-left: unset;
		pointer-events: auto;
	}
}

.page-toolbar button {
	padding: 0;
}

.page-toolbar .button::before {
	font-size: 0.875em;
	font-weight: 400;
}

.new-private-message {
	white-space: normal;
	line-height: 1.15;
}
.new-private-message::before {
	opacity: 0.7;
}

.logout-button {
	color: #d33;
	font-weight: 300;
}

/*===================*/
/* TOP PAGINATION UI */
/*===================*/

#top-nav-bar {
	justify-content: flex-start;
	padding: 1em 0 0.25em 0;
	font-size: 1em;
	margin: 0 0 0 -4px;
}
#top-nav-bar .page-number {
	line-height: 1.5;
}
#top-nav-bar .page-number span {
	display: none;
}
#top-nav-bar a::before {
	font-weight: 400;
}
#top-nav-bar a.disabled {
	visibility: visible;
	opacity: 0.4;
}

#content.user-page > #top-nav-bar {
	justify-content: center;
	padding: 0;
}
.archive-nav + #top-nav-bar {
	margin: 0.5em 0 0 -4px;
	padding: 0 0 0.25em 0;
}

/*==============*/
/* SUBLEVEL NAV */
/*==============*/

.sublevel-nav .sublevel-item {
	color: #92c396;
	padding: 4px 10px 1px 10px;
	background-color: #fff;
}
.sublevel-nav .sublevel-item:hover,
.sublevel-nav .sublevel-item.selected {
	background-color: #c4dbc4;
	color: #fff;
}
.sublevel-nav .sublevel-item:not(.selected):active {
	background-color: #92c396;
}

.sublevel-nav:not(.sort) .sublevel-item {
	border-style: solid;
	border-color: #c4dbc4;
	border-width: 1px 0 1px 1px;
}
.sublevel-nav:not(.sort) .sublevel-item:first-child {
	border-radius: 8px 0 0 8px;
}
.sublevel-nav:not(.sort) .sublevel-item:last-child {
	border-width: 1px;
	border-radius: 0 8px 8px 0;
}

/*=====================*/
/* SORT ORDER SELECTOR */
/*=====================*/
#content.index-page .sublevel-nav.sort {
	grid-row: 2;
	justify-self: right;
}

.sublevel-nav.sort {
	padding: 18px 0 0 0;
	border-radius: 8px;
	pointer-events: none;
}
.sublevel-nav.sort .sublevel-item {
	padding: 7px 8px 3px 7px;
	text-transform: uppercase;
	pointer-events: auto;
}
.sublevel-nav.sort::before {
	text-transform: uppercase;
	color: #444;
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
		0 18px 0 0 #fff inset,
		0 0 0 1px #c4dbc4 inset,
		0 18px 0 1px #c4dbc4 inset,
		0 0 0 4px #fff;
}

/*============*/
/* UI TOGGLES */
/*============*/

@media not screen and (hover: none) {
	#site-nav-ui-toggle,
	#post-nav-ui-toggle,
	#appearance-adjust-ui-toggle {
		visibility: visible;
		position: absolute;
		display: inline-block;
		border-radius: 50%;
		z-index: 1;
	}
	#site-nav-ui-toggle button,
	#post-nav-ui-toggle button,
	#appearance-adjust-ui-toggle button {
		font-weight: 400;
		font-size: 32px;
		padding: 10px;
		opacity: 0.4;
		-webkit-tap-highlight-color: transparent;
		transition:
			transform 0.2s ease,
			opacity 0.15s ease;
	}
	#site-nav-ui-toggle button:hover,
	#post-nav-ui-toggle button:hover,
	#appearance-adjust-ui-toggle button:hover {
		opacity: 1.0;
	}
	#site-nav-ui-toggle button::selection,
	#post-nav-ui-toggle button::selection,
	#appearance-adjust-ui-toggle button::selection {
		background-color: transparent;
	}
	#site-nav-ui-toggle button::-moz-focus-inner,
	#post-nav-ui-toggle button::-moz-focus-inner,
	#appearance-adjust-ui-toggle button::-moz-focus-inner {
		border: none;
	}
	#post-nav-ui-toggle.highlighted,
	#appearance-adjust-ui-toggle.highlighted {
		transform: scale(1.33);
	}
	#site-nav-ui-toggle.highlighted button {
		transform: scale(1.33);
	}
	#site-nav-ui-toggle.highlighted button,
	#post-nav-ui-toggle.highlighted button,
	#appearance-adjust-ui-toggle.highlighted button {
		opacity: 1.0;
		text-shadow:
			0 0 1px #fff,
			0 0 1px #64ff4c,
			0 0 3px #64ff4c,
			0 0 5px #64ff4c,
			0 0 8px #64ff4c;
	}
	
	#site-nav-ui-toggle {
		top: 0;
		left: 12px;
		pointer-events: none;
	}
	#site-nav-ui-toggle button {
		font-weight: 300;
		position: relative;
		left: 0;
		transition:
			left 0.2s ease,
			opacity 0.2s ease,
			width 0.2s ease;
		pointer-events: auto;
	}
	#site-nav-ui-toggle button:active {
		transform: none;
	}
	#site-nav-ui-toggle button.engaged {
		left: -92px;
		width: 2.125rem;
		overflow: hidden;
	}
	#site-nav-ui-toggle button.engaged::before {
		content: "\F00D";
		padding: 0 0.25em 0 0;
	}
	
	#primary-bar,
	#secondary-bar #nav-item-archive,
	#secondary-bar #nav-item-sequences,
	#secondary-bar #nav-item-about,
	.page-toolbar {
		visibility: hidden;
		top: 0;
		max-height: 0px;
	}
	#primary-bar,
	#secondary-bar #nav-item-archive,
	#secondary-bar #nav-item-sequences,
	#secondary-bar #nav-item-about,
	.page-toolbar {
		opacity: 0.0;
	}
	#primary-bar,
	#secondary-bar #nav-item-archive,
	#secondary-bar #nav-item-sequences,
	#secondary-bar #nav-item-about,
	.page-toolbar {
		transition:
			top 0.2s ease,
			max-height 0.2s ease,
			visibility 0.2s ease,
			opacity 0.2s ease;
	}
	#nav-item-search,
	#nav-item-login {
		visibility: visible;
	}
	#primary-bar.engaged,
	#secondary-bar.engaged #nav-item-archive,
	#secondary-bar.engaged #nav-item-sequences,
	#secondary-bar.engaged #nav-item-about,
	.page-toolbar.engaged {
		visibility: visible;
		max-height: 1000px;
	}
	#primary-bar.engaged,
	#secondary-bar.engaged #nav-item-archive,
	#secondary-bar.engaged #nav-item-sequences,
	#secondary-bar.engaged #nav-item-about,
	.page-toolbar.engaged {
		opacity: 1.0;
	}
	#primary-bar.engaged {
		top: 0;
	}
	#secondary-bar.engaged #nav-item-archive {
		top: 196px;
	}
	#secondary-bar.engaged #nav-item-sequences {
		top: 230px;
	}
	#secondary-bar.engaged #nav-item-about {
		top: 264px;
	}
	.page-toolbar.engaged {	
		top: 314px;
	}

	#post-nav-ui-toggle {
		bottom: 10px;
		right: -30px;
	}
	#content.comment-thread-page ~ #ui-elements-container #post-nav-ui-toggle {
		right: -54px;
	}
	#post-nav-ui-toggle button.engaged {
		transform: rotate(-90deg);
	}
	
	#quick-nav-ui,
	#new-comment-nav-ui,
	#hns-date-picker {
		bottom: 0;
		max-height: 0px;
		opacity: 0.0;
		transition:
			bottom 0.2s ease,
			max-height 0.2s ease,
			opacity 0.2s ease,
			visibility 0.2s ease;
	}
	#quick-nav-ui.engaged,
	#new-comment-nav-ui.engaged,
	#hns-date-picker.engaged {
		visibility: visible;
		max-height: 1000px;
		opacity: 1.0;
	}	
	
	#quick-nav-ui {
		right: -24px;
	}
	#content.post-page ~ #ui-elements-container #quick-nav-ui {
		right: -48px;
	}
	#quick-nav-ui.engaged {
		bottom: 64px;
	}
	#quick-nav-ui a {
		font-weight: 400;
	}
	
	#new-comment-nav-ui {
		right: -49px;
	}
	#new-comment-nav-ui.engaged {
		bottom: 216px;
	}

	#hns-date-picker {
		right: -186px;
	}
	#hns-date-picker.engaged {
		bottom: 247px;
	}
	@media only screen and (max-width: 1440px) {
		#hns-date-picker {
			background-color: rgba(255, 255, 255, 0.95);
			right: -18px;
		}
		#hns-date-picker::before {
			display: none;
		}
		#hns-date-picker input {
			background-color: #fff;
		}
		#hns-date-picker span {
			text-shadow:
				0 0 1px #fff,
				0 0 3px #fff,
				0 0 5px #fff,
				0 0 8px #fff,
				0 0 13px #fff,
				0 0 21px #fff;
		}
		#hns-date-picker.engaged {
			bottom: 238px;
		}
	}

	#appearance-adjust-ui-toggle {
		bottom: 10px;
		left: 10px;
	}
	#appearance-adjust-ui-toggle button.engaged {
		transform: rotate(-90deg);
	}

	#comments-view-mode-selector,
	#theme-selector,
	#width-selector,
	#text-size-adjustment-ui,
	#theme-tweaker-toggle {
		pointer-events: none;
		visibility: visible;
		opacity: 0.0;
		transition:
			opacity 0.35s ease;
	}
	<?php fit_content("#theme-tweaker-toggle", "width", "\t"); ?>
	#comments-view-mode-selector::after,
	#theme-selector::after,
	#theme-selector::before,
	#width-selector::after,
	#text-size-adjustment-ui::after {
		content: "";
		background-color: #fff;
		display: block;
		position: absolute;
		width: 100%;
		height: 100%;
		top: 0;
		left: 0;
	}
	#comments-view-mode-selector.engaged,
	#theme-selector.engaged,
	#width-selector.engaged,
	#text-size-adjustment-ui.engaged,
	#theme-tweaker-toggle.engaged {
		pointer-events: auto;
		opacity: 1.0;
	}

	#comments-view-mode-selector {
		left: -45px;
		top: calc(100% - 180px);
	}
	#comments-view-mode-selector::after {
		max-height: 1000px;
		transition:
			max-height 0.2s ease;
	}
	#comments-view-mode-selector.engaged::after {
		max-height: 0px;
	}

	#theme-selector {
		display: block;
		left: 16px;
		top: calc(100% - 316px);
		opacity: 1.0;
		visibility: hidden;
		transition:
			visibility 0.2s ease;
	}
	#theme-selector.engaged {
		visibility: visible;
	}
	#theme-selector::after,
	#theme-selector::before {
		top: -50px;
		height: calc(100% + 60px);
		max-height: 300px;
		transition:
			max-height 0.2s ease;
		z-index: 1;
	}
	#theme-selector.engaged::after,
	#theme-selector.engaged::before {
		max-height: 0px;
	}
	#theme-selector::before {
		z-index: 0;
	}

	#width-selector {
		display: table;
		left: -68px;
		top: calc(100% - 48px);
	}
	#width-selector::after {
		max-width: 100px;
		transition:
			max-width 0.15s ease;
	}
	#width-selector.engaged::after {
		max-width: 0px;
	}

	#text-size-adjustment-ui {
		left: -67px;
		top: calc(100% - 80px);
	}
	#text-size-adjustment-ui::after {
		max-width: 1000px;
		max-height: 1000px;
		transition:
			max-width 0.2s ease,
			max-height 0.2s ease;
	}
	#text-size-adjustment-ui.engaged::after {
		max-width: 0px;
		max-height: 0px;
	}

	#theme-tweaker-toggle {
		left: 19px;
		top: calc(100% - 356px);
		visibility: hidden;
		transition:
			visibility 0.5s ease-out;
	}
	#theme-tweaker-toggle.engaged {
		visibility: visible;
	}
	<?php echo $firefox_exclusive; ?> {
		#theme-tweaker-toggle {
			left: 18px;
		}
	}
	#theme-tweaker-toggle button {
		font-weight: 400;
	}

	#theme-tweaker-ui {
		visibility: visible;
	}
}

/*================*/
/* WIDTH SELECTOR */
/*================*/

#width-selector button {
	box-shadow:
		0 0 0 4px #fff inset,
		0 0 0 5px #aaa inset;
}
#width-selector button:hover,
#width-selector button.selected {
	box-shadow:
		0 0 0 1px #fff inset,
		0 0 0 2px #aaa inset,
		0 0 0 4px #fff inset,
		0 0 0 5px #aaa inset;
}

/*================*/
/* THEME SELECTOR */
/*================*/

#theme-selector button {
	box-shadow:
		0 0 0 4px #fff inset,
		0 0 0 5px #999 inset;
}
#theme-selector button:hover,
#theme-selector button.selected {
	box-shadow:
		0 0 0 1px #fff inset,
		0 0 0 2px #999 inset,
		0 0 0 4px #fff inset,
		0 0 0 5px #999 inset;
}

#theme-selector button::before {
	font-size: 0.9375em;
	font-weight: 300;
	padding: 6px;
	color: #aaa;
	background-color: #fff;
}
#theme-selector button:hover::before,
#theme-selector button.selected::before {
	color: #000;
}

/*======================*/
/* THEME TWEAKER TOGGLE */
/*======================*/

#theme-tweaker-toggle button {
	color: #777;
}

/*=================*/
/* QUICKNAV WIDGET */
/*=================*/

#quick-nav-ui a {
	color: #acd2af;
	border-radius: 4px;
	text-decoration: none;
	transition:
		color 0.15s ease,
		box-shadow 0.15s ease-out;
}
#quick-nav-ui a[href='#bottom-bar'] {
	line-height: 1.8;
}
#quick-nav-ui a:active {
	transform: scale(0.9);
}
#quick-nav-ui a[href='#comments'].no-comments,
#quick-nav-ui a[href='#answers'].no-answers {
	opacity: 0.4;
	color: #ddd;
}
@media only screen and (hover: hover) {
	#quick-nav-ui a:hover {
		color: #79a97e;
		box-shadow: 
			0 0 0 1px rgba(121, 169, 126, 0.3),
			0 0 0 2px #fff;
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
	color: #888;
	text-shadow: 0.5px 0.5px 0 #fff;
	top: 2px;
}
#new-comment-nav-ui .new-comments-count::after {
	color: #777;
}
#new-comment-nav-ui .new-comment-sequential-nav-button:disabled {
	color: #e6e6e6;
}

/*=================*/
/* HNS DATE PICKER */
/*=================*/

#hns-date-picker span {
	color: #999;
	font-weight: 400;
}
#hns-date-picker input {
	border: 1px solid #ddd;
	color: #999;
	padding: 3px 3px 0 3px;
}
#hns-date-picker input:focus {
	color: #000;
}

/*======================*/
/* ANTI-KIBITZER TOGGLE */
/*======================*/

#anti-kibitzer-toggle {
	bottom: unset;
	top: 30px;
	visibility: visible;
	right: -48px;
}
#anti-kibitzer-toggle button::before {
	font-weight: 300;
}
#anti-kibitzer-toggle button::before,
#anti-kibitzer-toggle button::after {
	opacity: 0.2;
}
#anti-kibitzer-toggle button:hover::before,
#anti-kibitzer-toggle button:hover::after {
	opacity: 1.0;
}

/*======================*/
/* TEXT SIZE ADJUSTMENT */
/*======================*/

#text-size-adjustment-ui button {
	font-weight: 400;
}

/*=============================*/
/* COMMENTS VIEW MODE SELECTOR */
/*=============================*/

#comments-view-mode-selector a {
	color: #acd2af;
	opacity: 0.5;
}

/*===============*/
/* KEYBOARD HELP */
/*===============*/

#keyboard-help-overlay {
	font-weight: 400;
}
#keyboard-help-overlay .keyboard-help-container h1 {
	padding: 15px 20px 5px 20px;
}
#nav-item-about button.open-keyboard-help {
	font-size: 0.875em;
	left: 0;
	top: -1px;
}

/*==========*/
/* ARCHIVES */
/*==========*/

.archive-nav {
	border: 1px solid transparent;
	margin: 1.25em 0 0 0;
	padding: 0;
}
.archive-nav *[class^='archive-nav-item'] {
	border-style: solid;
	border-color: #c4dbc4;
	border-width: 1px 0 1px 1px;
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
.archive-nav a:hover,
.archive-nav span[class^='archive-nav-item'] {
	background-color: #c4dbc4;
	color: #fff;
}

/*==========*/
/* LISTINGS */
/*==========*/

h1.listing,
#content.search-results-page h1.listing {
	margin: 0.7em 20px 0.1em 0;
	max-width: calc(100% - 20px);
	font-size: 1.25rem;
	line-height: 1.2;
}

h1.listing a[href^="http"] {
	color: #bbb;
	font-weight: 400;
	font-size: 0.8125em;
	top: 3px;
}
h1.listing a[href^="/posts"] {
	font-weight: 300;
	text-shadow: <?php echo ($platform == 'Mac' ? 'none' : '0 0 0 #444'); ?>;
	color: <?php echo ($platform == 'Mac' ? '#444' : '#000'); ?>;
}
<?php echo $firefox_exclusive; ?> {
	h1.listing a[href^="/posts"] {
		text-shadow: none;
	}
}
@media not all and (min-resolution:.001dpcm) { @media {
	h1.listing a[href^="/posts"] {
		color: #000;
	}
}}

@media only screen and (hover: hover) {
	h1.listing a:hover,
	h1.listing a:focus {
		color: #92c396;
		background-color: rgba(255, 255, 255, 0.85);
	}	
	h1.listing:focus-within::before {
		color: #79a97e;
		font-weight: 400;
		left: -0.625em;
	}
	h1.listing a[href^="http"]:hover {
		color: #79a97e;
	}
	h1.listing a[href^="http"]:focus {
		color: #a0d3a2;
		text-decoration: none;
		border-bottom: 2px dotted #a0d3a2;
	}
}

h1.listing .edit-post-link {
	padding: 5px 3px 24px 0.5em;
	top: 0;
	right: 0;
}
h1.listing .edit-post-link:hover {
	text-decoration: none;
}
#content.user-page h1.listing .edit-post-link {
	background-color: #fff;
}

/*======*/
/* SPAM */
/*======*/

h1.listing:not(:focus-within).spam {
	opacity: 0.15;
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
	font-size: 0.875rem;
	margin: 0 20px 0 1px;
}

h1.listing + .post-meta > * {
	color: #bbb;
	margin: 0 1.25em 0 0;
}
h1.listing + .post-meta a {
	color: #92c396;
}
h1.listing + .post-meta a:hover {
	color: #79a97e;
}
h1.listing + .post-meta .karma-value {
	cursor: default;
}
h1.listing + .post-meta .lw2-link {
	display: none;
}
h1.listing + .post-meta .post-section {
	overflow: visible;
	order: 1;
}
h1.listing + .post-meta .post-section::before {
	position: relative;
	left: unset;
	top: -1px;
}

/*============*/
/* USER PAGES */
/*============*/

#content.user-page h1.page-main-heading,
#content.user-page .user-stats {
	grid-row: 1;
}
#content.user-page #comments-list-mode-selector,
#content.user-page .sublevel-nav.sort {
	grid-row: 2 / span 2;
}
#content.user-page .sublevel-nav {
	grid-row: 2;
	margin-bottom: 1em;
}
#content.user-page #top-nav-bar {
	grid-row: 3;
}

#content.user-page h1.page-main-heading,
#content.conversation-page h1.page-main-heading {
	font-weight: normal;
	margin: 0.5em 0 0 0;
}
#content.user-page h1.page-main-heading {
	border-bottom: 1px solid #e6e6e6;
	line-height: 1;
}

#content.user-page h1.listing,
#content.user-page h1.listing + .post-meta {
	border-color: #ddd;
	border-style: solid;
}
#content.user-page h1.listing {
	max-width: 100%;
	padding: 6px 8px 0 8px;
	border-width: 1px 1px 0 1px;
	margin: 1rem 0 0 0;
}
#content.own-user-page h1.listing,
h1.listing.own-post-listing {
	padding-right: 36px;
}
@media only screen and (hover: hover) {
	#content.user-page h1.listing:focus-within::before {
		left: -0.625em;
	}
}
#content.user-page h1.listing + .post-meta {
	margin: 0 0 1rem 0;
	padding: 12px 8px 3px 8px;
	border-width: 0 1px 1px 1px;
	line-height: 1;
}

#content.conversations-user-page h1.listing {
	padding: 8px 6px 28px 10px;
	font-size: 1.25rem;
}
#content.conversations-user-page h1.listing + .post-meta {
	padding: 4px 10px 0.5em 6px;
	margin: 0;
}

.user-stats .karma-total {
	font-weight: 500;
}

/*===============*/
/* CONVERSATIONS */
/*===============*/

/*============*/
/* LOGIN PAGE */
/*============*/

.login-container form h1 {
	font-weight: 300;
}

.login-container form label {
	color: #aaa;
}

/* “Create account” form */

#signup-form {
	border: 1px solid #e4e4e4;
}
#signup-form input[type='submit'] {
	padding: 8px 12px 6px 12px;
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
	font-weight: bold;
}

/*===================*/
/* TABLE OF CONTENTS */
/*===================*/

.contents {
	padding-top: 0;
	margin-top: 1em;
	background-color: #fff;
}
.post-body .contents ul {
	font-size: 0.8125em;
}
.post-body .contents li::before {
	color: #bbb;
}

/*==================*/
/* POSTS & COMMENTS */
/*==================*/

.body-text {
	text-shadow: <?php echo ($platform == 'Mac' ? '0 0 0 rgba(0,0,0,0.7)' : 'none'); ?>;
	font-weight: <?php echo ($platform == 'Mac' ? '300' : '400'); ?>;
}

/*=======*/
/* POSTS */
/*=======*/

.post {
	overflow: visible;
	padding: 2em 0 0 0;
}

.post-body {
	font-size: 1.25rem;
}

h1.post-title {
	font-size: 2.75rem;
	font-weight: 300;
	line-height: 1.1;
	margin: 1.375em 0 0.5em 0;
}

/*=================*/
/* POST NAVIGATION */
/*=================*/

.post-nav-item {
	padding: 0 0 0 10px;	
}

@media only screen and (max-width: 900px) {
	.post-nav-item {
		padding: 0;
	}
	.sequence-title {
		border-top: 1px solid #ddd;
		padding: 0.375em 0.5em 0.125em 0.5em
	}
	.post-nav.prev {
		border-right: 1px solid #ddd;
	}
	.post-nav.next {
		border-left: 1px solid #ddd;
	}
}

/*===========*/
/* POST-META */
/*===========*/

.post-meta .post-section::before,
.comment-meta .alignment-forum {
	color: #dfdfdf;
	font-weight: 400;
	padding: 1px;
}
.post-meta .post-section.alignment-forum::before {
	color: #d6d7ff;
}
.post .post-meta .post-section::before {
	position: relative;
	top: -3px;
}

a.post-section::before {
	transition: color 0.15s ease;
}
a.post-section:hover::before {
	color: #79a97e;
}
a.post-section.alignment-forum:hover::before {
	color: #999bc1;
}

.post-meta > * {
	color: #bbb;
}
.post-meta a,
.post-meta a:visited {
	color: #92c396;
}
.post-meta a:hover {
	color: #79a97e;
}
.post-meta .lw2-link:hover {
	opacity: 1;
}

.post .top-post-meta {
	flex-flow: column;
	position: relative;
}
.post .top-post-meta .karma,
.post .top-post-meta .author,
.post .top-post-meta .qualified-linking {
	margin: auto;
}
<?php fit_content(".post .top-post-meta .karma, .post .top-post-meta .author, .post .top-post-meta .qualified-linking"); ?>

.post .top-post-meta .karma {
	order: -1;
	display: flex;
	flex-flow: column;
}
.post .top-post-meta .karma .karma-value {
	padding: 5px 0 0 0;
	color: #999;
	font-size: 1.125em;
	position: relative;
	background-color: #fff;
	width: 2.25em;
}
.post .top-post-meta .karma .karma-value::before {
	content: "";
	position: absolute;
	display: block;
	background-color: #ccc;
	height: 1px;
	width: 244px;
	top: 50%;
	left: -100px;
	z-index: -1;
}
.post .top-post-meta .karma .karma-value span {
	display: none;
}

.post .post-meta .karma.active-controls::after {
	bottom: -32px;
	padding: 0 4px;
}
.post .post-meta .karma.active-controls:hover::after {
	opacity: 0.7;
}
.post .top-post-meta .karma.active-controls::after {
	bottom: 0;
	left: calc(100% + 16px);
	max-width: unset;
	padding: 0;
	white-space: nowrap;
}
.post .post-meta .karma .karma-value::after {
	top: -20px;
}
.post .post-meta .karma .karma-value:hover::after {
	opacity: 0.7;
}
.post .top-post-meta .karma .karma-value::after {
	top: -6px;
	left: unset;
	width: unset;
	right: calc(100% + 8px);
}

.post .top-post-meta .author {
	padding: 4px 0 0 0;
	margin: 0.25em auto;
}
.post .top-post-meta .qualified-linking {
	z-index: 1;
}
.post .top-post-meta .qualified-linking label {
	margin: 0;
}

.post .top-post-meta .post-section,
.post .top-post-meta .lw2-link {
	display: none;
}

.post .top-post-meta .date,
.post .top-post-meta .comment-count {
	position: absolute;
	right: 100%;
}
.post .top-post-meta .date {
	top: calc(100% + 34px);
}
.post .top-post-meta .comment-count {
	top: calc(100% + 60px);
}
.post .top-post-meta .date > span,
.post .top-post-meta .comment-count > span {
	position: fixed;
	transform: translateX(-100%);
}

.post .bottom-post-meta {
	padding: 1.5em 0 1em 0;
	margin: 0.5em 0 0 0;
	position: relative;
	border-color: transparent;
}
.post .bottom-post-meta::before,
.post .bottom-post-meta::after {
	content: "";
	position: absolute;
	display: block;
	background-color: #ddd;
	height: 1px;
	width: calc(100% - 60px);
}
.post .bottom-post-meta::before {
	top: 0;
}
.post .bottom-post-meta::after {
	bottom: 0;
}

@media only screen and (max-width: 520px) {
	.post .bottom-post-meta {
		padding: 0.75em 0 0.125em 0;
	}
}

/*============*/
/* LINK POSTS */
/*============*/

.post.link-post a.link-post-link::before {
	opacity: 0.6;
}
.post.link-post a.link-post-link:hover::before {
	opacity: 1;
}
.post.link-post a.link-post-link:focus {
	color: #79a97e;
	border-bottom: 2px dotted #79a97e;
}

/*==========*/
/* COMMENTS */
/*==========*/

.comments::before {
	border-top: 1px solid #ddd;
}

.comments {
	padding: 0 0 0 10px;
}
#content > .comment-thread .comment-meta a.date:focus,
#content > .comment-thread .comment-meta a.permalink:focus {
	color: #888;
	outline: 1px dotted #999;
	position: relative;
	background-color: #fff;
	padding: 0 5px;
	left: -5px;
}
#content > .comment-thread .comment-meta a.date:focus + *,
#content > .comment-thread .comment-meta a.permalink:focus + *:not(.comment-post-title) {
	margin-left: -10px;
}
#content > .comment-thread .comment-meta a.permalink:focus {
	outline: 2px dotted #999;
}
.comment-item {
	border: 1px solid #ddd;
	background-color: var(--GW-comment-background-color);
}
.comment-parent-link::after {
	box-shadow: 
		0 28px 16px -16px var(--GW-comment-parent-background-color) inset,
		4px 16px 0 12px var(--GW-comment-background-color-target) inset,
		4px	4px 0 12px var(--GW-comment-background-color-target) inset;
}

.comment-body {
	font-size: 1.1875rem;
}
#content.user-page .comment-body,
#content.index-page .comment-body {
	font-size: 1.125rem;
}

/*==============*/
/* COMMENT-META */
/*==============*/

.comment-meta {
	padding-top: 4px;
}
.comment-meta > * {
	color: #bbb;
}
.comment-meta a,
.comment-meta a:visited {
	color: #92c396;
}
.comment-meta a:hover {
	color: #79a97e;
}
.comment-meta .author {
	font-size: 1.125em;
	font-weight: normal;
}

.comment-controls .karma {
	color: #bbb;
}

.comment-item .karma.active-controls::after,
.comment-item .karma .karma-value::after,
.author::before {
	background-color: #fff;
	color: #bbb;
	border-radius: 4px;
	box-shadow: 0 0 0 1px #eee inset;
}
.comment-item .karma.active-controls::after {
	padding: 6px 4px 4px 4px;
	bottom: -42px;
}
.comment-item .karma .karma-value::after {
	padding: 4px 8px 0 8px;
	top: -30px;
}

/*====================*/
/* ANTI-KIBITZER MODE */
/*====================*/

.author.redacted,
.inline-author.redacted {
	opacity: 0.9;
	font-weight: 300;
}

.karma-value.redacted {
	opacity: 0.7;
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
	color: #bbb;
	font-weight: 400;
}
a.comment-parent-link:hover::before {
	background-color: #ffd;
	color: #999;
}

div.comment-child-links {
	font-weight: 400;
}
div.comment-child-links a {
	font-weight: 300;
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

.comment-popup {
	background-color: #fff;
}

/*====================*/
/* COMMENT PERMALINKS */
/*====================*/

.individual-thread-page a.comment-parent-link:hover:empty::before {
	left: unset;
}

.comment-meta .permalink,
.comment-meta .lw2-link,
.individual-thread-page .comment-parent-link:empty {
	filter: hue-rotate(270deg);
	opacity: 0.4;
}
.comment-meta .permalink:hover,
.comment-meta .lw2-link:hover,
.individual-thread-page .comment-parent-link:empty:hover {
	opacity: 1.0;
}

/*=======================*/
/* COMMENTS COMPACT VIEW */
/*=======================*/

#comments-list-mode-selector {
	opacity: 0.4;
	transition: opacity 0.15s ease;
}
#content.index-page #comments-list-mode-selector {
	justify-self: end;
}
#comments-list-mode-selector:hover {
	opacity: 1.0;
}

#comments-list-mode-selector button {
	border: none;
	background-color: transparent;
	box-shadow:
		0 0 0 4px #fff inset,
		0 0 0 5px #aaa inset;
}
#comments-list-mode-selector button:hover,
#comments-list-mode-selector button.selected {
	box-shadow:
		0 0 0 1px #fff inset,
		0 0 0 2px #aaa inset,
		0 0 0 4px #fff inset,
		0 0 0 5px #aaa inset;
}

#content.compact > .comment-thread .comment-item {
	max-height: 53px;
}

@media only screen and (hover: hover) {
	#content.compact > .comment-thread .comment-item:hover .comment,
	#content.compact > .comment-thread .comment-item.expanded .comment {
		background-color: #fff;
		outline: 1px solid #92c396;
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
@media only screen and (hover: none) {
	#content.compact > .comment-thread.expanded .comment-item .comment {
		background-color: #fff;
		outline: 1px solid #92c396;
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
#content.compact > .comment-thread .comment-item:hover .comment {
	background-color: #fff;
	outline: 1px solid #92c396;
}
#content.compact > .comment-thread .comment-item:hover .comment::before {
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

#content.user-page.compact > h1.listing {
	margin-top: 0.5rem;
}
#content.user-page.compact > h1.listing + .post-meta {
	margin-bottom: 1rem;
}

/*===========================*/
/* HIGHLIGHTING NEW COMMENTS */
/*===========================*/

.new-comment::before {
	outline: 1px solid #5a5;
	box-shadow:
		0 0 6px -2px #5a5 inset, 
		0 0 4px #5a5;
}

/*=================================*/
/* COMMENT THREAD MINIMIZE BUTTONS */
/*=================================*/

.comment-minimize-button {
	color: #ddd;
	font-weight: 300;
	box-shadow: 0 0 0 1px transparent;
}
.comment-minimize-button:hover {
	color: #bbb;
	text-shadow: var(--GW-shadow-white-glow);
}
.comment-minimize-button::after {
	color: #999;
}
.comment-minimize-button.maximized::after {
	color: #ccc;
}

/*=================================*/
/* INDIVIDUAL COMMENT THREAD PAGES */
/*=================================*/

.individual-thread-page > h1 {
	margin: 2em 0 0 30px;
	font-weight: 300;
}
.individual-thread-page > .comments {
	padding: 0 0 0 30px;
}
.individual-thread-page > #bottom-bar.decorative::before {
	margin: 0 30px 0 60px;
}

/*==============*/
/* VOTE BUTTONS */
/*==============*/

.upvote,
.downvote {
	color: #ddd;
	font-weight: 400;
	position: relative;
}
.vote::before {
	position: relative;
	z-index: 1;
}
.upvote::before {
	content: "\F077";
}
.downvote::before {
	content: "\F078";
	position: relative;
	top: 1px;
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
	color: #ddd;
}

.upvote::after {
	content: "\F325";
	bottom: 5px;
	left: 7px;
}
.downvote::after {
	content: "\F322";
	top: 5px;
	left: 7px;
}
<?php echo $firefox_exclusive; ?> {
	.upvote::after {
		bottom: 3px;
		left: 8px;
	}
	.downvote::after {
		top: 4px;
		left: 8px;
	}
}

.post .top-post-meta .upvote::after,
.post .top-post-meta .downvote::after {
	left: 14px;
}

/*===========================*/
/* COMMENTING AND POSTING UI */
/*===========================*/

.posting-controls input[type='submit'] {
	padding: 6px 12px 3px 12px;
}

.comment-controls {
	margin: 0 4px 4px 16px;
}
.comment + .comment-controls .action-button {
	font-weight: 300;
}

.new-comment-button {
	margin: 0;
	padding: 0.125em;
}

.comment-controls .cancel-comment-button {
	color: #c00;
	text-shadow: 
		0 0 1px #fff,
		0 0 2px #fff;
	padding: 6px 8px 1px 4px;
}
.comment-controls .cancel-comment-button:hover {
	color: #f00;
}

.comment-controls .delete-button,
.comment-controls .retract-button {
	color: #fd7354;
}
.comment-controls .delete-button::before,
.comment-controls .unretract-button::before {
	font-weight: 300;
}
.comment-controls .retract-button::before {
	font-weight: 400;
}
.comment-controls .edit-button,
.comment-controls .unretract-button {
	color: #0b0;
}
.comment-controls .edit-button::before {
	font-weight: 300;
}
.comment-controls .action-button:hover {
	color: #f00;
	text-shadow: 0 0 0.5px #faa;
}

.post-controls {
	margin: 3em 2.5em 0 0;
	grid-row: 2;
}
.post {
	grid-row: 2;
}
.edit-post-link,
.edit-post-link:visited {
	color: #090;
}

.posting-controls textarea {
	font-weight: 300;
	color: #000;
	text-shadow: 0 0 0 #000;
	border-color: #eee;
	transition: border-color 0.15s ease;
}
.posting-controls textarea:focus {
	border-width: 29px 1px 1px 1px;
	border-color: #92c396;
}
.posting-controls.edit-existing-post textarea:focus,
.posting-controls form.edit-existing-comment textarea:focus {
	border-color: #090;
}

/*= Scroll bars =*/

.posting-controls textarea::-webkit-scrollbar {
	width: 16px;
	background-color: transparent;
}
.posting-controls textarea::-webkit-scrollbar-track {
	background-color: #fff;
}
.posting-controls textarea::-webkit-scrollbar-thumb {
	background-color: #eee;
	box-shadow: 0 0 0 1px #fff inset;
}
.posting-controls textarea:focus::-webkit-scrollbar-thumb {
	background-color: #c4dbc4;
	box-shadow: 0 0 0 1px #fff inset;
}

/* GUIEdit buttons */

.guiedit-buttons-container {
	background-color: #fff;
	border-bottom: 1px solid #eee;
}

.posting-controls.edit-existing-post .guiedit-buttons-container button,
.posting-controls form.edit-existing-comment .guiedit-buttons-container button {
    color: #050;
}
.guiedit-buttons-container button {
	border: 1px solid transparent;
}

.guiedit::after {
	color: #999;
	font-weight: 300;
	text-shadow: 0 0 0 #999;
	top: 3px;
}

.posting-controls .markdown-reference-link a {
	background-position: right 70%;
}
.markdown-reference-link {
	color: #999;
}

/* Markdown hints */

#markdown-hints-checkbox + label:hover {
	color: #79a97e;
}
#markdown-hints {
	border: 1px solid #faa;
	background-color: #fff;
}
#markdown-hints .markdown-hints-row span,
#markdown-hints .markdown-hints-row code {
	padding: 2px 12px 2px 2px;
}

/*================*/
/* EDIT POST FORM */
/*================*/

#edit-post-form .post-meta-fields input[type='checkbox'] + label {
	top: 2px;
	color: #acd2af;
	transition: color 0.15s ease;
}
#edit-post-form .post-meta-fields input[type='checkbox'] + label:hover {
	color: #79a97e;
}
#edit-post-form .post-meta-fields input[type='checkbox'] + label::before {
	top: 2px;
	border: 1px solid #eee;
	color: #bbb;
	transition: 
		box-shadow 0.3s ease,
		border-color 0.15s ease;
}
@media only screen and (hover:hover) {
	#edit-post-form .post-meta-fields input[type='checkbox'] + label:hover::before {
		border-color: #c4dbc4;
	}
}
#edit-post-form .post-meta-fields input[type='checkbox']:checked + label::before {
	border-color: #c4dbc4;
	box-shadow: 
		0 0 0 4px #fff inset,
		0 0 0 1em #c4dbc4 inset;
}

#edit-post-form label[for='url'],
#edit-post-form input[name='url'] {
	display: block;
	transition:
		max-height 0.15s ease,
		overflow 0.15s ease,
		margin-top 0.15s ease,
		margin-bottom 0.15s ease,
		padding 0.15s ease,
		border-color 0.15s ease;
		
}
#edit-post-form .link-post-checkbox:not(:checked) ~ label[for='url'],
#edit-post-form .link-post-checkbox:not(:checked) ~ input[name='url'] {
	max-height: 0;
	overflow: hidden;
	margin-top: 0;
	margin-bottom: 0;
	padding: 0;
	border-color: transparent;
}

#edit-post-form label[for='title'],
#edit-post-form label[for='url'],
#edit-post-form label[for='section'] {
	color: #aaa;
	text-shadow: 0 0 0 #aaa;
}

#edit-post-form input[type='radio'] + label {
	color: #92c396;
	border-color: #c4dbc4;
	padding: 6px 12px 3px 12px;
	position: relative;
	top: -2px;
	transition:
		background-color 0.15s ease,
		color 0.15s ease,
		border-color 0.15s ease;
}
#edit-post-form input[type='radio'][value='all'] + label {
	border-radius: 8px 0 0 8px;
	border-width: 1px;
}
#edit-post-form input[type='radio'][value='drafts'] + label {
	border-radius: 0 8px 8px 0;
	padding-right: 13px;
}
#edit-post-form input[type='radio'] + label:hover,
#edit-post-form input[type='radio']:focus + label,
#edit-post-form input[type='radio']:checked + label {
	background-color: #c4dbc4;
	color: #fff;
}
#edit-post-form input[type='radio']:active + label {
	border-color: #92c396;
	background-color: #92c396;
}

#edit-post-form input[type='submit'] {
	padding: 7px 14px 4px 14px;
}

/*=======*/
/* LINKS */
/*=======*/

a {
	text-decoration: none;
	transition: color 0.15s ease;
}
a:visited {
	color: #bebb84;
}
a:hover {
	color: #bbb;
}

/*=========*/
/* BUTTONS */
/*=========*/

button,
input[type='submit'] {
	color: #92c396;
}
input[type='submit'] {
	color: #92c396;
	background-color: #fff;
	border: 1px solid #c4dbc4;
	transition:
		color 0.15s ease,
		background-color 0.15s ease,
		border-color 0.15s ease;
}

input[type='submit']:hover,
input[type='submit']:focus {
	background-color: #c4dbc4;
	color: #fff;
}
input[type='submit']:active {
	background-color: #92c396;
	border-color: #92c396;
}
.button:visited {
	color: #92c396;
}
button:hover,
.button:hover {
	color: #79a97e;
	text-decoration: none;
}
button:active,
.button:active {
	transform: scale(0.9);
}
button:focus:not(:hover),
.button:focus:not(:hover) {
	transform: none;
}
<?php echo $firefox_exclusive; ?> {
	.button:active {
		transform: none;
	}
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
	font-weight: 300;
}
.body-text h1 {
	margin-top: 1.25em;
	box-shadow:
		0 -7px 0 0 #fff inset,
		0 -8px 0 0 #eee inset;
}
.body-text h6 {
	color: #555;
}

/*========*/
/* QUOTES */
/*========*/

blockquote {
	border-left: 5px solid #e6e6e6;
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

#images-overlay div::after {
	font-weight: 400;
	padding: 10px 12px 6px 12px;
}

/*=============*/
/* IMAGE FOCUS */
/*=============*/

#image-focus-overlay {
	visibility: visible;
}

#image-focus-overlay .caption p {
	margin: 1em 1.25em 0.875em 1.25em;
	font-weight: 400;
}

/*======*/
/* MISC */
/*======*/

hr {
	margin: 1em 0;
}
hr::before {
	content: "• • •";
	letter-spacing: 7px;
	color: #aaa;
	text-align: center;
	display: block;
	font-size: 0.875em;
}

code,
pre {
	font-size: 0.9375em;
	font-feature-settings: 'ss04';
}
code {
	background-color: #eee;
	padding: 0 5px 1px 5px;
	box-shadow: 0 0 0 1px #fff inset;
}

input[type='text'],
input[type='search'],
input[type='password'] {
	border: 1px solid #999;
	color: #000;
	background-color: transparent;
	border-color: transparent;
	border-bottom-color: #eee;
	transition: border-color 0.15s ease;
}
input[type='text']:focus,
input[type='search']:focus,
input[type='password']:focus {
	border-bottom-color: #c4dbc4;
}

select {
	color: #000;
}

strong, b {
	font-weight: 600;
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

#content.no-nav-bars ~ #ui-elements-container #site-nav-ui-toggle {
	display: none;
}
#content.no-comments ~ #ui-elements-container #post-nav-ui-toggle {
	display: none;
}

#aux-about-link a {
	color: #777;
}
#aux-about-link a:hover {
	opacity: 1.0;
	text-shadow: 0 0 1px #fff, 0 0 3px #fff, 0 0 5px #fff;
}

.qualified-linking label {
	color: #ccc;
	font-weight: 400;
}
.qualified-linking label:hover {
	color: #92c396;
}

.qualified-linking-toolbar {
	border: 1px solid #ccc;
	background-color: #fff;
}
.qualified-linking-toolbar a {
	padding: 3px 6px 0 6px;
}
.qualified-linking-toolbar a,
.qualified-linking-toolbar a:visited {
	color: #acd2af;
}
.qualified-linking-toolbar a:hover {
	color: #92c396;
	text-decoration: none;
	background-color: #e4f1e5;
}
.qualified-linking label::after {
	background-color: #fff;
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

#content.alignment-forum-index-page::after {
	margin: -0.25em 0 0.25em 0;
	text-align: center;
	font-weight: 600;
	background-color: #7f85b2;
	color: transparent;
	-webkit-background-clip: text;
	text-shadow: 
		rgba(255, 255, 255, 0.5) 0px 3px 3px;
}

/*====================*/
/* FOR NARROW SCREENS */
/*====================*/

@media not screen and (hover: none) {
	@media only screen and (max-width: 1080px) {
		#site-nav-ui-toggle button.engaged {
			left: -72px;
		}
		#text-size-adjustment-ui {
			left: -22px;
			top: calc(100% - 240px);
		}
		#width-selector {
			left: -23px;
			top: calc(100% - 140px);
		}
		#theme-tweaker-toggle button {
			width: unset;
			height: unset;
		}
	}
	@media only screen and (max-width: 1020px) {
	}
	@media only screen and (max-width: 1000px) {
		#site-nav-ui-toggle button.engaged {
			left: -56px;
		}
		#theme-selector {
			padding: 0;
		}
		#theme-selector button {
			margin: 1px 7px 0 7px;
		}
	}
}

/*========*/
/* MOBILE */
/*========*/

@media only screen and (hover: none) {
	#site-nav-ui-toggle {
		top: 10px;
		left: 10px;
	}
	#site-nav-ui-toggle button.engaged {
		width: 1.125em;
		overflow: hidden;
		position: relative;
		left: 5px;
		top: -3px;
	}
	#site-nav-ui-toggle button.engaged::before {
		content: "\F00D";
		font-size: 34px;
		padding: 0 0.25em 0 0;
	}
	#ui-elements-container > #site-nav-ui-toggle button.engaged {
		transform: rotate(90deg);
	}

	#ui-elements-container > div[id$='-ui-toggle'] button,
	#theme-selector .theme-selector-close-button  {
		color: #bbb;
		text-shadow:
			0 0 1px #fff,
			0 0 3px #fff,
			0 0 5px #fff,
			0 0 10px #fff,
			0 0 20px #fff,
			0 0 30px #fff;
	}
	#ui-elements-container > div[id$='-ui-toggle'] button {
		font-weight: 400;
	}
	#theme-selector .theme-selector-close-button {
		font-weight: 300;
	}

	#theme-selector {
		background-color: #fff;
		box-shadow: 
			0 0 0 1px #999,
			0 0 1px 3px #fff,
			0 0 3px 3px #fff,
			0 0 5px 3px #fff,
			0 0 10px 3px #fff,
			0 0 20px 3px #fff;
		border-radius: 12px;
	}
	#theme-selector::before {
		color: #999;
		font-weight: 300;
		position: relative;
		top: 6px;
	}
	#theme-selector button,
	#theme-selector button.selected {
		background-color: #fff;
		border-radius: 10px;
		box-shadow:
			0 0 0 4px #fff inset,
			0 0 0 5px #999 inset;
	}
	#theme-selector button.selected {
		background-color: #c4dbc4;
	}
	#theme-selector button::after {
		color: #819681;
		font-weight: 300;
		max-width: calc(100% - 3.5em);
		overflow: hidden;
		text-overflow: ellipsis;
		padding: 1px 0 0 0;
	}
	#theme-selector button.selected::after {
		color: #fff;
	}

	#theme-tweaker-toggle button {
		color: #999;
		font-weight: 400;
	}

	#quick-nav-ui {
		background-color: #fff;
	}
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
		font-weight: bold;
		box-shadow:
			0 0 1px 0 #fff,
			0 0 3px 0 #fff,
			0 0 5px 0 #fff;
		background-color: #fff;
		border-radius: 4px;
	}
	#quick-nav-ui,
	#new-comment-nav-ui {
		border-radius: 8px;
	}
	#new-comment-nav-ui {
		background-color: #fff;
		border: 1px solid #fff;
	}
	#new-comment-nav-ui::before {
		color: #aaa;
		font-weight: 500;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button {
		color: #79a97e;
	}
	#new-comment-nav-ui .new-comments-count {
		background-color: inherit;
		top: 0;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button:disabled {
		color: #e6e6e6;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button.new-comment-previous {
		border-radius: 7px 0 0 7px;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button.new-comment-next {
		border-radius: 0 7px 7px 0;
	}
	#hns-date-picker {
		background-color: #fff;
		border: 1px solid #fff;
	}

	#top-nav-bar {
		padding: 1.25em 0 0.25em 0;
		font-size: 1.625rem;
		margin: 0;
		grid-row: 2;
	}
	#top-nav-bar .page-number {
		line-height: 1.7;
	}
	#top-nav-bar .page-number span {
		display: block;
	}
	#top-nav-bar a.disabled {
		opacity: 0.2;
	}
	
	/*****************************************/
	@media only screen and (max-width: 900px) {
	/*****************************************/
		#theme-less-mobile-first-row-placeholder {
			grid-row: 1;
			height: 50px;
		}

		#primary-bar,
		#secondary-bar {
			position: static;
			width: 0;
			height: 0;
		}

		#primary-bar {
			position: fixed;
			left: 0;
			margin: 0;
			padding: 5px 0 5px 0;
			height: unset;
			background-color: #fff;
			border-bottom: 1px solid #ddd;
			box-shadow: 0 0 0 1px #fff;
			z-index: 2;
			visibility: hidden;
			transition:
				visibility 0.2s ease,
				width 0.2s ease,
				opacity 0.2s ease,
				filter 0.2s ease;
		}
		#primary-bar.engaged {
			width: 100%;
			visibility: visible;
			padding: 5px 4px 75px 60px;
		}
		#secondary-bar #nav-item-archive,
		#secondary-bar #nav-item-sequences,
		#secondary-bar #nav-item-about {
			opacity: 0.0;
			transition:
				opacity 0.3s ease,
				filter 0.2s ease;
		}
		#secondary-bar.engaged #nav-item-archive,
		#secondary-bar.engaged #nav-item-sequences,
		#secondary-bar.engaged #nav-item-about {
			opacity: 1.0;
			position: fixed;
			top: 80px;
			z-index: 3;
			width: 64px;
		}
		#secondary-bar.engaged #nav-item-archive {
			left: 8px;
		}
		#secondary-bar.engaged #nav-item-sequences {
			left: 72px;
		}
		#secondary-bar.engaged #nav-item-about {
			left: 136px;
		}

		#primary-bar.engaged.translucent-on-scroll,
		#secondary-bar.engaged.translucent-on-scroll #nav-item-archive,
		#secondary-bar.engaged.translucent-on-scroll #nav-item-sequences,
		#secondary-bar.engaged.translucent-on-scroll #nav-item-about,
		.page-toolbar.engaged.translucent-on-scroll {
			opacity: 0.6;
			filter: blur(2px);
			pointer-events: none;
		}

		.page-toolbar {
			position: fixed;
			height: unset;
			width: unset;
			z-index: 4;
			right: 100%;
			top: 80px;
			transition:
				right 0.2s ease,
				opacity 0.2s ease,
				filter 0.2s ease;
		}
		.page-toolbar.engaged {
			right: 0;
		}
		.page-toolbar,
		#content:not(.user-page) .page-toolbar {
			display: flex;
			flex-flow: row;
			justify-content: flex-end;
			padding: 0 8px 0 0;
		}
		.page-toolbar > * {
			right: unset;
			line-height: 1.15;
			padding: 6px 0;
			margin: 0;
		}
		.page-toolbar > form,
		.page-toolbar > .button {
			text-align: center;
			flex-basis: 25%;
			margin-left: 1.5em;
		}
		.page-toolbar .button {
			text-transform: uppercase;
			font-size: 0.625rem;
		}
		.page-toolbar .button::before,
		#content.user-page .page-toolbar .button::before,
		.page-toolbar form::before,
		#content.user-page .page-toolbar form::before {
			font-size: 1.375rem;
			display: block;
			padding: 2px;
			font-size: 1.375rem;
			display: block;
		}
		.page-toolbar .rss {
			white-space: nowrap;
			position: fixed;
			top: 143px;
			left: -60px;
			padding: 6px 10px 5px 10px;
			visibility: hidden;
			background-color: #fff;
			border-style: solid;
			border-color: #ddd;
			border-width: 0 1px 1px 0;
			box-shadow: 
				0 1px 0 0 #fff,
				1px 1px 0 0 #fff;
			transition: left 0.2s ease;
		}
		.page-toolbar .rss,
		#content.user-page .page-toolbar .rss {
			margin: 0;
		}
		.page-toolbar.engaged .rss {
			visibility: visible;
			left: 0;
		}

		#primary-bar .nav-inner {
			font-size: 1.375em;
		}
		#secondary-bar .nav-inner {
			font-size: 1.125em;
		}
		#secondary-bar .nav-item:not(#nav-item-search) .nav-inner {
			padding: 6px 10px;
		}

		#nav-item-search {
			max-width: calc(100% - 180px);
			top: 4px;
			left: 68px;
		}
		#nav-item-search input {
			width: calc(100% - 32px);
		}
		#nav-item-search button {
			position: relative;
			bottom: 5px;
			visibility: visible;
			height: 32px;
			width: 40px;
			padding: 9px 15px 3px 5px;
		}
		#nav-item-search form:not(:focus-within) button:not(:hover) {
			color: transparent;
		}
		#nav-item-search button::before {
			color: #ddd;
		}

		#nav-item-login {
			top: 16px;
			right: 8px;
		}
		#nav-item-login .nav-inner {
			text-transform: none;
			font-size: 1.75em;
		}
		#nav-item-login .nav-inner::before {
			display: none;
		}
		#inbox-indicator::before {
			font-size: 1.75em;
			left: 2px;
		}

		#bottom-bar .nav-inner {
			padding: 1rem 0 1.25rem 0;
		}
		#bottom-bar .nav-inner::after {
			position: absolute;
		}

		#content.search-results-page #comments-list-mode-selector {
			grid-row: 2;
			justify-self: end;
		}

		#content,
		#content.comment-thread-page {
			padding: 0 4px;
		}

		h1.listing + .post-meta > * {
			line-height: 1.5;
		}
		h1.listing + .post-meta .post-section {
			overflow: visible;
			order: 1;
			width: unset;
		}
		h1.listing + .post-meta .post-section::before {
			position: unset;
		}

		.archive-nav *[class^='archive-nav-item-'] {
			border-width: 1px !important;
		}
		.archive-nav > *[class^='archive-nav-'] + *[class^='archive-nav-']::before {
			background-color: #aaa;
		}

		.post {
			padding: 0;
		}
		.post .top-post-meta .author {
			margin: 1em auto 0 auto;
		}
		.post .top-post-meta .date,
		.post .top-post-meta .comment-count {
			position: static;
		}
		.post .top-post-meta .date {
			margin: 1.5em auto 0 auto;
		}
		.post .top-post-meta .comment-count span {
			display: initial;
			position: static;
		}
		h1.post-title {
			line-height: 1.3;
		}

		.comments {
			padding: 0;
		}
		.comment-item .comment-item {
			margin: 0.75em 3px 3px 6px;
		}
		.comment-item .comment-item + .comment-item {
			margin: 1.5em 3px 3px 6px;
		}

		.comment-controls {
			position: relative;
		}
		.comment-controls .cancel-comment-button,
		.comments > .comment-controls .cancel-comment-button {
			right: 4px;
		}

		.sublevel-nav:not(.sort) .sublevel-item,
		.sublevel-nav:not(.sort) .sublevel-item:first-child,
		.sublevel-nav:not(.sort) .sublevel-item:last-child {
			border-radius: 8px;
			border-width: 1px;
			margin: 2px;
		}

		#content.user-page #theme-less-mobile-first-row-placeholder {
			height: 60px;
		}
		#content.user-page h1.page-main-heading,
		#content.user-page .user-stats {
			grid-row: 2;
		}
		#content.user-page h1.page-main-heading {
			margin: 0.5em 0 0 0.125em;
		}
		#content.user-page #comments-list-mode-selector,
		#content.user-page .sublevel-nav.sort {
			grid-row: 3 / span 2;
		}
		#content.user-page .sublevel-nav {
			grid-row: 3;
			margin-bottom: 1em;
		}
		#content.user-page #top-nav-bar {
			grid-row: 4;
			margin: 0.5em 0 0 0;
		}

		#content.conversation-page #theme-less-mobile-first-row-placeholder {
			height: 64px;
		}
		#content.conversation-page #comments-list-mode-selector {
			grid-row: 6;
			margin-top: -32px;
		}
		#content.conversation-page .conversation-participants {
			grid-row: 4;
			align-self: end;
		}
	/*******************************************/
	} @media only screen and (max-width: 720px) {
	/*******************************************/
		#content.index-page > .sublevel-nav.sort {
			flex-flow: column;
			margin-right: 4px;
		}
	/*******************************************/
	} @media only screen and (max-width: 520px) {
	/*******************************************/
		h1.listing,
		#content.search-results-page h1.listing {
			font-size: 1.25rem;
			margin: 18px 6px 4px 6px;
			max-width: calc(100% - 12px);
		}
		h1.listing + .post-meta {
			margin: 4px 6px;
		}
		#content.conversations-user-page h1.listing::after {
			height: calc(100% + 2.25em);
		}
		#content.conversations-user-page h1.listing + .post-meta .date {
			margin: 0 0 0 1em;
		}
		
		.comment-body {
			font-size: 1.125rem;
		}
		
		#content.compact > .comment-thread .comment-item {
			max-height: 105px;
		}
		
		.textarea-container:focus-within textarea {
			background-color: #fff;
			border-width: 1px;
			box-shadow: 0 0 0 2px #fff;
		}
		.textarea-container:focus-within .guiedit-mobile-auxiliary-button {
			padding: 5px 6px 6px 6px;
			font-weight: bold;
		}
		.textarea-container:focus-within .guiedit-mobile-help-button.active {
			color: #c00;
		}
		.textarea-container:focus-within .guiedit-buttons-container {
			background-color: #fff;
			border-top: 1px solid #ddf;
		}
		.posting-controls .textarea-container:focus-within .guiedit-buttons-container {
			box-shadow: none;
		}
		#content.conversation-page .textarea-container:focus-within::after {
			background-color: #fff;
		}
		#markdown-hints::after {
			color: #090;
		}

		#edit-post-form .post-meta-fields input[type='checkbox'] + label {
			top: 2px;
			font-weight: 400;
		}
		#edit-post-form .post-meta-fields input[type='checkbox'] + label::before {
			top: 1px;
		}
	}
}

