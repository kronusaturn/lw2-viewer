/*****************/
/* CLASSIC THEME */
/*****************/

/*===========*/
/* VARIABLES */
/*===========*/

/*	Typography.
	*/
:root {
	--GW-UI-font: 'Liberation Sans', var(--GW-sans-serif-fallback-font-stack);
	--GW-UI-font-weight-light: 400;
	--GW-UI-font-weight-heavy: 700;

	--GW-monospaced-font: 'Liberation Mono', var(--GW-monospaced-fallback-font-stack);

	--GW-body-text-font: 'Liberation Sans', var(--GW-sans-serif-fallback-font-stack);
}

/*	Layout.
	*/
:root {
	--GW-comment-compact-height: 49px;
	--GW-comment-compact-height-mobile: 95px;
	--GW-comment-minimized-height: 30px;
	--GW-comment-minimized-height-mobile: 52px;
}

/*	Color scheme.
	*/
:root {
	--GW-body-background-color: #d8d8d8;
	--GW-content-background-color: #fff;

	--GW-hyperlink-color: #6a8a6b;
	--GW-hyperlink-visited-color: #8a8a8b;
	--GW-hyperlink-hover-color: #3d3d3e;

	--GW-button-hover-color: #d00;
	--GW-button-active-color: #f00;

	--GW-nav-bar-item-color: #999;
	--GW-nav-bar-item-hover-color: #777; 
	--GW-nav-bar-item-active-color: #666;

	--GW-shadow-white-glow:
		0 0 1px #fff,
		0 0 3px #fff,
		0 0 5px #fff;

	--GW-comment-background-color-odd: #f7f7f8;
	--GW-comment-background-color-even: #fff;
	--GW-comment-background-color-target: #ffd;

	--GW-comment-item-outline-width: 2px;
	--GW-comment-item-new-comment-outline-color: #5a5;
	--GW-comment-item-focused-outline-color: #f00;
	--GW-comment-item-higlight-color: #e7b200;
	--GW-comment-item-highlight-faint-color: #f8e7b5;

	--GW-comment-item-expanded-background-color: #fff;
	--GW-comment-item-expanded-box-shadow:
		0 0  3px #fff,
		0 0  5px #fff,
		0 0  7px #fff,
		0 0 10px #fff,
		0 0 20px #fff,
		0 0 30px #fff,
		0 0 40px #fff;

	--GW-vote-button-color: #c8c8c8;
	--GW-upvote-button-color: #0b0;
	--GW-downvote-button-color: #f00;

	--GW-theme-selector-outline-color: #aaa;
}

/*======*/
/* BASE */
/*======*/

body {
	color: #000;
}
#content {
	line-height: 1.5;
}
#content::before {
	box-shadow: 0px 0px 10px #555;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

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

/*==========*/
/* NAV BARS */
/*==========*/

#primary-bar .nav-inner,
#bottom-bar .nav-inner {
	font-size: 1.125em;
}
#secondary-bar .nav-inner {
	font-size: 0.875em;
}

/*= Decorative bottom bar =*/

#bottom-bar.decorative {
	color: #d8d8d8;
	background-color: var(--GW-content-background-color);
	border: none;
}

/*= Accesskey hints =*/

.nav-inner::after {
	left: 5px;
	top: 0;
	font-size: 0.7em;
	color: #ddd;
}
.nav-inner:hover::after {
	color: #bbb;
}

/*= This makes the navbar items look like tabs: =*/

.nav-bar {
	background-color: #f5f5f5;
}
.nav-bar {
	border-bottom: 1px solid #d6d5d6;
}
#bottom-bar {
	border-bottom: none;
	border-top: 1px solid #d6d5d6;
}
.nav-bar .nav-current {
	color: #666;
}
.nav-item:nth-of-type(n+2) {
	box-shadow: -9px 0 0 -8px #d6d5d6;
}

/*= Search tab =*/

#nav-item-search form::before {
	color: #bbb;
}
#nav-item-search input::placeholder {
	color: #d00;
}

/*= Recent Comments tab =*/

#nav-item-recent-comments span {
	margin: 0 5px 0 0;
}

/*= User/login tab =*/

#inbox-indicator::before {
	color: #ccc;
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
	#primary-bar .nav-inner {
		font-size: 1.25em;
	}
	#nav-item-all .nav-inner::before {
		margin-bottom: 1px;
	}
}
@media only screen and (max-width: 520px) {
	#nav-item-all .nav-inner::before {
		margin-bottom: 0;
	}
}

/*===============*/
/* PAGINATION UI */
/*===============*/

#bottom-bar .nav-item a::before {
	font-weight: 900;
}

/*==============*/
/* PAGE TOOLBAR */
/*==============*/

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 640px) {
	#content.user-page .page-toolbar .button,
	#content.user-page .page-toolbar .rss {
		border: 1px solid #d6d5d6;
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
	line-height: 1.8;
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
	padding: 7px 7px 6px 8px;
	text-transform: uppercase;
	box-shadow: 1px 1px 0 0 #bbb inset;
}
.sublevel-nav.sort {
	padding: 18px 0 0 0;
}
.sublevel-nav.sort::before {
	text-transform: uppercase;
	color: #777;
	text-shadow: 0.5px 0.5px 0 #fff;
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
		0 0 0 1px #bbb inset,
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
		0 0 0 4px #d8d8d8 inset,
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
	#theme-selector::before {
		color: #000;
		font-weight: normal;
	}
	#theme-selector .theme-selector-close-button {
		color: #777;
		font-weight: 300;
	}
	#theme-selector button {
		background-color: #e6e6e6;
		border-radius: 10px;
	}
	#theme-selector button::after {
		color: #444;
	}
	#theme-selector button.selected::after {
		color: #000;
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
#theme-tweaker-toggle button:hover {
	color: #666;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 1000px) {
	#theme-tweaker-toggle {
		background-color: var(--GW-body-background-color);
		box-shadow:
			0 0 0 1px #aaa inset;
	}
}
@media only screen and (max-width: 960px) {
	#theme-tweaker-toggle {
		box-shadow: none;
		background-color: transparent;
	}
}

/*=================*/
/* QUICKNAV WIDGET */
/*=================*/

#quick-nav-ui a {
	color: #999;
	background-color: #e4e4e4;
	border-radius: 4px;
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
	color: #bbb;
}
@media only screen and (hover:hover) and (pointer:fine) {
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
@media only screen and (hover:hover) and (pointer:fine) {
	#new-comment-nav-ui .new-comments-count:hover {
		text-shadow: 
			0 0 1px #fff,
			0 0 3px #fff,
			0 0 5px #fff,
			0 0 8px #fff,
			0.5px 0.5px 0 #fff;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button.highlighted {
		color: var(--GW-hyperlink-active-color);
		transform: scale(0.9);
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button {
		transition:
			color 0.1s ease,
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
		background-color: #e4e4e4;
		border: 1px solid #999;
	}
	#new-comment-nav-ui::before {
		color: #777;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button {
		box-shadow: 0 0 0 1px #999;
		color: #538d4d;
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
#text-size-adjustment-ui button:hover {
	color: #222;
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
	margin: 0.25em 0;
	font-size: 1.125rem;
	padding-left: 30px;
	max-height: unset;
	display: block;
}

h1.listing a[href^='/'] {
	color: #538d4d;
	white-space: unset;
	display: inline;
	border-bottom: 2px dotted transparent;
	line-height: 1.3;
}
h1.listing a[href^='/']:hover {
	border-bottom: 2px dotted currentColor;
}
h1.listing a[href^='/']:visited {
	color: #5a5a5b;
}
h1.listing a[href^="http"] {
	min-width: 1.25em;
	line-height: 1;
	left: 1px;
	display: inline-block;
}

@media only screen and (hover:hover) and (pointer:fine) {
	h1.listing a[href^='/posts/']:hover,
	h1.listing a[href^='/posts/']:focus,
	h1.listing a[href^='/s/']:hover,
	h1.listing a[href^='/s/']:focus,
	h1.listing a[href^='/conversation?id']:hover,
	h1.listing a[href^='/conversation?id']:focus {
		text-decoration: none;
		color: #3d3d3e;
	}
	h1.listing:focus-within::before {
		display: none;
	}
	h1.listing:focus-within + .post-meta .karma-value {
		box-shadow: 
			0 0 0 3px #fff,
			0 0 0 6px #3d3d3e;
	}
	h1.listing a[href^="http"]:hover,
	h1.listing a[href^="http"]:focus {
		color: #4879ec;
		text-shadow: 
			 0.5px 0.5px 0 #fff,
			 -0.5px -0.5px 0 #fff,
			 0 0 2px #fff,
			 0 0 3px #00c;
		border: none;
	}
}

/*=----------------------=*/
/*= In-listing edit link =*/
/*=----------------------=*/

h1.listing a.edit-post-link,
h1.listing a.edit-post-link:visited {
	color: #090;
}
h1.listing a.edit-post-link:hover {
	color: #d00;
	border: none;
}

/*=------------------=*/
/*= Post type prefix =*/
/*=------------------=*/

h1.listing .post-type-prefix {
	width: 1.25em;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 520px) {
	h1.listing a[href^='http'] {
		top: 0;
	}
}

/*======*/
/* SPAM */
/*======*/

h1.listing:not(:focus-within).spam {
	opacity: 0.15;
}
h1.listing:not(:focus-within).spam + .post-meta {
	opacity: 0.2;
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
	font-size: 0.8125rem;
	padding-left: 30px;
	justify-content: flex-end;
}

h1.listing + .post-meta .author {
	font-weight: bold;
	color: #6a8a6b;
	text-decoration: none;
	margin: 0 0 0 1.5em;
}
h1.listing + .post-meta .author:hover {
	color: #3d3d3e;
}
h1.listing + .post-meta .date,
h1.listing + .post-meta .read-time {
	color: #999;
	font-style: italic;
}
h1.listing + .post-meta a {
	color: #8a8a8b;
	text-decoration: underline;
}
h1.listing + .post-meta a:hover {
	color: #3d3d3e;
}

h1.listing + .post-meta .karma-value {
	background-color: #538d4d;
	color: #fff;
	font-weight: bold;
	font-size: 0.875rem;
	position: absolute;
	right: calc(100% - 1.625em);
	top: -2rem;
	display: flex;
	align-items: center;
	justify-content: center;
	min-width: 1.875rem;
	height: 1.875rem;
	border-radius: 1rem;
}
h1.listing + .post-meta .karma-value span,
h1.listing + .post-meta .lw2-link span,
h1.listing + .post-meta .comment-count span {
	display: none;
}

h1.listing + .post-meta > * {
	text-align: right;
	margin: 0 0 0 0.5em;
}
h1.listing + .post-meta .read-time {
	width: 6.5em;
	order: 2;
}
h1.listing + .post-meta .comment-count {
	order: -1;
}
h1.listing + .post-meta .date {
	width: 11em;
	order: 1;
}
h1.listing + .post-meta .lw2-link {
	margin: 0 0 0 1em;
	order: 3;
}
h1.listing + .post-meta .post-section::before {
	left: unset;
	right: calc(100% + 1.15em);
	top: -1.8125rem;
}
h1.listing + .post-meta .link-post-domain {
	order: -2;
	margin: 0 1em 0 0;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	h1.listing + .post-meta .post-section {
		order: -2;
		width: unset;
		text-decoration: none;
		margin: 0 1em 0 0;
	}
	h1.listing + .post-meta .post-section::before {
		position: unset;
	}
}
@media only screen and (max-width: 520px) {
	h1.listing {
		padding-left: 40px;
	}
	h1.listing + .post-meta > *:not(.karma) {
		line-height: 1.7;
		width: unset;
		margin: 0 0 0 1em;
	}
	h1.listing + .post-meta .karma-value {
		right: calc(100% - 2.25em);
		top: -1.875rem;
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
	background-color: #f7f7f8;
	border-style: solid;
	border-color: #bbbcbf;
}
#content.user-page h1.listing,
#content.search-results-page h1.listing {
	padding: 0.5em 6px 0 48px;
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
		background-color: #f7f7f8;
	}
}

#content.user-page h1.listing + .post-meta,
#content.search-results-page h1.listing + .post-meta {
	padding: 3px 8px;
	border-width: 0 1px 1px 1px;
}
#content.user-page h1.listing + .post-meta .karma-value {
	right: calc(100% - 2.75em);
	top: -2em;
}
#content.user-page h1.listing + .post-meta .post-section {
	overflow: visible;
	order: -2;
	margin: 0 2em 0 0;
	text-decoration: none;
}
#content.user-page h1.listing + .post-meta .post-section::before {
	position: unset;
}

/*=--------------------=*/
/*= Conversations list =*/
/*=--------------------=*/

#content.conversations-user-page h1.listing {
	padding: 8px 8px 0 8px;
	font-size: 1.25rem;
}
#content.conversations-user-page h1.listing + .post-meta > * {
	margin: 0 0 0 0.75em;
}

/*===============*/
/* CONVERSATIONS */
/*===============*/

#content.conversation-page h1.page-main-heading {
	font-size: 1.375em;
	text-align: left;
}
#content.conversation-page .conversation-participants {
	text-align: left;
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
	font-size: 1.25em;
}
.post-body .contents li::before {
	color: #999;
}
.post-body .contents a {
	text-decoration: none;
}
.post-body .contents a:hover {
	background-color: #e6e6e6;
	box-shadow: 5px 0 0 0 #d8d8d8;
}
.post-body .contents li:hover::before {
	color: #000;
}

/*=================*/
/* POST NAVIGATION */
/*=================*/

.post-nav-item {
	font-size: 0.875em;
	font-weight: bold;
	background-color: #f5f5f5;
	box-shadow: 
		0 -1px 0 0 #d6d5d6;
}
.post-nav-label {
	opacity: 0.75;
}

.post-nav.next,
.sequence-title {
	box-shadow: -9px 0 0 -8px #d6d5d6;
}
.post-nav.prev {
	padding: 0.5em 0.75em 0.5em 0.5em;
}

.post-nav.next {
	padding: 0.5em 0.5em 0.5em 0.75em;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	.sequence-title {
		box-shadow: 0 -9px 0 -8px #d6d5d6;
	}
}

/*==================*/
/* POSTS & COMMENTS */
/*==================*/

.body-text {
	font-size: 1rem;
}
.body-text a {
	text-decoration: underline;
}

/*=======*/
/* POSTS */
/*=======*/

h1.post-title {
	font-size: 1.375rem;
	text-align: left;
	margin: 2em 0 0.5em 0;
	line-height: 1.2;
}

/*===========*/
/* POST-META */
/*===========*/

.bottom-post-meta {
	border-color: #d6d5d6;
}

.post-meta {
	line-height: 1.9;
}
.post .post-meta {
	font-size: 0.875rem;
	justify-content: flex-start;
}
article > .post-meta > *,
.post .post-meta > * {
	margin: 0 0.5em;
}
.post .post-meta a {
	text-decoration: underline;
}

.post .post-meta .karma-value span,
.post .post-meta .lw2-link span,
.post .post-meta .comment-count span {
	display: none;
}

/*	Comment count.
	*/
.post-meta .comment-count::before {
	content: "Comments ("
}
.post-meta .comment-count::after {
	content: ")"
}

/*	Post section.
	*/
.post .post-meta .post-section {
	margin: 0;
}
.post .post-meta .post-section::before {
	position: absolute;
	top: 1px;
	left: -2em;
}
.post .bottom-post-meta .post-section::before {
	top: 22px;
}
.post-meta .post-section::before,
.comment-meta .alignment-forum {
	color: #fff;
	text-shadow: 
		1px 1px 0 #538d4d, 
		0 1px 0 #538d4d, 
		0 0 5px #538d4d;
}
a.post-section:hover::before {
    color: #538d4d;
}
.post-meta .post-section.alignment-forum::before {
	text-shadow:
		1px 1px 0   #626dd7,
		0   1px 0   #626dd7,
		0   0   5px #626dd7;
}
a.post-section.alignment-forum:hover::before {
    color: #626dd7;
}

/*	Author.
	*/
.post .post-meta .author {
	font-weight: bold;
	text-decoration: none;
}

/*	Date.
	*/
.post .post-meta .date {
	color: #999;
	font-style: italic;
}

/*	Karma.
	*/
.post .post-meta .karma {
	order: -1;
	margin-left: 0;
}
.post .post-meta .karma-value {
	display: inline-flex;
	align-items: center;
	justify-content: center;
	min-width: 1.875rem;
	height: 1.875rem;
	border-radius: 1rem;
	font-size: 0.875rem;
	font-weight: bold;
	background-color: #538d4d;
	color: #fff;
	line-height: 1;
	float: left;
	margin: 0 0.5em 0 0;
	position: relative;
	top: -1px;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 720px) {
	.post-meta .comment-count::before {
		font-family: inherit;
		font-size: inherit;
		margin: 0;
	}
}

/*============*/
/* LINK POSTS */
/*============*/

.post.link-post a.link-post-link {
	text-decoration: none;
}
.post.link-post a.link-post-link:focus {
	color: #aaa;
	border-bottom: 2px dotted #aaa;
}

/*==========*/
/* COMMENTS */
/*==========*/

.comments::before {
	border-top: 1px solid #777;
	box-shadow: 0 3px 3px -4px #444 inset;
}

.comment-item {
	border: 1px solid #bbbcbf;
}
.comment-item .comment-item {
	margin: 1em -1px 8px 16px;
}
.comment-item .comment-item + .comment-item {
	margin: 2em -1px 8px 16px;
}

a.comment-parent-link::after {
	display: none;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	.comment-item .comment-item,
	.comment-item .comment-item + .comment-item {
		margin-bottom: 3px;
		margin-left: 6px;
	}
}

/*=========*/
/* ANSWERS */
/*=========*/

.answer-item::after {
	left: -1px;
	text-transform: uppercase;
	color: #c5c6c9;
}

/*==================*/
/* COMMENT LISTINGS */
/*==================*/

.listings .comment-meta a.date:focus::after,
.listings .comment-meta .nav-links-container a:focus::after {
	outline: 2px dotted #444;
	box-shadow:
		0 0 0 3px #fff inset;
}
#content.compact .listings .comment-meta .nav-links-container a:focus::after {
	box-shadow:
		0 0 0 2px #fff inset;
}
.listings .comment-meta a.date:focus {
	color: #444;
	background-color: #fff;
	box-shadow:
		0 0 0 3px #fff;
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
	color: #bbb;
}

/*==============*/
/* COMMENT-META */
/*==============*/

.comment-meta {
	padding-top: 4px;
}
.comment-meta > * {
	font-size: 0.9375rem;
}

.comment-meta .comment-post-title a:focus {
	border-bottom: 1px dotted currentColor;
}

/*	Comment author.
	*/
.comment-meta .author {
	font-weight: bold;
	color: #538d4d;
}
.comment-meta .author:hover,
.comment-meta .author:focus {
	color: #444;
}
.comment-meta .author:focus {
	border-bottom: 2px dotted currentColor;
	line-height: 1;
}
.comment-item .author:not(.redacted).original-poster::after {
	opacity: 0.8;
}

/*	Comment date.
	*/
.comment-meta .date {
	color: #999;
	font-style: italic;
}
.comment-meta .date:hover {
	color: #666;
}

/*	Karma controls.
	*/
.comment-meta .karma-value,
.comment-controls .karma .karma-value {
	color: #666;
	float: left;
	margin-right: 0.5em;
}
.comment-meta .karma-value:only-child {
	float: none;
}

.comment-item .karma.active-controls::after,
.comment-item .karma .karma-value::after,
.post .karma.active-controls::after,
.post .karma .karma-value::after,
.author::before {
	background-color: #fff;
	color: #888;
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
	top: -28px;
	font-weight: normal;
	line-height: 1.5;
}

/*====================*/
/* COMMENT PERMALINKS */
/*====================*/
/*==================*/
/* COMMENT LW LINKS */
/*==================*/

.nav-links-container {
	padding: 1px 0 0 0;
}
.comment-meta .permalink,
.comment-meta .lw2-link,
.comment-meta .comment-parent-link span,
.post .post-meta .lw2-link {
	filter: hue-rotate(270deg);
}
.comment-meta .permalink,
.comment-meta .lw2-link,
.comment-meta .comment-parent-link span {
	top: 3px;
}
.post .post-meta .lw2-link {
	top: 5px;
}

/*====================*/
/* ANTI-KIBITZER MODE */
/*====================*/

.author.redacted,
.inline-author.redacted {
	opacity: 0.8;
	font-weight: 400;
}

.karma-value.redacted {
	opacity: 0.5;
}

.link-post-domain.redacted {
	opacity: 0.5;
}

/*=================================*/
/* COMMENT THREAD MINIMIZE BUTTONS */
/*=================================*/

.comment-minimize-button {
	font-size: 1.25rem;
	font-weight: 300;
	color: #ccc;
	top: 2px;
}
.comment-minimize-button:hover {
	font-weight: 900;
	text-shadow: var(--GW-shadow-white-glow);
	color: #aaa;
}
.comment-minimize-button::after {
	color: #777;
}
.comment-minimize-button.minimized::after {
	left: unset;
	right: calc(100%);
	top: 3px;
}
.comment-minimize-button.maximized::after {
	color: #bbb;
}

/*==============*/
/* VOTE BUTTONS */
/*==============*/

.upvote,
.downvote {
	padding: 0 5px;
	position: relative;
}
.vote::before {
	position: relative;
}
.upvote::before {
	content: '\F164';
}
.downvote::before {
	content: '\F165';
	top: 2px;
}

.vote {
	text-shadow: 
		 1px  1px 0 #aaa,
		-1px  1px 0 #aaa,
		 1px -1px 0 #aaa,
		-1px -1px 0 #aaa;
}
.upvote.selected,
.upvote:hover {
	text-shadow: 
		 1px  1px 0 #080,
		-1px  1px 0 #080,
		 1px -1px 0 #080,
		-1px -1px 0 #080;
}

.downvote.selected,
.downvote:hover {
	text-shadow: 
		 1px  1px 0 #b00,
		-1px  1px 0 #a00,
		 1px -1px 0 #b00,
		-1px -1px 0 #b00;
}
@media only screen and (min-resolution: 192dpi) {
	.vote {
		text-shadow: 
			 0.5px  0.5px 0 #aaa,
			-0.5px  0.5px 0 #aaa,
			 0.5px -0.5px 0 #aaa,
			-0.5px -0.5px 0 #aaa;
	}
	.upvote.selected,
	.upvote:hover {
		text-shadow: 
			 0.5px  0.5px 0 #060,
			-0.5px  0.5px 0 #060,
			 0.5px -0.5px 0 #060,
			-0.5px -0.5px 0 #060;
	}

	.downvote.selected,
	.downvote:hover {
		text-shadow: 
			 0.5px  0.5px 0 #900,
			-0.5px  0.5px 0 #900,
			 0.5px -0.5px 0 #900,
			-0.5px -0.5px 0 #900;
	}
}

.vote::before {
	z-index: 1;
}

.vote::after {
	position: absolute;
	color: #fff;
	visibility: hidden;
}
.vote:hover::after,
.vote.big-vote::after,
.vote:not(.big-vote).clicked-twice::after {
	visibility: visible;
}
.vote.big-vote::after,
.vote:not(.big-vote).clicked-twice::after {
	color: inherit;
}
.karma:not(.waiting) .vote.clicked-once::after {
	color: #c8c8c8;	
}

.upvote::after {
	content: "\F164";
	left: -1px;
	top: -3px;
}

.downvote::after {
	content: "\F165";
	left: -1px;
	top: 5px;
}

.comment-controls .karma {
	margin-left: -6px;
}

/*===========================*/
/* COMMENTING AND POSTING UI */
/*===========================*/

/*=------------------=*/
/*= Edit post button =*/
/*=------------------=*/

.post-controls .edit-post-link,
.post-controls .edit-post-link:visited {
	font-size: 1.25em;
	font-weight: bold;
	color: #090;
}
.post-controls .edit-post-link:hover {
	color: #d00;
}
.post-controls .edit-post-link::before {
	top: -2px;
	left: -1px;
}

/*=-----------------------------------------------=*/
/*= Submit [post/comment] & Cancel [post] buttons =*/
/*=-----------------------------------------------=*/

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
	max-width: 1.2em;
	overflow: hidden;
	margin-right: 0.375em;
}
.comment-controls .cancel-comment-button::before,
.posting-controls .cancel-post-editing-button::before {
	font-size: 1.25em;
}
.comment-controls .cancel-comment-button:hover,
.posting-controls .cancel-post-editing-button:hover {
	color: #f00;
}

/*=----------------=*/
/*= Action buttons =*/
/*=----------------=*/

.comment-controls .action-button::before {
	font-size: 1em;
}
.comment-controls .action-button::after {
	content: attr(data-label);
	display: block;
	position: absolute;
	font-size: 0.5em;
	text-transform: uppercase;
	visibility: hidden;
	text-shadow: none;
}
.comment-controls .action-button:hover::after {
	visibility: visible;
}
.comment-controls .delete-button::before,
.comment-controls .retract-button::before {
	color: #ca3232;
}
.comment-controls .delete-button::after {
	transform: translateX(-8px);
}
.comment-controls .retract-button::after {
	transform: translateX(-8px);
}
.comment-controls .unretract-button::after {
	transform: translateX(-18px);
}
.comment-controls .reply-button::before {
	content: "\F086";
	font-weight: 400;
	font-size: 1.125rem;
	color: #6a8a6b;
}
.comment-controls .reply-button::after {
	transform: translateX(-4px);
}
.comment-controls .edit-button::before {
	font-size: 0.9375em;
	color: #0b0;
}
.comment-controls .unretract-button::before {
	font-size: 1.125rem;
	color: #0b0;
}
.comment-controls .edit-button::after {
	transform: translateX(-1px);
}
.comment-item .comment-controls .action-button:hover::before {
	color: #f00;
	text-shadow:
		0.5px 0.5px 0.5px #faa;
}

/*=----------=*/
/*= Textarea =*/
/*=----------=*/

.posting-controls textarea {
	font-size: 1rem;
	color: #000;
	background-color: #fff;
	border-color: #aaa;
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

/*=-------------=*/
/*= Scroll bars =*/
/*=-------------=*/

.posting-controls textarea::-webkit-scrollbar {
	width: 12px;
	background-color: transparent;
}
.posting-controls textarea::-webkit-scrollbar-track {
	background-color: #eee;
	border-left: 1px solid #aaa;
	border-top: 1px solid #eee;
}
.posting-controls textarea:focus::-webkit-scrollbar-track {
	border-left: 1px solid var(--GW-hyperlink-color);
	border-top: 1px solid #ddf;
}
.posting-controls textarea::-webkit-scrollbar-thumb {
	background-color: #aaa;
	box-shadow: 0 0 0 1px #eee inset;
	border-left: 1px solid #aaa;
}
.posting-controls textarea:focus::-webkit-scrollbar-thumb {
	border-left: 1px solid var(--GW-hyperlink-color);
	background-color: #abbdab;
	box-shadow: 
		0 1px 0 0 #ddf inset,
		0 0 0 1px #eee inset;
}
.posting-controls textarea:focus::-webkit-scrollbar-thumb:hover {
	background-color: #8da28e;
}
.posting-controls textarea:focus::-webkit-scrollbar-thumb:active {
	background-color: #7e977f;
}

/*=-----------------=*/
/*= GUIEdit buttons =*/
/*=-----------------=*/

button.guiedit {
	color: #070;
}
button.guiedit:hover {
	color: #c00;
	text-shadow: var(--GW-shadow-white-glow);
}

.guiedit-buttons-container {
	background-image: linear-gradient(to bottom, #fff 0%, #d7e3d3 50%, #c5d7bf 75%, #99b990 100%);
}

.guiedit::after {
	color: #777;
	text-shadow: none;
	top: 2px;
}

/*=----------------=*/
/*= Markdown hints =*/
/*=----------------=*/

#markdown-hints {
	border: 1px solid #c00;
	background-color: #ffa;
	line-height: 1.7;
}
#markdown-hints .markdown-hints-row code {
	line-height: 1;
	padding-top: 8px;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 520px) {
	.textarea-container:focus-within textarea {
		background-color: #fff;
	}
	.textarea-container:focus-within .guiedit-mobile-auxiliary-button {
		padding: 6px;
		font-weight: bold;
	}
	.textarea-container:focus-within .guiedit-mobile-help-button.active {
		box-shadow:
			0 0 0 2px #c00;
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
	.textarea-container:focus-within button.guiedit {
		border: 1px solid #6a8a6b;
		border-radius: 6px;
	}
}

/*================*/
/* EDIT POST FORM */
/*================*/

#edit-post-form .post-meta-fields input[type='checkbox'] + label::before {
	border-radius: 3px;
	border: 1px solid #ddd;
	color: #777;
	top: 2px;
}
@media only screen and (hover:hover) and (pointer:fine) {
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
#edit-post-form #markdown-hints-checkbox + label {
	padding: 3px 0 0 14px;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 520px) {
	#edit-post-form .post-meta-fields input[type='checkbox'] + label {
		top: 1px;
	}
	#edit-post-form .post-meta-fields input[type='checkbox'] + label::before {
		top: 1px;
	}
	
	#edit-post-form textarea {
		min-height: calc(100vh - 345px);
	}
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
	font-weight: bold;
}

button:active,
input[type='submit']:active,
.button:active {
	transform: scale(0.9);
}
.button:focus:not(:hover) {
	transform: none;
}

@media only screen and (hover: hover) and (pointer: fine) {
	.button:hover {
		text-decoration: none;
	}
}

/*==========*/
/* HEADINGS */
/*==========*/

.body-text h3,
.body-text h5,
.body-text h6 {
	font-weight: 600;
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
	border: 1px solid #bbb;
}

/*======*/
/* MISC */
/*======*/

hr {
	border-bottom: 1px solid #999;
}

/*=------=*/
/*= Code =*/
/*=------=*/

code {
	background-color: #eff6ed;
	border: 1px solid #c5debd;
	border-radius: 4px;
	padding: 4px 4px 2px 5px;
}

pre > code {
	box-shadow: 
		0 0 4px 0 #aca inset;
	padding: 8px 6px 6px 12px;
}

/*=---------------=*/
/*= Form elements =*/
/*=---------------=*/

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
}

/*======*/
/* MATH */
/*======*/

.mathjax-block-container .mjx-chtml {
	margin: 0;
	padding: 0;
	overflow: visible;
	display: inline;
}
.mathjax-block-container {
	border: 1px solid #7e977f;
	display: block;
	margin: 2em 0 1.75em 0;
	padding: 1em;
	overflow-x: scroll;
	box-shadow: 
		0 0 4px 0 #aca inset;
}

.mathjax-block-container::-webkit-scrollbar {
	height: 12px;
}
.mathjax-block-container::-webkit-scrollbar:disabled {
	height: 0;
}
.mathjax-block-container::-webkit-scrollbar-track {
	background-color: #eff6ed;
	border-top: 1px solid #7e977f;
}
.mathjax-block-container::-webkit-scrollbar-thumb {
	background-color: #abbdab;
	border-top: 1px solid #7e977f;
	box-shadow: 0 0 0 1px #eff6ed inset;
}
.mathjax-block-container::-webkit-scrollbar-thumb:hover {
	background-color: #8da28e;
}
.mathjax-block-container::-webkit-scrollbar-thumb:active {
	background-color: #7e977f;
}

.mathjax-inline-container .mjx-chtml::-webkit-scrollbar {
	height: 8px;
	background-color: #eff6ed;
	border-radius: 4px;
	border: 1px solid #cee7c5;
}
.mathjax-inline-container .mjx-chtml::-webkit-scrollbar-thumb {
	background-color: #d4e0d0;
	border-radius: 4px;
	border: 1px solid #c5debd;
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
	border-left: 1px solid #abbdab;
}
#keyboard-help-overlay .keyboard-help-container::-webkit-scrollbar-thumb {
	background-color: #abbdab;
	box-shadow:
		1px 0 0 0 #fff inset,
		-1px 0 0 0 #fff inset,
		0 1px 0 0 #fff inset,
		0 -1px 0 0 #fff inset;
	border-left: 1px solid #abbdab;
}
#keyboard-help-overlay .keyboard-help-container::-webkit-scrollbar-thumb:hover {
	background-color: #8da28e;
}
#keyboard-help-overlay .keyboard-help-container::-webkit-scrollbar-thumb:active {
	background-color: #7e977f;
}

/*=--------------------=*/
/*= Dividers & heading =*/
/*=--------------------=*/

#keyboard-help-overlay .keyboard-help-container h1,
#keyboard-help-overlay .keyboard-help-container .keyboard-shortcuts-lists {
	border-color: #abbdab;
}

/*=------=*/
/*= Keys =*/
/*=------=*/

#keyboard-help-overlay .keyboard-help-container code {
	background-color: #eee;
	border: 1px solid #ccc;
	padding: 5px 8px 3px 8px;
}
#keyboard-help-overlay .keyboard-help-container code.ak {
	background-color: #ffeb83;
	border-color: #d4a500;
}

/*=--------------=*/
/*= Close button =*/
/*=--------------=*/

#keyboard-help-overlay button.close-keyboard-help {
	font-size: 1.375em;
}
#keyboard-help-overlay button.close-keyboard-help:hover {
	background-color: #eee;
	box-shadow:
		1px 0px 0 0 #ddd inset;
}
#keyboard-help-overlay button.close-keyboard-help:active {
	box-shadow:
		0 0 0 1px #ddd;
}


/*=================*/
/* ALIGNMENT FORUM */
/*=================*/

#content.alignment-forum-index-page::before {
	background-color: #f4f5ff;
}
#content.alignment-forum-index-page::after {
	background-color: #7f85b2;
	color: transparent;
	-webkit-background-clip: text;
	text-shadow: 
		rgba(255, 255, 255, 0.5) 0px 3px 3px;
}
@media only screen and (hover:hover) and (pointer:fine) {
	#content.alignment-forum-index-page h1.listing a:hover,
	#content.alignment-forum-index-page h1.listing a:focus {
		background-color: rgba(244, 245, 255, 0.85);
	}	
}
