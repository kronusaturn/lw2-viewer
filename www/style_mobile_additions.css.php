/*****************/
/* MOBILE LAYOUT */
/*****************/

/* Hide the mobile elements on desktop screens: */

@media not screen and (hover: none), not screen and (-moz-touch-enabled) {
	#post-nav-ui-toggle,
	#appearance-adjust-ui-toggle,
	#theme-selector .theme-selector-close-button {
		display: none;
	}
}

@media only screen and (hover: none), only screen and (-moz-touch-enabled) {

/*====================*/
/* MOBILE UI ELEMENTS */
/*====================*/

#ui-elements-container {
	height: unset;
	position: unset;
}
#ui-elements-container > * {
	position: fixed;
	visibility: hidden;
	opacity: 1.0 !important;
	z-index: 10000;
}

#appearance-adjust-ui-toggle,
#post-nav-ui-toggle {
	visibility: visible;
	display: inline-block;
	border-radius: 50%;
	z-index: 1;
}
#appearance-adjust-ui-toggle button,
#post-nav-ui-toggle button,
#theme-selector .theme-selector-close-button {
	font-family: Font Awesome;
	font-weight: 900;
	font-size: 32px;
	padding: 10px;
	opacity: 0.8;
	-webkit-tap-highlight-color: transparent;
	transition: transform 0.2s ease;
}
#appearance-adjust-ui-toggle button::selection,
#post-nav-ui-toggle button::selection,
#theme-selector .theme-selector-close-button::selection {
	background-color: transparent;
}
#appearance-adjust-ui-toggle button::-moz-focus-inner,
#post-nav-ui-toggle button::-moz-focus-inner,
#theme-selector .theme-selector-close-button::-moz-focus-inner {
	border: none;
}
#appearance-adjust-ui-toggle button.engaged,
#post-nav-ui-toggle button.engaged {
	transform: rotate(-90deg);
	opacity: 1.0;
}

#appearance-adjust-ui-toggle {
	bottom: 10px;
	left: 10px;
}

#post-nav-ui-toggle {
	bottom: 10px;
	right: 10px;
}
	
#theme-selector.engaged,
#quick-nav-ui.engaged,
#new-comment-nav-ui.engaged,
#hns-date-picker.engaged {
	visibility: visible;
}
	
/*=========*/
/* GENERAL */
/*=========*/

@media only screen and (max-width: 900px) {
	#content,
	#images-overlay,
	#ui-elements-container {
		min-width: unset;
		width: unset;
	}
	#content {
		padding: 0 4px;
	}
}

/*================*/
/* THEME SELECTOR */
/*================*/

#theme-selector {
	display: flex;
	flex-flow: column;
	width: calc(100vw - 20px);
	max-width: 360px;
	padding: 0 0 3px 0;
	overflow: hidden;
	max-height: 0;
	transition: 
		top 0.2s ease,
		max-height 0.2s ease,
		visibility 0.2s ease;
	top: calc(100% - 20px);
	left: 0;
	right: 0;
	margin: auto;
}
#theme-selector.engaged {
	max-height: 1000px;
	top: 10px;
    z-index: 10001;
}
#theme-selector::before {
	content: "Select theme";
	white-space: nowrap;
	display: block;
	font-weight: 600;
	font-size: 2rem;
	margin: 0.375em 1em 0.5em 1em;
	text-align: center;
}
#theme-selector button {
	width: calc(100% - 0.5em);
	background-repeat: no-repeat;
	padding: 1em 0.875em;
	margin: 1px 4px;
	line-height: 1;
	height: unset;
	position: relative;
}
#theme-selector button::after {
	content: attr(data-theme-description);
	white-space: nowrap;
	position: absolute;
	text-align: left;
	left: 2.5em;
	top: 1em;
}
#theme-selector .theme-selector-close-button {
	position: absolute;
	width: unset;
	background-color: transparent;
	top: 0;
	right: -3px;
}
#theme-selector .theme-selector-close-button,
#theme-selector .theme-selector-close-button:focus,
#theme-selector .theme-selector-close-button:active,
#theme-selector .theme-selector-close-button:hover {
	box-shadow: none;
}
	
/*=================*/
/* QUICKNAV WIDGET */
/*=================*/

#quick-nav-ui {
	max-width: 0px;
	transition:
		max-width 0.2s ease,
		visibility 0.2s ease;
	display: flex;
	right: 72px;
	bottom: 14px;
}
#quick-nav-ui.engaged {
	max-width: 1000px;
}
#quick-nav-ui a {
	position: relative;
	margin: 2px;
}
#quick-nav-ui a + a {
	margin-left: 5px;
}
#quick-nav-ui a::after {
	position: absolute;
	top: 100%;
	font-size: 0.375rem;
	left: 0;
	right: 0;
	margin: auto;
	line-height: 1;
	padding: 4px 0 2px 0;
	text-transform: uppercase;
	z-index: -1;
}
#quick-nav-ui a[href='#top']::after {
	content: "Top";
	left: -1px;
}
#quick-nav-ui a[href='#comments']::after {
	content: "Comments";
}
#quick-nav-ui a[href='#bottom-bar']::after {
	content: "Bottom";
}
	
/*======================*/
/* NEW COMMENT QUICKNAV */
/*======================*/

#new-comment-nav-ui {
	max-width: 0px;
	max-height: 0px;
	transition: 
		max-width 0.2s ease,
		max-height 0.2s ease,
		visibility 0.2s ease;
	display: flex;
	right: 78px;
	bottom: 70px;
}
#new-comment-nav-ui::before {
	content: "New Comments";
	position: absolute;
	bottom: 100%;
	font-size: 0.5625rem;
	left: 0;
	right: 0;
	margin: auto;
	padding: 2px 0;
	text-transform: uppercase;
	z-index: -1;
	text-align: center;
}
#new-comment-nav-ui.engaged {
	max-width: 1000px;
	max-height: 1000px;
}
#new-comment-nav-ui .new-comment-sequential-nav-button {
	top: unset;
	bottom: unset;
	padding: 2px 7px;
}
#new-comment-nav-ui .new-comment-sequential-nav-button.new-comment-previous {
	padding: 2px 7px 3px 7px;
}
#new-comment-nav-ui .new-comments-count {
	padding: 4px 0 5px 0;
}
#new-comment-nav-ui .new-comments-count::before {
	display: none;
}
#new-comment-nav-ui button::after {
	position: absolute;
	font-size: 0.375rem;
	left: 0;
	right: 0;
	margin: auto;
	line-height: 1;
	text-transform: uppercase;
	pointer-events: none;
}
#new-comment-nav-ui button.new-comment-previous::after {
	content: "Previous";
	bottom: 5px;
}
#new-comment-nav-ui button.new-comment-next::after {
	content: "Next";
	top: 7px;
}

/*=================*/
/* HNS DATE PICKER */
/*=================*/

#hns-date-picker {
	max-height: 0px;
	bottom: 124px;
	right: 62px;
	transition:
		max-height 0.2s ease,
		visibility 0.2s ease;
}
#hns-date-picker.engaged {
	max-height: 1000px;
}
#hns-date-picker::before {
	width: calc(100% + 2px);
	border-width: 1px !important;
}

/*=========*/
/* NAV BAR */
/*=========*/

#bottom-bar { 
	padding: 0 4.5rem;
}
#bottom-bar .nav-item {
	box-shadow: none;
	position: relative;
}
#bottom-bar .nav-inner {
	font-size: 2rem;
	padding: 1rem 0 1.25rem 0;
	visibility: hidden;
	position: static;
	width: 0;
}
#content #bottom-bar .nav-item .nav-inner::before {
	margin: 0;
	visibility: visible;
	position: absolute;
	width: 100%;
	height: 100%;
	left: 0;
	top: 0;
	padding: 1rem 0;
}
#bottom-bar .nav-inner::after {
	display: block;
	visibility: visible;
	text-transform: uppercase;
	color: unset;
	font-size: 0.75rem;
	top: unset;
	left: 0;
	bottom: 1rem;
	width: 100%;
}
#bottom-bar #nav-item-first .nav-inner::after {
	content: "First Page";
}
#bottom-bar #nav-item-prev .nav-inner::after {
	content: "Prev. Page";
}
#bottom-bar #nav-item-top .nav-inner::after {
	content: "Top";
}
#bottom-bar #nav-item-next .nav-inner::after {
	content: "Next Page";
}
#bottom-bar #nav-item-last .nav-inner::after {
	content: "Last Page";
}

@media only screen and (max-width: 900px) {
	#primary-bar,
	#secondary-bar {
		font-size: 0.75rem;
	}
	.nav-bar {
		width: calc(100% + 8px);
		margin: 0 -4px;
	}
	.nav-bar .nav-inner::after {
		display: none;
	}

	#primary-bar .nav-item {
		flex: 1 1 100%;
	}
	#secondary-bar .nav-item:not(#nav-item-search) {
		flex: 1 1 60px;
	}
	#primary-bar .nav-inner,
	#secondary-bar .nav-inner {
		text-transform: uppercase;
		padding: 6px;
	}
	#primary-bar .nav-inner::before, 
	#secondary-bar .nav-inner::before {
		display: block;
		font-family: "Font Awesome";
		font-size: 2em;
		font-weight: 900;
	}

	#nav-item-home .nav-inner::before {
		content: "\F015";
	}
	#nav-item-featured .nav-inner::before {
		content: "\F005";
	}
	#nav-item-all .nav-inner::before {
		content: "\F069";
	}
	#nav-item-meta .nav-inner::before {
		content: "\F077";
	}
	#nav-item-recent-comments > * > span {
		display: none;
	}
	#nav-item-recent-comments .nav-inner::before {
		content: "\F036";
	}
	#nav-item-archive .nav-inner::before {
		content: "\F187";
	}
	#nav-item-about .nav-inner::before {
		content: "\F129";
	}
	#nav-item-search {
		font-size: 2em;
		padding: 10px;
	}
	#nav-item-search .nav-inner::before {
		content: none;
	}
	#nav-item-search .nav-inner {
		height: 100%;
		display: flex;
	}
	#nav-item-search input {
		width: 100%;
		height: 100%;
	}
	#nav-item-search button {
		height: 100%;
		padding: 5px 5px 5px 10px;
		width: 40px;
		overflow: visible;
		visibility: hidden;
	}
	#nav-item-search button::before {
		content: "\F002";
		font-family: Font Awesome;
		font-weight: 900;
		visibility: visible;
	}
	#nav-item-login {
		padding: 0;
	}
	#nav-item-login .nav-inner::before {
		content: "\F007";
	}
}
@media only screen and (max-width: 520px) {
	#primary-bar,
	#secondary-bar {
		font-size: 0.5rem;
	}

	#nav-item-search .nav-inner {
		padding: 0;
	}
	#nav-item-search button {
		width: 31px;
	}

	#bottom-bar #nav-item-first .nav-inner::after {
		content: "First";
	}
	#bottom-bar #nav-item-prev .nav-inner::after {
		content: "Prev";
	}
	#bottom-bar #nav-item-next .nav-inner::after {
		content: "Next";
	}
	#bottom-bar #nav-item-last .nav-inner::after {
		content: "Last";
	}
}

/*=================*/
/* INBOX INDICATOR */
/*=================*/

@media only screen and (max-width: 900px) {
	#inbox-indicator {
		width: 100%;
		top: 0;
		pointer-events: none;
	}
	#inbox-indicator::before {
		width: 100%;
		font-size: 1rem;
		text-align: right;
		padding: 1px 6px;
	}
	#inbox-indicator.new-messages {
		pointer-events: auto;
	}
	#inbox-indicator.new-messages::before {
		box-shadow: 0 0 8px 1px #f00 inset;
	}
}
@media only screen and (max-width: 520px) {
	#inbox-indicator::before {
		font-size: 0.75rem;
		padding: 2px 5px;
	}
}
@media only screen and (max-width: 374px) {
	#inbox-indicator::before {
		font-size: 0.625rem;
	}
}

/*===================*/
/* TOP PAGINATION UI */
/*===================*/

#top-nav-bar {
	font-size: 1.75rem;
}

/*==============*/
/* PAGE TOOLBAR */
/*==============*/

@media only screen and (max-width: 900px) {
	.page-toolbar {
		font-size: 1rem;
	}
}
@media only screen and (max-width: 520px) {
	#content:not(.user-page) .page-toolbar {
		display: flex;
		flex-direction: column-reverse;
		text-align: right;
	}
}

/*==============*/
/* SUBLEVEL NAV */
/*==============*/

@media only screen and (max-width: 720px) {
	.sublevel-nav {
		flex-wrap: wrap;
		max-width: calc(100% - 200px);
		margin: -1em auto 0 auto;
	}
	.sublevel-nav .sublevel-item {
		margin: 1px;
		flex-basis: 7em;
	}
}
@media only screen and (max-width: 520px) {
	.sublevel-nav {
		max-width: calc(100% - 100px);
	}
	#content .sublevel-nav:not(.sort) .sublevel-item {
		font-size: 1rem;
	}
}

/*=====================*/
/* SORT ORDER SELECTOR */
/*=====================*/

@media only screen and (max-width: 900px) {
	.sublevel-nav.sort {
		flex-flow: row;
	}
}
@media only screen and (max-width: 720px) {
	.sublevel-nav.sort .sublevel-item {
		margin: 0;
	}
}
@media only screen and (max-width: 520px) {
	.sublevel-nav.sort {
		flex-flow: column;
		right: 4px;
	}
}

/*==========*/
/* ARCHIVES */
/*==========*/

@media only screen and (max-width: 900px) {
	.archive-nav {
		margin: 1.25em 0.5em;
	}
	div[class^='archive-nav-'] {
		flex-wrap: wrap;
		justify-content: flex-start;
	}
	.archive-nav *[class^='archive-nav-item'],
	.archive-nav *[class^='archive-nav-item']:first-child {
		padding: 10px;
		margin: 2px;
		max-width: unset;
		flex: 0 1 calc((100% / 8) - 4px);
	}
	.archive-nav .archive-nav-item-day,
	.archive-nav .archive-nav-item-day:first-child {
		flex-basis: calc((100% / 16) - 4px);
	}
	.archive-nav > *[class^='archive-nav-'] + *[class^='archive-nav-'] {
		margin-top: 8px;
		position: relative;
	}
	.archive-nav > *[class^='archive-nav-'] + *[class^='archive-nav-']::before {
		content: "";
		display: block;
		position: absolute;
		height: 1px;
		width: calc(100% + 8px);
		left: -4px;
		top: -4px;
	}
}
@media only screen and (max-width: 720px) {
	.archive-nav .archive-nav-item-day,
	.archive-nav .archive-nav-item-day:first-child {
		flex-basis: calc((100% / 12) - 4px);
	}
}
@media only screen and (max-width: 520px) {
	.archive-nav *[class^='archive-nav-item'],
	.archive-nav *[class^='archive-nav-item']:first-child {
		flex-basis: calc((100% / 5) - 4px);
	}
	.archive-nav .archive-nav-item-day,
	.archive-nav .archive-nav-item-day:first-child {
		flex-basis: calc((100% / 8) - 4px);
	}
}

/*==========*/
/* LISTINGS */
/*==========*/

h1.listing {
	max-height: unset;
}

/*============*/
/* USER PAGES */
/*============*/

#content.user-page h1.page-main-heading {
	margin: 1em 0.125em 0.75em 0.125em;
}
#content.user-page .user-stats {
	margin-right: 0.25em;
}

/*============*/
/* LOGIN PAGE */
/*============*/

@media only screen and (max-width: 640px) {
	.login-container {
		flex-flow: column;
		margin: 0 auto 3em auto;
		max-width: 400px;
	}
	.login-container #login-form,
	.login-container #signup-form {
		width: unset;
		margin: 0;
	}
	.login-container #signup-form {
		margin-top: 2em;
	}
}

/*==================*/
/* POSTS & COMMENTS */
/*==================*/

/*===========*/
/* POST-META */
/*===========*/

.post-meta {
	line-height: 1.9;
}
@media only screen and (max-width: 720px) {
	.post-meta .lw2-link span,
	.post-meta .karma-value span,
	.post-meta .comment-count span {
		display: none;
	}
	.post-meta .comment-count::before {
		content: "\F086";
		font-family: Font Awesome;
		font-size: 0.875em;
		margin: 0 0.25em 0 0;
		font-weight: 400;
	}
}

/*=======*/
/* POSTS */
/*=======*/

@media only screen and (max-width: 900px) {
	.post-body,
	.post > h1:first-child {
		padding: 0 6px;
	}
}
@media only screen and (max-width: 520px) {
	.post-body {
		font-size: 1.2rem;
		line-height: 1.45;
	}
	.post > h1:first-child {
		font-size: 2em;
	}
}

/*==========*/
/* COMMENTS */
/*==========*/

@media only screen and (max-width: 900px) {
	.comment-body ul {
		padding-left: 30px;
	}
	.comment-body ol {
		padding-left: 30px;
	}
}

/*==============*/
/* COMMENT-META */
/*==============*/

a.comment-parent-link::after {
	display: none;
}
@media only screen and (max-width: 900px) {
	.comment-meta {
		padding: 2px 40px 2px 10px;
	}
}
@media only screen and (max-width: 720px) {
	.comment-meta .karma-value span {
		display: none;
	}
	.comment-meta .comment-parent-link {
		opacity: 1.0;
	}
}
@media only screen and (max-width: 520px) {
	.comment-meta {
		padding: 2px 10px;
		position: relative;
	}
	.comment-meta .author {
		flex-basis: 100%;
	}
	.comment-post-title2 {
		display: block;
		text-overflow: ellipsis;
		overflow: hidden;
	}
	.comment-meta .lw2-link {
		display: none;
	}
}

/*=======================*/
/* COMMENTS COMPACT VIEW */
/*=======================*/

#comments-list-mode-selector {
	top: 185px;
	left: 12px;
}
#content.user-page #comments-list-mode-selector {
	top: 316px;
}
@media only screen and (max-width: 900px) {
	#content.search-results-page #comments-list-mode-selector {
		top: 130px;
		right: 24px;
	}
}
@media only screen and (max-width: 720px) {
	#content.user-page #comments-list-mode-selector {
		top: 268px;
	}
	#content.search-results-page #comments-list-mode-selector {
		top: 111px;
		right: 103px;
	}
	#content.search-results-page #comments-list-mode-selector::after {
		content: "";
		position: absolute;
		display: block;
		left: -8px;
		right: -8px;
		top: -8px;
		bottom: -8px;
		z-index: -1;
		background-color: inherit;
	}
}
@media only screen and (max-width: 520px) {
	#comments-list-mode-selector {
		top: 123px;
	}
	#content.user-page #comments-list-mode-selector {
		top: 203px;
	}
	#comments-list-mode-selector button {
		display: block;
	}
	#content.search-results-page #comments-list-mode-selector {
		top: 72px;
		right: 16px;
	}
	#content.search-results-page #comments-list-mode-selector button {
		display: inline-block;
	}
}

/*===========================*/
/* COMMENT THREAD NAVIGATION */
/*===========================*/

@media only screen and (max-width: 900px) {
	a.comment-parent-link {
		width: 0;
		visibility: hidden;
		position: relative;
	}
	a.comment-parent-link::before {
		padding: 0;
		font-size: 1em;
		left: 0;
		top: 0;
		line-height: inherit;
		visibility: visible;
		content: "\F3BF";
		transform: scaleX(-1);
		width: 2em;
		text-align: center;
	}
}
@media only screen and (max-width: 520px) {
	a.comment-parent-link {
		position: static;
	}
	a.comment-parent-link::before {
		padding: 6px;
		left: unset;
		right: 0;
		top: unset;
		bottom: 0;
		height: 2em;
	}
}

/*=================================*/
/* COMMENT THREAD MINIMIZE BUTTONS */
/*=================================*/

@media only screen and (max-width: 520px) {
	.comment-minimize-button{
		right: 2px;
	}
}

/*===========================*/
/* COMMENTING AND POSTING UI */
/*===========================*/

@media only screen and (max-width: 900px) {
	.comment-controls .edit-button,
	.comment-controls .cancel-comment-button {
		max-width: 1.3em;
		overflow: hidden;
		margin-right: 0.125em;
	}
	.comment-controls .edit-button {
		color: #0b0;
	}
	.comment-controls .edit-button::before {
		font-size: 1rem;
	}
	#comments > .comment-controls .cancel-comment-button {
		right: 8px;	
	}
	.comment-controls .cancel-comment-button {
		right: 10px;
	}
	.comment-controls .cancel-comment-button::before {
		font-size: 1.25rem;		
	}
}
@media only screen and (max-width: 520px) {
	.comment-controls form:focus-within textarea,
	.textarea-container:focus-within textarea {
		position: fixed;
		top: 0;
		left: 2px;
		width: calc(100vw - 4px);
		height: calc(100% - 100px);
		min-height: unset;
		max-height: unset;
		border-width: 1px;
		z-index: 11001;
	}
	.comment-controls form:focus-within .guiedit-buttons-container,
	.textarea-container:focus-within .guiedit-buttons-container {
		position: fixed;
		z-index: 11002;
		left: 0;
		width: 100vw;
		height: auto;
		background-image: none;
		padding: 3px 4px 4px 4px;
		margin: 0;
		text-align: center;
		top: auto;
		bottom: 0;
	}
	.comment-controls form:focus-within button.guiedit,
	.textarea-container:focus-within button.guiedit {
		font-size: 0.9375rem;
		line-height: 1.5;
		height: auto;
		width: calc((100% / 10) - 2px);
		padding: 10px 1px 8px 0;
		position: relative;
		margin: 1px;
	}
	.comment-controls form:focus-within .guiedit-mobile-auxiliary-button,
	.textarea-container:focus-within .guiedit-mobile-auxiliary-button {
		z-index: 11011;
		position: fixed;
		bottom: 7px;
		width: calc(((100% - 16px) / 10) * 3 - 7px);
		font-size: 1.25rem;
		padding: 5px 5px 6px 5px;
		display: block;
	}
	.comment-controls form:focus-within button.guiedit sup,
	.textarea-container:focus-within button.guiedit sup {
		position: absolute;
		left: calc(50% + 0.65em);
		top: calc(50% - 1.3em);
	}
	.comment-controls form:focus-within .guiedit-mobile-help-button,
	.textarea-container:focus-within .guiedit-mobile-help-button {
		left: 8px;
	}
	.comment-controls form:focus-within .guiedit-mobile-exit-button,
	.textarea-container:focus-within .guiedit-mobile-exit-button {
		right: 8px;
	}
	.guiedit::after {
		display: none;
	}

	.markdown-hints,
	#edit-post-form .markdown-hints {
		z-index: 11111;
		position: fixed;
		top: 40px;
		left: 0;
		right: 0;
		margin: auto;
		padding: 4px 0 4px 8px;
		width: 310px;
		border-width: 3px;
		border-style: double;
		pointer-events: none;
	}
	.markdown-hints::after {
		content: "(Type to hide this help box.)";
		color: #090;
		display: block;
		margin: 12px 18px 13px 10px;
		padding: 5px;
		font-size: 0.9em;
		text-align: center;
	}
}

/*================*/
/* EDIT POST FORM */
/*================*/

@media only screen and (max-width: 520px) {
	#edit-post-form {
		padding-bottom: 0;
	}
	#edit-post-form label[for='title'] {
		width: 2.5em;
	}
	#edit-post-form label[for='url'] {
		width: 2.5em;
	}
	#edit-post-form label[for='section'] {
		width: 4.1em;
	}
	#edit-post-form label[for='url'], 
	#edit-post-form label[for='section'],
	#edit-post-form label[for='title'] {
		text-align: left;
		padding-left: 0;
	}
	#edit-post-form input[name='title'],
	#edit-post-form input[name='url'] {
		max-width: calc(100% - 6.5em);
	}
	#edit-post-form label[for='link-post'] {
		white-space: normal;
		line-height: 0.9;
		width: 2em;
		height: 1em;
		top: -1px;
	}
	#edit-post-form .textarea-container:focus-within textarea {
		top: -1px;
		height: calc(100% - 101px);
		min-height: unset;
	}

	#markdown-hints-checkbox,
	#markdown-hints-checkbox + label {
		display: none;
	}

	#edit-post-form div:last-child {
		clear: both;
		overflow: auto;
	}
	#edit-post-form input[type='submit'] {
		float: none;
		display: block;
		font-size: 1.5rem;
		margin: 1rem auto 1.5rem auto;
		padding: 6px 12px 8px 12px;
	}
}

/*===================*/
/* TABLE OF CONTENTS */
/*===================*/

@media only screen and (max-width: 900px) {
	.contents {
		float: none;
		display: table;
		max-width: none;
		margin-left: auto;
		margin-right: auto;
	}
}
@media only screen and (max-width: 520px) {
	.contents {
		max-width: 100%;
		margin: 1em 0 0 0;
		display: inline-block;
	}
	.contents-head {
		font-size: 1.2em;
	}
	.contents {
		padding: 0.35em 0.75em 0.4em 0.35em;
	}
	.post-body .contents ul {
		font-size: unset;
	}
}

/*========================*/
/* QUALIFIED HYPERLINKING */
/*========================*/

@media only screen and (max-width: 520px) {
	.qualified-linking-toolbar {
		right: -5em;
	}
}

} /* END MOBILE LAYOUT */