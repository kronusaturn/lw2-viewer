/*****************/
/* MOBILE LAYOUT */
/*****************/

@media only screen and (hover: none), only screen and (-moz-touch-enabled) {
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
		overflow: hidden;
		z-index: 1;
	}
	#appearance-adjust-ui-toggle button,
	#post-nav-ui-toggle button {
		font-family: Font Awesome;
		font-weight: 900;
		font-size: 32px;
		padding: 10px;
		opacity: 0.8;
		transition: transform 0.2s ease;
	}
	#appearance-adjust-ui-toggle button::selection,
	#post-nav-ui-toggle button::selection {
		background-color: transparent;
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
	#appearance-adjust-ui-toggle button.engaged {
		transform: rotate(90deg);
	}
	
	#post-nav-ui-toggle {
		bottom: 10px;
		right: 10px;
	}
	#post-nav-ui-toggle button.engaged {
		transform: rotate(-90deg);
	}
	
	#theme-selector.engaged,
	#text-size-adjustment-ui.engaged,
	#quick-nav-ui.engaged,
	#new-comment-nav-ui.engaged,
	#hns-date-picker.engaged {
		visibility: visible;
	}
	
	#theme-selector {
		display: flex;
		flex-flow: column;
		width: calc(100vw - 20px);
		max-width: 360px;
		padding: 0 0 3px 0;
		border-radius: 12px;
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
	
	#quick-nav-ui {
		padding: 4px 4px 4px 4px;
		max-width: 0px;
		overflow: hidden;
		transition:
			max-width 0.2s ease,
			visibility 0.2s ease;
		display: flex;
		right: 68px;
		bottom: 10px;
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
	#quick-nav-ui::before {
		content: "";
		display: block;
		position: absolute;
		top: 2px;
		left: 2px;
		width: calc(100% - 4px);
		height: calc(100% - 4px);
		border-radius: 6px;
	}
	
	#new-comment-nav-ui {
		overflow: hidden;
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
}

/* Hide the mobile elements on desktop screens: */

@media only screen and (hover: hover) {
	#post-nav-ui-toggle {
		display: none;
	}
}

/*****************************************/
@media only screen and (max-width: 900px) {
/*****************************************/

#content,
#images-overlay,
#ui-elements-container {
	min-width: unset;
	width: unset;
}

#content {
	padding: 0 4px;
}
#content.post-page {
	padding-bottom: 12px;
}
#content > a:last-child,
#content > a:first-child {
	margin: 0 -4px;
}
.nav-bar {
	width: calc(100% + 8px);
}
.nav-bar .nav-inner {
	padding: 8px 3.33vw;
}
#secondary-bar .nav-inner {
	padding: 2px 0 3px 0;
}
.nav-bar {
	margin: 0 -4px;
}
.login-container, .login-container > div {
	display: block;
}
.contents {
	float: none;
	display: table;
	max-width: none;
	margin-left: auto;
	margin-right: auto;
}
.post-body,
.post > h1:first-child {
	padding: 0 6px;
}
.post-body, .comment-body {
	text-align: left;
	-webkit-hyphens: none;
	-ms-hyphens: none;
	hyphens: none;
}
@-moz-document url-prefix() {
	.post-body, .comment-body {
		text-align: justify;
		-webkit-hyphens: auto;
		-ms-hyphens: auto;
		hyphens: auto;
	}
}
.nav-inner::after {
	display: none;
}

/*******************************************/
} @media only screen and (max-width: 768px) {
/*******************************************/

#login-form-container,
#create-account-form-container {
	width: unset;
	float: unset;
}
.sublevel-nav.sort {
	position: unset;
	margin-top: 1.75em;
	margin-bottom: -1.25em;
}

/*******************************************/
} @media only screen and (max-width: 520px) {
/*******************************************/

.nav-inner,
#secondary-bar .nav-inner {
	font-size: 0.85em;
}
#bottom-bar .nav-inner {
	font-size: 1em;
}
h1.listing {
	font-size: 1.3rem;
	line-height: 1.1;
	margin: 0.5em 6px 0 6px;
}
h1.listing a {
	display: inline-block;
	padding: 0.4em 0 0.1em 0;
	text-indent: 33px;
}
h1.listing a[href^='/'] {
	text-indent: 0;
}
h1.listing a[href^="http"] {
	top: 10px;
}
h1.listing + .post-meta {
	margin: 0 6px 0 7px;
}
#secondary-bar #nav-item-search form {
	padding: 3px 4px 4px 4px;
}
.post-body {
	font-size: 1.2rem;
	line-height: 1.45;
	text-align: center;
}
.post-body > * {
	text-align: left;
}
.post > h1:first-child {
	margin: 1em 0.25em 0.25em 0.25em;
	font-size: 2em;
}
.comment-item .comment-item {
	margin: 0.75em 4px 4px 4px;
	box-shadow: 
		0 0 2px #ccc, 
		0 0 4px #ccc, 
		0 0 7px #ccc;
}
.comment-item .comment-item + .comment-item {
	margin: 1.5em 4px 4px 4px;
}
.comment-body ul {
	padding-left: 30px;
}
.contents {
	max-width: 100%;
	margin: 1em 0 0 0;
	display: inline-block;
}
.contents-head {
	font-size: 1.2em;
}
div[class^='archive-nav-'] {
	display: block;
	text-align: justify;
}
.archive-nav *[class^='archive-nav-item'],
.archive-nav *[class^='archive-nav-item']:first-child {
	display: inline-block;
	width: auto;
	padding: 6px 10px;
	width: 4em;
	margin: 2px;
}
.archive-nav *[class^='archive-nav-item'],
.archive-nav *[class^='archive-nav-item-'],
.archive-nav div[class^='archive-nav-']:nth-of-type(n+2) *[class^='archive-nav-item'] {
	border: 1px solid #ddd;
}
.archive-nav > *[class^='archive-nav-'] +	*[class^='archive-nav-'] {
	margin-top: 0.5em;
}
#nav-item-recent-comments > * > span {
	display: none;
}
#primary-bar,
#secondary-bar {
	table-layout: fixed;
	font-size: 0.55em;
}
#secondary-bar {
	font-size: 0.5em;
}
#primary-bar .nav-inner,
#secondary-bar .nav-inner {
	text-transform: uppercase;
	padding: 6px;
	font-weight: 600;
}
#secondary-bar .nav-inner {
	padding: 4px;
}
#primary-bar .nav-inner::before, 
#secondary-bar .nav-inner::before {
	display: block;
	font-family: "Font Awesome";
	font-size: 1.25rem;
	font-weight: 900;
}
#secondary-bar .nav-inner::before {
	font-size: 0.875rem;
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
#nav-item-recent-comments .nav-inner::before {
	content: "\F036";
}
#nav-item-archive {
	width: auto;
}
#nav-item-archive .nav-inner::before {
	content: "\F187";
}
#nav-item-about {
	width: auto;
}
#nav-item-about .nav-inner::before {
	content: "\F129";
}
#nav-item-search {
	font-size: 2em;
	vertical-align: middle;
}
#nav-item-search .nav-inner::before {
	content: none;
}
#nav-item-search input {
	width: calc(100% - 28px);
}
#nav-item-search button {
	width: 22px;
	color: transparent;
	vertical-align: bottom;
	padding-left: 4px;
	overflow: visible;
}
#nav-item-search button::before {
	content: "\F002";
	color: #00e;
	font-family: Font Awesome;
	font-weight: 900;
	font-size: 1rem;
}
#nav-item-login {
	width: auto;
}
#nav-item-login .nav-inner::before {
	content: "\F007";
}
.post-meta {
	line-height: 1.9;
}
.post-meta > *,
.post-meta .lw2-link {
	margin: 0 0.5em;
}
.post-meta .lw2-link {
	opacity: 0.5;
}
a.comment-parent-link {
	position: relative;
	width: 0;
	visibility: hidden;
}
a.comment-parent-link::before {
	display: inline-block;
	width: unset;
	height: unset;
	padding: 0;
	font-size: 1em;
	left: 0;
	top: 0;
	line-height: inherit;
	visibility: visible;
	color: #000;
	content: "\F3BF";
	transform: scaleX(-1);
}
a.comment-parent-link::after {
	display: none;
}
.page-toolbar {
	font-size: 1rem;
	margin: 0.25em;
}
#top-nav-bar {
	margin-top: 1.5em;
}
.sublevel-nav {
	display: table;
	margin: auto;
	padding: 0 1em;
}
.sublevel-nav .sublevel-item {
	display: table-cell;
	font-size: 1rem;
	padding: 0.25em 0.5em;
}
.comment-minimize-button::after {
	height: 100%;
	top: 0;
	left: -2em;
	width: 1.5em;
	line-height: 1.6;
	text-align: right;
}
#edit-post-form label[for='title'] {
	width: 2.5em;
}
#edit-post-form label[for='url'] {
	width: 2.5em;
}
#edit-post-form label[for='section'] {
	width: 3.6em;
}
#edit-post-form label[for='url'], 
#edit-post-form label[for='section'],
#edit-post-form label[for='title'] {
	clear: left;
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
}
#edit-post-form textarea {
	min-height: unset;
}
#edit-post-form .textarea-container:focus-within textarea {
	position: fixed;
	top: -1px;
	left: 2px;
	width: calc(100vw - 4px);
	height: calc(100% - 101px) !important;
	max-height: unset;
	border-width: 1px;
	z-index: 11001;
}
#edit-post-form .textarea-container:focus-within .guiedit-buttons-container {
	position: fixed;
	z-index: 11002;
	left: 0;
	width: 100vw;
	height: auto;
	background-image: none;
	background-color: white;
	border-top: 1px solid #ddf;
	padding: 3px 4px 4px 4px;
	margin: 0;
	text-align: center;
}
#edit-post-form .textarea-container:focus-within .guiedit-buttons-container {
	top: auto;
	bottom: 0;
}
#edit-post-form .textarea-container:focus-within button.guiedit {
	font-size: 0.9375rem;
	line-height: 1.5;
	height: auto;
	width: calc((100% / 10) - 2px);
	padding: 10px 1px 8px 0;
	position: relative;
	background-color: #eee;
	border: 1px solid #ddd;
	border-radius: 6px;
	margin: 1px;
}
#edit-post-form .textarea-container:focus-within button.guiedit sup {
	position: absolute;
	left: calc(50% + 0.65em);
	top: calc(50% - 1.3em);
}
.textarea-container button:active {
	background-color: #ccc;
}
.textarea-container .guiedit-mobile-auxiliary-button {
	z-index: 11011;
	position: fixed;
	bottom: 7px;
	width: calc(((100% - 16px) / 10) * 3 - 7px);
	font-size: 1.25rem;
	padding: 5px;
	background-color: #eee;
	border: 1px solid #ddd;
	border-radius: 6px;
}
.textarea-container:focus-within .guiedit-mobile-auxiliary-button {
	display: block;
}
.textarea-container .guiedit-mobile-help-button {
	left: 8px;
}
.textarea-container .guiedit-mobile-exit-button {
	right: 8px;
}
.guiedit::after {
	display: none;
}
#edit-post-form input[type='submit'] {
	margin-top: -6px;
}
.comment-meta {
	padding: 2px 10px;
}
.comment-post-title2 {
	display: block;
	text-overflow: ellipsis;
	overflow: hidden;
}
.comment-meta author {
	display: block;
}
.comment-meta .karma-value {
	font-weight: 600;
}
.comment-meta .karma-value span {
	display: none;
}
.comment-meta .lw2-link {
	display: none;
}
.comment-body {
	font-size: 1.125rem;
}
.comment-body ol {
	padding-left: 30px;
}
.contents {
	padding: 0.35em 0.75em 0.4em 0.35em;
}
.post-body .contents ul {
	font-size: unset;
}
.login-container {
	margin: 0;
}
#login-form-container h1 {
	padding: 0;
	margin: 0.5em 0 0.25em 0;
}
.login-container form label,
.login-container form input {
	float: none;
	display: block;
}
#login-form label,
#signup-form label {
	padding: 0.5em 0 0 1px;
	color: #666;
}
.login-container form label,
.login-container form input[type='text'],
.login-container form input[type='password'] {
	width: calc(100% - 2em);
	margin: 0 1em;
	text-align: left;
		font-size: 1.125rem;
}
.login-container form input[type='text'],
.login-container form input[type='password'] {
		font-weight: 600;
}
.login-container form input[type='text'],
.login-container form input[type='password'] {
}
.login-container form input[type='submit'] {
		width: auto;
	margin: 0.75em auto;
	padding: 8px 12px;
	font-size: 1.125rem;
	float: none;
}
#create-account-form-container {
	margin: 1em 0 0 0;
	padding: 0;
}
#inbox-indicator {
	width: 100%;
}
#inbox-indicator::before {
	width: 100%;
	height: 100%;
	font-size: 0.75rem;
	text-align: right;
	padding: 1px 5px;
}
a#inbox-indicator.new-messages::before {
	box-shadow: 0 0 5px 1px inset;
}

/*******************************************/
} @media only screen and (max-width: 374px) {
/*******************************************/

.nav-bar .nav-inner {
	padding: 6px 3.33vw;
}
.sublevel-nav {
	padding: 0 0.5em;
}
.sublevel-nav .sublevel-item {
	font-size: 0.9375rem;
}
#inbox-indicator::before {
	font-size: 0.625rem;
	text-align: right;
	padding: 1px 5px;
}

/***/
  }
/***/
