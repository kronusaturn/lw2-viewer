/*****************/
/* MOBILE LAYOUT */
/*****************/

/*==============*/
/* PAGE TOOLBAR */
/*==============*/

/*=--------------------------------------=*/
@media only screen and (max-width: 900px) {
/*=--------------------------------------=*/
	#content > .page-toolbar {
		font-size: 1rem;
		margin-right: 0;
	}
	#content.user-page > .page-toolbar {
		grid-column: 2 / span 2;
		margin: 0 0 6px 0;
	}
/*=---------------------------------------=*/
} @media only screen and (max-width: 520px) {
/*=---------------------------------------=*/
	#content:not(.user-page) .page-toolbar {
		display: flex;
		flex-direction: column-reverse;
		text-align: right;
		align-self: start;
		padding: 4px 0 0 0;
	}
	#content.user-page .page-toolbar {
		display: flex;
		flex-flow: row;
		justify-content: flex-end;
		padding: 2px 0 0 0;
	}
	#content.user-page .page-toolbar > form,
	#content.user-page .page-toolbar > .button {
		text-align: center;
		flex-basis: 25%;
		margin-left: 1.5em;
	}
	#content.user-page .page-toolbar .button {
		text-transform: uppercase;
		font-size: 0.625rem;
	}
	#content.user-page .page-toolbar .button::before {
		font-size: 1.375rem;
		display: block;
		padding: 0;
	}
	#content.user-page .page-toolbar .rss {
		white-space: nowrap;
		margin: 0 0 0 1.5em;
	}
	.page-toolbar > * {
		line-height: 1.15;
		padding: 6px 0;
		margin: 0;
	}
}

/*==============*/
/* SUBLEVEL NAV */
/*==============*/

/*=-------------------------------------=*/
@media only screen and (max-width: 900px) {
/*=-------------------------------------=*/
	.sublevel-nav:not(.sort) {
		flex-wrap: wrap;
		width: calc(100vw - 200px);
	}
	.sublevel-nav:not(.sort) .sublevel-item {
		margin: 1px;
		flex-basis: 7em;
	}
/*=---------------------------------------=*/
} @media only screen and (max-width: 720px) {
/*=---------------------------------------=*/
	.sublevel-nav:not(.sort) {
		width: calc(100vw - 200px);
	}
/*=---------------------------------------=*/
} @media only screen and (max-width: 520px) {
/*=---------------------------------------=*/
	.sublevel-nav:not(.sort) {
		width: calc(100vw - 100px);
	}
	.sublevel-nav:not(.sort) .sublevel-item {
		font-size: 1rem;
	}
}

/*=====================*/
/* SORT ORDER SELECTOR */
/*=====================*/

/*=-------------------------------------=*/
@media only screen and (max-width: 720px) {
/*=-------------------------------------=*/
	#content.index-page > .sublevel-nav.sort {
		flex-flow: column;
		margin-left: 4px;
	}
}

/*==========*/
/* ARCHIVES */
/*==========*/

/*=-------------------------------------=*/
@media only screen and (max-width: 900px) {
/*=-------------------------------------=*/
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
/*=---------------------------------------=*/
} @media only screen and (max-width: 720px) {
/*=---------------------------------------=*/
	.archive-nav .archive-nav-item-day,
	.archive-nav .archive-nav-item-day:first-child {
		flex-basis: calc((100% / 12) - 4px);
	}
/*=---------------------------------------=*/
} @media only screen and (max-width: 520px) {
/*=---------------------------------------=*/
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

/*=--------------------------------=*/
@media only screen and (hover: none) {
/*=--------------------------------=*/
	h1.listing {
		max-height: unset;
	}
}

/*============*/
/* USER PAGES */
/*============*/

#content.user-page h1.page-main-heading {
	align-self: end;
}
@media only screen and (max-width: 520px) {
/*=-------------------------------------=*/
@media only screen and (max-width: 720px) {
/*=-------------------------------------=*/
	#content.user-page h1.page-main-heading {
		padding-right: 100px;
		overflow: hidden;
		text-overflow: ellipsis;
	}
	#content.user-page .user-stats {
		grid-column: 2 / span 2;
	}
	#content.user-page .user-stats .karma-type {
		display: block;
	}
}

/*============*/
/* LOGIN PAGE */
/*============*/

/*=-------------------------------------=*/
@media only screen and (max-width: 640px) {
/*=-------------------------------------=*/
	.login-container {
		flex-flow: column;
		margin: 0 auto 3em auto;
		max-width: 400px;
	}
	.login-container #login-form,
	.login-container #signup-form {
		padding: 0 1em 1.25em 1em;
		grid-row-gap: 0;
	}
	.login-container #signup-form {
		padding-top: 1em;
	}
	.login-container #login-form > *,
	.login-container #signup-form > * {
		grid-column: 1 / span 2;
	}
	.login-container form label {
		text-align: left;
		padding: 0;
		line-height: 1;
	}
	.login-container form input {
		margin: 0.25em 0 0.75em 0;
		padding: 0.5em;
	}
	.login-container form h1 {
		grid-column: 1 / span 2;
		margin: 0 0 0.25em 0;
	}
	.login-container form a {
		margin: 0.75em 0 0 0;
	}
	.login-container .login-tip {
		margin: 1.5em 1em 0 1em;
	}
}

/*==================*/
/* POSTS & COMMENTS */
/*==================*/

/*===========*/
/* POST-META */
/*===========*/

/*=-------------------------------------=*/
@media only screen and (max-width: 960px) {
/*=-------------------------------------=*/
	.post-meta {
		line-height: 1.9;
	}
/*=---------------------------------------=*/
} @media only screen and (max-width: 720px) {
/*=---------------------------------------=*/
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

/*=-------------------------------------=*/
@media only screen and (max-width: 900px) {
/*=-------------------------------------=*/
	.post-body,
	h1.post-title {
		padding: 0 6px;
	}
/*=---------------------------------------=*/
} @media only screen and (max-width: 520px) {
/*=---------------------------------------=*/
	.post-body {
		font-size: 1.2rem;
		line-height: 1.45;
	}
	h1.post-title {
		font-size: 2em;
	}
}

/*==========*/
/* COMMENTS */
/*==========*/

/*=-------------------------------------=*/
@media only screen and (max-width: 900px) {
/*=-------------------------------------=*/
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

/*=--------------------------------=*/
@media only screen and (hover: none) {
/*=--------------------------------=*/
	a.comment-parent-link::after {
		display: none;
	}
}

/*=-------------------------------------=*/
@media only screen and (max-width: 900px) {
/*=-------------------------------------=*/
	.comment-meta {
		padding: 2px 40px 2px 10px;
	}
/*=---------------------------------------=*/
} @media only screen and (max-width: 720px) {
/*=---------------------------------------=*/
	.comment-meta .karma-value span {
		display: none;
	}
	.comment-meta .comment-parent-link {
		opacity: 1.0;
	}
/*=---------------------------------------=*/
} @media only screen and (max-width: 520px) {
/*=---------------------------------------=*/
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

/*===========================*/
/* COMMENT THREAD NAVIGATION */
/*===========================*/

/*=-------------------------------------=*/
@media only screen and (max-width: 900px) {
/*=-------------------------------------=*/
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
/*=---------------------------------------=*/
} @media only screen and (max-width: 520px) {
/*=---------------------------------------=*/
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

/*=-------------------------------------=*/
@media only screen and (max-width: 520px) {
/*=-------------------------------------=*/
	.comment-minimize-button{
		right: 2px;
	}
}

/*===========================*/
/* COMMENTING AND POSTING UI */
/*===========================*/

/*=-------------------------------------=*/
@media only screen and (max-width: 900px) {
/*=-------------------------------------=*/
	.comment-controls .cancel-comment-button {
		max-width: 1.3em;
		overflow: hidden;
		margin-right: 0.125em;
	}
	.comment-controls .edit-button::before {
		font-size: 0.9375em;
	}
	.comments > .comment-controls .cancel-comment-button {
		right: 8px;	
	}
	.comment-controls .cancel-comment-button::before {
		font-size: 1.25rem;		
	}
/*=---------------------------------------=*/
} @media only screen and (max-width: 520px) {
/*=---------------------------------------=*/
	.comment-controls {
		position: static;
	}
	.comment-controls:focus-within {
		z-index: 10001;
	}
	.comment-controls .cancel-comment-button {
		right: 10px;
	}
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
	#content.conversation-page .textarea-container:focus-within textarea {
		height: calc(100% - 54px);
	}
	#content.conversation-page .textarea-container:focus-within::after {
		content: "";
		display: block;
		width: 100%;
		height: 50px;
		position: fixed;
		left: 0;
		bottom: 0;
		z-index: 11000;
	}
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
	.textarea-container:focus-within button.guiedit {
		font-size: 0.9375rem;
		line-height: 1.5;
		height: auto;
		width: calc((100% / 10) - 2px);
		padding: 10px 1px 8px 0;
		position: relative;
		margin: 1px;
	}
	.textarea-container:focus-within .guiedit-mobile-auxiliary-button {
		z-index: 11011;
		position: fixed;
		bottom: 7px;
		width: calc(((100% - 16px) / 10) * 2.5 - 7px);
		font-size: 1.25rem;
		padding: 5px 5px 6px 5px;
		display: block;
	}
	.textarea-container:focus-within button.guiedit sup {
		position: absolute;
		left: calc(50% + 0.65em);
		top: calc(50% - 1.3em);
	}
	.textarea-container:focus-within .guiedit-mobile-help-button {
		left: 8px;
	}
	.textarea-container:focus-within .guiedit-mobile-exit-button {
		right: 8px;
	}
	.guiedit::after {
		display: none;
	}

	#markdown-hints,
	#edit-post-form #markdown-hints {
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
	#markdown-hints::after {
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

/*=-------------------------------------=*/
@media only screen and (max-width: 520px) {
/*=-------------------------------------=*/
	#edit-post-form {
		padding-bottom: 0;
	}
	#edit-post-form .post-meta-fields {
		grid-template-columns: 4.5em auto auto auto 1fr auto;
	}
	#edit-post-form label[for='url'], 
	#edit-post-form label[for='section'],
	#edit-post-form label[for='title'] {
		padding-left: 0;
	}
	#edit-post-form .post-meta-fields input[type='checkbox'] + label {
		white-space: normal;
		line-height: 0.9;
		top: -1px;
		font-family: Font Awesome;
		font-weight: 900;
		justify-self: start;
	}
	#edit-post-form .post-meta-fields .question-checkbox,
	#edit-post-form .post-meta-fields .question-checkbox + label {
		grid-column: 6;
		margin-left: unset;
	}
	#edit-post-form .post-meta-fields input[type='radio'] + label {
		align-self: start;
	}
	#edit-post-form .textarea-container:focus-within textarea {
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

/*=-------------------------------------=*/
@media only screen and (max-width: 900px) {
/*=-------------------------------------=*/
	.contents {
		float: none;
		display: table;
		max-width: none;
		margin-left: auto;
		margin-right: auto;
	}
/*=---------------------------------------=*/
} @media only screen and (max-width: 520px) {
/*=---------------------------------------=*/
	.contents {
		max-width: 100%;
		margin: 1em auto 0 auto;
		display: table;
	}
	.contents-head {
		font-size: 1.2em;
	}
	div.post-body .contents ul {
		font-size: unset;
	}
}

/*========================*/
/* QUALIFIED HYPERLINKING */
/*========================*/

/*=-------------------------------------=*/
@media only screen and (max-width: 520px) {
/*=-------------------------------------=*/
	.qualified-linking-toolbar {
		right: -5em;
	}
}
