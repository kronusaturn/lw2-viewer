/**************/
/* USER PAGES */
/**************/

/*=---------------------=*/
/*= User’s display name =*/
/*=---------------------=*/

#content.user-page h1.page-main-heading {
	margin: 0.25em 0 0 var(--GW-user-page-content-side-padding);
	line-height: 1.1;
	grid-row: 4;
	align-self: end;
}

/*=--------------------=*/
/*= User’s karma total =*/
/*=--------------------=*/

#content.user-page .user-stats {
	grid-row: 4;
	margin: 0 var(--GW-user-page-content-side-padding) 0 0;
	text-align: right;
	align-self: end;
}

#content.user-page .user-stats .karma-type {
	white-space: nowrap;
}

#content.user-page .user-stats .karma-total {
	font-weight: var(--GW-user-page-karma-font-weight);
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 720px) {
	#content.user-page h1.page-main-heading {
		padding-right: 100px;
		overflow: hidden;
		text-overflow: ellipsis;
	}
	#content.user-page .user-stats .karma-type {
		display: block;
	}
}

/*****************/
/* CONVERSATIONS */
/*****************/

/*=----------------------=*/
/*= List of participants =*/
/*=----------------------=*/

#content.conversation-page .conversation-participants {
	grid-row: 3;
	text-align: right;
	margin: 0.5em 0 0 0;
}

.conversation-participants ul,
.conversation-participants li {
	list-style-type: none;
	display: inline-block;
	margin: 0;
	padding: 0;
}
.conversation-participants li {
	margin-left: 0.375em;
}
.conversation-participants li:not(:last-of-type)::after {
	content: ",";
}

/*=-------------------------=*/
/*= Posting controls (form) =*/
/*=-------------------------=*/

#content.conversation-page .posting-controls {
	padding: 0 0 1em 0;
}
#content.conversation-page .post-meta-fields {
	overflow: auto;
	display: flex;
	flex-flow: row wrap;
}
#content.conversation-page textarea {
	margin-top: 0.375em;
}
#conversation-form {
	padding: 0 1em 3em 1em;
}
#conversation-form input[type='text'],
#conversation-form label {
	margin: 0.25em 0;
}
#conversation-form label {
	width: 4em;
	text-align: right;
	padding: 2px 6px;
	border: 1px solid transparent;
}
#conversation-form input[type='text'] {
	width: calc(100% - 4em);
	padding: 0.25em;
}
#conversation-form input[type='submit'] {
	float: right;
}
#content.conversation-page #markdown-hints-checkbox ~ label {
	white-space: nowrap;
}
#content.conversation-page #markdown-hints {
	top: calc(100% + 2em);
}

/*=--------------------=*/
/*= Conversation title =*/
/*=--------------------=*/

#content.conversation-page h1.page-main-heading {
	text-align: center;
	margin: 0.5em 0;
	line-height: 1.15;
}

/*=----------=*/
/*= Messages =*/
/*=----------=*/

#content.conversation-page > ul.comment-thread:last-of-type {
	margin-bottom: 2em;
}

/******************/
/* SEARCH RESULTS */
/******************/

#content.search-results-page h1.listing,
#content.sequence-page h1.listing {
	font-size: 1.625em;
}

/**************/
/* LOGIN PAGE */
/**************/

.login-container {
	margin: 2em 0;
	padding: 1em;
	display: flex;
	flex-flow: row wrap;
}

.login-container form {
	flex-basis: 50%;
	display: grid;
	grid-row-gap: 0.5em;
	align-content: start;
}
.login-container form label {
	text-align: right;
	padding: 0.25em 0.5em;
	white-space: nowrap;
	grid-column: 1;
}
.login-container form input {
	grid-column: 2;
	padding: 0.25em;
}
.login-container form input[type='submit'],
.login-container form a {
	grid-column: 2;
	justify-self: center;
}
.login-container form input[type='submit'] {
	width: 10em;
	padding: 0.35em;
	line-height: 1;
	margin: 0.5em 0 0 0;
}
.login-container form h1 {
	text-align: center;
	margin: 0;
	grid-column: 2;
}

/* “Log in” form */

#login-form {
	grid-template-columns: 5.5em 1fr;
	padding: 0.5em 2em 0.5em 0;
}

/* “Create account” form */

#signup-form {
	font-size: 0.9em;
	grid-template-columns: 8.5em 1fr;
	padding: 0.5em 1em 1em 1em;
}
#signup-form h1 {
	font-size: 1.7em;
}
#signup-form input[type='submit'] {
	padding: 0.4em 0.5em 0.5em 0.5em;
}

/* Log in tip */

.login-container .login-tip {
	padding: 0.5em 0.5em 0.5em 3em;
	margin: 2em 4em 0 4em;
	text-indent: -2em;
	line-height: 1.4;
}
.login-container .login-tip span {
	font-weight: var(--GW-login-page-tip-label-font-weight);
}

/* Message box */

#content.login-page .error-box {
	margin: 1.5em 0.875em -1.5em 0.875em;
}
.error-box, .success-box {
	padding: 0.25em;
	text-align: center;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 640px) {
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

/***********************/
/* PASSWORD RESET PAGE */
/***********************/

.reset-password-container {
	margin-bottom: 2em;
}
.reset-password-container input[type='submit'] {
	padding: 0.2em 0.5em;
	width: unset;
}
.reset-password-container input {
	margin-left: 0.5em;
	width: 12em;
}
.reset-password-container label {
	display: inline-block;
	width: 9em;
}
.reset-password-container form > div {
	margin: 0.2em;
}
.reset-password-container .action-container {
	padding-left: 11em;
	padding-top: 0.2em;
}
.reset-password-container .error-box {
	margin: unset;
}

/**************/
/* ERROR PAGE */
/**************/

.error-retry-form {
	margin: 0.5em 0;
}

.error-retry-form input[type="submit"] {
	border: 1px solid #aaa;
	font-weight: var(--GW-UI-font-weight-heavy);
	font-size: 1.125rem;
	padding: 0.5em 1.25em;
}

/**************/
/* ABOUT PAGE */
/**************/

#content.about-page .contents {
	margin-top: 0.25em;
}
#content.about-page .accesskey-table {
	border-collapse: collapse;
	margin: auto;

	font-family: var(--GW-UI-font);
}
#content.about-page .accesskey-table th,
#content.about-page .accesskey-table td {
	padding: 2px 6px;
}
#content.about-page .accesskey-table td:first-child {
	padding-right: 1.5em;
}
#content.about-page .accesskey-table td:last-child {
	text-align: center;
	font-family: var(--GW-monospaced-font);
}
#content.about-page h3:nth-of-type(n+2) {
	clear: both;
}

/*******************/
/* ALIGNMENT FORUM */
/*******************/

#content.alignment-forum-index-page::after {
	content: "Alignment Forum";
	font-family: var(--GW-Alignment-Forum-logotype-font);
	font-weight: var(--GW-Alignment-Forum-logotype-font-weight);
	grid-row: 3;
	font-size: 1.5rem;
	margin: 0.375em 0 0 -0.375em;
}

