/**************/
/* USER PAGES */
/**************/

/*=---------------------=*/
/*= User’s display name =*/
/*=---------------------=*/

#content.user-page h1.page-main-heading {
	margin: 0.25em var(--GW-user-page-content-side-padding) 0 var(--GW-user-page-content-side-padding);
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
	#content.user-page .user-stats {
		grid-row: 3;
		justify-self: start;
		margin: 0.5em 0 0 var(--GW-user-page-content-side-padding);
	}
}

/*=--------------------=*/
/*= Conversations list =*/
/*=--------------------=*/

/*	List of participants.
	*/
#content.conversations-user-page .post-meta .conversation-participants {
	white-space: normal;
}
#content.conversations-user-page .post-meta .conversation-participants ul {
	list-style-type: none;
	margin: 0;
	padding: 0;
}
#content.conversations-user-page .post-meta .conversation-participants ul,
#content.conversations-user-page .post-meta .conversation-participants li {
	display: inline;
}
#content.conversations-user-page .post-meta .conversation-participants li:nth-last-of-type(n+2)::after {
	content: ", ";
	margin: 0 0.25em 0 0;
}

/*****************/
/* CONVERSATIONS */
/*****************/

/*=--------------------=*/
/*= Conversation title =*/
/*=--------------------=*/

#content.conversation-page h1.page-main-heading {
	text-align: center;
	margin: 1em 1em 0.25em 1em;
	line-height: 1.15;
}

/*=----------------------=*/
/*= List of participants =*/
/*=----------------------=*/

#content.conversation-page .conversation-participants {
	text-align: center;
	padding: 0 var(--GW-conversation-page-content-side-padding);
	margin: 0 0 0.5em 0;
}

#content.conversation-page .conversation-participants ul,
#content.conversation-page .conversation-participants li {
	display: inline;
	margin: 0;
	padding: 0;
}
#content.conversation-page .conversation-participants li {
	margin-left: 0.375em;
}
#content.conversation-page .conversation-participants li:not(:last-of-type)::after {
	content: ", ";
}

/*=-------------------------=*/
/*= Posting controls (form) =*/
/*=-------------------------=*/

#content.conversation-page .posting-controls {
	padding: 1em var(--GW-conversation-page-content-side-padding) 1em var(--GW-conversation-page-content-side-padding);
}
#content.conversation-page .post-meta-fields {
	overflow: auto;
	display: flex;
	flex-flow: row wrap;
	margin: 0 0 0.5em 0;
}
#conversation-form .post-meta-fields input[type='text'],
#conversation-form .post-meta-fields label {
	margin: 0.25em 0;
}
#conversation-form .post-meta-fields label {
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
#content.conversation-page #markdown-hints {
	top: calc(100% + 2em);
}

/**************/
/* LOGIN PAGE */
/**************/

.login-container {
	padding: 3em;
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
}
.login-container form input {
	padding: 0.25em;
}
.login-container form input[type='submit'],
.login-container form a {
	grid-column: 2;
	justify-self: center;
}
.login-container form input[type='submit'] {
	width: 10em;
	margin: 0.5em 0 0 0;
}
.login-container form h1 {
	text-align: center;
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

@media only screen and (max-width: 900px) {
	.login-container {
		flex-flow: column;
		margin: auto;
		padding: 2em 1em;
		max-width: 54ch;
		font-size: 1.125rem;
	}
	.login-container #login-form,
	.login-container #signup-form {
		padding: 0 1em 4em 1em;
		grid-row-gap: 0;
	}
	.login-container #signup-form {
		padding: 1em;
	}
	.login-container #login-form > *,
	.login-container #signup-form > * {
		grid-column: 1 / span 2;
	}
	.login-container form label {
		text-align: left;
		padding: 0;
		line-height: 1;
		font-size: 1.25em;
	}
	.login-container form input {
		margin: 0.25em 0 0.75em 0;
		padding: 0.5em;
		font-size: 1.25em;
	}
	.login-container form h1 {
		margin: 0 0 0.5em 0;
	}
	.login-container form a {
		margin: 0.75em 0 0 0;
		font-size: 1.25em;
	}
	.login-container .login-tip {
		margin: 3em 0 0 0;
	}
}

/***********************/
/* PASSWORD RESET PAGE */
/***********************/

.reset-password-container {
	padding: 1.5em 2.5em;
	font-size: 1.125em;
}

.reset-password-container h1 {
	text-align: center;
}
.reset-password-container h2 {
	margin: 1.25em 0 0.25em 0;
}

.reset-password-container form {
	text-align: center;
	display: flex;
	flex-flow: column;
	max-width: 40ch;
	margin: auto;
}
.reset-password-container input[type='text'],
.reset-password-container input[type='password'] {
	padding: 0.25em;
	margin: 0.75em 0 1em 0;
}
.reset-password-container input[type='submit'] {
	padding: 0.5em 1em;
	display: block;
	margin: auto;
	grid-column: 1 / span 2;
}

.reset-password-container form:nth-of-type(2) {
	margin: 2em auto 0 auto;
}
.reset-password-container form div {
	display: flex;
	flex-flow: column;
}
.reset-password-container div input[type='text'],
.reset-password-container div input[type='password'] {
	margin: 0.25em 0 0.75em 0;
}
.reset-password-container div input[type='submit'] {
	margin: 1em auto 0 auto;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 520px) {
	.reset-password-container {
		padding: 1.5em;
	}
}

/**************/
/* ERROR PAGE */
/**************/

.error-container {
	padding: 2em;
}

.error-container h1 {
	text-align: center;
}

.error-container input[type="submit"] {
	font-weight: var(--GW-UI-font-weight-heavy);
	font-size: 1.125rem;
	padding: 0.5em 1.25em;
	margin: 2em auto 0 auto;
	display: block;
}

/**************/
/* ABOUT PAGE */
/**************/

#content.about-page .contents {
	margin-top: 0.25em;
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
	grid-row: 4;
	font-size: 1.75rem;
	margin: 0 0 0 40px;
}
@media only screen and (max-width: 900px) {
	#content.alignment-forum-index-page::after {
		margin: 0 0 0 calc(100% / 45);
	}
}
@media only screen and (max-width: 720px) {
	#content.alignment-forum-index-page::after {
		margin: 15px 0 0 calc(100% / 45);
	}
}
