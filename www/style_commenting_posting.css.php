/*****************************/
/* COMMENTING AND POSTING UI */
/*****************************/

.comment-controls {
	text-align: right;
	margin: 4px 8px 8px 16px;
	position: relative;
	-webkit-user-select: none;
	-moz-user-select: none;
	user-select: none;
	z-index: 5001;
}
.comment-controls.active {
	z-index: 9999;
}
.comment-thread .comment-controls + .comment-thread > li:first-child {
	margin-top: 8px;
}
.comments > .comment-controls {
	margin: 8px 0 0 0;
}
.comments > .comment-controls:last-child {
	margin: 8px 0 16px 0;
}

.posting-controls input[type='submit'],
.posting-controls .button.cancel-post-editing-button,
.comment-controls .cancel-comment-button,
.new-comment-button {
	font-weight: var(--GW-UI-font-weight-heavy);
}

.posting-controls input[type='submit'],
.posting-controls .button.cancel-post-editing-button {
	margin: 6px;
	padding: 4px 10px;
	font-size: 1.125rem;
}

.comment-controls .cancel-comment-button {
	position: absolute;
	right: 0;
	margin: 0;
	height: 27px;
	font-size: inherit;
	padding: 4px 8px 2px 4px;
	z-index: 1;
}
.comment-controls .cancel-comment-button::before {
	font-family: var(--GW-Font-Awesome);
	margin-right: 3px;
	content: '\F00D';
	font-weight: 900;
	font-size: 0.9em;
}

.comment + .comment-controls .action-button {
	font-weight: var(--GW-UI-font-weight-light);
	font-size: 1.0625em;
	padding: 1px 6px;
}
.comment-controls .action-button::before {
	font-family: var(--GW-Font-Awesome);
	margin-right: 3px;
}
.new-comment-button {
	font-size: 1.5rem;
	margin: 0 0.25em;
}
.comment-controls .reply-button::before {
	content: '\F3E5';
	font-weight: 400;
	font-size: 0.9em;
	opacity: 0.75;
}

.edit-post-link {
	display: inline-block;
	margin-bottom: 0.25em;
	font-size: 1.125rem;
}
.edit-post-link::before {
	margin-right: 0.3em;
}
.comment-controls .edit-button::before,
.edit-post-link::before {
	content: '\F303';
	font-family: var(--GW-Font-Awesome);
	font-weight: 900;
	font-size: 0.75em;
	position: relative;
	top: -1px;
}

.comment-controls .delete-button {
	margin-right: 0.25em;
}
.comment-controls .edit-button,
.comment-controls .retract-button,
.comment-controls .unretract-button {
	margin-right: 1em;
}
.comment-controls .edit-button::before {
	font-weight: 300;
}
.comment-controls .retract-button::before {
	content: '\F4B3';
	opacity: 0.7;
}
.comment-controls .unretract-button::before {
	content: '\F075';
	opacity: 0.8;
}
.comment-controls .delete-button::before {
	content: '\F05E';
	opacity: 0.7;
	margin-right: 4px;
}
.comment-controls .retract-button::before,
.comment-controls .unretract-button::before,
.comment-controls .delete-button::before {
	font-weight: 400;
	font-size: 0.9em;
}

.comment-controls form {
	position: relative;
}
.textarea-container {
	position: relative;
}
.posting-controls textarea {
	display: block;
	width: 100%;
	height: 15em;
	min-height: 15em;
	max-height: calc(100vh - 8em);
	margin: 2px 0 0 0;
	padding: 4px 5px;
	font-size: 1.2rem;
	line-height: 1.2;
	border-style: solid;
	border-width: 29px 1px 1px 1px;
	resize: none;
	overscroll-behavior: none;

	font-family: var(--GW-editor-font);
	font-weight: var(--GW-editor-font-weight);
}

/* GUIEdit buttons */

.guiedit-buttons-container {
	position: absolute;
	left: 1px;
	top: 1px;
	width: calc(100% - 2px);
	height: 28px;
	text-align: left;
	padding: 2px 4px 0 4px;
	overflow: hidden;
}
.comment-thread-page .guiedit-buttons-container {
	padding-right: 60px;
}
.guiedit-buttons-container button {
	height: 26px;
	padding: 0 7px;
	font-weight: 900;
	font-size: 0.875rem;
	line-height: 1;

	font-family: var(--GW-Font-Awesome), var(--GW-body-text-font);
}
.guiedit-buttons-container button:active {
	transform: none;
}
.guiedit-buttons-container button:active div {
	transform: scale(0.9);
}
.guiedit-buttons-container button sup {
	font-weight: var(--GW-UI-font-weight-heavy);
}
button.guiedit {
	font-family: var(--GW-UI-font), var(--GW-Font-Awesome);
}
.guiedit::after {
	content: attr(data-tooltip);
	position: absolute;
	font-size: 1rem;
	top: 2px;
	left: 464px;
	height: 25px;
	padding: 4px 0;
	white-space: nowrap;
	visibility: hidden;

	font-family: var(--GW-UI-font);
	font-weight: var(--GW-UI-font-weight-light);
}
.guiedit:hover::after {
	visibility: visible;
}

/* Markdown hints */

.posting-controls .markdown-reference-link {
	float: left;
	padding: 1px 0 0 6px;
}

#markdown-hints-checkbox + label {
	float: left;
	margin: 2px 0 0 1em;
	line-height: 1.3;
	cursor: pointer;

	color: var(--GW-hyperlink-color);
}
#markdown-hints-checkbox + label:hover,
#markdown-hints-checkbox + label:focus {
	color: var(--GW-hyperlink-hover-color);
}
#markdown-hints-checkbox + label:active {
	color: var(--GW-hyperlink-active-color);
}
#edit-post-form #markdown-hints-checkbox + label {
	padding: 0;
}
#markdown-hints-checkbox {
	visibility: hidden;
	margin: 0;
	float: left;
}
#markdown-hints-checkbox + label::after {
	content: "(Show Markdown help)";
}
#markdown-hints-checkbox:checked + label::after {
	content: "(Hide Markdown help)";
}
#markdown-hints-checkbox + label::before {
	content: '\F059';
	font-family: var(--GW-Font-Awesome);
	font-weight: 900;
	margin-right: 3px;
}
#markdown-hints-checkbox:checked + label::before {
	font-weight: 400;
}
#markdown-hints {
	margin: 4px 0 0 4px;
	padding: 4px 8px;
	position: absolute;
	text-align: left;
	z-index: 1;
	display: none;
}
.comment-controls #markdown-hints {
	top: calc(100% + 1.75em);
}
#markdown-hints-checkbox:checked ~ #markdown-hints {
	display: block;
}
.markdown-hints-row {
	display: table-row;
}
#markdown-hints .markdown-hints-row span,
#markdown-hints .markdown-hints-row code {
	float: none;
	display: table-cell;
	border: none;
	background-color: inherit;
	padding: 0 1.5em 0 0;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	.comment-controls .cancel-comment-button {
		max-width: 1.2em;
		overflow: hidden;
		top: -1px;
		right: 6px;
	}
	.comment-controls .cancel-comment-button::before {
		font-size: 1.375em;
	}
	.comment-controls .edit-button::before {
		font-size: 0.9375em;
	}
}
@media only screen and (max-width: 720px) {
	.comment-controls {
		   margin: 4px 8px 8px 16px;
	}
	.comment-controls .karma {
		   display: none;
	}
}
@media only screen and (max-width: 520px) {
	.comment-controls:focus-within {
		z-index: 10001;
	}
	.textarea-container:focus-within textarea {
		position: fixed;
		top: 0;
		left: 2px;
		width: calc(100% - 4px);
		height: calc(100% - 100px);
		min-height: unset;
		max-height: unset;
		border-width: 1px;
		z-index: 11001;
	}
	.textarea-container:focus-within .guiedit-buttons-container {
		position: fixed;
		z-index: 11002;
		left: 0;
		width: 100%;
		height: auto;
		background-image: none;
		padding: 3px 4px 4px 4px;
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
	.textarea-container:focus-within button.guiedit::before {
		content: "";
		position: absolute;
		left: -2px;
		right: -2px;
		top: -2px;
		bottom: -2px;
	}
	.textarea-container:focus-within .guiedit-mobile-auxiliary-button {
		z-index: 11011;
		position: fixed;
		bottom: 6px;
		width: calc(((100% - 16px) / 12.5) * 2.5 - 7px);
		font-size: 1.25rem;
		padding: 6px 5px;
		display: block;

		font-weight: var(--GW-UI-font-weight-heavy);
	}
	.textarea-container:focus-within button.guiedit sup {
		position: absolute;
		left: calc(50% + 0.65em);
		top: calc(50% - 1.3em);
	}
	.textarea-container:focus-within .guiedit-mobile-auxiliary-button::before {
		content: "";
		position: absolute;
		top: -5px;
		bottom: -4px;
	}
	.textarea-container:focus-within .guiedit-mobile-help-button {
		left: 8px;
	}
	.textarea-container:focus-within .guiedit-mobile-help-button::before {
		left: -5px;
		right: -6px;
	}
	.textarea-container:focus-within .guiedit-mobile-exit-button {
		right: 8px;
	}
	.textarea-container:focus-within .guiedit-mobile-exit-button::before {
		left: -6px;
		right: -5px;
	}
	.guiedit::after {
		display: none;
	}

	.comment-controls #markdown-hints,
	#edit-post-form #markdown-hints {
		z-index: 11111;
		position: fixed;
		top: 40px;
		left: 0;
		right: 0;
		margin: auto;
		max-width: calc(100% - 12px);
		padding: 0.25em 0.5em;
		border-width: 3px;
		border-style: double;
		pointer-events: none;
		font-size: 1.125rem;
	}
	<?php fit_content(".comment-controls #markdown-hints, #edit-post-form #markdown-hints"); ?>
	#markdown-hints::after {
		content: "(Type to hide this help box.)";
		display: block;
		margin: 0.5em auto;
		padding: 5px;
		text-align: center;
	}
}

/******************/
/* EDIT POST FORM */
/******************/

.post-controls {
	text-align: right;
	margin: 0;
	grid-row: 3;
	align-self: start;
	justify-self: end;
}
.post-controls .edit-post-link {
	font-size: 1.375em;
	padding: 0.375em 0.75em 0 0.25em;
	margin: 0;
}

#edit-post-form {
	padding: 2em var(--GW-current-page-content-right-side-padding) 2em var(--GW-current-page-content-left-side-padding);
	margin: 0 0 2em 0;
}
#edit-post-form .post-meta-fields {
	display: grid;
	grid-template-columns: 5em auto auto auto 1fr auto;
	margin-bottom: 0.625em;
}

#edit-post-form label[for='title'],
#edit-post-form label[for='url'],
#edit-post-form label[for='section'] {
	grid-column: 1;
}
#edit-post-form input[type='text'] {
	padding: 0.25em;
	grid-column: 2 / span 4;
	margin-bottom: 0.5em;
}

#edit-post-form .link-post-checkbox,
#edit-post-form .link-post-checkbox + label {
	grid-row: 1;
	grid-column: 6;
}
#edit-post-form .link-post-checkbox + label {
	margin: 0 0 0.5em 0.125em;
	border: none;
}
#edit-post-form .question-checkbox,
#edit-post-form .question-checkbox + label {
	grid-row: 3;
	grid-column: 5;
	justify-self: start;
	margin-left: 1.5em;
}

#edit-post-form .post-meta-fields input[type='checkbox'] {
	height: 0;
	opacity: 0;
	pointer-events: none;
}
#edit-post-form .post-meta-fields input[type='checkbox'] + label {
	white-space: nowrap;
	position: relative;
	cursor: pointer;
	padding: 0.25em 0.5em 0.25em calc(20px + 0.25em + 0.3725em);
	align-self: stretch;
	user-select: none;
}
#edit-post-form .post-meta-fields input[type='checkbox'] + label::before {
	content: "";
	font-family: var(--GW-Font-Awesome);
	font-size: 1.375rem;
	line-height: 0.7;
	text-indent: 1px;
	font-weight: 900;
	position: absolute;
	width: 20px;
	height: 20px;
	left: 5px;
}
#edit-post-form label[for='url'],
#edit-post-form input[name='url'] {
	display: none;
}
#edit-post-form .link-post-checkbox:checked ~ label[for='url'],
#edit-post-form .link-post-checkbox:checked ~ input[name='url'] {
	display: initial;
}
#edit-post-form label {
	line-height: normal;
	border: 1px solid transparent;
	text-align: right;
    padding: 0.25em 0.5em;
    white-space: nowrap;
}
#edit-post-form input[type='radio'] {
	width: 0;
	margin: 0;
	opacity: 0;
	pointer-events: none;

	font-weight: var(--GW-UI-font-weight-heavy);
}
#edit-post-form input[type='radio'] + label {
	border-width: 1px 1px 1px 0;
	cursor: pointer;
}
#edit-post-form input[type='radio']:checked + label {
	cursor: default;
}

#edit-post-form label[for='section'] {
	grid-row: 3;
}
#edit-post-form input[type='radio'] + label {
	grid-row: 3;
}
<?php fit_content("#edit-post-form input[type='radio'] + label"); ?>

#edit-post-form textarea {
	min-height: 24em;
}

#edit-post-form input[type='submit'],
.posting-controls .button.cancel-post-editing-button {
	padding: 6px 12px;
	float: right;
	line-height: normal;
}

.posting-controls:not(.comment-controls) #markdown-hints {
	top: calc(100% + 2em);
}

#edit-post-form button.guiedit div {
	overflow: visible;
}
.guiedit-mobile-auxiliary-button {
	display: none;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	#edit-post-form {
		padding-top: 1.5em;
		padding-bottom: 1.5em;
	}
}
@media only screen and (max-width: 840px) {
	#edit-post-form {
		padding-top: 1.25em;
		padding-bottom: 1.25em;
	}
}
@media only screen and (max-width: 720px) {
	#edit-post-form {
		padding-top: 1em;
		padding-bottom: 1em;
		margin: 0 0 1em 0;
	}
	#edit-post-form > div:last-of-type {
		display: flex;
		justify-content: center;
		clear: both;
	}
	#edit-post-form input[type='submit'],
	#edit-post-form .button.cancel-post-editing-button {
		font-size: 1.5rem;
		margin: 1.25rem 0.75em 0 0.75em;
		padding: 0.5em 0.75em;
	}
	#edit-post-form .button.cancel-post-editing-button {
		order: -1;
	}
}
@media only screen and (max-width: 640px) {
	#edit-post-form label[for='title'],
	#edit-post-form label[for='section'] {
		padding: 0.25em 0.5em 0.25em 0.25em;
	}
	#edit-post-form .post-meta-fields {
		grid-template-columns: 4.5em auto auto auto 1fr auto;
	}
}
@media only screen and (max-width: 520px) {
	.post-controls .edit-post-link {
		padding-top: 0.25em;
	}

	#content.edit-post-page {
		min-height: 100vh;
		grid-template-rows: auto auto 1fr auto;
	}
	#edit-post-form {
		height: 100%;
		display: flex;
		flex-flow: column;
		padding-top: 0.75em;
		padding-bottom: 0.75em;
	}
	#edit-post-form .textarea-container {
		 flex: 1 1 auto;
	}
	#edit-post-form textarea {
		min-height: unset;
		height: calc(100% - 1em);
		margin: 0;
	}
	#edit-post-form .textarea-container:focus-within textarea {
		height: calc(100% - 100px);
		top: 2px;
	}

	#edit-post-form label[for='link-post'],
	#edit-post-form label[for='question'] {
		font-family: var(--GW-Font-Awesome);
		font-weight: 900;
		line-height: 1.25;
	}
	#markdown-hints-checkbox,
	#markdown-hints-checkbox + label {
		display: none;
	}
}
@media only screen and (max-width: 420px) {
	#edit-post-form {
		padding-top: 0.5em;
		padding-bottom: 0.5em;
	}
	#edit-post-form .post-meta-fields {
		grid-template-columns: 4em auto auto auto 1fr auto;
	}
	#edit-post-form label[for='title'],
	#edit-post-form label[for='section'] {
		padding-right: 0.25em;
		padding-left: 0;
	}
	#edit-post-form label[for='question'] {
		grid-column: 5 / span 2;
		margin: 0;
	}
}