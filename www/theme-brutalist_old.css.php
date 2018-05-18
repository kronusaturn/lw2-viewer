#top-nav-bar a:hover {
	color: #777;
}
.rss:hover {
	text-decoration: dotted underline;
}
.nav-bar:nth-of-type(2) {
	border-bottom: 2px solid #000;
}
.post-meta {
	overflow: hidden;
}
h1.listing + .post-meta .post-section::before {
	left: -30px;
}
.comment-meta .author:hover {
	color: inherit;
	text-decoration: dotted underline;
}
a.comment-parent-link::after {
	display: none;
}
a.comment-parent-link::before {
	padding: 4px 3px 0 2px;
}

@media only screen and (min-width: 1201px) {
	#hns-date-picker {
		bottom: 75px;
		text-indent: -16px;
	}
	#hns-date-picker span {
		text-indent: 0px;
	}
}
@media only screen and (max-width: 1200px) {
	#hns-date-picker {
		bottom: 64px;
		background-color: #fff;
	}
	#hns-date-picker::before {
		width: 56%;
		border: 2px solid #000;
		border-width: 2px 0 2px 2px;
	}
}
#hns-date-picker input {
	width: 160px;
}


#theme-tweaker-toggle button:hover {
	text-decoration: none;
}
button.guiedit:hover {
	text-decoration: none;
}





.archive-nav {
	border: 2px solid #000;
}
.archive-nav *[class^='archive-nav-item'] {
	background-color: transparent;
	border: none;
	padding: 6px 4px 5px 7px;
}
.archive-nav a:link,
.archive-nav a:visited {
	color: inherit;
}
.archive-nav span[class^="archive-nav-item"],
.archive-nav a:hover {
	background-color: transparent;
	box-shadow: 
		0 0 0 1px #fff inset,
		0 0 0 3px #000 inset;
}
.archive-nav a:active {
	transform: none;
	box-shadow: 
		0 0 0 3px #fff inset,
		0 0 0 5px #000 inset;
}
.archive-nav span.archive-nav-item-day,
.archive-nav a.archive-nav-item-day:hover {
	background-color: transparent;
} 
.archive-nav .archive-nav-item-day:first-child,
.archive-nav a.archive-nav-item-day:first-child:hover {
	padding-left: 6px;
}


#edit-post-form input[name='title'] {
	max-width: calc(100% - 14em);
}
#edit-post-form input[type='radio'] {
	width: 0;
	margin: -5px;
	opacity: 0;
	pointer-events: none;
}
#edit-post-form input[type='radio'] + label {
	border-color: #000;
	color: #000;
}
#edit-post-form input[type='radio'][value='frontpage'] + label {
	border-radius: 0;
}
#edit-post-form input[type='radio'][value='drafts'] + label {
	border-radius: 0;
}
#edit-post-form input[type='radio'] + label:hover,
#edit-post-form input[type='radio']:focus + label {
	background-color: transparent;
	color: #000;
	box-shadow: 
		0 0 0 1px #fff inset,
		0 0 0 2px #000 inset;
}
#edit-post-form input[type='radio']:active + label {
	box-shadow: 
		0 0 0 2px #fff inset,
		0 0 0 3px #000 inset;
}
#edit-post-form input[type='radio']:focus + label {
	box-shadow: 
		0 0 0 1px #000;
}
#edit-post-form input[type='radio']:checked + label {
	background-color: transparent;
	border-color: #000;
	box-shadow: 
		0 0 0 1px #fff inset,
		0 0 0 2px #000 inset;
	text-shadow: none;
	font-weight: bold;
}

#edit-post-form .link-post-checkbox + label::before {
	color: #000;
	border-radius: 0;
	border: 1px solid #000;
	top: 2px;
}
#edit-post-form .link-post-checkbox + label:hover,
#edit-post-form .link-post-checkbox:focus + label {
	text-shadow: none;
}
#edit-post-form .link-post-checkbox + label:hover::before,
#edit-post-form .link-post-checkbox:focus + label::before {
	border-color: #000;
	box-shadow: 
		0 0 0 1px #fff inset,
		0 0 0 2px #000 inset;
}
#edit-post-form .link-post-checkbox:active + label::before {
	border-color: #000;
	box-shadow: 
		0 0 0 3px #fff inset,
		0 0 0 4px #000 inset;
}
#edit-post-form .link-post-checkbox:checked + label::before {
	content: "";
	background-color: #000;
	box-shadow: 
		0 0 0 4px #fff inset;
}

.posting-controls form span {
	padding: 4px 0 0 6px;
}
.markdown-hints {
	border: 2px solid #000;
	background-color: #fff;
}

.comment-controls .edit-button,
.comment-controls .edit-button:hover {
	color: inherit;
}

.post-meta .new-comment-sequential-nav-button:hover {
	text-decoration: none;
}


#create-account-form-container {
	background-color: transparent;
	border: 2px solid #000;
}
.aligned-form input[type='submit'],
#signup-form input[type='submit'] {
	background-color: transparent;
}
#signup-form label {
	width: 10em;
}
#signup-form input[type='text'],
#signup-form input[type='password'] {
	width: calc(100% - 12em);
}

