<?php include("theme-grey.css.php"); ?>

/*********************/
/* ULTRAMODERN THEME */
/*********************/

body {
	font-family: Proxima Nova;
	font-weight: 300;
	background-color: #888;
	color: #444;
}
.post-meta > *,
.post-meta .author,
.post-meta .date,
.post-meta .comment-count,
.comment-meta > *,
.comment-meta .author,
.comment-meta .date { 
	color: #444;
}
.bottom-post-meta {
	border: none;
}
.vote {
	color: #666;
}
#content {
	box-shadow: none;
	background-color: transparent;
}
.post-body,
.comment-body,
.contents-head {
	font-family: Raleway;
	font-weight: 200;
	color: #000;
	text-shadow: 
		0px 0px 1px #777,
		0.5px 0.5px 1px #aaa,
		0.5px 0.5px 1px #bbb;
}
.post h1:first-child{
	font-family: Raleway;
	font-weight: 100;
	text-shadow: 
		0px 0px 1px #777,
		0.5px 0.5px 1px #aaa,
		0.5px 0.5px 1px #bbb;
}
.post-body h1, 
.post-body h2, 
.post-body h3, 
.post-body h4, 
.post-body h5, 
.post-body h6,
.comment-body h1, 
.comment-body h2, 
.comment-body h3, 
.comment-body h4, 
.comment-body h5, 
.comment-body h6 {
	font-family: Raleway;
	font-weight: 100;
	text-shadow: 
		0px 0px 1px #777,
		0.5px 0.5px 1px #aaa,
		0.5px 0.5px 1px #bbb;
}
h1 strong,
h2 strong,
h3 strong,
h4 strong,
h5 strong,
h6 strong {
	font-weight: normal;
}
.post-body h1 {
	padding-bottom: 2px;
	border-bottom-color: #777;
}
.contents {
	font-family: Raleway;
	border: none;
	background-color: transparent;
}
.post-body .contents a:link,
.post-body .contents a:visited {
	color: inherit;
}
#primary-bar > *,
#secondary-bar > *,
#bottom-bar > * {
	background-color: transparent;
	box-shadow: none;
	border: none;
}
#primary-bar, 
#secondary-bar {
	box-shadow: none;
}
#nav-item-top {
	box-shadow: none;
}
.nav-item a:link,
.nav-item a:visited,
.nav-inner {
	color: #444;
	font-weight: 300;
}
.nav-current .nav-inner {
	color: #ccc;
}
.nav-item a:hover {
	text-shadow: none;
	text-decoration: underline;
}
.nav-item:hover,
#bottom-bar a:hover {
	background-color: transparent !important;
}
.comment-item {
	background-color: transparent !important;
}
#theme-selector button {
	box-shadow:
		0 0 0 5px #888 inset;
}
#theme-selector button:hover,
#theme-selector button.selected {
	box-shadow:
		0 0 0 2px #888 inset,
		0 0 0 3px #ccc inset,
		0 0 0 5px #888 inset;
	text-decoration: none;
}
.theme-selector button:hover {
	text-decoration: none;
}
#width-selector button {
	box-shadow:
		0 0 0 4px #888 inset,
		0 0 0 5px #ccc inset;
}
#width-selector button:hover,
#width-selector button.selected {
	box-shadow:
		0 0 0 1px #888 inset,
		0 0 0 2px #ccc inset,
		0 0 0 4px #888 inset,
		0 0 0 5px #ccc inset;
}
#theme-tweaker-toggle button:hover {
	text-decoration: none;
}

input[type='text'], 
input[type='search'], 
input[type='password'], 
.posting-controls textarea {
	border: 1px solid #999;
    background-color: transparent;
    box-shadow: none;
}
input[type='text']:focus, 
input[type='search']:focus, 
input[type='password']:focus, 
.posting-controls textarea:focus,
.aligned-form label + input:focus {
	border: 1px solid #ccc;
    background-color: transparent;
    box-shadow: none;
}
.posting-controls textarea,
.posting-controls textarea:focus {
	border-top-width: 29px;
}
.guiedit-buttons-container {
    background-image: none;
    background-color: #888;
    box-shadow: 0 -1px 0 0 #999 inset;
}
.textarea-container:focus-within .guiedit-buttons-container {
    box-shadow: 0 -1px 0 0 #ccc inset;
}
button.guiedit {
    color: #444;
}
button.guiedit:hover {
    text-decoration: none;
    color: #ccc;
}
button.guiedit::after {
    font-family: Proxima Nova;
    font-weight: 300;
    color: #444;
    top: 2px;
    height: 25px;
}
#edit-post-form .link-post-checkbox + label::before {
    border: 1px solid #999;
}
#edit-post-form .link-post-checkbox + label:hover, 
#edit-post-form .link-post-checkbox:focus + label {
    text-shadow: none;
    text-decoration: underline;
}
#edit-post-form .link-post-checkbox + label:hover::before, 
#edit-post-form .link-post-checkbox:focus + label::before {
    border: 1px solid #ccc;
}

#edit-post-form input[type='radio'] + label {
	border-color: #999;
	color: #444;
}
#edit-post-form input[type='radio'] + label:hover,
#edit-post-form input[type='radio']:focus + label {
	background-color: #999;
	color: #000;
	box-shadow: none;
}
#edit-post-form input[type='radio']:focus + label {
	box-shadow: none;
}
#edit-post-form input[type='radio']:checked + label {
	background-color: transparent;
	border-color: #000;
	box-shadow: none;
	text-shadow: none;
}
#edit-post-form input[type='radio'][value='all']:checked + label {
    border: none;
    position: relative;
    top: 1px;
    box-shadow: 0 0 0 1px #000;
}

#edit-post-form input[type='submit'] {
    color: #444;
    border: none;
    background-color: transparent;
}
#edit-post-form input[type='submit']:hover {
    text-shadow: none;
    text-decoration: underline;
}

.posting-controls form span {
    padding: 5px 0 0 6px;
}

#markdown-hints-checkbox + label {
    color: #444;
}
#markdown-hints-checkbox + label:hover {
    color: #444;
    text-shadow: none;
    text-decoration: underline;
}
.markdown-hints {
    background-color: #888;
    border: 1px solid #ccc;
}
#nav-item-search input:focus {
	background-color: transparent;
	border: 1px solid #ccc;
}
#nav-item-search:focus-within,
.inactive-bar #nav-item-search:focus-within,
#nav-item-search.nav-current:focus-within {
	background-color: transparent;
}
#nav-item-search form::before {
	font-weight: 900;
}
.nav-bar a, 
#nav-item-search button, 
.button, 
.button:visited, 
.rss, 
.rss:visited {
	color: #444;
	font-weight: 300;
}
.nav-bar a:hover,
.nav-bar a:focus {
	text-shadow: none;
}
.button:hover {
	text-shadow: none;
	text-decoration: underline;
}

button:hover {
	text-shadow: none;
	color: unset;
	text-decoration: underline;
}

#comments > .comment-controls > button,
.comment + .comment-controls .action-button {
	font-weight: normal;
	color: #444;
}

#comments {
	border-top: 1px solid transparent;
	box-shadow: none;
}
.comment-meta .author {
	font-weight: 300;
	font-size: 1.125em;
}
#quick-nav-ui a  {
	color: #666;
	background-color: transparent;
	box-shadow: 0 0 0 1px #999;
}
#quick-nav-ui a:hover  {
	color: #444;
	background-color: transparent;
	box-shadow: 0 0 0 1px #ccc;
}
#quick-nav-ui a[href='#comments'].no-comments {
	color: #777;
}

h1.listing a[href^='/'] {
	font-family: Raleway;
	font-weight: 100;
	text-shadow: 
		0px 0px 1px #777,
		0.5px 0.5px 1px #aaa,
		0.5px 0.5px 1px #bbb;
}
h1.listing a:hover {
	background-color: transparent;
	color: #f60;
	text-shadow: 
		0px 0px 1px #777,
		0.5px 0.5px 1px #aaa,
		0.5px 0.5px 1px #bbb,
		0 0 1px #f60,
		0 0 2px #f60,
		0 0 3px #f60;
}

.comment-item {
	border-width: 0 0 0 1px;
	border-color: #666;
	box-shadow:
		1.5px 0 1.5px -1.5px #bbb inset, 
		1px 0 1px -1px #777 inset;
}
.new-comment::before {
	display: none;
}
.new-comment {
	border: 1px solid #e00;
	box-shadow: 
		0 0 1px #f00, 
		0 0 1px #f00 inset;
}
a.comment-parent-link::after {
	display: none;
}
a.comment-parent-link::before {
	padding: 2px 3px 0 4px;
}
a.comment-parent-link:hover::before {
	background-color: transparent;
}

#new-comment-nav-ui .new-comments-count {
	text-shadow: none;
}
#new-comment-nav-ui .new-comments-count::after {
	color: #666;
}

.post-body strong,
.comment-body strong {
	font-weight: 500;
}

.post-body a:link,
.comment-body a:link {
	color: inherit;
	text-shadow: 
		0px 0px 1px #bd5984, 
		0.5px 0.5px 1px #f68a84, 
		0.5px 0.5px 1px #ff9b8c;
}
.post-body a:visited,
.comment-body a:visited {
	color: inherit;
	text-shadow:
		0px 0px 1px #a766dd, 
		0.5px 0.5px 1px #d9f, 
		0.5px 0.5px 1px #efa9ff;
}
.post-body a:hover,
.comment-body a:hover {
	color: #f60;
	text-shadow:
		0px 0px 1px #bd5984, 
		0.5px 0.5px 1px #f68a84, 
		0.5px 0.5px 1px #ff9b8c,
		0px 0px 5px #f60;
	text-decoration: none;
}

blockquote {
	border-left-color: #777;
}

.comment-minimize-button {
	color: #777;
}

.new-comment-sequential-nav-button {
	color: #bbb;
}
.post-meta .new-comment-sequential-nav-button:disabled {
	color: #929292;
}

.comment-popup {
	background-color: #949494;
}

.archive-nav {
	border: 1px solid #ccc;
}
.archive-nav *[class^='archive-nav-item'] {
	background-color: transparent;
	border: none;
}
.archive-nav a:link,
.archive-nav a:visited {
	color: inherit;
}
.archive-nav span[class^="archive-nav-item"],
.archive-nav a:hover {
	font-weight: normal;
	background-color: transparent;
	box-shadow: 
		0 0 0 1px #ccc inset,
		0 0 0 2px #888 inset;
	text-shadow: none;
}
.archive-nav span.archive-nav-item-day,
.archive-nav a.archive-nav-item-day:hover {
	background-color: transparent;
}

.new-post::before {
	color: #555;
}

#content.user-page h1.page-main-heading {
	border-bottom: 1px solid #777;
}
.user-page .sublevel-nav a.sublevel-item,
.user-page .sublevel-nav span.sublevel-item {
	background-color: transparent;
	border-color: #999;
	color: #444;
	text-shadow: none;
}
.user-page .sublevel-nav a.sublevel-item:hover {
	background-color: #999;
	color: #000;
}
.user-page .sublevel-nav span.sublevel-item {
	border-color: #000;
	color: #000;
}
#content.user-page h1.page-main-heading {
	border-bottom: 1px solid #777;
}

#create-account-form-container {
	background-color: transparent;
	border: 1px solid #aaa;
}
.aligned-form input[type='submit'],
.aligned-form input[type='submit']:hover,
#signup-form input[type='submit'],
#signup-form input[type='submit']:hover {
	background-color: transparent;
	border: none;
	color: #444;
	text-shadow: none;
	font-weight: normal;
}
.aligned-form input[type='submit']:hover {
	text-decoration: underline;
}
.login-container h1 {
	font-weight: 300;
}

