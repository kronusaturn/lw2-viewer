body {
	background-color: #fff;
	font-family: Anonymous Pro;
}
#content {
	background-color: #fff;
	box-shadow: 0 2px 0 #000;
	border-style: solid;
	border-color: #000;
	border-width: 0 2px;
}
.post-body,
.comment-body {
	font-family: Input Sans;
	font-weight: 200;
}
.post-body {
	font-size: 1.125rem;
}
a:link,
a:visited {
	color: inherit;
}
.post-body a,
.comment-body a {
	border-bottom: 2px dotted #000;
}
.post-body a:hover,
.comment-body a:hover {
	text-decoration: none;
	color: #999;
}
.comment-body {
	font-size: 1rem;
	line-height: 1.5;
}
.post-body img,
.comment-body img {
	border: none;
}
.nav-bar {
	box-shadow: none;
}
a:hover {
	text-decoration: dotted underline;
}
#primary-bar .nav-item,
#primary-bar .nav-item:hover,
#secondary-bar .nav-item,
#secondary-bar .nav-item:hover,
#bottom-bar .nav-item,
#bottom-bar .nav-item:hover,
.nav-bar a:link,
.nav-bar a:visited,
.nav-bar a:hover {
	box-shadow: none;
	border: none;
	background-color: transparent !important;
	color: #000;
}
.nav-bar a:hover,
.rss:hover {
	box-shadow: none;
	border: none;
}
#top-nav-bar a:hover {
	color: #777;
}
.rss:hover {
	text-decoration: dotted underline;
}
#primary-bar .nav-inner::after {
	display: none;
}
.nav-bar:nth-of-type(2) {
	border-bottom: 2px solid #000;
}

.nav-current .nav-inner,
.nav-bar .nav-item:not(#nav-item-search) .nav-inner:hover {
	box-shadow: 
		0 0 0 4px #fff inset,
		0 0 0 6px #000 inset;
}
.nav-bar .nav-item:not(#nav-item-search) .nav-inner:active {
	box-shadow: 
		0 0 0 8px #fff inset,
		0 0 0 10px #000 inset;
}

.contents {
	border: 2px solid #000;
	background-color: transparent;
	font-family: Anonymous Pro;
}
.post-body .contents ul {
	font-size: 0.9375em;
}
.contents-head {
	font-size: 1.125em;
}
.contents ul a {
	border-bottom: 2px dotted #999;
}

.post-body h1, 
.post-body h2, 
.post-body h3, 
.post-body h4, 
.post-body h5, 
.post-body h6 {
	font-family: Anonymous Pro;
}
.post h1:first-child {
	font-family: Anonymous Pro;
	font-size: 2.25rem;
}
.post-body h1 {
	border-bottom: 2px solid #000;
}

h1.listing {
	font-size: 1.75rem;
}
h1.listing a {
	padding: 0 0 0 1.5px;
	color: #000;
}
h1.listing a[href^='/'] {
	font-family: Anonymous Pro;
}
h1.listing a[href^="http"] {
	font-size: 0.7em;
	top: 5px;
    color: #fff;
    text-shadow: 
         0.5px 0.5px 0 #000, 
        -0.5px -0.5px 0 #000,
         0 0 2px #000;
}
h1.listing a[href^="http"]:hover {
    color: #fff;
    text-shadow: 
         0.5px 0.5px 0 #000, 
        -0.5px -0.5px 0 #000,
         0 0 2px #000,
         0 0 3px #000;
}

#comments {
	border-top: 2px solid #000;
	box-shadow: none;
}

.post-meta > *,
.post-meta .author,
.post-meta .date,
.post-meta .comment-count,
.post-meta .lw2-link,
.comment-meta a,
.comment-meta .date,
.comment-meta .lw2-link,
.comment-meta .comment-parent-link {
	color: inherit;
}
.comment-meta .author:hover {
	color: inherit;
	text-decoration: dotted underline;
}

.comment-item {
	border: 2px solid #000;
}
.comment-item:not(:target) {
	background-color: #fff !important;
}
.comment-item:target {
	background-color: #eee !important;
}
.new-comment::before {
	display: none;
}
.new-comment {
	border: 2px dotted #000;
}

button,
.button,
input[type='submit'],
.rss,
.rss:visited,
.new-post,
.new-post:visited,
.edit-post,
.edit-post:visited,
#comments > .comment-controls .cancel-comment-button,
#markdown-hints-checkbox + label {
	color: inherit;
	text-shadow: none;
}
button:hover,
.button:hover,
#markdown-hints-checkbox + label:hover {
	color: inherit;
	text-shadow: none;
	text-decoration: dotted underline;
}
.rss::before {
	filter: grayscale(100%);
}

input[type='text'],
input[type='search'], 
input[type='password'] {
	border: 1px solid #000;
}
input[type='text']:focus,
input[type='search']:focus, 
input[type='password']:focus {
	border: 1px dashed #000 !important;
	background-color: transparent !important;
	box-shadow: none !important;
}
input[type='text'], 
input[type='search'], 
input[type='password'], 
textarea {
	background-color: #fff;
	color: #000;
}

input[type='submit'] {
	border: 2px solid #000 !important;
}
input[type='submit']:hover,
input[type='submit']:focus {
	text-shadow: none !important;
	background-color: transparent !important;
	color: inherit !important;
	box-shadow: 
		0 0 0 2px #fff inset,
		0 0 0 4px #000 inset;
}
input[type='submit']:active {
	transform: none;
	color: inherit !important;
	box-shadow: 
		0 0 0 4px #fff inset,
		0 0 0 6px #000 inset;
}

.button:active {
	transform: none;
}
#nav-item-search button:hover {
	color: #000;
	text-decoration: dotted underline;
}

#theme-selector button {
	box-shadow:
		0 0 0 5px #fff inset;
	padding: 0 0 0 1px;
}
#theme-selector button:hover,
#theme-selector button.selected {
	box-shadow:
		0 0 0 2px #fff inset,
		0 0 0 3px #000 inset,
		0 0 0 5px #fff inset;
	text-decoration: none;
}
#width-selector button {
	box-shadow:
		0 0 0 4px #fff inset,
		0 0 0 5px #000 inset;
}
#width-selector button:hover,
#width-selector button.selected {
	box-shadow:
		0 0 0 1px #fff inset,
		0 0 0 2px #000 inset,
		0 0 0 4px #fff inset,
		0 0 0 5px #000 inset;
}

a.comment-parent-link::after {
	display: none;
}
a.comment-parent-link::before {
	padding: 4px 3px 0 2px;
}
a.comment-parent-link:hover::before {
	background-color: transparent;
}

#quick-nav-ui a {
	border-radius: 0;
	color: #000;
	background-color: transparent;
	box-shadow: 
		0 0 0 1px #fff,
		0 0 0 3px #000;
}
#quick-nav-ui a:hover {
	background-color: transparent;
	box-shadow: 
		0 0 0 1px #fff inset,
		0 0 0 3px #000 inset,
		0 0 0 1px #fff,
		0 0 0 3px #000;
}
#quick-nav-ui a:active {
	transform: none;
	box-shadow: 
		0 0 0 3px #fff inset,
		0 0 0 5px #000 inset,
		0 0 0 1px #fff,
		0 0 0 3px #000;
}

#text-size-adjustment-ui button {
	color: #000;
}
#text-size-adjustment-ui button:hover {
	text-decoration: none;
}

#comments-view-mode-selector a {
	color: #000;
}

.guiedit-buttons-container {
	background-image: none;
	background-color: #000;
	color: #fff;
	box-shadow: 0 0 0 1px #000;
}
.posting-controls textarea {
	border: 2px solid #000;
	border-top-width: 28px;
	box-shadow: none;
	font-family: Input Sans;
	font-weight: 200;
	font-size: 1rem;
	line-height: 1.4;
}
.posting-controls textarea:focus {
	background-color: transparent;
	border: 2px dotted #000;
	border-top-width: 28px;
	box-shadow: none;
}
.comment-controls .cancel-comment-button,
#comments > .comment-controls .cancel-comment-button {
	height: 28px;
}
.comment-controls .cancel-comment-button,
.comment-controls .cancel-comment-button:hover {
	color: #fff;
	text-shadow: none;
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

.vote {
	color: #ddd;
}
.post-meta .vote:hover,
.post-meta .vote.selected,
.comment-meta .vote:hover,
.comment-meta .vote.selected {
	text-decoration: none;
	color: inherit;
}
.upvote::before {
	content: "\F0FE";
}
.downvote::before {
	content: "\F146";
}

.bottom-post-meta {
	border-top: 1px dotted #000;
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
#edit-post-form input[type='radio'][value='meta'] + label {
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
button.guiedit:hover {
	text-decoration: none;
}
button.guiedit::after {
	font-family: Anonymous Pro;
	color: #fff;
	top: 2px;
	height: 25px;
}

.comment-controls .edit-button,
.comment-controls .edit-button:hover {
	color: inherit;
}

.post-meta .new-comment-sequential-nav-button:hover {
	text-decoration: none;
}

.comment-popup .comment-body {
    font-size: 0.9375rem;
}
.comment-item-highlight,
.comment-item-highlight-faint {
    border: 2px solid #ccc;
    box-shadow: none;
}
.comment-popup {
    border: 2px solid #000;
    box-shadow: none;
}

.comment-minimize-button::after {
	font-family: Anonymous Pro;
	font-size: 0.8125rem;
	top: 20px;
	left: 0.5px;
}

code {
    border: 1px dotted #000;
    background-color: transparent;
    border-radius: 0;
}

#theme-tweaker-toggle button:hover {
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

#content.user-page h1.page-main-heading {
	border-bottom: 1px dotted #000;
}

.user-page .sublevel-nav .sublevel-item {
	border-color: #000;
	color: #000;
}
.user-page .sublevel-nav .sublevel-item:first-child,
.user-page .sublevel-nav .sublevel-item:last-child {
	border-radius: 0;
}
.user-page .sublevel-nav a.sublevel-item:hover {
	background-color: transparent;
	box-shadow: 
		0 0 0 2px #fff inset,
		0 0 0 3px #000 inset;
}
.user-page .sublevel-nav a.sublevel-item:active,
.user-page .sublevel-nav span.sublevel-item {
	background-color: transparent;
	border-color: #000;
	box-shadow: 
		0 0 0 2px #fff inset,
		0 0 0 3px #000 inset;
	text-shadow: none;
	font-weight: bold;
}
.user-page .sublevel-nav a.sublevel-item:active {
	box-shadow: 
		0 0 0 4px #fff inset,
		0 0 0 5px #000 inset;
	font-weight: normal;
}

blockquote {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("checkerboard_1px.gif")) ?>');
	background-size: 5px 2px;
	background-repeat: repeat-y;
	background-position: top left;
	padding-left: calc(0.5em + 5px);
	border-left-width: 0;
}