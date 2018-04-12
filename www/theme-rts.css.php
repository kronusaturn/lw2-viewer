/******************************/
/* READTHESEQUENCES.COM THEME */
/******************************/

body {
	font-family: Proxima Nova;
	background-color: #fffffa;
}
#content {
	box-shadow: none;
	background-color: transparent;
}

.active-bar {
	box-shadow: none;
	border-style: solid;
	border-color: #bbb;
	border-width: 1px 0;
}
a.nav-inner,
#nav-item-search button {
	font-weight: <?php echo ($platform == 'Mac') ? '200' : '300'; ?>;
}
.nav-bar a:link,
.nav-bar a:visited,
.nav-bar button {
	color: #888;
}
.nav-bar .nav-current,
.nav-bar .nav-inactive,
.nav-bar .nav-current + .nav-inactive,
.active-bar .nav-inactive:first-child {
	background-color: transparent;
	box-shadow: none;
	border: none;
}
.nav-inner::after {
	font-weight: <?php echo ($platform == 'Mac') ? '200' : '300'; ?>;
}
#bottom-bar {
	border-top: 1px solid #bbb;
	margin-top: 2em;
}
.post-page #bottom-bar {
	margin-top: 0.5em;
}
#content.no-nav-bars {
	margin: auto;
}
#content.no-comments #bottom-bar {
	margin-top: 0.125em;
}
.nav-bar .nav-item:not(:last-child) {
	border-right: 1px solid #bbb;
}
.nav-bar .nav-item:not(.nav-current):not(#nav-item-search):hover,
.inactive-bar #nav-item-search:focus-within {
	background-color: #f0f0eb;
}
.nav-bar a:hover,
.nav-bar a:focus,
.nav-bar button:hover,
.nav-bar button:focus {
	color: #333;
	text-shadow: 0px 0px 0.5px #333;
}
#bottom-bar .nav-item a::before,
#top-nav-bar a::before {
	font-size: 1em;
	bottom: -2px;
	color: #aaa;
}
#bottom-bar .nav-item a:hover::before,
#top-nav-bar a:hover::before {
	color: #333;
}
#bottom-bar #nav-item-first a::before,
#top-nav-bar a.nav-item-first::before {
	content: "\F0D9\F0D9";
}
#bottom-bar #nav-item-prev a::before,
#top-nav-bar a.nav-item-prev::before {
	content: "\F0D9";	
}
#bottom-bar #nav-item-top a::before {
	content: "\F0D8";
}
#bottom-bar #nav-item-next a::before,
#top-nav-bar a.nav-item-next::before {
	content: "\F0DA";
}
#bottom-bar #nav-item-next a::before {
	margin-left: -1em;
	left: 2.85em;
}

h1.listing {
	text-align: center;
}
h1.listing a[href^='/'] {
	font-family: Garamond Premier Pro;
	color: #690010;
	font-weight: 500;
	text-shadow: 0.5px 0.5px 0.5px #de7069;
	padding: 0 2px 1px 1px;
}
h1.listing a[href^='/']:hover {
	text-shadow: 
		0px 0px 0.5px #ff987b, 
		0px 0px 1.5px #c05651,
		0.5px 0.5px 0.5px #de7069;
	text-decoration: none;
}
.post-meta,
h1.listing + .post-meta {
	text-align: center;
	font-weight: <?php echo ($platform == 'Mac') ? '300' : '400'; ?>;
}
.post-meta > *,
.post-meta .author,
.post-meta .date,
.post-meta a:visited,
.comment-meta,
.comment-meta a {
	color: #999;
}
.post-meta a:hover,
.comment-meta a:hover,
.comment-meta .author:hover {
	color: #333;
	text-shadow: 0px 0px 0.5px #333;
	text-decoration: none;
}
.post-meta .vote,
.post-meta .qualified-linking label {
	color: #bbb;
}
.post-meta .upvote::before,
.comment-meta .upvote::before {
	content: "\F077";
}
.post-meta .downvote::before,
.comment-meta .downvote::before {
	content: "\F078";
	position: relative;
	left: -2px;
	top: 1px;
}
.upvote:hover,
.upvote.selected {
	text-shadow:
		0 0 0.5px #fff, 
		0 0 8px #0f0;
}
.downvote:hover,
.downvote.selected {
	text-shadow:
		0 0 0.5px #fff, 
		0 0 8px #f00;
}
.comment-meta {
	font-weight: <?php echo ($platform == 'Mac') ? '300' : '400'; ?>;
	padding-top: 5px;
	padding-bottom: 4px;
}
.comment-meta .author {
	font-size: 1.125em;
	color: #666;
	font-weight: 600;
}

h1.listing a[href^="http"] {
	font-size: 0.6875em;
	top: 3px;
	color: #888;
}
h1.listing a[href^="http"]:hover {
	color: #690010;
}

#width-selector button,
.theme-selector button {
	box-shadow: 
		0 0 0 4px #fffffa inset, 
		0 0 0 5px #bbb inset;
}
#width-selector button:hover,
#width-selector button.selected {
	box-shadow: 
		0 0 0 1px #bbb inset,
		0 0 0 4px #fffffa inset,
		0 0 0 5px #bbb inset;
}
.theme-selector button:hover,
.theme-selector button.selected {
	box-shadow: 
		0 0 0 1px #bbb inset,
		0 0 0 4px #fffffa inset,
		0 0 0 5px #bbb inset;
}

.page-toolbar a,
.new-post, 
.new-post:visited {
	color: #888;
	font-weight: <?php echo ($platform == 'Mac') ? '300' : '400'; ?>;
}
.new-post::before {
	opacity: 0.5;
}
.page-toolbar a:hover,
.new-post:hover {
	color: #333;
	text-shadow: 0px 0px 0.5px #333;
	text-decoration: none;
}

#top-nav-bar {
	margin: 0 0 -1.5em 0;
}
#top-nav-bar .page-number {
	font-family: Garamond Premier Pro;
}
#top-nav-bar .page-number-label {
	font-family: Proxima Nova;
}
#top-nav-bar::after {
	content: "";
	background-image: linear-gradient(to right, rgba(0, 0, 0, 0), rgba(0, 0, 0, 0.75), rgba(0, 0, 0, 0));
	display: block;
	width: 75%;
	margin: 0.125em auto 2.75em auto;
	height: 1px;
}

.post > h1:first-child {
	font-family: Garamond Premier Pro;
	font-weight: 600;
}

.post-body,
.comment-body {
	font-family: Garamond Premier Pro;
	font-weight: 500;
	line-height: 1.45;
}
.post-body {
	font-size: 1.375rem;
}
.comment-body {
	font-size: 1.25rem;
}

.post-body a,
.post-body a:visited {
	text-decoration: none;
	color: inherit;
}
.post-body a:link::after,
.post-body a:visited::after,
.comment-body a:link::after,
.comment-body a:visited::after {
    position: relative;
    content: "﻿°";
    margin-left: 2px;
    margin-right: 1px;
    color: #933;
}
.post-body a:hover {
    color: #999;
    border-bottom: 1px dotted #999;
}

.post-body hr {
	border: none;
	height: 1px;
	background-image: linear-gradient(to right, rgba(0, 0, 0, 0), rgba(0, 0, 0, 0.75), rgba(0, 0, 0, 0));
}

#inbox-indicator::before {
	color: #ddd;
}

#nav-item-search form::before {
	color: #999;
}

input[type='text'], 
input[type='search'], 
input[type='password'], 
textarea {
	background-color: transparent;
	border: 1px solid #ccc;
}

#quick-nav-ui a {
	background-color: transparent;
	box-shadow: 0 0 0 1px #ccc inset;
	color: #bbb;
}
#quick-nav-ui a:hover {
	background-color: #f0f0eb;
	color: #333;
	text-shadow: 0px 0px 0.5px #333;
}

#new-comment-nav-ui .new-comment-sequential-nav-button {
	color: #999;
}
#new-comment-nav-ui .new-comment-sequential-nav-button:disabled {
	color: #e6e6e6;
}
#new-comment-nav-ui .new-comments-count,
#new-comment-nav-ui .new-comments-count::after {
	color: #999;
	font-weight: normal;
}
#hns-date-picker input[type='text'] {
	border: 1px solid <?php echo ($platform == 'Mac') ? '#bbb' : '#aaa'; ?>;
	color: <?php echo ($platform == 'Mac') ? '#888' : '#666'; ?>;
}
#hns-date-picker input[type='text']:hover,
#hns-date-picker input[type='text']:focus {
	color: <?php echo ($platform == 'Mac') ? '#666' : '#444'; ?>;
}
#hns-date-picker span {
	color: <?php echo ($platform == 'Mac') ? '#aaa' : '#888'; ?>;
}
@media only screen and (max-width: 1200px) {
	#hns-date-picker {
		bottom: 61px;
		border-radius: 4px;
	}
	#hns-date-picker::before {
		content: none;
	}
}

.contents {
	background-color: transparent;
	border: none;
	margin-left: 1.5em;
	font-family: Garamond Premier Pro;
	min-width: unset;
}
.contents-head::after {
	content: "";
	background-image: linear-gradient(to right, rgba(0, 0, 0, 0), rgba(0, 0, 0, 0.75), rgba(0, 0, 0, 0));
	display: block;
	margin: 0 auto 0.5em auto;
	width: 75%;
	height: 1px;
}
.post-body .contents ul {
    margin: 0 0.5em;
    padding: 0 1em;
}
.contents a {
	color: #690010;
	font-weight: 600;
}
.contents a:hover {
	color: #690010;
	text-shadow: 
		0px 0px 0.5px #ff987b, 
		0px 0px 1px #c05651;
	text-decoration: none;
	border: none;
}
.contents a::after {
	display: none;
}
.post-body .contents li::before {
	font-feature-settings: 'onum';
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
	font-family: Garamond Premier Pro;
	margin: 1.5em 0 0.25em 0;
}
.post-body h4,
.comment-body h4 {
	font-size: 1.15em;
}
.post-body h3, 
.comment-body h3 {
	font-variant: small-caps;
	font-size: 1.3em;
}
.post-body h2, 
.comment-body h2 {
	font-style: italic;
	font-size: 1.5em;
}
.post-body h1,
.comment-body h1 {
	font-size: 1.9em;
	border: none;
}
.post-body h1 {
	text-align: center;
	margin: 1em 0 0 0;
}
.post-body h2 {
	margin: 1em 0 0 0;
}
.post-body h1::before {
	content: "❦";
	display: block;
	margin: 0 auto 1em auto;
	font-size: 0.625em;
	font-weight: normal;
}
.post-body h2::before {
	content: "☙";
	text-align: center;
	display: block;
	margin: 0 auto 0.5em auto;
	font-weight: normal;
	font-style: normal;
}
.post-body h1:first-child::before,
.post-body .contents + h1::before,
.post-body h2:first-child::before,
.post-body .contents + h2::before {
	content: "";
}

.post-page .post-meta::after {
	display: block;
	margin: 1em 0 0 0;
	font-size: 1.5rem;
}
.post-page .post-meta:first-of-type::after {
	content: "❦";
}
.post-page .post-meta:last-of-type::after {
	content: "❖";
	font-size: 1.75rem;
	margin: 0.625em 0 0 0;
}
.post-body {
	margin: 0;
}

.qualified-linking label:hover {
	color: #333;
	text-shadow: 0px 0px 0.5px #333;
}

#comments {
	border: none;
	box-shadow: none;
}
.comment-item {
	background-color: transparent !important;
	border: none;
	box-shadow: 
		0 0  3px #bbb,
		0 0  5px #bbb,
		0 0  7px #bbb,
		0 0 10px #bbb;
}
.comment-meta {
	border-bottom: 1px dotted #bbb;
}

#comments > .comment-controls {
	margin: 0;
}

button,
input[type='submit'] {
	color: #888;
}
button:hover,
input[type='submit']:hover {
	color: #333;
	text-shadow: 0px 0px 0.5px #333;
}
.comment + .comment-controls .action-button {
	font-weight: <?php echo ($platform == 'Mac') ? '300' : '400'; ?>;
}