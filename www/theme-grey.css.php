/**************/
/* GREY THEME */
/**************/

body {
    background: #eee;
}

#content {
    box-shadow: 0px 0px 10px #bbb;
}

h1.listing {
	margin: 0.6em 20px 0.1em 20px;
}
h1.listing a {
    font-size: 1.5rem;
    color: #f60;
}
h1.listing a:hover {
	overflow: hidden;
}
.listing a[href^='/'] {
	font-weight: normal;
}
h1.listing a[href^='http'] {
	font-size: 1.125rem;
	top: 5px;
	color: #999;
	opacity: 0.7;
}

h1.listing + .post-meta * {
    color: #222;
    font-size: 1em;
}

h1.listing + .post-meta .karma {
    float: left;
    margin-right: 3px;
}

h1.listing + .post-meta .karma::after {
    content: " by";
}

h1.listing + .post-meta .author {
    margin-right: 0;
}

h1.listing + .post-meta .date::before {
    content: "on ";
}

h1.listing + .post-meta .date::after {
    content: " — ";
	opacity: 0.5;
}

h1.listing + .post-meta .date {
    margin-right: 5px;
}

h1.listing + .post-meta .comment-count {
    margin-left: 0;
    margin-right: 4px;
}

.nav-item a:link,
.nav-item a:visited {
    font-weight: normal;
    color: #888;
}

.nav-inner::after {
    display: none;
}

#secondary-bar .nav-item {
    font-size: 0.875rem;
}

#primary-bar .nav-item {
    font-size: 0.875rem;
}

#nav-item-search form::before {
    opacity: 0.4;
	font-size: 0.9375rem;
}

.nav-bar a, 
#nav-item-search button, 
.button, 
.button:visited, 
.rss,
.rss:visited {
    color: #999;
}

a:hover {
    text-shadow: none;
}

.post > h1:first-child {
	margin: 1.1em 0 0.25em 0;
	font-weight: 400;
	color: #222;
	font-size: 3em;
}
.post-meta a,
.post-meta .author,
.post-meta .date {
	color: #222;
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
.comment-meta .lw2-link {
	opacity: 0.5;
}
.post-body a:link,
.comment-body a:link {
	color: #f60;
}
.post-body a:visited,
.comment-body a:visited {
	color: #ff943b;
}
.post-body,
.comment-body {
	font-family: Source Sans Pro, Trebuchet MS, Helvetica, Arial, Verdana, sans-serif;
	font-weight: 400;
}
@-moz-document url-prefix() {
	.post-body, .comment-body {
		font-weight: <?php global $platform; echo ($platform == 'Windows' ? '300' : '400'); ?>;
	}
}
.post-body {
	font-size: 1.1875rem;
	line-height: 1.6;
}
@media (-webkit-max-device-pixel-ratio: 1), (max-resolution: 191dpi) { 
	.post-body {
		font-size: 1.125rem;
	}
}
.comment-body {
	font-size: 1.125rem;
}
.comment-meta a {
	color: #222;
}
.comment-meta .author {
	color: #999;
	font-weight: 600;
}

button,
input[type='submit'] {
	color: #f60;
}
.guiedit-buttons-container button {
	color: #00e;
}
#markdown-hints-checkbox + label {
	color: #999;
}

#theme-selector button,
#width-selector button {
	box-shadow:
		0 0 0 4px #eee inset,
		0 0 0 5px #ccc inset;
}
#theme-selector button:hover,
#theme-selector button.selected,
#width-selector button:hover,
#width-selector button.selected {
	box-shadow:
		0 0 0 5px #ccc inset;
}
#quick-nav-ui a:hover  {
	color: #000;
	background-color: #d8d8d8;
}

@media only screen and (max-width: 1200px) {
	#hns-date-picker {
		background-color: #eee;
	}
	#hns-date-picker::before {
		border-color: #ccc;
	}
}

.new-comment::before {
	outline: 2px solid #9037ff;
	box-shadow:
		0 0 6px -2px #9037ff inset, 
		0 0 4px #9037ff, 
		0 0 6px #9037ff;
}

.archive-nav a:link,
.archive-nav a:visited {
	color: #888;
}
#content.search-results-page .post-meta .author, 
#content.user-page .post-meta .author {
	font-weight: normal;
}
#content.search-results-page h1.listing a[href^='http'],
#content.user-page h1.listing a[href^='http'] {
	top: 6px;
}

.contents {
	padding-right: 0.5em;
}
.post-body .contents a:link {
	color: #d64400;
}
.post-body .contents ul {
	font-size: 0.85em;
}


.post-body h1 {
	font-weight: 400;
}
.post-body h2 {
	font-weight: 400;
	border-bottom: 1px dotted #ccc;
}
.post-body h3 {
	font-weight: 400;
}
.post-body h4 {
	font-weight: 400;
}
.post-body h5 {
	font-weight: 400;
}
.post-body h6 {
	font-weight: 400;
}

.frac {
	padding-left: 2px;
}
.frac sup {
	position: relative;
	left: -1px;
}
.frac sub {
	position: relative;
	left: -0.5px;
}

.post-meta > * {
	margin: 0;
}
.post-meta .comment-count span,
.post-meta .read-time span,
.post-meta .word-count span,
.post-meta .lw2-link span {
	display: none;
}
.post-meta .comment-count::before,
.post-meta .read-time::before,
.post-meta .word-count::before,
.post-meta .lw2-link::before {
	font-family: Font Awesome;
	margin: 0 0.25em 0 0;
	font-size: 0.875em;
	color: #ccc;
}
.post-meta .comment-count {
	margin: 0 0.25em 0 0;
}
.post-meta .read-time,
.post-meta .word-count,
.post-meta .lw2-link {
	margin: 0 0.25em 0 0.5em;
}
.post-meta .lw2-link {
	opacity: 1;
}
.post-meta .comment-count:hover,
.post-meta .lw2-link:hover {
	text-decoration: none;
	text-shadow: 
		0 0 0.5px #fff,
		0 0 1px #fff,
		0 0 8px #777;
}
.post-meta .comment-count:hover::before,
.post-meta .lw2-link:hover::before {
	color: #777;
}
.post-meta .read-time:hover::before {
	color: #777;
	cursor: pointer;
}
.post-meta .comment-count::before {
	content: "\F086";
}
.post-meta .read-time::before {
	content: "\F017";
}
.post-meta .read-time::after {
	content: " min";
}
.post-meta .word-count::before {
	content: "\F15C";
}
.post-meta .word-count::after {
	content: "";
}
.post-meta .lw2-link::before {
	content: "\F0C1";
	font-weight: 900;
	opacity: 0.8;
	font-size: 0.75em;
	position: relative;
	bottom: 1px;
}

.post .post-meta .comment-count {
	margin: 0 0.5em;
}
.post .post-meta .lw2-link {
	margin: 0 1em 0 0.5em;
}
.post .post-meta .karma {
	margin: 0 0 0 0.5em;
}
