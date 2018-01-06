<?php
	header ('Content-type: text/css; charset=utf-8');
?>

<?php echo file_get_contents('fa-custom.css'); ?>

html {
	box-sizing: border-box;
	font-size: 16px;
}
*, *::before, *::after {
	box-sizing: inherit;
}
body {
	background-color: #d8d8d8;
	padding: 0;
	margin: 0;
	font-family: 'Geometric 415';
}
input {
	font-family: inherit;
	font-size: inherit;
	font-weight: inherit;
	border: 1px solid #ddd;
}
#content {
	background-color: #fff;
	box-shadow: 0px 0px 10px #555;
	margin: 0 auto;
	padding: 0 30px;
	overflow: auto;
	max-width: 900px;
/* 	min-height: 100vh; */
	line-height: 1.55;
	position: relative;
}

.rss {
	position: absolute;
	vertical-align: top;
	font-size: 0.9em;
	right: 0.4em;
	line-height: 1.8;
}
.rss::before {
	content: url('data:image/svg+xml;base64,<?php echo base64_encode(file_get_contents("rss.svg")) ?>');
	display: inline-block;
	width: 1em;
	padding-right: 0.2em;
	position: relative;
	top: 1px;
}

/***********/
/* NAV BAR */
/***********/

.nav-bar {
	margin: 0 -30px;
}
.nav-bar {
	overflow: auto;
	display: table;
	width: calc(100% + 60px);
}
.nav-item {
	display: table-cell;
}
.nav-inner {
	padding: 12px 30px;
	font-weight: 500;
	font-size: 1.15em;
	text-align: center;
	display: block;
	position: relative;
}
#primary-bar.inactive-bar .nav-inner {
	padding-top: 11px;
	padding-bottom: 13px;
}
.nav-inner::after {
	position: absolute;
	left: 5px;
	top: -2px;
	content: attr(accesskey);
	font-weight: 400;
	font-size: 0.8em;
	color: #d6d6d6;
}
.nav-inner:hover::after {
	color: #bbb;
}
#secondary-bar .nav-inner {
	font-size: 0.95em;
	padding: 3px 0 4px 0;
}
#secondary-bar.active-bar .nav-inner {
	padding-top: 4px;
	padding-bottom: 4px;
}
#bottom-bar a {
	float: left;
	width: 100%;
}
.nav-bar a:link,
.nav-bar a:visited {
	color: #00e;
}
.nav-bar a:hover,
.nav-bar a:focus {
	text-decoration: none;
	text-shadow:
		0 0 1px #fff,
		0 0 3px #fff,
		0 0 5px #fff;
	outline: none;
}
.nav-bar .nav-item:not(.nav-current):not(#nav-item-search):hover {
	background-color: #ddd;
}
.inactive-bar .nav-item:not(.nav-current):not(#nav-item-search):hover {
	background-color: #d8d8d8;
}
#bottom-bar a:hover {
	background-color: #ddd;
}
#content > h1:first-child {
	margin-top: 2em;
}
#content > .post-meta:nth-last-child(2) {
	margin-bottom: 20px;
}

/* This makes the navbar items look like tabs: */

.nav-inactive {
	box-shadow: 
		 0 -1px #d8d8d8 inset,
		 1px 0 #fff inset;
}
.nav-current {
	border-right: 1px solid #d8d8d8;
	border-left: 1px solid #d8d8d8;
}
.nav-current:first-child {
	border-left: none;
}
.nav-current:last-child {
	border-right: none;
}
.nav-current + .nav-inactive,
.nav-inactive:first-child  {
	box-shadow: 0 -1px #d8d8d8 inset;
}

.inactive-bar .nav-inactive {
	background-color: #e4e4e4;
}
.active-bar .nav-inactive {
	background-color: #eee;
}
/* For Webkit: */
.active-bar {
	box-shadow: 0 -3px 8px -2px #ccc;
}

.active-bar .nav-inactive {
	box-shadow: 
		0 -4px 8px -4px #bbb inset,
		1px 0 #fff inset;
}
.active-bar .nav-inactive:first-child {
	box-shadow: 
		0 -4px 8px -4px #bbb inset;
}
.active-bar .nav-current + .nav-inactive {
	box-shadow: 
		4px -4px 8px -4px #bbb inset;
}
.active-bar .nav-item-last-before-current {
	box-shadow: 
		-4px -4px 8px -4px #bbb inset,
		1px 0 #fff inset;
}
.active-bar .nav-item-last-before-current:first-child {
	box-shadow: 
		-4px -4px 8px -4px #bbb inset;
}
/* And for Gecko: */
@-moz-document url-prefix() {
	.active-bar {
		box-shadow: 0 -3px 4px -2px #ccc;
	}

	.active-bar .nav-inactive {
		box-shadow: 
			0 -4px 4px -4px #bbb inset,
			1px 0 #fff inset;
	}
	.active-bar .nav-inactive:first-child {
		box-shadow: 
			0 -4px 4px -4px #bbb inset;
	}
	.active-bar .nav-current + .nav-inactive {
		box-shadow: 
			4px -4px 4px -4px #bbb inset;
	}
	.active-bar .nav-item-last-before-current {
		box-shadow: 
			-4px -4px 4px -4px #bbb inset,
			1px 0 #fff inset;
	}
	.active-bar .nav-item-last-before-current:first-child {
		box-shadow: 
			-4px -4px 4px -4px #bbb inset;
	}
}

#secondary-bar {
	table-layout: fixed;
}

/* Search tab */

#nav-item-search {
	width: 60%;
}
#secondary-bar #nav-item-search form {
	padding-top: 3px;
	padding-bottom: 4px;
}
#nav-item-search form::before {
	content: "";
	display: inline-block;
	vertical-align: top;
	background-image: url('data:image/svg+xml;base64,<?php echo base64_encode(file_get_contents("search.svg")) ?>');
	height: 23px;
	width: 23px;
	padding: 3px 3px 3px 3px;
	background-repeat: no-repeat;
	background-origin: content-box;
}
#nav-item-search input {
	height: 23px;
	width: calc(95% - 96px);
	padding: 1px 4px;
}
#nav-item-search input:focus {
	outline: none;
	background-color: #ffd;
	border: 1px solid #bbb;
}
#nav-item-search:focus-within {
	background-color: #ddd;
}
.inactive-bar #nav-item-search:focus-within {
	background-color: #d8d8d8;
}
#nav-item-search.nav-current:focus-within {
	background-color: #fff;
}
#nav-item-search button {
	color: #00e;
	border: none;
	background-color: transparent;
	font-family: inherit;
	font-size: inherit;
	font-weight: inherit;
	height: 21px;
	cursor: pointer;
}
#nav-item-search button:hover {
	text-shadow:
			0 0 1px #fff,
			0 0 3px #fff,
			0 0 5px #fff;
}
#nav-item-search.nav-current button:hover {
	text-shadow:
			0 0 1px #ddd,
			0 0 3px #ddd,
			0 0 5px #ddd;
}
#nav-item-search button:active,
#nav-item-search button:focus {
	color: #f00;
}

#nav-item-archive {
	width: 10%;
}

/*******************/
/* QUICKNAV WIDGET */
/*******************/

#bottom-bar a[href='#top']::after,
.post-meta a[href='#comments']::after,
.post-meta a[href='#bottom-bar']::after {
	color: #999;
	background-color: #e4e4e4;
	font-family: 'Font Awesome';
	font-weight: 900;
	font-size: 1.5rem;
	line-height: 1.7;
	display: block;
	position: fixed;
	top: unset;
	left: unset;
	right: calc((100vw - 900px) / 2 - 75px);
	width: 40px;
	height: 40px;
	border-radius: 4px;
}

#bottom-bar a[href='#top']::after {
	content: '\F106';
	bottom: 120px;
}
.post-meta a[href='#comments']::after {
	content: '\F036';
	bottom: 70px;
}
.post-meta a[href='#bottom-bar']::after {
	content: '\F107';
	bottom: 20px;
}

#bottom-bar a[href='#top']:hover::after,
.post-meta a[href='#comments']:hover::after,
.post-meta a[href='#bottom-bar']:hover::after  {
	color: #000;
	background-color: #eee;
}

.listing ~ #bottom-bar a[href='#top']::after {
	display: none;
}

/************/
/* ARCHIVES */
/************/

.archive-nav {
	margin: 1.25em 0.5em -1.25em;
	padding: 0.25em;
	border: 1px solid #aaa;
}

div[class^='archive-nav-'] {
	display: table;
	table-layout: fixed;
	width: 100%;
}

.archive-nav *[class^='archive-nav-item'] {
	border-style: solid;
	border-color: #ddd;
	border-width: 1px 0 1px 1px;
	background-color: #eee;
	display: table-cell;
	text-align: center;
	padding: 6px 4px 4px 4px;
	line-height: 1;
}
@-moz-document url-prefix() {
	.archive-nav *[class^='archive-nav-item'] {
		padding: 5px 4px;
	}
}
.archive-nav div[class^='archive-nav-']:nth-of-type(n+2) *[class^='archive-nav-item'] {
  border-top-width: 0;
}
.archive-nav *[class^='archive-nav-item']:last-child {
	border-right-width: 1px;
}
.archive-nav span[class^='archive-nav-item'] {
	font-weight: bold;
	background-color: #e0e0e0;
}
.archive-nav span[class^='archive-nav-item-day'] {
	background-color: #ddd;
}
.archive-nav-years .archive-nav-item-year:first-child {
	width: 6.5%;
}
.archive-nav-months .archive-nav-item-month:first-child {
	width: 7.5%;
}
.archive-nav-days .archive-nav-item-day {
	font-size: 0.8em;
	padding: 7px 0 5px 0;
}
.archive-nav-days .archive-nav-item-day:first-child {
	width: 4%;
}
a.archive-nav-item-day:nth-of-type(n+10) {
	letter-spacing: -1px;
	padding-right: 2px;
}
a.archive-nav-item-day:nth-of-type(n+20) {
	letter-spacing: normal;
	padding-right: 0;
	padding-left: 1px;
}
a.archive-nav-item-day:nth-of-type(21),
a.archive-nav-item-day:nth-of-type(31) {
	letter-spacing: -1px;
}

.archive-nav a:link, .archive-nav a:visited {
	color: rgba(0, 0, 238, 0.7);
}
.archive-nav a:hover {
	text-decoration: none;
	color: #00e;
	background-color: #e0e0e0;
}
.archive-nav a.archive-nav-item-day:hover {
	background-color: #ddd;
}

/************/
/* LISTINGS */
/************/

h1.listing {
	font-family: 'Geometric 415';
	font-size: 1.75rem;
	line-height: 1.15;
	margin: 0.8em 20px 0.1em 20px;
}
@media only screen and (min-width: 901px) {
	h1.listing {
		max-height: 1.15em;
	}
}
h1.listing:first-of-type {
	margin-top: 1.5em;
}
.listing + .post-meta {
	text-align: left;
	margin: 0 20px 0 21px;
}
h1.listing a {
	color: #00c;
	position: relative;
	padding-left: 38px;
}
@media only screen and (min-width: 901px) {
	h1.listing a {
		max-width: 100%;
		display: inline-block;
		white-space: nowrap;
		text-overflow: ellipsis;
		overflow: hidden;
		border-bottom: 1px solid transparent;
		-moz-hyphens: auto;
		hyphens: auto;
		z-index: 1;
	}
}
h1.listing a[href^="/"] {
	color: #000;
	padding-left: 0;
}
h1.listing a:hover {
	color: #4879ec;
	text-decoration: dotted underline;
	white-space: initial;
	overflow: visible;
	background-color: rgba(255,255,255,0.85);
}
<?php $margin_of_hover_error = '10px'; ?>
h1.listing a:hover::before {
	content: "";
	position: absolute;
	top: -<?php echo $margin_of_hover_error; ?>;
	right: -<?php echo $margin_of_hover_error; ?>;
	bottom: -<?php echo $margin_of_hover_error; ?>;
	left: -<?php echo $margin_of_hover_error; ?>;
	z-index: -1;
}
h1.listing a[href^="/"]:hover {
	color: #777;
}
h1.listing a::after { 
	content: url('data:image/svg+xml;base64,<?php echo base64_encode(file_get_contents("chain-link.svg")) ?>');
	width: 30px;
	position: absolute;
	top: 0px;
	left: 0px;
}
h1.listing a[href^="/"]::after {
	 content: none;
}

/******************/
/* SEARCH RESULTS */
/******************/

#content.search-results-page h1.listing,
#content.search-results-page .post-meta {
	text-align: left;
}
#content.search-results-page h1.listing {
	margin-left: 0;
	font-size: 1.5em;
}
#content.search-results-page .post-meta {
	margin-left: 2px;
	font-size: 0.875rem;
	opacity: 0.7;
}
#content.search-results-page .post-meta .author {
  font-weight: 500;
}

/*********************/
/* TABLE OF CONTENTS */
/*********************/

.contents {
	font-family: 'Geometric 415';
	border: 1px solid #ddd;
	background-color: #eee;
	float: right;
	min-width: 12em;
	max-width: 40%;
	margin: 1.25em 0 0.75em 1.25em;
	padding: 0.35em 0.35em 0.4em 0.85em;
	-webkit-hyphens: none;
	hyphens: none;
}

.contents-head {
	text-align: center;
	font-weight: bold;
	margin-bottom: 0.25em;
}

.contents ul {
	list-style-type: none;
	margin: 0;
	counter-reset: toc-item-1 toc-item-2 toc-item-3;
	padding-left: 1em;
	font-size: 0.75em;
}
.contents li {
	margin: 0.15em 0 0.3em 1em;
	text-align: left;
	text-indent: -1em;
	line-height: 1.2;
	position: relative;
}
.contents li::before {
	position: absolute;
	width: 3em;
	display: block;
	text-align: right;
	left: -4.5em;
	color: #999;
}
.contents .toc-item-1 {
	counter-increment: toc-item-1;
	counter-reset: toc-item-2 toc-item-3;
}
.contents .toc-item-1::before {
	content: counter(toc-item-1);
}
.contents .toc-item-1 ~ .toc-item-2 {
	margin-left: 3em;
	font-size: 0.95em;
}
.contents .toc-item-2 {
	counter-increment: toc-item-2;
	counter-reset: toc-item-3;
}
.contents .toc-item-1 ~ .toc-item-2::before {
	content: counter(toc-item-1) "." counter(toc-item-2);
}
.contents .toc-item-2::before {
	content: counter(toc-item-2);
}
.contents .toc-item-1 + .toc-item-3 {
	counter-increment: toc-item-2 toc-item-3;
}
.contents .toc-item-2 ~ .toc-item-3 {
	margin-left: 3em;
	font-size: 0.95em;
}
.contents .toc-item-1 ~ .toc-item-3 {
	margin-left: 6em;
	font-size: 0.9em;
}
.contents .toc-item-3 {
	counter-increment: toc-item-3;
	margin-left: 6em;
}
.contents .toc-item-1 ~ .toc-item-2 ~ .toc-item-3::before {
	content: counter(toc-item-1) "." counter(toc-item-2) "." counter(toc-item-3);
}
.contents .toc-item-1 ~ .toc-item-3::before {
	content: counter(toc-item-1) "." counter(toc-item-3);
}
.contents .toc-item-2 ~ .toc-item-3::before {
	content: counter(toc-item-2) "." counter(toc-item-3);
}
.contents .toc-item-3::before {
}

/********************/
/* POSTS & COMMENTS */
/********************/

.post-meta *,
.comment-meta * {
	display: inline-block;
	margin-right: 1em;
}
.comment-meta .comment-post-title {
	display: block;
}
.post-body, .comment-body {
	text-align: justify;
	-webkit-hyphens: auto;
	hyphens: auto;
}
.post-body p, .comment-body p {
	margin: 1em 0;
}
.post-meta .date {
	color: #888;
}

/*********/
/* POSTS */
/*********/

.post-meta {
	text-align: center;
}
.post-meta:last-child {
	margin-bottom: 40px;
}
.author {
	color: #090;
}
.post-body {
	min-height: 8em;
	font-family: Charter;
	padding: 0 30px;
	line-height: 1.5;
	font-size: 1.3rem;
	overflow: auto;
	margin: 0.5em 0 0 0;
}
.post > h1:first-child {
	margin: 1em 0 0.25em 0;
	padding: 0 30px;
	text-align: center;
	font-size: 2.5em;
	line-height: 1.1;
}

/************/
/* COMMENTS */
/************/

#comments {
	border-top: 1px solid #000;
}
#comments:empty::before {
	content: "No comments.";
	display: block;
	width: 100%;
	text-align: center;
	padding: 0.75em 0 0.9em 0;
	font-size: 1.2em;
}

.comment-item input[id^="expand"] {
	display: none;
}
.comment-item input[id^="expand"] + label {
	display: block;
	visibility: hidden;
	position: relative;
	margin: 8px 9px;
}
.comment-item input[id^="expand"] + label::after {
	content: "(Expand " attr(data-child-count) "  below)";
	visibility: visible;
	position: absolute;
	left: 0;
	white-space: nowrap;
	color: #00e;
	font-weight: 500;
}
.comment-item input[id^="expand"]:checked + label::after {
	content: "(Collapse " attr(data-child-count) "  below)";
}
.comment-item input[id^="expand"] + label:hover::after {
	color: #c00;
}
.comment-item input[id^="expand"] + label:active::after,
.comment-item input[id^="expand"] + label:focus::after{
	color: #c00;
}
.comment-item input[id^="expand"] ~ .comment-thread {
	max-height: 34px;
	overflow: hidden;
}
.comment-item input[id^="expand"] ~ .comment-thread > li:first-child {
	margin-top: 0;
}
.comment-item input[id^="expand"]:checked ~ .comment-thread {
	max-height: 1000000px;
}

<?php
	function nested_stuff($segment, $tip, $last_tip, $nesting_levels) {
		for ($i = $nesting_levels; $i > 0; $i--) {
			for ($j = $i; $j > 0; $j--)
				echo $segment;
			echo $tip;
		}
		echo $last_tip;
	}
?>

<?php nested_stuff(".comment-item .comment-item ", ".comment-item,\n", ".comment-item", 7); ?> {
	background-color: #eee;
}

<?php nested_stuff(".comment-item .comment-item ", ".comment-item .comment-item,\n", ".comment-item .comment-item", 7); ?> {
	background-color: #fff;
}

<?php nested_stuff(".comment-item ", ".comment-item:target,\n", ".comment-item:target", 13); ?> {
	background-color: #ffd;
}

ul.comment-thread {
	list-style-type: none;
	padding: 0;
}

.comment-item {
	border: 1px solid #ccc;
	margin: 2em 0 0 0;
}

.comment-item .comment-item {
	margin: 1em 8px 8px 16px;
}

.comment-item .comment-item + .comment-item {
	margin: 2em 8px 8px 16px;
}

.comment-meta {
	padding: 2px 10px;
	margin: 0 -1px;
	border: none;
}
.comment-meta div:first-child {
	font-weight: bold;
	font-size: 1.2em;
}

.comment-body {
	font-family: Charter;
	line-height: 1.45;
	font-size: 1.2rem;
	padding: 10px;
}

.comment-body ul {
	list-style-type: circle;
}
.comment-body > *:first-child {
	margin-top: 0;
}
.comment-body > *:last-child {
	margin-bottom: 0;
}

a.comment-parent-link {
	opacity: 0.5;
}
a.comment-parent-link:hover {
	opacity: 1.0;
}

a.comment-parent-link::before {
	content: "";
	font-family: "Font Awesome";
	font-weight: 900;
	font-size: 0.75rem;
	color: #bbb;
	line-height: 1;
	position: absolute;
	z-index: 1;
	display: block;
	padding: 3px 3px 0 3px;
	width: 16px;
	height: calc(100% + 2px);
	top: -1px;
	left: -17px;
	content: "\F062";
}
a.comment-parent-link:hover::before {
	background-color: #ffd;
	color: #999;
}
a.comment-parent-link::after {
	content: "";
	position: absolute;
	z-index: 0;
	display: block;
	width: 100%;
	height: 100%;
	top: 0;
	left: 0;
	pointer-events: none;
	box-shadow: 
		0 0 0 1px #ccc,
		-4px -4px 0 13px #ffd;
	overflow: hidden;
	visibility: hidden;
}
a.comment-parent-link:hover::after {
	visibility: visible;
}

/*******************************/
/* COMMENT OVERVIEW/NAVIGATION */
/*******************************/

#comments:not(:empty)::before {
	content: "Comment threads";
	display: block;
	position: fixed;
	width: calc((100% - 950px) / 2);
	top: 0.5em;
	left: 10px;
	font-weight: bold;
	border-color: #aaa;
	border-style: solid;
	border-width: 1px 1px 0 1px;
	padding: 2px 6px 0 6px;
	background-color: #fff;
}
#comments > .comment-thread > li > .comment > .comment-meta a:first-of-type::before {
	content: attr(title);
	display: block;
	position: fixed;
	width: calc((100% - 950px) / 2);
	border-color: #aaa;
	border-style: solid;
	border-width: 0 1px;
	padding: 0 6px;
	background-color: #fff;
	white-space: nowrap;
	text-overflow: ellipsis;
	overflow: hidden;
	left: 10px;
}
#comments > .comment-thread > li:hover > .comment > .comment-meta a:first-of-type::before {
	background-color: #ffd;
	box-shadow: 
		0 0 2px #ccc inset,
		0 0 4px #ccc inset;
}
#comments > .comment-thread > li > .comment > .comment-meta a:first-of-type:hover::before,
#comments > .comment-thread > li:target > .comment > .comment-meta a:first-of-type:hover::before {
	background-color: #fff02a;
	box-shadow: 
		0 0 2px #ccc inset,
		0 0 4px #ccc inset;
}
#comments > .comment-thread > li:last-child > .comment > .comment-meta a:first-of-type::before {
	border-width: 0 1px 1px 1px;
	padding-bottom: 2px;
}
#comments > .comment-thread > li:target > .comment > .comment-meta a:first-of-type::before {
	background-color: #ffb;
	box-shadow: 0 0 4px #ccc inset;
}
<?php
	$num_comment_threads = 20;
	$comment_links_offset = 0.6;
	$comment_link_height = 1.5;
	$comment_link_height_unit = 'em';
	
	$out = '';
	for ($i = 1; $i <= $num_comment_threads; $i++) {
		$out .= "#comments > .comment-thread > li:nth-child($i) > .comment > .comment-meta a:first-of-type::before {\n";
		$out .= "\ttop: ";
		$out .= $comment_links_offset + $i * $comment_link_height;
		$out .= $comment_link_height_unit;
		$out .= ";\n}\n";
	}
	
	echo $out;
?>
#comments .comment-thread > li {
	position: relative;
}
#comments .comment-meta a:first-of-type::after {
	content: "";
}
#comments .comment-meta a:first-of-type:hover::after {
	content: "";
	background-color: transparent;
	box-shadow: 
		  0 0 0 1px #ccc,
		  0 0 3px #ffde00,
		  0 0 5px #ffde00,
		  0 0 7px #ffde00,
		  0 0 10px #ffde00,
		  0 0 15px #ffde00;
	pointer-events: none;
	display: block;
	position: absolute;
	width: 100%;
	height: 100%;
	top: 0;
	left: 0;
}

#comments::after {
	content: "";
	background-color: #d8d8d8;
	display: block;
	position: fixed;
	height: 500px;
	height: calc(100vh - 1em);
	width: calc((100% - 950px) / 2);
	top: 0.5em;
	left: 10px;
	opacity: 0.8;
	pointer-events: none;
}
#comments:hover::after {
	opacity: 0;
}
#comments:active::after,
#comments:target::after,
#comments:focus-within::after {
	display: none;
}

/*********/
/* LINKS */
/*********/

a {
	text-decoration: none;
}
a:hover {
	text-decoration: underline;
}

/************/
/* HEADINGS */
/************/

.post-body h1,
.post-body h2,
.post-body h3,
.post-body h4 {
	font-family: 'Geometric 415';
	line-height: 1.1;
	margin: 1em 0 0.75em 0;
	text-align: left;
}

/********/
/* MISC */
/********/

blockquote {
	font-size: 0.9em;
	margin: 1em 0;
	padding-left: 0.5em;
	border-left: 5px solid #ccc;
	margin-left: 1px;
	padding-bottom: 3px;
}
blockquote *:first-child {
	margin-top: 0;
}
blockquote *:last-child {
	margin-bottom: 0;
}

.post-body img {
	max-width: 100%;
}

li {
	margin-bottom: 0.5em;
}

sup, sub {
	vertical-align: baseline;
	position: relative;
	top: -0.5em;
	left: 0.05em;
	font-size: 0.8em;
}
sub {
	top: 0.3em;
	hyphens: none;
}

hr {
	border: none;
	border-bottom: 1px solid #999;
}

/*************/
/* FOOTNOTES */
/*************/

ol {
	counter-reset: ordered-list;
}
.footnote-definition {
	font-size: 0.9em;
	list-style-type: none;
	counter-increment: ordered-list;
	position: relative;
}
.footnote-definition p {
	font-size: inherit !important;
}
.footnote-definition::before {
	content: counter(ordered-list) ".";
	position: absolute;
	left: -2.5em;
	font-weight: bold;
	text-align: right;
	width: 2em;
}

/*********/
/* LISTS */
/*********/

.post-body ol {
	list-style: none;
	padding: 0;
	counter-reset: ol;
}
.post-body ol li {
	position: relative;
	counter-increment: ol;
	overflow: hidden;
	padding-left: 2.5em;
	text-indent: -2.5em;
}
.post-body ol li::before {
	content: counter(ol) ".";
	display: inline-block;
	width: 2.5em;
	padding-right: 0.5em;
	text-align: right;
}
.post-body > ul {
	list-style: none;
	padding: 0;
}
.post-body > ul li {
	position: relative;
/* 	overflow: hidden; */
	padding-left: 2.5em;
	text-indent: -2.5em;
}
.post-body > ul li::before {
	content: "•";
	display: inline-block;
	width: 2.5em;
	padding-right: 0.4em;
	text-align: right;
}

/**********************/
/* FOR NARROW SCREENS */
/**********************/

@media only screen and (max-width: 900px) {
	#content {
		padding: 0 4px;
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
	.nav-bar {
		margin: 0 -4px;
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
		hyphens: none;
	}
	@-moz-document url-prefix() {
		.post-body, .comment-body {
			text-align: justify;
			-webkit-hyphens: auto;
			hyphens: auto;
		}
	}
  .nav-inner::after {
		display: none;
	}
}
@media only screen and (max-width: 520px) {
	.nav-inner,
	#secondary-bar .nav-inner {
		font-size: 0.85em;
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
	h1.listing a::after {
		left: -33px;
		top: 8px;
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
	}
	.post > h1:first-child {
		margin: 1em 0.25em 0.25em 0.25em;
		font-size: 2em;
	}
	.comment-item .comment-item {
		margin: 0.75em 4px 4px 12px;
	}
	.comment-item .comment-item + .comment-item {
		margin: 1.5em 4px 4px 12px;
	}
	.comment-body ul {
		padding-left: 30px;
	}
	.contents {
		max-width: 100%;
		margin: 1em 0 1.5em 0;
	}
	.contents-head {
		font-size: 1.2em;
	}
	.contents ul {
		font-size: 1em;
	}
}
@media only screen and (max-width: 374px) {
	.nav-bar .nav-inner {
		padding: 6px 3.33vw;
	}
	#nav-item-recent-comments > * > span {
		display: none;
	}
}
