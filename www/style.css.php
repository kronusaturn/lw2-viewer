<?php
	header ('Content-type: text/css; charset=utf-8');
?>

@import url('https://fonts.obormot.net?fonts=Charter,Geometric415'); 

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
//	content: url('data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIj8+DQo8IURPQ1RZUEUgc3ZnIFBVQkxJQyAiLS8vVzNDLy9EVEQgU1ZHIDEuMS8vRU4iICJodHRwOi8vd3d3LnczLm9yZy9HcmFwaGljcy9TVkcvMS4xL0RURC9zdmcxMS5kdGQiPiANCjxzdmcgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiB2ZXJzaW9uPSIxLjEiIGlkPSJSU1NpY29uIiB2aWV3Qm94PSIwIDAgMjU2IDI1NiI+DQo8ZGVmcz4NCjxsaW5lYXJHcmFkaWVudCB4MT0iMC4wODUiIHkxPSIwLjA4NSIgeDI9IjAuOTE1IiB5Mj0iMC45MTUiIGlkPSJSU1NnIj4NCjxzdG9wICBvZmZzZXQ9IjAuMCIgc3RvcC1jb2xvcj0iI0UzNzAyRCIvPjxzdG9wICBvZmZzZXQ9IjAuMTA3MSIgc3RvcC1jb2xvcj0iI0VBN0QzMSIvPg0KPHN0b3AgIG9mZnNldD0iMC4zNTAzIiBzdG9wLWNvbG9yPSIjRjY5NTM3Ii8+PHN0b3AgIG9mZnNldD0iMC41IiBzdG9wLWNvbG9yPSIjRkI5RTNBIi8+DQo8c3RvcCAgb2Zmc2V0PSIwLjcwMTYiIHN0b3AtY29sb3I9IiNFQTdDMzEiLz48c3RvcCAgb2Zmc2V0PSIwLjg4NjYiIHN0b3AtY29sb3I9IiNERTY0MkIiLz4NCjxzdG9wICBvZmZzZXQ9IjEuMCIgc3RvcC1jb2xvcj0iI0Q5NUIyOSIvPg0KPC9saW5lYXJHcmFkaWVudD4NCjwvZGVmcz4NCjxyZWN0IHdpZHRoPSIyNTYiIGhlaWdodD0iMjU2IiByeD0iNTUiIHJ5PSI1NSIgeD0iMCIgIHk9IjAiICBmaWxsPSIjQ0M1RDE1Ii8+DQo8cmVjdCB3aWR0aD0iMjQ2IiBoZWlnaHQ9IjI0NiIgcng9IjUwIiByeT0iNTAiIHg9IjUiICB5PSI1IiAgZmlsbD0iI0Y0OUM1MiIvPg0KPHJlY3Qgd2lkdGg9IjIzNiIgaGVpZ2h0PSIyMzYiIHJ4PSI0NyIgcnk9IjQ3IiB4PSIxMCIgeT0iMTAiIGZpbGw9InVybCgjUlNTZykiLz4NCjxjaXJjbGUgY3g9IjY4IiBjeT0iMTg5IiByPSIyNCIgZmlsbD0iI0ZGRiIvPg0KPHBhdGggZD0iTTE2MCAyMTNoLTM0YTgyIDgyIDAgMCAwIC04MiAtODJ2LTM0YTExNiAxMTYgMCAwIDEgMTE2IDExNnoiIGZpbGw9IiNGRkYiLz4NCjxwYXRoIGQ9Ik0xODQgMjEzQTE0MCAxNDAgMCAwIDAgNDQgNzMgViAzOGExNzUgMTc1IDAgMCAxIDE3NSAxNzV6IiBmaWxsPSIjRkZGIi8+DQo8L3N2Zz4NCg==');
	content: url('data:image/png;base64,<?php echo base64_encode(file_get_contents("rss.svg")) ?>');
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
	font-size: 1.1em;
	text-align: center;
	display: block;
}
#secondary-bar .nav-inner {
	font-size: 0.9em;
	padding: 3px 30px 4px 30px;
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
#nav-item-search form {
	padding: 2px 30px;
}
#nav-item-search form::before {
	content: "";
	display: inline-block;
	vertical-align: top;
//	background-image: url('data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0idXRmLTgiPz4NCjwhRE9DVFlQRSBzdmcgUFVCTElDICItLy9XM0MvL0RURCBTVkcgMS4xLy9FTiIgImh0dHA6Ly93d3cudzMub3JnL0dyYXBoaWNzL1NWRy8xLjEvRFREL3N2ZzExLmR0ZCI+DQo8c3ZnIHZlcnNpb249IjEuMSIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiB4bWxuczp4bGluaz0iaHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluayIgeD0iMHB4IiB5PSIwcHgiIHZpZXdCb3g9IjAgMCAxMDAwIDEwMDAiIGVuYWJsZS1iYWNrZ3JvdW5kPSJuZXcgMCAwIDEwMDAgMTAwMCIgeG1sOnNwYWNlPSJwcmVzZXJ2ZSI+DQo8Zz48cGF0aCBkPSJNOTYzLjEsODMzLjZMNzA0LjgsNTc1LjNjMCwwLDAsMC02LjEsMGMzMC43LTYxLjUsNDkuMi0xMjMsNDkuMi0xOTAuNmMwLTIwMi45LTE2Ni0zNjguOS0zNjguOS0zNjguOVMxMCwxODEuOCwxMCwzODQuN2MwLDIwMi45LDE2NiwzNjguOSwzNjguOSwzNjguOWM2Ny42LDAsMTM1LjMtMTguNCwxOTAuNi01NS4zbDAsMGwyNTguMiwyNTguMmMzNi45LDM2LjksOTIuMiwzNi45LDEyOS4xLDBDOTk5LjksOTI1LjgsOTk5LjksODcwLjUsOTYzLjEsODMzLjZ6IE0xMzMsMzg0LjdjMC0xMzUuMywxMTAuNy0yNDUuOSwyNDUuOS0yNDUuOWMxMzUuMywwLDI0NS45LDExMC43LDI0NS45LDI0NS45YzAsMTM1LjMtMTEwLjcsMjQ2LTI0NS45LDI0NkMyNDMuNyw2MzAuNywxMzMsNTIwLDEzMywzODQuN3oiLz48L2c+DQo8L3N2Zz4NCg==');
	background-image: url('data:image/png;base64,<?php echo base64_encode(file_get_contents("search.svg")) ?>');
	height: 23px;
	width: 23px;
	padding: 3px 3px 3px 3px;
	background-repeat: no-repeat;
	background-origin: content-box;
}
#nav-item-search input {
	height: 23px;
	width: calc(100% - 120px);
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

/************/
/* LISTINGS */
/************/

h1.listing {
	font-family: 'Geometric 415';
	font-weight: 500;
	font-size: 1.9rem;
	line-height: 1.1;
	margin: 1em 0.5em 0.2em 0.5em;
	text-align: center;
}
h1.listing a {
	color: #00c;
	position: relative;
}
h1.listing a[href^="/"] {
	color: #000;
}
h1.listing a:hover {
	color: #4879ec;
	border-bottom: 1px dotted #4879ec;
	text-decoration: none;
}
h1.listing a[href^="/"]:hover {
	color: #777;
	border-bottom: 1px dotted #777;
}
h1.listing a::after { 
//	content: url('data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiIHN0YW5kYWxvbmU9Im5vIj8+CjxzdmcgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiB2aWV3Qm94PSIwIDAgNDIzIDI5OCI+Cgk8ZGVmcyBpZD0iZGVmczQ4IiAvPgoJPGcgaWQ9IkxheWVyXzVfOTFfIiB0cmFuc2Zvcm09Im1hdHJpeCgwLDEsMSwwLC0yLjY0NzM4NzJlLTQsLTYyLjg2MjE0OSkiPgoJCTxwYXRoIGQ9Im0gMjYzLjY4MiwxMjIuMTI4IC0xMy45OTEsLTYuODA4IGMgLTQuNjkyLC0yLjI4NCAtMTAuMzYzLC0wLjMyNiAtMTIuNjQ3LDQuMzY2IEwgMTU0Ljk5NiwyODguMjggYyAtMi4yODIsNC42ODggLTAuMzI0LDEwLjM1OCA0LjM2OSwxMi42NDUgbCAxMy45ODksNi44MSBjIDQuNjg5LDIuMjggMTAuMzYxLDAuMzIyIDEyLjY0MywtNC4zNjQgbCA4Mi4wNDksLTE2OC41OTUgYyAyLjI4MywtNC42OTMgMC4zMjUsLTEwLjM2NCAtNC4zNjQsLTEyLjY0OCB6IiBpZD0icGF0aDEwIiAvPgoJCTxwYXRoIGQ9Im0gMTgwLjQ1LDE3OC45MzEgYyAxLjY4MywyLjUyMyA2LjYzNSw0LjU1IDguNTQ0LDAuNjE4IDMuOTQ5LC04LjEzMyAxMC40NjIsLTIxLjUzOCAxMy4yNjMsLTI3LjIzMyAyLjUwNSwtNS4wOTUgMC42NzMsLTEzLjM3IDAuMDY5LC0xNi44ODMgLTEuMDUxLC02LjEyMSAtMC43MTcsLTEyLjEyNCAxLjgwMSwtMTcuMjk4IGwgMjkuNzUsLTYxLjEyOSBjIDkuNywtMTkuOTMzIDQwLjg4NCwtMjQuNjExIDYwLjIzMywtMTUuMTkzIDAuMjMzLDAuMTEzIDAuNDYyLDAuMjQxIDAuNjk0LDAuMzU5IDAuMjM0LDAuMTExIDAuNDc5LDAuMjE1IDAuNzA5LDAuMzMgMTkuMjcxLDkuNTc4IDM0LjYwNCwzNy4xMzEgMjQuNzM2LDU2Ljk4MiBsIC0zMC4yNTQsNjAuODgxIGMgLTIuNTYsNS4xNTIgLTcuMTA4LDkuMDgyIC0xMi42MDEsMTEuOTg2IC0zLjE1LDEuNjY3IC0xMC44MjMsNS4yNjggLTEzLjMyOSwxMC4zNjIgLTIuODAxLDUuNjk1IC05LjQ0MSwxOS4wMzggLTEzLjQ3NSwyNy4xMyAtMS45NDcsMy45MTMgMi42ODIsNi42IDUuNzA1LDYuMzkxIDI2LjUyMSwtMS44MjMgNTQuNTQsLTE0LjM4IDY2Ljk0NSwtMzkuMzQ3IGwgMzAuMjU0LC02MC44OCBDIDM3MS40NjMsNzkuODUgMzUxLjY5OSwyOC45NjYgMzEyLjAzLDkuMjUzIDMxMS43NTEsOS4xMTQgMzExLjQ2OCw4Ljk4MyAzMTEuMTg3LDguODQ5IDMxMC45MDgsOC43MDggMzEwLjYzMiw4LjU2NSAzMTAuMzUxLDguNDI4IDI3MC41MjEsLTEwLjk1NiAyMTguMTU0LDQuNDU1IDIwMC40ODMsNDAuNzU5IGwgLTI5Ljc1LDYxLjEyOSBjIC0xMi4xOSwyNS4wNyAtNS4wMjgsNTQuOTI1IDkuNzE3LDc3LjA0MyB6IiBpZD0icGF0aDEyIiAvPgoJCTxwYXRoIGQ9Im0gMjQyLjU5NiwyNDQuMTA4IGMgLTEuNjg1LC0yLjUyMiAtNi42MzgsLTQuNTUxIC04LjU0NywtMC42MTYgLTMuOTQ2LDguMTMzIC0xMC40NjEsMjEuNTM3IC0xMy4yNjIsMjcuMjMxIC0yLjUwNiw1LjA5NiAtMC42NzQsMTMuMzY5IC0wLjA3LDE2Ljg4NCAxLjA1Myw2LjEyIDAuNzE5LDEyLjEyNCAtMS44MDEsMTcuMjk3IGwgLTI5Ljc1LDYxLjEyOSBjIC05LjcwMSwxOS45MzQgLTQwLjg4MywyNC42MTEgLTYwLjIzNCwxNS4xOTMgLTAuMjMzLC0wLjExMiAtMC40NjMsLTAuMjQgLTAuNjk1LC0wLjM1OCAtMC4yMzUsLTAuMTEgLTAuNDc4LC0wLjIxNiAtMC43MDksLTAuMzMgLTE5LjI3MywtOS41NzcgLTM0LjYwNCwtMzcuMTMgLTI0LjczOCwtNTYuOTgyIGwgMzAuMjU0LC02MC44ODEgYyAyLjU2MSwtNS4xNTEgNy4xMTEsLTkuMDgxIDEyLjYwMSwtMTEuOTg1IDMuMTUxLC0xLjY2NyAxMC44MjMsLTUuMjY4IDEzLjMyOSwtMTAuMzYgMi44MDEsLTUuNjk1IDkuNDQxLC0xOS4wMzcgMTMuNDczLC0yNy4xMzIgMS45NDksLTMuOTEzIC0yLjY4LC02LjU5OSAtNS43MDUsLTYuMzkxIC0yNi41MiwxLjgyNSAtNTQuNTM5LDE0LjM4IC02Ni45NDUsMzkuMzQ4IGwgLTMwLjI1NCw2MC44ODEgYyAtMTcuOTY4LDM2LjE1NSAxLjc5Niw4Ny4wNCA0MS40NjUsMTA2Ljc1MyAwLjI3OSwwLjE0IDAuNTYzLDAuMjcxIDAuODQzLDAuNDA0IDAuMjc4LDAuMTQxIDAuNTU1LDAuMjgzIDAuODM2LDAuNDIxIDM5LjgyOSwxOS4zODQgOTIuMTk4LDMuOTc0IDEwOS44NjgsLTMyLjMzMSBsIDI5Ljc0OCwtNjEuMTI5IGMgMTIuMTk4LC0yNS4wNzEgNS4wMzYsLTU0LjkyOCAtOS43MDcsLTc3LjA0NiB6IiBpZD0icGF0aDE0IiAvPgoJPC9nPgo8L3N2Zz4=');
	content: url('data:image/png;base64,<?php echo base64_encode(file_get_contents("search.svg")) ?>');
	width: 30px;
	position: absolute;
	bottom: 4px;
	right: -40px;
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
	float: left;
	min-width: 12em;
	max-width: 35%;
	margin: 1.625em 1.75em 0.75em -0.75em;
	padding: 0.5em;
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
.contents .toc-item-2 {
	counter-increment: toc-item-2;
	counter-reset: toc-item-3;
	margin-left: 3em;
}
.contents .toc-item-2::before {
	content: counter(toc-item-1) "." counter(toc-item-2);
}
.contents .toc-item-3 {unset
	counter-increment: toc-item-3;
	margin-left: 6em;
}
.contents .toc-item-3::before {
	content: counter(toc-item-1) "." counter(toc-item-2) "." counter(toc-item-3);
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
	hyphens: auto;
}

.post-body p, .comment-body p {
	margin: 1em 0;
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
}
.post > h1:first-child {
	margin: 1em 0 0.5em 0;
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
	content: "(Expand comments below)";
	visibility: visible;
	position: absolute;
	left: 0;
	white-space: nowrap;
	color: #00e;
	font-weight: 500;
}
.comment-item input[id^="expand"]:checked + label::after {
	content: "(Collapse comments below)";
}
.comment-item input[id^="expand"] + label:hover::after {
	color: #c00;
}
.comment-item input[id^="expand"] + label:active::after,
.comment-item input[id^="expand"] + label:focus::after{
	color: #c00;
}
.comment-item input[id^="expand"] ~ .comment-thread {
	max-height: 50px;
	overflow: hidden;
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

<?php nested_stuff(".comment-item .comment-item ", ".comment-item,\n", ".comment-item", 5); ?> {
	background-color: #eee;
}

<?php nested_stuff(".comment-item .comment-item ", ".comment-item .comment-item,\n", ".comment-item .comment-item", 5); ?> {
	background-color: #fff;
}

<?php nested_stuff(".comment-item ", ".comment-item:target,\n", ".comment-item:target", 9); ?> {
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
.comment-body *:first-child {
	margin-top: 0;
}
.comment-body *:last-child {
	margin-bottom: 0;
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
blockquote *:last-child: {
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
		padding: 8px 4px;
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
	.post-body {
		padding: 0 6px;
	}
	.post-body, .comment-body {
		text-align: left;
		hyphens: none;
	}
	@-moz-document url-prefix() {
		.post-body, .comment-body {
			text-align: justify;
			hyphens: auto;
		}
	}
}
