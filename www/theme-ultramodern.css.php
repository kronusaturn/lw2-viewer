<?php include("theme-grey.css.php"); ?>

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
.post-body h6 {
	font-family: Raleway;
	font-weight: 100;
	text-shadow: 
		0px 0px 1px #777,
		0.5px 0.5px 1px #aaa,
		0.5px 0.5px 1px #bbb;
}
.contents {
	font-family: Raleway;
	border: none;
	background-color: transparent;
}
.post-body .contents a:link {
	color: #f60;
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
#nav-item-search input {
	border: 1px solid #999;
	background-color: transparent;
}
#nav-item-search input:focus {
	border: 1px solid #ccc;
	background-color: transparent;
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
#comments {
	border-top: 1px solid transparent;
	box-shadow: none;
}
.comment-meta .author {
	font-weight: 300;
	font-size: 1.125em;
}
#bottom-bar a[href='#top']::after,
.post-meta a[href='#comments']::after,
.post-meta a[href='#bottom-bar']::after  {
	color: #666;
	background-color: transparent;
	box-shadow: 0 0 0 1px #999;
}
#bottom-bar a[href='#top']:hover::after,
.post-meta a[href='#comments']:hover::after,
.post-meta a[href='#bottom-bar']:hover::after  {
	color: #444;
	background-color: transparent;
	box-shadow: 0 0 0 1px #ccc;
}
h1.listing {
	font-family: Raleway;
}
h1.listing a[href^='/'] {
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