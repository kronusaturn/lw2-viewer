/*=================*/
/* BRUTALIST THEME */
/*=================*/

/*++++++*/
/* TAGS */
/*++++++*/

#tags {
	padding: 0.5em;
}
#tags::before {
	content: "Tags:";
	margin: 0 0.25em 0 0;
	color: #888;
}
#tags a {
	border: 1px dotted #000;
	padding: 5px 7px 4px 7px;
	line-height: 1;
	margin: 0.25em;
	font-size: 0.9375em;
}
#tags a:hover {
	text-decoration: none;
	background-color: #fff;
	border-style: solid;
	box-shadow:
		0 0 0 2px #fff inset,
		0 0 0 3px #000 inset;
}
#tags a:active {
	box-shadow:
		0 0 0 4px #fff inset,
		0 0 0 5px #000 inset;
}
