/*============*/
/* LESS THEME */
/*============*/

/*++++++*/
/* TAGS */
/*++++++*/

#tags {
	padding: 0.25em 0 0.5em 0;
	align-items: center;
}
#tags::before {
	content: "\F02C";
	font-family: Font Awesome;
	font-weight: 300;
	font-size: 0.875em;
	margin: 0 0.5em 0 0;
	opacity: 0.6;
	position: relative;
	top: -2px;
}
#tags a {
	margin: 0 0.375em 0 0;
}
#tags a:not(:last-of-type)::after {
	content: ","
}

.top-post-meta #tags {
	display: none;
}