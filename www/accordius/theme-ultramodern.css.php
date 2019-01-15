/*===================*/
/* ULTRAMODERN THEME */
/*===================*/

/*++++++*/
/* TAGS */
/*++++++*/

#tags {
    padding: 0.5em;
    align-items: center;
}
#tags::before {
    content: "\F02C";
    font-family: Font Awesome;
    font-weight: 400;
    font-size: 0.875em;
    display: inline-block;
    margin: 0 0.5em 0 0;
    opacity: 0.6;
}
#tags a {
	margin: 0 0.375em 0 0;
}
#tags a:not(:last-of-type)::after {
	content: ","
}
