/*********/
/* LINKS */
/*********/

a {
	color: var(--GW-hyperlink-color);
}
a:visited {
	color: var(--GW-hyperlink-visited-color);
}
a:hover,
a:focus {
	color: var(--GW-hyperlink-hover-color);
}
a:active {
	color: var(--GW-hyperlink-active-color);
}

/*********************/
/* TABLE OF CONTENTS */
/*********************/

.contents {
	float: right;
	min-width: 6em;
	max-width: 40%;
	margin: 0.25em 0 0.75em 1.5em;
	padding: 0.625em 0.75em 0.5em 0.5em;
	position: relative;
	z-index: 1;

	font-family: var(--GW-TOC-font);
}

.contents-head {
	text-align: center;
	margin: 0 0 0.375em 0;
	font-weight: var(--GW-TOC-heading-font-weight);
	line-height: 1;
}

.post-body .contents ul {
	list-style-type: none;
	margin: 0 0 0 0.5em;
	counter-reset: toc-item-1 toc-item-2 toc-item-3;
	font-size: 0.75em;
}
.post-body .contents li {
	margin: 0 0 0 1em;
	text-align: left;
	line-height: 1.2;
	position: relative;
}
.post-body .contents li::before {
	position: absolute;
	width: 3em;
	display: block;
	text-align: right;
	left: -3.25em;
	top: 0.15em;
}
.contents .toc-item-1 {
	counter-increment: toc-item-1;
	counter-reset: toc-item-2 toc-item-3;
}
.contents .toc-item-1::before {
	content: counter(toc-item-1);
}
.contents .toc-item-1 ~ .toc-item-2 {
	margin-left: 2.9em;
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
.contents .toc-item-2 ~ .toc-item-3,
.contents .toc-item-1 ~ .toc-item-3 {
	margin-left: 2.9em;
	font-size: 0.95em;
}
.contents .toc-item-1 ~ .toc-item-2 ~ .toc-item-3 {
	margin-left: 5.7em;
	font-size: 0.9em;
}
.contents .toc-item-3 {
	counter-increment: toc-item-3;
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
	content: counter(toc-item-3);
}
.contents .toc-item-4,
.contents .toc-item-5,
.contents .toc-item-6 {
	display: none;
}

.post-body .contents a {
	display: inline-block;
	text-indent: 0;
	width: 100%;
	padding: 0.15em 0.5em 0.15em 0.25em;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	.contents {
		float: none;
		display: table;
		max-width: none;
		margin-left: auto;
		margin-right: auto;
		margin-bottom: 1.5em;
		font-size: 1.25em;
	}
}
@media only screen and (max-width: 520px) {
	.contents {
		font-size: 1.5em;
	}
}

/************/
/* HEADINGS */
/************/

.body-text h1,
.body-text h2,
.body-text h3,
.body-text h4,
.body-text h5,
.body-text h6 {
	line-height: 1.1;
	margin: 1.5em 0 0.5em 0;
	text-align: left;

	font-family: var(--GW-content-headings-font);
	font-weight: var(--GW-content-headings-font-weight);
}

.post-body h5,
.post-body h6 {
	font-size: 1em;
}
.post-body h4 {
	font-size: 1.2em;
}
.post-body h3 {
	font-size: 1.4em;
}
.post-body h2 {
	font-size: 1.75em;
}
.post-body h1 {
	font-size: 2.1em;
}

.comment-body h5,
.comment-body h6 {
	font-size: 1em;
}
.comment-body h4 {
	font-size: 1.15em;
}
.comment-body h3 {
	font-size: 1.3em;
}
.comment-body h2 {
	font-size: 1.5em;
}
.comment-body h1 {
	font-size: 1.75em;
}

/**********/
/* QUOTES */
/**********/

blockquote,
.post-body .comment-box .comment-body {
	font-size: 0.9em;
	margin: 1em 0 1em 1px;
	padding: 0 0 0.1em 0.5em;
}
blockquote *:first-child {
	margin-top: 0;
}
blockquote *:last-child {
	margin-bottom: 0;
}
blockquote blockquote {
	font-size: 0.95em;
}

/* Pseudo-blockquotes that LW sometimes uses for some reason */

.post-body .comment-box .user-name {
	font-style: italic;
}
.post-body .comment-box .user-name::after {
	content: ":";
}
.post-body .comment-box {
	zoom: 1.25;
}

/**********/
/* IMAGES */
/**********/

#content img {
	max-width: 100%;
}

img.inline-latex {
	position: relative;
	top: 2.5px;
	margin: 0 2px;
}

#content figure,
#content .imgonly {
	margin: 1.5em auto;
}

figure img,
.imgonly img {
	display: block;
	margin: auto;
}

/**********/
/* TABLES */
/**********/

.body-text table {
	border-collapse: collapse;
	font-family: var(--GW-tables-font);
	font-size: 0.875em;
}
.body-text table th,
.body-text table td {
	text-align: left;
	padding: 4px 6px;
	line-height: 1.3;
}
.body-text table td:nth-of-type(n+2) {
	text-align: right;
}
.body-text table caption {
	margin: 0 0 0.25em 0;
	font-weight: var(--GW-UI-font-weight-heavy);
	font-size: 1.125em;
}

/********/
/* MISC */
/********/

/*= Superscripts & subscripts =*/

/*	Make sure superscripts and subscripts do not affect line spacing. */
sup, sub {
	vertical-align: baseline;
	position: relative;
	left: 0.05em;
	font-size: 0.8em;
}
sup {
	top: -0.5em;
}
sub {
	top: 0.3em;
}

/*= Code blocks & other “unstyled” text. =*/

pre,
code {
	font-family: var(--GW-monospaced-font);
}
pre {
	white-space: pre-wrap;
}
.body-text pre {
	text-align: left;
}
code {
	font-size: 0.95em;
	padding: 0 4px 1px 5px;
}
pre > code {
	display: block;
	border-radius: 0;
	padding: 3px 4px 5px 8px;
}

/*= Fractions =*/

.frac::after {
	content: "\200B";
}

/*	Remove "embossed" appearance of horizontal rules. */
hr {
	border: none;
	margin: 1em 0;
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
	font-weight: var(--GW-footnote-numbers-font-weight);
	text-align: right;
	width: 2em;
}

/*********/
/* LISTS */
/*********/

li {
	margin-bottom: 0.5em;
}

.body-text ol p,
.body-text ul p {
	margin: 0.5em 0;
}

.post-body ol {
	list-style: none;
	padding: 0;
	counter-reset: ol;
}
.post-body ol > li {
	position: relative;
	counter-increment: ol;
	padding: 0 0 0 2.5em;
	margin: 0.25em 0 0 0;
}
.post-body ol > li::before {
	content: counter(ol) ".";
	position: absolute;
	width: 2em;
	text-align: right;
	left: 0;
}
.post-body ul {
	list-style: none;
	padding: 0;
}
.post-body ul:not(.contents-list) > li {
	position: relative;
	padding: 0 0 0 1.75em;
	margin: 0.25em 0 0 0;
}
.post-body ul:not(.contents-list) > li ul > li {
	padding: 0 0 0 2em;
}
.post-body ul:not(.contents-list) > li::before {
	content: "•";
	position: absolute;
	width: 1.25em;
	text-align: right;
	left: 0;
}
.post-body ul:not(.contents-list) > li ul > li::before {
	width: 1.5em;
}
.post-body li > ul:first-child > li {
	padding-left: 0;
}
.post-body li > ul:first-child > li::before {
	content: none;
}

/********/
/* MATH */
/********/

.mathjax-inline-container {
	max-width: 100%;
	display: inline;
	overflow-x: auto;
	overflow-y: hidden;
	padding: 0 1px;
}

.mathjax-block-container .mjx-chtml {
	overflow-y: hidden;
	margin: 2em 0;
	padding: 0;
	display: block;
	padding: 0 0 8px 0;
}

/*	Fix for LessWrong being weird.
	*/
td.mathjax-inline-container,
th.mathjax-inline-container {
	display: table-cell;
}

/************/
/* SPOILERS */
/************/

.spoiler {
    color: #000;
    background-color: currentColor;
    transition: none;
    text-shadow: none;
	margin: 1em 0;
    box-shadow: 0 0 0 1px currentColor inset;
    overflow: auto;
}
.spoiler:not(:last-child) {
    margin-bottom: 0;
}
#content .spoiler * {
	color: inherit;
	border: none;
}
.spoiler:hover {
    color: unset;
    background-color: unset;
    text-shadow: unset;
    transition:
    	color 0.1s ease-out 0.1s,
    	background-color 0.1s ease-out 0.1s,
    	text-shadow 0.1s ease-out 0.1s;
}
.spoiler::selection,
.spoiler ::selection {
	color: #fff;
	background-color: #000;
}
.spoiler:not(:hover)::selection,
.spoiler:not(:hover) ::selection {
    background-color: transparent;
}

/*= Fix for LessWrong being weird =*/

.spoiler > p {
    padding: 0 7px;
}
.spoiler > p:first-child {
	margin-top: 0.25em;
}
.spoiler > p:last-child {
	margin-bottom: 0;
	padding-bottom: 0.25em;
}
.spoiler > p:hover ~ p {
	background-color: currentColor;
}
.spoiler > p + p {
	margin-top: -1em;
}
.spoiler > p:not(:first-child) {
	padding-top: 0.5em;
}
.spoiler > p:not(:last-child) {
	padding-bottom: 0.5em;
}

