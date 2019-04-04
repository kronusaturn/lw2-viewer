/*********************/
/* TABLE OF CONTENTS */
/*********************/

.contents {
	float: right;
	min-width: 6em;
	max-width: 40%;
	margin: 1.25em 0 0.75em 1.25em;
	padding: 7px 14px 10px 10px;
	position: relative;
	z-index: 1;

	font-family: var(--GW-TOC-font);
	font-weight: var(--GW-TOC-heading-font-weight);
}

.contents-head {
	text-align: center;
	margin-bottom: 0.25em;
}

.post-body .contents ul {
	list-style-type: none;
	margin: 0 0 0 0.5em;
	counter-reset: toc-item-1 toc-item-2 toc-item-3;
	padding-left: 1em;
	font-size: 0.75em;
}
.post-body .contents li {
	margin: 0.15em 0 0.3em 1em;
	text-align: left;
	text-indent: -1em;
	line-height: 1.2;
	position: relative;
}
.post-body .contents li::before {
	position: absolute;
	width: 3em;
	display: block;
	text-align: right;
	left: -4.5em;
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
	}
}
@media only screen and (max-width: 520px) {
	.contents {
		max-width: 100%;
		margin: 1em auto 0 auto;
		display: table;
	}
	.contents-head {
		font-size: 1.2em;
	}
	div.post-body .contents ul {
		font-size: unset;
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
	margin: 1em 0 0.75em 0;
	text-align: left;

	font-family: var(--GW-content-headings-font);
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
	margin: 1em 0;
	padding-left: 0.5em;
	margin-left: 1px;
	padding-bottom: 3px;
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

#content figure {
	text-align: center;
	margin: 1.5em auto;
}

p.imgonly,
div.imgonly {
	text-align: center;
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
	top: -0.5em;
	left: 0.05em;
	font-size: 0.8em;
}
sub {
	top: 0.3em;
}

/*= Code blocks & other "unstyled" text. =*/

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
	display: inline-block;
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
}

h1 {
	margin: 0;
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

.mathjax-block-container {
	overflow-y: hidden;
	border-radius: 6px;
	margin: 1em 0 1.5em 0;
}
.mathjax-inline-container {
	max-width: 100%;
	display: inline-block;
	overflow-x: auto;
	overflow-y: hidden;
	position: relative;
	vertical-align: text-top;
	padding: 0 1px;
}
.post .mathjax-inline-container {
	line-height: 1.1;
	top: 2px;
}
.comment .mathjax-inline-container {
	top: 3px;
	line-height: 1;
}
.mathjax-inline-container .mjx-chtml {
	padding: 0;
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

