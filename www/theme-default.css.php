.post-meta {
	padding-right: 320px;
}
.post-meta > * {
	display: inline;
}
.post-meta .karma-value span,
.post-meta .comment-count span,
.post-meta .lw2-link span,
.post-meta .read-time span {
	display: none;
}
.post-meta .karma-value::before,
.post-meta .comment-count::before,
.post-meta .lw2-link::before,
.post-meta .read-time::before {
	color: #fff;
	font-family: Font Awesome;
	margin: 0 8px 0 0;
	box-shadow: 0 0 0 2px #ddd;
}
.post-meta .karma-value::before {
	content: "\F139";
	font-weight: 900;
}
.post-meta .comment-count::before {
	content: "\F086";
	font-weight: 900;
}
.post-meta .lw2-link::before {
	content: "\F0C1";
	font-weight: 900;
}
.post-meta .read-time::before {
	content: "\F2F2";
	font-weight: 900;
}
.post-meta .word-count::before {
	content: "\F15C";
	font-weight: 900;
	margin: 0 10px 0 0;
}

.post-meta .karma-value,
.post-meta .comment-count,
.post-meta .lw2-link,
.post-meta .read-time {
	border-radius: 4px;
	padding: 0 4px 0 2px;
	text-shadow: 0.5px 0.5px 0.5px #999;
	margin: 0 0.25em 0 0.5em;
}
.post-meta .karma-value {
	box-shadow: 
		23px 0 0 0 #ddd inset,
		0 0 0 3px #ddd;
	cursor: default;
	color: #c00;
}
.post-meta .comment-count {
	box-shadow: 
		25px 0 0 0 #ddd inset,
		0 0 0 3px #ddd;
	color: #009100;
}
.post-meta .comment-count:hover,
.post-meta .lw2-link:hover {
	text-decoration: none;
	color: #fff;
}
.post-meta .comment-count:hover {
	background-color: #009100;
}
.post-meta .lw2-link:hover {
	background-color: #00f;
}
.post-meta .comment-count:hover::before {
	color: #009100;
}
.post-meta .lw2-link:hover::before {
	color: #00f;
}

.post-meta .lw2-link {
	box-shadow: 
		23px 0 0 0 #ddd inset,
		0 0 0 3px #ddd;
}

.post-meta .read-time {
	box-shadow: 
		21px 0 0 0 #ddd inset,
		0 0 0 3px #ddd;
}
.post-meta .read-time::after {
	content: " min";
}
.post-meta .word-count {
	box-shadow: 
		22px 0 0 0 #ddd inset,
		0 0 0 3px #ddd;
	padding: 0 4px 0 4px;
}
.post-meta .read-time.word-count::after {
	content: none;
}
.post-meta .read-time::before {
	cursor: pointer;
}
.post-meta .read-time:hover::before {
	color: #777;
}

.post-meta .link-post-domain {
	margin: 0 0 0 0.5em;
}

h1.listing::after {
	content: "";
	display: block;
	height: 1px;
	width: 100%;
	background-color: #ddd;
	position: relative;
	top: 35px;
}
.post-meta .karma-value,
.post-meta .comment-count,
.post-meta .lw2-link,
.post-meta .read-time {
	position: absolute;
	line-height: 1.15;
	top: 12px;
}
.post-meta .karma-value {
	right: 31%;
}
.post-meta .comment-count {
	right: 22%;
}
.post-meta .read-time {
	right: 10%;
}
.post-meta .lw2-link {
	right: 0;
}
