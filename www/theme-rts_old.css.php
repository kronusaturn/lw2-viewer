h1.listing a[href^='/']:hover {
	text-decoration: none;
}
#nav-item-login .nav-inner {
	padding-top: 4px;
}
#inbox-indicator::before {
	top: 1px;
	color: #ddd;
}

#new-comment-nav-ui .new-comment-sequential-nav-button {
	color: #999;
}
#new-comment-nav-ui .new-comment-sequential-nav-button:disabled {
	color: #e6e6e6;
}
#new-comment-nav-ui .new-comments-count,
#new-comment-nav-ui .new-comments-count::after {
	color: #999;
	font-weight: normal;
}
#hns-date-picker input[type='text'] {
	border: 1px solid <?php echo ($platform == 'Mac') ? '#bbb' : '#aaa'; ?>;
	color: <?php echo ($platform == 'Mac') ? '#888' : '#666'; ?>;
}
#hns-date-picker input[type='text']:hover,
#hns-date-picker input[type='text']:focus {
	color: <?php echo ($platform == 'Mac') ? '#666' : '#444'; ?>;
}
#hns-date-picker span {
	color: <?php echo ($platform == 'Mac') ? '#aaa' : '#888'; ?>;
}
@media only screen and (max-width: 1200px) {
	#hns-date-picker {
		bottom: 61px;
		border-radius: 4px;
	}
	#hns-date-picker::before {
		content: none;
	}
}

.contents {
	background-color: transparent;
	border: none;
	margin-left: 1.5em;
	font-family: Garamond Premier Pro;
	min-width: unset;
}
.contents-head::after {
	content: "";
	background-image: linear-gradient(to right, rgba(0, 0, 0, 0), rgba(0, 0, 0, 0.75), rgba(0, 0, 0, 0));
	display: block;
	margin: 0 auto 0.5em auto;
	width: 75%;
	height: 1px;
}
.post-body .contents ul {
    margin: 0 0.5em;
    padding: 0 1em;
}
.contents a,
.contents a:visited {
	color: #690010;
	font-weight: 600;
}
.contents a:hover {
	color: #690010;
	text-shadow: 
		0px 0px 0.5px #ff987b, 
		0px 0px 1px #c05651;
	text-decoration: none;
	border: none;
}
.contents a::after {
	display: none;
}
.post-body .contents li::before {
	font-feature-settings: 'onum';
}





a.comment-parent-link::after {
	display: none;
}
a.comment-parent-link:hover::before {
	background-color: transparent;
	text-shadow: none;
	background-image: linear-gradient(to right, transparent 0%, #bbb 100%);
	background-repeat: no-repeat;
	box-shadow: 1px 0 0 0 #bbb;
}
.comment-item input[id^="expand"] ~ .comment-thread {
	max-height: 39px;
	padding-top: 5px;
}