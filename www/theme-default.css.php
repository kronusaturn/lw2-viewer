<?php
	$UI_font = (($platform == 'Mac') ? "'Concourse', 'a_Avante'" : "'Whitney', 'a_Avante'") . ", Arial, sans-serif";
	$listings_font = (($platform == 'Mac') ? "'Concourse', 'a_Avante'" : "'Mundo Sans', 'a_Avante'") . ", Arial, sans-serif";
	$UI_font_smallcaps = (($platform == 'Mac') ? "'Concourse Smallcaps', 'a_Avante'" : "'Whitney Smallcaps', 'a_Avante'") . ", Arial, sans-serif";
	$text_font = "'Charter', 'Georgia', serif";
	$hyperlink_color = "#00e";
	$white_glow = "0 0 1px #fff, 0 0 3px #fff, 0 0 5px #fff";
?>

/*****************/
/* DEFAULT THEME */
/*****************/

body {
	color: #000;
	background-color: #d8d8d8;
	font-family: <?php echo $UI_font; ?>;
	font-feature-settings: 'ss07';
}
#content {
	background-color: #fff;
	box-shadow: 0px 0px 10px #555;
	line-height: 1.55;
}

/*=========*/
/* NAV BAR */
/*=========*/

.nav-inner {
	font-size: 1.375em;
	font-weight: 600;
}
#secondary-bar .nav-inner {
	font-size: 1em;
}

.nav-bar .nav-item:not(.nav-current):not(#nav-item-search):hover,
#bottom-bar a:hover,
#nav-item-search:not(.nav-current):focus-within {
	background-color: #ddd;
}
.inactive-bar .nav-item:not(.nav-current):not(#nav-item-search):hover,
.inactive-bar #nav-item-search:not(.nav-current):focus-within {
	background-color: #d8d8d8;
}

.nav-bar a:visited {
	color: <?php echo $hyperlink_color; ?>;
}
.nav-bar a:hover,
.nav-bar a:focus {
	text-decoration: none;
	text-shadow: <?php echo $white_glow; ?>;
}

#bottom-bar.decorative::before,
#bottom-bar.decorative::after {
	content: "GW";
	display: block;
	text-align: center;
	padding: 0.25em 0 1em 0;
}
#bottom-bar.decorative::before {
	width: 100%;
	color: transparent;
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/one_pixel_DDD.gif")) ?>');
	background-repeat: repeat-x;
	background-position: center 35%;
	margin: 0 30px;
}
#bottom-bar.decorative::after {
	color: #d8d8d8;
	position: absolute;
	left: 0;
	right: 0;
	margin: auto;
	background-color: #fff;
	padding-right: 4px;
	padding-left: 4px;
}
<?php foreach (["-moz-fit-content", "fit-content"] as $pvalue) echo 
"@supports (width: {$pvalue}) {
	#bottom-bar.decorative::after {
		width: {$pvalue};
	}
}
"; ?>

/* Accesskey hints */

.nav-inner::after {
	display: block;
	position: absolute;
	left: 5px;
	top: -2px;
	font-weight: 400;
	font-size: 0.7em;
	color: #d8d8d8;
}
.inactive-bar .nav-inner::after {
	color: #ccc;
}
.nav-inner:hover::after {
	color: #bbb;
}

/* This makes the navbar items look like tabs: */

.nav-inactive {
	box-shadow: 
		 0 -1px #d8d8d8 inset,
		 1px 0 #fff inset;
}
.nav-inactive:first-child {
	box-shadow: 0 -1px #d8d8d8 inset;
}
.inactive-bar .nav-inactive {
	background-color: #e4e4e4;
}
.active-bar .nav-inactive {
	background-color: #eee;
}
.active-bar {
	position: relative;
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
		5px -4px 8px -4px #bbb inset;
}
.active-bar .nav-item-last-before-current {
	box-shadow: 
		-5px -4px 8px -4px #bbb inset,
		1px 0 #fff inset;
}
.active-bar .nav-item-last-before-current:first-child {
	box-shadow: 
		-5px -4px 8px -4px #bbb inset;
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
			5px -4px 4px -4px #bbb inset;
	}
	.active-bar .nav-item-last-before-current {
		box-shadow: 
			-5px -4px 4px -4px #bbb inset,
			1px 0 #fff inset;
	}
	.active-bar .nav-item-last-before-current:first-child {
		box-shadow: 
			-5px -4px 4px -4px #bbb inset;
	}
}

/* Search tab */

#nav-item-search button {
	border: none;
	font-weight: inherit;
}

/*==============*/
/* PAGE TOOLBAR */
/*==============*/

.button.new-post:not(:hover),
.button.new-private-message:not(:hover) {
	color: #090;
}
.button.logout-button {
	color: #d33;
}

/*==============*/
/* SUBLEVEL NAV */
/*==============*/

.sublevel-nav .sublevel-item {
	background-color: #fff;
	border-color: #ddd;
	border-style: solid;
	border-width: 1px 1px 1px 0;
	color: #777;
}
.sublevel-nav .sublevel-item:first-child {
	border-radius: 8px 0 0 8px;
	border-width: 1px;
}
.sublevel-nav .sublevel-item:last-child {
	border-radius: 0 8px 8px 0;
}
.sublevel-nav a.sublevel-item:hover {
	background-color: #ddd;
	color: #000;
	text-decoration: none;
}
.sublevel-nav a.sublevel-item:active,
.sublevel-nav span.sublevel-item {
	background-color: #ddd;
	border-color: #ddd;
	color: #000;
	text-shadow: 
		0 -1px 0 #fff,
		0 0.5px 0.5px #000;
}

/*=====================*/
/* SORT ORDER SELECTOR */
/*=====================*/

.sublevel-nav.sort .sublevel-item {
	font-family: <?php echo $UI_font_smallcaps; ?>;
}
.sublevel-nav.sort {
	border: 2px solid #bbb;
	padding: 18px 0 0 0;
	border-radius: 8px;
	box-shadow: 0 18px #bbb inset;
}
.sublevel-nav.sort::before {
	text-transform: uppercase;
	font-weight: 600;
	color: #444;
	text-shadow: 0.5px 0.5px 0 #fff;
}
.sublevel-nav.sort .sublevel-item {
	border: 1px solid #aaa;
	padding: 6px 6px 4px 6px;
	text-transform: uppercase;
}

/* Vertical */
.sublevel-nav.sort .sublevel-item:first-child {
	border-radius: 6px 6px 0 0;
}
.sublevel-nav.sort .sublevel-item:last-child {
	border-radius: 0 0 6px 6px;
	border-width: 0 1px 1px 1px;
}

/* Horizontal */
.sublevel-nav.sort.horizontal .sublevel-item:first-child {
	border-radius: 6px 0 0 6px;
}
.sublevel-nav.sort.horizontal .sublevel-item:last-child {
	border-radius: 0 6px 6px 0;
	border-width: 1px 1px 1px 0;
}

.sublevel-nav.sort .sublevel-item:active {
	border-color: #aaa;
}

/*================*/
/* WIDTH SELECTOR */
/*================*/
/* THEME SELECTOR */
/*================*/

#width-selector button,
#theme-selector button {
	box-shadow:
		0 0 0 4px #d8d8d8 inset,
		0 0 0 5px #bbb inset;
}
#width-selector button:hover,
#width-selector button.selected,
#theme-selector button:hover,
#theme-selector button.selected {
	box-shadow:
		0 0 0 5px #bbb inset;
}

#theme-selector button::before {
	color: #999;
	background-color: #d8d8d8;
}
#theme-selector button:hover::before,
#theme-selector button.selected::before {
	color: #666;
}
#width-selector button::after {
	color: #999;
}

/*======================*/
/* THEME TWEAKER TOGGLE */
/*======================*/

#theme-tweaker-toggle button {
	color: #777;
}

/*=================*/
/* QUICKNAV WIDGET */
/*=================*/

#quick-nav-ui a {
	color: #999;
	background-color: #e4e4e4;
	border-radius: 4px;
	text-decoration: none;
}
#quick-nav-ui a[href='#bottom-bar'] {
	line-height: 1.8;
}
#quick-nav-ui a:active {
	transform: scale(0.9);
}
#quick-nav-ui a[href='#comments'].no-comments {
	opacity: 0.4;
	color: #bbb;
}
@media only screen and (hover:hover), not screen and (-moz-touch-enabled) {
	#quick-nav-ui a:hover {
		color: #000;
		background-color: #eee;
	}
	#quick-nav-ui a:focus:not(:hover) {
		transform: none;
		text-shadow: none;
	}
}

/*======================*/
/* NEW COMMENT QUICKNAV */
/*======================*/

#new-comment-nav-ui .new-comments-count {
	font-weight: 600;
	color: #666;
	text-shadow: 0.5px 0.5px 0 #fff;
}
#new-comment-nav-ui .new-comments-count::after {
	font-weight: 600;
	color: #777;
}
#new-comment-nav-ui .new-comment-sequential-nav-button:disabled {
	color: #bbb;
	text-shadow: none;
}
@media only screen and (hover:hover), not screen and (-moz-touch-enabled) {
	#new-comment-nav-ui .new-comments-count:hover {
		text-shadow: 
			0 0 1px #fff,
			0 0 3px #fff,
			0 0 5px #fff,
			0 0 8px #fff,
			0.5px 0.5px 0 #fff;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button:focus {
		color: #d00;
		text-shadow: 0 0 1px #fff, 0 0 3px #fff, 0 0 5px #fff;
	}
}

/*=================*/
/* HNS DATE PICKER */
/*=================*/

#hns-date-picker span {
	color: #777;
	text-shadow: 0.5px 0.5px 0 #fff;
	font-weight: 600;
}
#hns-date-picker input {
	border: 1px solid #777;
	background-color: transparent;
	color: #666;
	box-shadow: 0 0 0 1px transparent;
}
#hns-date-picker input:focus {
	color: #000;
}

/*======================*/
/* ANTI-KIBITZER TOGGLE */
/*======================*/

#anti-kibitzer-toggle button::before,
#anti-kibitzer-toggle button::after {
	background-color: #888;
	-webkit-background-clip: text;
	color: transparent;
	text-shadow: rgba(255,255,255,0.5) 0px 1px 1px;
}
#anti-kibitzer-toggle button:hover::before,
#anti-kibitzer-toggle button:hover::after {
	background-color: #444;
}

/*======================*/
/* TEXT SIZE ADJUSTMENT */
/*======================*/

#text-size-adjustment-ui button {
	color: #777;
}
#text-size-adjustment-ui button.default {
	font-weight: 600;
}
#text-size-adjustment-ui button:disabled:hover {
	text-shadow: none;
}
#text-size-adjustment-ui::after {
	color: #999;
}

/*=============================*/
/* COMMENTS VIEW MODE SELECTOR */
/*=============================*/

#comments-view-mode-selector a {
	color: #777;
}

/*==========*/
/* ARCHIVES */
/*==========*/

.archive-nav {
	border: 1px solid #aaa;
}
.archive-nav *[class^='archive-nav-item'] {
	border-style: solid;
	border-color: #ddd;
	border-width: 1px 0 1px 1px;
	background-color: #eee;
}
.archive-nav div[class^='archive-nav-']:nth-of-type(2) *[class^='archive-nav-item'] {
	border-top-width: 0;
	border-bottom-width: 0;
}
.archive-nav div[class^='archive-nav-']:last-of-type *[class^='archive-nav-item'] {
	border-bottom-width: 1px;
}
.archive-nav *[class^='archive-nav-item']:last-child {
	border-right-width: 1px;
}
.archive-nav span[class^='archive-nav-item'] {
	font-weight: bold;
	background-color: #ddd;
}

.archive-nav a:link,
.archive-nav a:visited {
	color: rgba(0, 0, 238, 0.7);
}
.archive-nav a:hover {
	text-decoration: none;
	color: #c00;
	background-color: #e0e0e0;
	text-shadow: <?php echo $white_glow; ?>;
}
.archive-nav a:active {
	transform: scale(0.9);
}
.archive-nav a:focus:not(:hover) {
	transform: none;
}
.archive-nav a.archive-nav-item-day:hover {
	background-color: #ddd;
}

/*==========*/
/* LISTINGS */
/*==========*/

h1.listing {
	font-family: <?php echo $listings_font; ?>, 'Font Awesome';
	font-weight: <?php echo ($platform == 'Mac') ? "700" : "800"; ?>;
	margin: 0.7em 20px 0 20px;
	max-width: calc(100% - 40px);
	top: <?php echo ($platform == 'Mac') ? "0" : "0.125em"; ?>; ;
}

h1.listing a {
	color: #000;
}
h1.listing a[href^="http"] {
	color: #00c;
}

@media only screen and (hover: hover), not screen and (-moz-touch-enabled) {
	h1.listing a:hover,
	h1.listing a:focus {
		color: #777;
		background-color: rgba(255,255,255,0.85);
	}	
	h1.listing:focus-within::before {
		color: #00f;
		left: -0.625em;
	}
	h1.listing a[href^="http"]:hover {
		color: #4879ec;
		text-shadow: 
			 0.5px 0.5px 0 #fff,
			 -0.5px -0.5px 0 #fff,
			 0 0 2px #fff,
			 0 0 3px #00c;
	}
}

/*===================*/
/* LISTING POST-META */
/*===================*/

h1.listing + .post-meta {
	padding-right: 330px;
}
h1.listing + .post-meta .karma-value,
h1.listing + .post-meta .comment-count,
h1.listing + .post-meta .lw2-link,
h1.listing + .post-meta .read-time {
	border-radius: 4px;
	padding: 0 4px 0 2px;
	text-shadow: 0.5px 0.5px 0.5px #999;
	margin: 0 0.25em 0 0.5em;
	position: absolute;
	line-height: 1.15;
	bottom: -6px;
}
h1.listing + .post-meta .karma-value span,
h1.listing + .post-meta .comment-count span,
h1.listing + .post-meta .lw2-link span,
h1.listing + .post-meta .read-time span {
	display: none;
}
h1.listing + .post-meta .karma-value::before,
h1.listing + .post-meta .comment-count::before,
h1.listing + .post-meta .lw2-link::before,
h1.listing + .post-meta .read-time::before {
	color: #fff;
	font-family: Font Awesome;
	font-weight: 900;
	margin: 0 8px 0 0;
	box-shadow: 0 0 0 2px #ddd;
}
_::-webkit-full-page-media, _:future, :root h1.listing + .post-meta .karma-value::before,
_::-webkit-full-page-media, _:future, :root h1.listing + .post-meta .comment-count::before,
_::-webkit-full-page-media, _:future, :root h1.listing + .post-meta .lw2-link::before,
_::-webkit-full-page-media, _:future, :root h1.listing + .post-meta .read-time::before {
	text-shadow: 0 0 3px #999;
}

h1.listing + .post-meta .karma {
	margin: 0;
}
h1.listing + .post-meta .karma-value {
	box-shadow: 
		22px 0 0 0 #ddd inset,
		0 0 0 3px #ddd;
	cursor: default;
	color: #c00;
	right: 264px;
}
h1.listing + .post-meta .karma-value::before {
	content: "\F139";
	text-shadow: none;
	font-size: 0.9375em;
	line-height: 1.3;
}

h1.listing + .post-meta .comment-count::before {
	content: "\F086";
}
h1.listing + .post-meta .comment-count {
	box-shadow: 
		25px 0 0 0 #ddd inset,
		0 0 0 3px #ddd;
	color: #009100;
	right: 176px;
}
h1.listing + .post-meta .comment-count:hover {
	text-decoration: none;
	color: #fff;
	background-color: #009100;
}
h1.listing + .post-meta .comment-count:hover::before {
	color: #009100;
}
h1.listing + .post-meta .comment-count.new-comments::before {
	color: #009100;
	text-shadow: 0.5px 0.5px 0.5px #fff;
}
h1.listing + .post-meta .comment-count.new-comments:hover::before {
	text-shadow: 0.5px 0.5px 0.5px #999;
}


h1.listing + .post-meta .lw2-link {
	box-shadow: 
		23px 0 0 0 #ddd inset,
		0 0 0 3px #ddd;
	right: 0;
}
h1.listing + .post-meta .lw2-link::before {
	content: "\F0C1";
}
h1.listing + .post-meta .lw2-link:hover {
	text-decoration: none;
	color: #fff;
	background-color: #00f;
}
h1.listing + .post-meta .lw2-link:hover::before {
	color: #00f;
}

h1.listing + .post-meta .read-time {
	box-shadow: 
		21px 0 0 0 #ddd inset,
		0 0 0 3px #ddd;
	right: 80px;
}
h1.listing + .post-meta .read-time::before {
	content: "\F2F2";
	cursor: pointer;
}
h1.listing + .post-meta .read-time::after {
	content: " min";
}
h1.listing + .post-meta .read-time:hover::before {
	color: #777;
}

h1.listing + .post-meta .word-count {
	box-shadow: 
		22px 0 0 0 #ddd inset,
		0 0 0 3px #ddd;
	padding: 0 4px 0 4px;
}
h1.listing + .post-meta .word-count::before {
	content: "\F15C";
	margin: 0 10px 0 0;
}
h1.listing + .post-meta .read-time.word-count::after {
	content: none;
}

h1.listing + .post-meta .link-post-domain {
	margin: 0 0 0 0.5em;
}

h1.listing + .post-meta::after {
	content: "";
	display: block;
	height: 1px;
	width: 100%;
	background-color: #ddd;
	position: absolute;
	bottom: -14px;
}

/*============*/
/* USER PAGES */
/*============*/

#content.user-page h1.page-main-heading {
	border-bottom: 1px solid #ccc;
}

#content.user-page h1.listing,
#content.user-page h1.listing + .post-meta {
	background-color: #eee;
	border-style: solid;
	border-color: #ccc;
}
#content.user-page h1.listing {
	padding: 0 6px;
	padding-top: <?php echo ($platform == 'Mac') ? "0.125em" : "0.25em"; ?>;
	border-width: 1px 1px 0 1px;
	margin: 1rem 0 0 0;
	max-width: 100%;
}
@media only screen and (hover: hover), not screen and (-moz-touch-enabled) {
	#content.user-page h1.listing a:hover,
	#content.user-page h1.listing a:focus {
		background-color: #eee;
	}
	#content.user-page h1.listing:focus-within::before {
		left: -0.625em;
	}
}
#content.user-page h1.listing + .post-meta {
	padding: 0.125em 6px 1em 36px;
	border-width: 0 1px 1px 1px;
	margin: 0 0 1rem 0;
}
#content.user-page h1.listing + .post-meta::after {
	display: none;
}
@media only screen and (min-width: 521px) {
	#content.user-page h1.listing + .post-meta .karma-value,
	#content.user-page h1.listing + .post-meta .comment-count,
	#content.user-page h1.listing + .post-meta .lw2-link,
	#content.user-page h1.listing + .post-meta .read-time {
		bottom: 10px;
	}
}
#content.user-page h1.listing + .post-meta .post-section::before {
	left: -1px;
}

#content.conversations-user-page h1.listing {
	padding: 4px 6px;
	font-size: 1.75rem;
}
#content.conversations-user-page h1.listing + .post-meta {
	padding: 6px 4px;
	margin: 0 0 0.25rem 0;
}

.user-stats .karma-total {
	font-weight: bold;
}

/*===============*/
/* CONVERSATIONS */
/*===============*/

/*============*/
/* LOGIN PAGE */
/*============*/

.login-container form input[type='submit'] {
	font-weight: bold;
	background-color: #eee;
	border: 1px solid #ccc;
}
.login-container form input[type='submit']:hover,
.login-container form input[type='submit']:focus {
	background-color: #ddd;
	border: 1px solid #aaa;
}

/* “Create account” form */

#signup-form {
	background-color: #f3f3f3;
	border: 1px solid #ddd;
}
#signup-form input[type='submit'] {
	background-color: #e4e4e4;
	border: 1px solid #ccc;
}
#signup-form input[type='submit']:hover {
	background-color: #d8d8d8;
	border: 1px solid #aaa;
}

/* Log in tip */

.login-container .login-tip {
	border: 1px solid #eee;
}

/* Message box */

.error-box {
	border: 1px solid red;
	background-color: #faa;
}
.success-box {
	border: 1px solid green;
	background-color: #afa;
}

/*=====================*/
/* PASSWORD RESET PAGE */
/*=====================*/

.reset-password-container input[type='submit'] {
	background-color: #e4e4e4;
	border: 1px solid #ccc;
	font-weight: bold;
}

/*===================*/
/* TABLE OF CONTENTS */
/*===================*/

.contents {
	font-family: <?php echo $UI_font; ?>;
	border: 1px solid #ddd;
	background-color: #eee;
}
.contents-head {
	font-weight: bold;
}
.post-body .contents li::before {
	color: #999;
	font-feature-settings: "tnum";
}

/*==================*/
/* POSTS & COMMENTS */
/*==================*/

.post-body,
.comment-body {
	font-family: <?php echo $text_font; ?>;
}

/*=======*/
/* POSTS */
/*=======*/

.post > h1:first-child {
	font-family: <?php echo $listings_font; ?>;
	font-weight: <?php echo ($platform == 'Mac') ? "700" : "800"; ?>;
}

/*===========*/
/* POST-META */
/*===========*/

.post-meta .post-section::before {
	color: #fff;
	text-shadow: 
		1px 1px 0 #090, 
		0 1px 0 #090, 
		0 0 5px #090;
}
a.post-section:hover {
	text-decoration: none;
}
a.post-section:hover::before {
	color: #97ff7c;
}
.post-meta .post-section.alignment-forum::before {
	text-shadow:
		1px 1px 0   #626dd7, 
		0   1px 0   #626dd7, 
		0   0   5px #626dd7;
}
a.post-section.alignment-forum:hover::before {
	color: #e6e5ff;
}
.post-meta .date {
	color: #888;
}
.post-meta .author {
	color: #090;
}
.bottom-post-meta {
	border-top: 1px solid #ddd;
}

/*============*/
/* LINK POSTS */
/*============*/

.post.link-post a.link-post-link {
	text-decoration: none;
	font-family: <?php echo $UI_font; ?>;
	font-weight: 600;
}
.post.link-post a.link-post-link:hover {
	color: #c00;
}
.post.link-post a.link-post-link:hover::before {
	color: #4879ec;
	text-shadow: 
		0.5px 0.5px 0 #fff,
		-0.5px -0.5px 0 #fff,
		0 0 2px #fff,
		0 0 3px #00c;
}

/*==========*/
/* COMMENTS */
/*==========*/

#comments {
	border-top: 1px solid #000;
	box-shadow: 0 3px 4px -4px #000 inset;
}
.comment-item {
	border: 1px solid #ccc;
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
	$comment_nesting_depth = 10;
?>

<?php nested_stuff(".comment-item .comment-item ", ".comment-item,\n", ".comment-item", $comment_nesting_depth); ?> {
	background-color: #eee;
}
<?php nested_stuff(".comment-item .comment-item ", ".comment-item a.comment-parent-link::after,\n", ".comment-item a.comment-parent-link::after", $comment_nesting_depth); ?> {
	box-shadow: 
		0 28px 16px -16px #fff inset,
		4px 16px 0 12px #ffd inset,
		4px	4px 0 12px #ffd inset;
}

<?php nested_stuff(".comment-item .comment-item ", ".comment-item .comment-item,\n", ".comment-item .comment-item", $comment_nesting_depth); ?> {
	background-color: #fff;
}
<?php nested_stuff(".comment-item .comment-item ", ".comment-item .comment-item a.comment-parent-link::after,\n", ".comment-item .comment-item a.comment-parent-link::after", $comment_nesting_depth); ?> {
	box-shadow: 
		0 28px 16px -16px #eee inset,
		4px 16px 0 12px #ffd inset,
		4px	4px 0 12px #ffd inset;
}

<?php nested_stuff(".comment-item ", ".comment-item:target,\n", ".comment-item:target", (2 * $comment_nesting_depth) - 1); ?> {
	background-color: #ffd;
}
.comment-item:target > .comment-thread > .comment-item > .comment > .comment-meta > a.comment-parent-link::after {
	box-shadow: 
		0 28px 16px -16px #ffd inset,
		4px 16px 0 12px #ffd inset,
		4px	4px 0 12px #ffd inset !important;
}

/*================================*/
/* DEEP COMMENT THREAD COLLAPSING */
/*================================*/

.comment-item input[id^="expand"] + label::after {
	color: <?php echo $hyperlink_color; ?>;
	font-weight: 600;
}
.comment-item input[id^="expand"] + label:hover::after {
	color: #c00;
}
.comment-item input[id^="expand"] + label:active::after,
.comment-item input[id^="expand"] + label:focus::after{
	color: #c00;
}
.comment-item input[id^="expand"]:checked ~ .comment-thread .comment-thread .comment-item {
	border-width: 1px 0 0 0;
}

/*==============*/
/* COMMENT-META */
/*==============*/

.comment-meta .author {
	font-weight: bold;
	font-size: 1.25em;
	color: #000;
}
.comment-meta .author:hover {
	text-decoration: none;
	color: #090;
}

/*====================*/
/* ANTI-KIBITZER MODE */
/*====================*/

.author.redacted,
.comment-in-reply-to a.redacted {
	opacity: 0.4;
	font-weight: 400;
}

.karma-value.redacted {
	opacity: 0.4;
}

.link-post-domain.redacted {
	opacity: 0.4;
}

/*===========================*/
/* COMMENT THREAD NAVIGATION */
/*===========================*/

a.comment-parent-link::before {
	color: #bbb;
}
a.comment-parent-link:hover::before {
	background-color: #ffd;
	color: #999;
}

div.comment-child-links {
	font-weight: 600;
}
div.comment-child-links a {
	font-weight: normal;
}
div.comment-child-links a::first-letter {
	color: #aaa;
}

.comment-item-highlight {
	box-shadow:
		0 0	2px #e7b200,
		0 0	3px #e7b200,
		0 0	5px #e7b200,
		0 0	7px #e7b200,
		0 0 10px #e7b200;
	border: 1px solid #e7b200;
}
.comment-item-highlight-faint {
	box-shadow:
		0 0	2px #f8e7b5,
		0 0	3px #f8e7b5,
		0 0	5px #f8e7b5,
		0 0	7px #f8e7b5,
		0 0 10px #f8e7b5;
	border: 1px solid #f8e7b5;
}

.comment-popup {
	background-color: #fff;
}

/*=======================*/
/* COMMENTS COMPACT VIEW */
/*=======================*/

#comments-list-mode-selector button {
	box-shadow:
		0 0 0 4px #fff inset,
		0 0 0 5px #bbb inset;
}
#comments-list-mode-selector button:hover,
#comments-list-mode-selector button.selected {
	box-shadow:
		0 0 0 5px #bbb inset;
}
#content.compact > .comment-thread .comment-item::after {
	color: <?php echo $hyperlink_color; ?>;
	background: linear-gradient(to right, transparent 0%, #fff 50%, #fff 100%);
}

@media only screen and (hover: hover), not screen and (-moz-touch-enabled) {
	#content.compact > .comment-thread .comment-item:hover .comment {
		background-color: #fff;
		outline: 3px solid #00c;
	}
	#content.compact > .comment-thread .comment-item:hover .comment::before {
		background-color: #fff;
		box-shadow: 
			0 0  3px #fff,
			0 0  5px #fff,
			0 0  7px #fff,
			0 0 10px #fff,
			0 0 20px #fff,
			0 0 30px #fff,
			0 0 40px #fff;
	}
}
@media only screen and (hover: none), only screen and (-moz-touch-enabled) {
	#content.compact > .comment-thread.expanded .comment-item .comment {
		background-color: #fff;
		outline: 3px solid #00c;
	}
	#content.compact > .comment-thread.expanded .comment-item .comment::before {
		background-color: #fff;
		box-shadow: 
			0 0  3px #fff,
			0 0  5px #fff,
			0 0  7px #fff,
			0 0 10px #fff,
			0 0 20px #fff,
			0 0 30px #fff,
			0 0 40px #fff;
	}
}

#content.user-page.compact > h1.listing {
	margin-top: 0.5rem;
}
#content.user-page.compact > h1.listing + .post-meta {
	margin-bottom: 0.5rem;
}

/*===========================*/
/* HIGHLIGHTING NEW COMMENTS */
/*===========================*/

.new-comment::before {
	outline: 2px solid #5a5;
	box-shadow:
		0 0 6px -2px #5a5 inset, 
		0 0 4px #5a5, 
		0 0 6px #5a5;
}

/*=================================*/
/* COMMENT THREAD MINIMIZE BUTTONS */
/*=================================*/

.comment-minimize-button {
	color: #ccc;
}
.comment-minimize-button:hover {
	color: #aaa;
	text-shadow: <?php echo $white_glow; ?>;
}
.comment-minimize-button::after {
	font-family: <?php echo $UI_font; ?>;
	color: #777;
}
.comment-minimize-button.maximized::after {
	color: #ccc;
}

/*==============*/
/* VOTE BUTTONS */
/*==============*/

.vote::before {
	content: "";
	border-radius: 50%;
	background-size: 17px 17px;
	width: 17px;
	height: 17px;
	display: inline-block;
	position: relative;
	top: 2.5px;
}
.vote:active {
	transform: none;
}
.vote:hover::before,
.vote.selected::before,
.vote.clicked-once::before,
.vote.clicked-twice::before {
	filter: drop-shadow(0 0 1px #fff);
}

.upvote::before,
.waiting .upvote.big-vote.clicked-twice::before {
	background-image: url('data:image/svg+xml;base64,<?php echo base64_encode(file_get_contents("assets/upvote-green-circle-plus.svg")) ?>');
	filter: grayscale(100%) brightness(128%);
}
.downvote::before,
.waiting .downvote.big-vote.clicked-twice::before {
	background-image: url('data:image/svg+xml;base64,<?php echo base64_encode(file_get_contents("assets/downvote-red-circle-minus.svg")) ?>');
	filter: grayscale(100%) brightness(188%);
}

.vote.clicked-once::before,
.vote.big-vote.clicked-once::before {
	box-shadow:
		0 0 0 1px #fff,
		0 0 0 4px #c8c8c8,
		0 0 0 5px transparent;
}

.vote.big-vote.clicked-twice::before,
.waiting .vote.big-vote:not(.clicked-twice)::before,
.waiting .vote:not(.big-vote).clicked-once::before {
	box-shadow: none;
}

.upvote.clicked-twice::before,
.upvote.big-vote::before {
	box-shadow:
		0 0 0 1px #fff,
		0 0 0 4px #00d800,
		0 0 0 5px transparent;
}

.downvote.clicked-twice::before,
.downvote.big-vote::before {
	box-shadow:
		0 0 0 1px #fff,
		0 0 0 4px #eb4c2a,
		0 0 0 5px transparent;
}

/*===========================*/
/* COMMENTING AND POSTING UI */
/*===========================*/

.posting-controls input[type='submit'] {
	background-color: #fff;
	border: 1px solid #aaa;
	font-weight: bold;
}
.posting-controls input[type='submit']:hover,
.posting-controls input[type='submit']:focus {
	background-color: #ddd;
	border: 1px solid #999;
}

.comment-controls .cancel-comment-button {
	font-weight: 600;
	color: #c00;
	text-shadow: 
		0 0 1px #fff,
		0 0 2px #fff;
}
.comment-controls .cancel-comment-button:hover {
	color: #f00;
	text-shadow: <?php echo $white_glow; ?>;
}

.new-comment-button {
	font-weight: 600;
}

.comment-controls .edit-button {
	color: #0b0;
}
.comment-controls .edit-button:hover {
	color: #f00;
}

.button.edit-post-link:not(:hover) {
	color: #090;
}

.posting-controls textarea {
	font-family: <?php echo $text_font; ?>;
	color: #000;
	background-color: #fff;
	border-color: #aaa;
	box-shadow: 
		0 0 0 1px #eee inset;
}
.posting-controls textarea:focus {
	background-color: #ffd;
	border-color: <?php echo $hyperlink_color; ?>;
	box-shadow: 
		0 0 0 1px #ddf inset,
		0 0 0 1px #fff,
		0 0 0 2px <?php echo $hyperlink_color; ?>;
}
.posting-controls.edit-existing-post textarea:focus,
.posting-controls form.edit-existing-comment textarea:focus {
	border-color: #090;
	box-shadow: 
		0 0 0 1px #81ff7f inset,
		0 0 0 1px #fff,
		0 0 0 2px #090;
}

/* GUIEdit buttons */

.guiedit-buttons-container {
	background-image: linear-gradient(to bottom, #fff 0%, #ddf 50%, #ccf 75%, #aaf 100%);
}

.posting-controls.edit-existing-post .guiedit-buttons-container button,
.posting-controls form.edit-existing-comment .guiedit-buttons-container button {
	color: #050;
}
.guiedit-buttons-container button {
	font-family: Font Awesome, <?php echo $text_font; ?>;
}

.guiedit::after {
	font-family: <?php echo $UI_font; ?>;
	color: #777;
	text-shadow: none;
}

/* Markdown hints */

#markdown-hints-checkbox + label {
	color: <?php echo $hyperlink_color; ?>;
}
#markdown-hints-checkbox + label:hover {
	color: #e00;
	text-shadow: <?php echo $white_glow; ?>;
}
.markdown-hints {
	border: 1px solid #c00;
	background-color: #ffa;
}

/*================*/
/* EDIT POST FORM */
/*================*/

#edit-post-form .link-post-checkbox + label {
	top: -2px;
}
#edit-post-form .link-post-checkbox + label::before {
	border-radius: 3px;
	border: 1px solid #ddd;
	color: #777;
}
#edit-post-form .link-post-checkbox + label:hover,
#edit-post-form .link-post-checkbox:focus + label {
	text-shadow: 
		0 0 1px #fff,
		0 0 2px #fff,
		0 0 2.5px #aaa;
}
#edit-post-form .link-post-checkbox + label:hover::before,
#edit-post-form .link-post-checkbox:focus + label::before {
	border-color: #aaa;
}
#edit-post-form .link-post-checkbox:checked + label::before {
	content: "\F00C";
}
#edit-post-form input[type='radio'] + label {
	color: #777;
	border-color: #ddd;
	padding: 4px 12px 5px 12px;
}
#edit-post-form input[type='radio'][value='all'] + label {
	border-radius: 8px 0 0 8px;
	border-width: 1px;
}
#edit-post-form input[type='radio'][value='drafts'] + label {
	border-radius: 0 8px 8px 0;
}
#edit-post-form input[type='radio'] + label:hover,
#edit-post-form input[type='radio']:focus + label {
	background-color: #ddd;
	color: #000;
}
#edit-post-form input[type='radio']:focus + label {
	color: #000;
	box-shadow: 
		0 0 0 1px #aaa;
}
#edit-post-form input[type='radio']:checked + label {
	background-color: #ddd;
	border-color: #ddd;
	color: #000;
	text-shadow: 
		0 -1px 0 #fff,
		0 0.5px 0.5px #000;
}

/*=======*/
/* LINKS */
/*=======*/

a {
	text-decoration: none;
	color: <?php echo $hyperlink_color; ?>;
}
a:visited {
	color: #551a8b;
}
a:hover {
	text-decoration: underline;
}

/*=========*/
/* BUTTONS */
/*=========*/

button,
input[type='submit'] {
	color: <?php echo $hyperlink_color; ?>;
}

button:active,
input[type='submit']:active {
	color: #f00;
	transform: scale(0.9);
}
.button:visited {
	color: <?php echo $hyperlink_color; ?>;
}
.button:active {
	transform: scale(0.9);
}
@-moz-document url-prefix() {
	.button:active {
		transform: none;
	}
}

@media only screen and (hover:hover), not screen and (-moz-touch-enabled) {
	button:hover,
	input[type='submit']:hover,
	button:focus,
	input[type='submit']:focus {
		color: #f00;
		text-shadow: <?php echo $white_glow; ?>;
	}

	.button:hover {
		color: #f00;
		text-shadow: <?php echo $white_glow; ?>;
		text-decoration: none;
	}
	.button:focus:not(:hover) {
		transform: none;
	}
}

/*==========*/
/* HEADINGS */
/*==========*/

.post-body h1,
.post-body h2,
.post-body h4,
.comment-body h1,
.comment-body h2,
.comment-body h4 {
	font-family: <?php echo $UI_font; ?>;
}
.post-body h3,
.comment-body h3,
.post-body h5,
.post-body h6,
.comment-body h5,
.comment-body h6 {
	font-weight: 600;
	font-family: <?php echo $UI_font_smallcaps; ?>;
}
.post-body h6,
.comment-body h6 {
	color: #555;
}
.post-body h1,
.comment-body h1 {
	border-bottom: 1px solid #aaa;
}

/*========*/
/* QUOTES */
/*========*/

blockquote {
	border-left: 5px solid #ccc;
}

/*========*/
/* IMAGES */
/*========*/

#content img {
	border: 1px solid #ccc;
}
#content img[style^='float'] {
	border: 1px solid transparent;
}
#content img[src$='.svg'] {
	border: none;
}
#content figure img {
	border: 1px solid #000;
}
#content figure img[src$='.svg'] {
	border: none;
}

/*========*/
/* TABLES */
/*========*/

.post-body table,
.comment-body table,
.post-body table th,
.post-body table td,
.comment-body table th,
.comment-body table td {
	border: 1px solid #ccc;
}

/*======*/
/* MISC */
/*======*/

hr {
	border-bottom: 1px solid #999;
}

code {
	background-color: #f6f6ff;
	border: 1px solid #ddf;
	border-radius: 4px;
}

input[type='text'],
input[type='search'],
input[type='password'] {
	background-color: #fff;
	border: 1px solid #ddd;
	color: #000;
}
input[type='text']:focus,
input[type='search']:focus,
input[type='password']:focus {
	background-color: #ffd;
	border: 1px solid #bbb;
	box-shadow: 0 0 1px #bbb;
}

select {
	color: #000;
}

<?php if ($platform != 'Mac') echo <<<EOT
@-moz-document url-prefix() {
	h1.listing s,
	.post > h1:first-of-type s {
	  position: relative;
	  text-decoration: none;
	}
	h1.listing s::after,
	.post > h1:first-of-type s::after {
	  position: absolute;
	  border-bottom: 3px solid #000;
	  content: "";
	  top: 0;
	  left: 0;
	  width: 100%;
	  height: 50%;
	}
}
EOT;
?>

/*============*/
/* ABOUT PAGE */
/*============*/

.about-page u {
	background-color: #e6e6e6;
	text-decoration: none;
	box-shadow: 
		0 -1px 0 0 #000 inset, 
		0 -3px 1px -2px #000 inset;
	padding: 0 1px;
}

#content.about-page .accesskey-table {
	font-family: <?php echo $UI_font; ?>;
	border-color: #ddd;
}

#content.about-page img {
	border: 1px solid #000;
}

/*========================*/
/* QUALIFIED HYPERLINKING */
/*========================*/

#aux-about-link a {
	color: #777;
}
#aux-about-link a:hover {
	opacity: 1.0;
	text-shadow: <?php echo $white_glow; ?>;
}

.qualified-linking label {
	color: <?php echo $hyperlink_color; ?>;
}
.qualified-linking label:hover {
	text-shadow:
		0 0 1px #fff,
		0 0 3px #fff,
		0 0 5px #00e;
}

.qualified-linking-toolbar {
	border: 1px solid #000;
	background-color: #fff;
}
.qualified-linking-toolbar a {
	background-color: #eee;
	border: 1px solid #ccc;
	border-radius: 4px;
}
.qualified-linking-toolbar a:visited {
	color: <?php echo $hyperlink_color; ?>;
}
.qualified-linking-toolbar a:hover {
	text-decoration: none;
	background-color: #ddd;
	text-shadow: <?php echo $white_glow; ?>;
}

/*======*/
/* MATH */
/*======*/

.mathjax-block-container::-webkit-scrollbar {
	height: 12px;
	background-color: #f6f6ff;
	border-radius: 6px;
	border: 1px solid #ddf;
}
.mathjax-block-container::-webkit-scrollbar-thumb {
	background-color: #dde;
	border-radius: 6px;
	border: 1px solid #cce;
}
.mathjax-inline-container::-webkit-scrollbar {
	height: 8px;
	background-color: #f6f6ff;
	border-radius: 4px;
	border: 1px solid #ddf;
}
.mathjax-inline-container::-webkit-scrollbar-thumb {
	background-color: #dde;
	border-radius: 4px;
	border: 1px solid #cce;
}

/*====================*/
/* FOR NARROW SCREENS */
/*====================*/

@media only screen and (max-width: 1440px) {
	#hns-date-picker {
		background-color: #d8d8d8;
		opacity: 1.0;
	}
	#hns-date-picker::before {
		border: 1px solid #999;
		border-width: 1px 0 1px 1px;
	}
}
@media only screen and (max-width: 1160px) {
	#theme-selector:hover::after {
		background-color: #999;
	}
}
@media only screen and (max-width: 1080px) {
	#text-size-adjustment-ui button {
		border: 1px solid #999;
		padding: 0 0 0 1px;
		border-radius: 50%;
		box-shadow: 
			0 0 6px #999 inset,
			0 0 0 1px transparent;
	}
	#theme-tweaker-toggle button {
		border: 1px solid #999;
		box-shadow: 
			0 0 10px #999 inset,
			0 0 0 1px transparent;
		border-radius: 50%;
		transform: scale(0.8);
	}
}
@media only screen and (max-width: 1020px) {
	#quick-nav-ui a {
		box-shadow: 
			0 0 0 1px #999,
			0 0 0 2px transparent;
	}
	#new-comment-nav-ui .new-comments-count::before {
		background-color: #d8d8d8;
		box-shadow: 
			0 0 0 1px #999,
			0 0 0 2px transparent;
		border-radius: 8px;
	}
	#anti-kibitzer-toggle {
		box-shadow: 
			0 0 0 1px #999,
			0 0 0 2px transparent;
		background-color: #d8d8d8;
		border-radius: 6px;
		overflow: hidden;
	}
}
@media only screen and (max-width: 1000px) {
	#theme-selector {
		background-color: #d8d8d8;
		box-shadow: 
			0 0 0 1px #999,
			0 0 0 2px transparent;
	}
	#text-size-adjustment-ui button {
		background-color: #ddd;
	}
	#text-size-adjustment-ui button:hover {
		background-color: #eee;
	}
	#theme-tweaker-toggle button {
		background-color: #ddd;
	}
}

/*========*/
/* MOBILE */
/*========*/

/**************************************************************************/
@media only screen and (hover: none), only screen and (-moz-touch-enabled) {
/**************************************************************************/
	#ui-elements-container > div[id$='-ui-toggle'] button  {
		color: #888;
		text-shadow:
			0 0 1px #fff,
			0 0 3px #fff,
			0 0 5px #fff,
			0 0 10px #fff,
			0 0 20px #fff,
			0 0 30px #fff;
	}

	#theme-selector {
		background-color: #d8d8d8;
		box-shadow: 
			0 0 0 1px #999,
			0 0 1px 3px #fff,
			0 0 3px 3px #fff,
			0 0 5px 3px #fff,
			0 0 10px 3px #fff,
			0 0 20px 3px #fff;
		border-radius: 12px;
	}
	#theme-selector::before,
	#theme-selector .theme-selector-close-button {
		color: #666;
		text-shadow: 0.5px 0.5px 0 #fff;
	}
	#theme-selector button {
		background-color: #e6e6e6;
		border-radius: 10px;
	}
	#theme-selector button::after {
		color: #000;
		padding-bottom: 2px;
		max-width: calc(100% - 3.25em);
		overflow: hidden;
		text-overflow: ellipsis;
	}
	#theme-selector button.selected::after {
		text-shadow: 
			0 -1px 0 #fff,
			0 0.5px 0.5px #000;
	}

	#quick-nav-ui {
		background-color: #fff;
	}
	#quick-nav-ui,
	#new-comment-nav-ui,
	#hns-date-picker {
		box-shadow:
			0 0 1px 3px #fff,
			0 0 3px 3px #fff,
			0 0 5px 3px #fff,
			0 0 10px 3px #fff,
			0 0 20px 3px #fff;
	}
	#quick-nav-ui a::after,
	#new-comment-nav-ui::before {
		font-family: <?php echo $UI_font; ?>;
		font-weight: 600;
		box-shadow:
			0 0 1px 0 #fff,
			0 0 3px 0 #fff,
			0 0 5px 0 #fff;
		background-color: #fff;
		border-radius: 4px;
	}
	#quick-nav-ui,
	#new-comment-nav-ui {
		border-radius: 8px;
	}
	#new-comment-nav-ui {
		background-color: #d8d8d8;
		border: 1px solid #999;
	}
	#new-comment-nav-ui::before {
		color: #777;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button {
		box-shadow: 0 0 0 1px #999;
		color: #777;
	}
	#new-comment-nav-ui .new-comments-count {
		background-color: inherit;
		box-shadow: 0 -1px 0 0 #999;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button:disabled {
		color: #bbb;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button.new-comment-previous {
		border-radius: 7px 0 0 7px;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button.new-comment-next {
		border-radius: 0 7px 7px 0;
	}
	#new-comment-nav-ui button::after {
		font-family: <?php echo $UI_font; ?>;
	}
	
	/*****************************************/
	@media only screen and (max-width: 900px) {
	/*****************************************/
		h1.listing {
			font-size: 1.75rem;
			line-height: 1;
		}
		h1.listing a[href^='http'] {
			top: 2px;
		}
		h1.listing + .post-meta .karma-value,
		h1.listing + .post-meta .comment-count,
		h1.listing + .post-meta .lw2-link,
		h1.listing + .post-meta .read-time {
			bottom: 0;
		}
		h1.listing + .post-meta .post-section::before {
			position: unset;
		}
		h1.listing + .post-meta .post-section {
			overflow: visible;
			order: 1;
		}
		h1.listing + .post-meta .link-post-domain {
			order: 2;
			line-height: 1;
			flex-basis: 100%;
		}
		h1.listing + .post-meta::after {
			bottom: -10px;
		}
		#content.user-page h1.listing + .post-meta {
			margin-bottom: 1em;
		}
		#content.user-page h1.link-post-listing::after {
			height: calc(100% + 2em);
		}

		#nav-item-search button::before {
			color: #00e;
		}

		.archive-nav > *[class^='archive-nav-'] + *[class^='archive-nav-']::before {
			background-color: #aaa;
		}

		.comment-item .comment-item {
			margin: 0.75em 2px 4px 6px;
			box-shadow: 
				0 0 2px #ccc, 
				0 0 4px #ccc, 
				0 0 7px #ccc;
		}
		.comment-item .comment-item + .comment-item {
			margin: 1.5em 2px 4px 6px;
		}
		.comment-body {
			font-size: 1.125rem;
		}

		a.comment-parent-link:hover::before {
			background-color: unset;
		}

		.sublevel-nav .sublevel-item,
		.sublevel-nav .sublevel-item:first-child,
		.sublevel-nav .sublevel-item:last-child {
			border-width: 1px;
			border-radius: 8px;
		}
	/*******************************************/
	} @media only screen and (max-width: 720px) {
	/*******************************************/
		h1.listing {
			margin: 10px 6px 6px 6px;
			max-width: calc(100% - 12px);
			font-size: 1.5rem;
			padding-right: 35px;
		}
		#content.conversations-user-page h1.listing {
			font-size: 1.5rem;
		}
		h1.listing + .post-meta {
			margin: 0 6px 0 7px;
			clear: both;
		}
		h1.listing + .post-meta {
			padding: .25em 254px 0 0;
		}
		h1.listing + .post-meta::after {
			bottom: -2px;
		}
		h1.listing + .post-meta > * {
			line-height: 1;
			display: block;
		}
		#content.conversations-user-page h1.listing + .post-meta > * {
			line-height: 1.5;
		}
		h1.listing + .post-meta .date,
		h1.listing + .post-meta .author {
			line-height: 1.3;		
		}
		h1.listing + .post-meta .karma-value,
		h1.listing + .post-meta .comment-count,
		h1.listing + .post-meta .lw2-link,
		h1.listing + .post-meta .read-time {
			top: unset;
			font-size: 1rem;
			box-shadow: none;
		}
		h1.listing + .post-meta .karma-value::before,
		h1.listing + .post-meta .comment-count::before,
		h1.listing + .post-meta .lw2-link::before,
		h1.listing + .post-meta .read-time::before {
			box-shadow: none;
		}
		h1.listing + .post-meta .karma-value,
		h1.listing + .post-meta .comment-count,
		h1.listing + .post-meta .read-time,
		h1.listing + .post-meta .lw2-link {
			bottom: 4px;
		}

		h1.listing + .post-meta .karma-value {
			right: 192px;
		}
		h1.listing + .post-meta .karma-value::before {
			text-shadow: 0.5px 0.5px 0.5px #999;
		}
		h1.listing + .post-meta .comment-count {
			right: 132px;
		}
		h1.listing + .post-meta .read-time {
			right: 56px;
		}
		h1.listing + .post-meta .lw2-link {
			opacity: 1;
			right: 0;
		}
		h1.listing + .post-meta .link-post-domain {
			margin: 0;
			line-height: 1.3;
			overflow: hidden;
			text-overflow: ellipsis;
		}
		h1.listing + .post-meta .post-section::before {
			position: absolute;
			left: unset;
			right: 0;
			bottom: 30px;
			top: unset;
		}
		h1.listing a {
			display: inline;
		}
	/*******************************************/
	} @media only screen and (max-width: 520px) {
	/*******************************************/
		h1.listing + .post-meta {
			padding: .25em 144px 0 0;
			flex-flow: column;
		}
		#content.conversations-user-page h1.listing + .post-meta {
			flex-flow: row wrap;
		}
		h1.listing + .post-meta .date {
			margin: 0.375em 0 0.25em 0;
			line-height: 1;
		}
		#content.user-page h1.listing::after {
			height: calc(100% + 2.125em);
		}
		#content.user-page h1.link-post-listing::after {
			height: calc(100% + 3.125em);
		}
		#content.user-page h1.listing + .post-meta {
			padding: 0.25em 144px 0.5em 36px;
		}
		#content.conversations-user-page h1.listing + .post-meta .date {
			margin: 0 0 0 1em;
		}

		h1.listing + .post-meta .karma-value {
			bottom: 28px;
			right: 0;
		}
		h1.listing + .post-meta .comment-count {
			bottom: 28px;
			right: 56px;
		}
		h1.listing + .post-meta .read-time {
			right: 56px;
			bottom: 4px;
		}
		h1.listing + .post-meta .lw2-link {
			right: 0;
			bottom: 4px;
		}
		h1.listing + .post-meta .link-post-domain {
			max-width: 100%;
		}
		h1.listing + .post-meta .post-section::before {
			right: 120px;
		}

		#content.compact > .comment-thread .comment-item {
			max-height: 110px;
		}
	
		.textarea-container:focus-within button:active {
			background-color: #ccc;
		}
		.textarea-container:focus-within .guiedit-mobile-auxiliary-button {
			background-color: #eee;
			border: 1px solid #ddd;
			border-radius: 6px;
		}
		.textarea-container:focus-within .guiedit-mobile-help-button.active {
			border-color: #c00;
			box-shadow:
				0 0 0 1px #fff,
				0 0 0 2px #c00;
			color: #c00;
			font-weight: 600;
		}
		#content.conversation-page .textarea-container:focus-within::after {
			background-color: #fff;
		}
		.textarea-container:focus-within .guiedit-buttons-container {
			background-color: white;
			border-top: 1px solid #ddf;
		}
		.textarea-container:focus-within button.guiedit {
			background-color: #eee;
			border: 1px solid #ddd;
			border-radius: 6px;
		}
		.markdown-hints::after {
			color: #090;
		}
	/*******************************************/
	} @media only screen and (max-width: 320px) {
	/*******************************************/
		h1.listing {
			font-size: 1.25rem;
		}
		#content.user-page h1.listing::after {
			height: calc(100% + 2.625em);
		}
		#content.user-page h1.link-post-listing::after {
			height: calc(100% + 3.75em);
		}
	}
}

