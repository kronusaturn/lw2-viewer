/************/
/* NAV BARS */
/************/

.nav-bar {
	display: flex;
	margin: 0 calc(-1 * var(--GW-content-side-padding));
}

/*=---------------=*/
/*= Nav bar items =*/
/*=---------------=*/

.nav-item,
.nav-inner {
	display: flex;
	justify-content: center;
	align-items: center;
}
.nav-item {
	flex: 1 1 auto;
	position: relative;
}
.nav-item * {
	text-overflow: ellipsis;
	white-space: nowrap;
	overflow: hidden;
}
.nav-inner {
	width: 100%;
	height: 100%;

	font-weight: var(--GW-UI-font-weight-heavy);
}
#primary-bar .nav-inner,
#bottom-bar .nav-inner {
	padding: 12px 30px;
}
#secondary-bar .nav-inner {
	padding: 4px 10px;
}

/*=---------=*/
/*= Styling =*/
/*=---------=*/

.nav-bar a,
.nav-bar a:visited {
	color: var(--GW-nav-bar-item-color);
}
.nav-bar a:hover,
.nav-bar a:focus {
	color: var(--GW-nav-bar-item-hover-color);
}
.nav-bar a:active {
	color: var(--GW-nav-bar-item-active-color);
}

/*=------------=*/
/*= Bottom bar =*/
/*=------------=*/

#bottom-bar .nav-item {
	width: 0;
}

/*=-----------------=*/
/*= Accesskey hints =*/
/*=-----------------=*/

.nav-inner::after {
	content: attr(accesskey);
	display: none;

	font-weight: var(--GW-UI-font-weight-light);
}

/*=---------------=*/
/*= Pagination UI =*/
/*=---------------=*/

#bottom-bar .nav-item a::before,
#top-nav-bar a::before {
	font-family: var(--GW-Font-Awesome);
	font-weight: 900;
	font-size: 0.8em;
	position: relative;
	bottom: 1px;
	margin: 0.5em;
	display: inline-block;
}

<?php

$pagination_icons = [
	'first'	=> '\F33E',
	'prev'	=> '\F060',
	'top'	=> '\F062',
	'next'	=> '\F061',
	'last'	=> '\F340'
];

foreach ($pagination_icons as $k => $v) {
echo <<<EOT
#bottom-bar #nav-item-{$k} a::before,
#top-nav-bar a.nav-item-{$k}::before {
	content: "{$v}";
}

EOT;
}

?>
#bottom-bar #nav-item-next a::before,
#bottom-bar #nav-item-last a::before {
	order: 1;
}

/*= Hover tooltips =*/

#top-nav-bar a {
	position: relative;
}
#top-nav-bar a::after {
	bottom: calc(100% - 3px);
	content: attr(data-target-page);
}
#bottom-bar a::after {
	content: "Page " attr(data-target-page);
	top: unset;
	left: 0;
	bottom: 4px;
}
#top-nav-bar a::after,
#bottom-bar a::after {
	display: block;
	position: absolute;
	font-size: 0.75rem;
	width: 100%;
	line-height: 1;
	visibility: hidden;
	text-align: center;
}
#bottom-bar #nav-item-top a::after {
	display: none;
}
#top-nav-bar a:hover::after,
#bottom-bar a:hover::after {
	visibility: visible;
}

/*=-----------------------=*/
/*= Decorative bottom bar =*/
/*=-----------------------=*/
/* (On short pages with no pagination) */

#bottom-bar.decorative {
	position: relative;
}
#bottom-bar.decorative .nav-item {
	display: none;
}

/*=------------=*/
/*= Search tab =*/
/*=------------=*/

#nav-item-search {
	flex: 4 1 auto;
}
#nav-item-search .nav-inner {
	flex: 1 1 100%;
}
#nav-item-search form {
	align-items: stretch;
}
#nav-item-search form::before {
	content: "\F002";
	font-family: var(--GW-Font-Awesome);
	font-weight: 900;
	padding: 0 5px;
}
#nav-item-search input {
	padding: 1px 4px;
	flex: 1 1 100%;
	margin: 0 0 1px 0;
}
#nav-item-search button {
	height: 100%;
	flex: 1 0 auto;
	padding: 0 0.5em;
	line-height: 1;
}
#nav-item-search button::before {
	content: "Search";
}

/*=-----------=*/
/*= Login tab =*/
/*=-----------=*/

#nav-item-login {
	position: relative;
	padding-right: 0.5em;
}

/*=---------------=*/
/*= Sequences tab =*/
/*=---------------=*/

#nav-item-sequences .nav-inner::before {
	font-family: var(--GW-Font-Awesome);
	content: "\F5DB";
}
@media only screen and (min-width: 901px) {
	#nav-item-sequences .nav-inner {
		max-width: 0;
		text-align: left;
		visibility: hidden;
	}
	#nav-item-sequences .nav-inner::before {
		font-size: 0.9375em;
		display: block;
		position: absolute;
		visibility: visible;
		padding: 0 0 1px 2px;
	}
	#nav-item-sequences .nav-inner::after {
		visibility: visible;
	}
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 960px) {
	#bottom-bar {
		padding: 0 4.5rem;
		font-size: 1.5em;
	}
	#bottom-bar .nav-inner {
		flex-flow: column;
		color: transparent;
		line-height: 0;
		text-shadow: none;
		justify-content: flex-start;
		padding: 20px 30px;
	}
	#bottom-bar .nav-item a::before {
		color: var(--GW-hyperlink-color);
		text-shadow: var(--GW-shadow-white-glow);
		line-height: initial;
		top: -0.5rem;
	}
	#bottom-bar #nav-item-next a::before,
	#bottom-bar #nav-item-last a::before {
		order: 0;
	}
	#bottom-bar .nav-inner::after {
		display: block;
		color: var(--GW-hyperlink-color);
		text-shadow: var(--GW-shadow-white-glow);
		visibility: visible;
		text-transform: uppercase;
		font-size: 0.375em;
		bottom: 1.5rem;
	}
<?php

$pagination_labels = [
	'first'	=> 'First Page',
	'prev'	=> 'Prev. Page',
	'top'	=> 'Top',
	'next'	=> 'Next Page',
	'last'	=> 'Last Page'
];
foreach ($pagination_labels as $k => $v) {
echo <<<EOT
	#bottom-bar #nav-item-{$k} .nav-inner::after {
		content: "{$v}";
	}

EOT;
}

?>
	#bottom-bar #nav-item-top a::after {
		display: initial;
	}
}
@media only screen and (max-width: 900px) {
	#primary-bar .nav-item {
		flex: 1 1 100%;
	}
	.nav-bar .nav-inner {
		text-transform: uppercase;
		padding: 1px 0 0 0;
	}
	.nav-bar .nav-inner::after {
		display: none;
	}
	.nav-bar .nav-inner::before {
		font-family: var(--GW-Font-Awesome);
		font-weight: 900;
		margin: 0 0.5em 1px 0;
	}

	#nav-item-home .nav-inner::before {
		content: "\F015";
	}
	#nav-item-featured .nav-inner::before {
		content: "\F005";
	}
	#nav-item-all .nav-inner::before {
		content: "\F069";
	}
	#nav-item-meta .nav-inner::before {
		content: "\F077";
	}
	#nav-item-recent-comments > * > span {
		display: none;
	}
	#nav-item-recent-comments .nav-inner::before {
		content: "\F036";
	}
	#nav-item-archive .nav-inner::before {
		content: "\F187";
	}
	#nav-item-about .nav-inner::before {
		content: "\F129";
	}
	#nav-item-search .nav-inner::before {
		content: none;
	}
	#nav-item-search button::before {
		content: "\F002";
		font-family: var(--GW-Font-Awesome);
		font-weight: 900;
	}
	#nav-item-login {
		padding: 0;
	}
	#nav-item-login .nav-inner::before {
		content: "\F007";
	}
}
@media only screen and (max-width: 840px) {
	#primary-bar .nav-inner {
		padding: 8px 20px;
	}
	#primary-bar .nav-inner {
		flex-flow: column;
	}
	#primary-bar .nav-inner::before {
		margin: 0;
		font-size: 1.125em;
	}
}
@media only screen and (max-width: 720px) {
	.nav-bar .nav-item:not(#nav-item-search) .nav-inner {
		flex-flow: column;
	}
	.nav-bar .nav-inner::before {
		margin: 0;
		font-size: 1.125em;
	}
	.nav-bar #nav-item-search {
		padding: 5px;
	}
	.nav-bar #nav-item-search button::before {
		font-size: 1.5rem;
	}
}
@media only screen and (max-width: 520px) {
	#bottom-bar #nav-item-first .nav-inner::after {
		content: "First";
	}
	#bottom-bar #nav-item-prev .nav-inner::after {
		content: "Prev";
	}
	#bottom-bar #nav-item-next .nav-inner::after {
		content: "Next";
	}
	#bottom-bar #nav-item-last .nav-inner::after {
		content: "Last";
	}
}

/*******************/
/* INBOX INDICATOR */
/*******************/

#inbox-indicator {
	height: 100%;
	width: 0;
	color: transparent;
}
#inbox-indicator::before {
	content: "\F0E0";
	font-family: var(--GW-Font-Awesome);
	font-weight: 900;
	color: #bbb;
	font-size: 1.1875rem;
	position: absolute;
	right: 0;
	height: 100%;
	padding: 1px 0.45em 0 0.45em;
}
#inbox-indicator.new-messages::before {
	color: #f00;
	text-shadow: 
		0 0 1px #777,
		0.5px 0.5px 1px #777;
}
a#inbox-indicator:hover::before {
	color: #fff;
	text-shadow: 
		0 0 1px #000,
		0 0 2px #000,
		0 0 4px #000,
		0 0 1px #777,
		0.5px 0.5px 1px #777;
}
a#inbox-indicator.new-messages:hover::before {
	text-shadow: 
		0 0 1px #f00,
		0 0 2px #f00,
		0 0 4px #f00,
		0 0 1px #777,
		0.5px 0.5px 1px #777;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	#inbox-indicator {
		padding: 0 0.75em 0 0;
	}
	#inbox-indicator::before {
		width: 100%;
		text-align: right;
		pointer-events: none;
	}
	#inbox-indicator.new-messages::before {
		pointer-events: auto;
		box-shadow: 0 0 8px 1px #f00 inset;
	}
}
@media only screen and (max-width: 720px) {
	#inbox-indicator {
		padding: 0;
	}
	#inbox-indicator::before {
		font-size: 1em;
	}
}
@media only screen and (max-width: 374px) {
	#inbox-indicator::before {
		font-size: 0.875rem;
	}
}

/****************/
/* PAGE TOOLBAR */
/****************/

.page-toolbar {
	grid-row: 3;
	text-align: right;
	display: flex;
	justify-content: flex-end;
	padding: 4px 8px;
	margin: 0 calc(-1 * var(--GW-content-side-padding));
	align-self: start;
	justify-self: end;
}

/*=--------------------------=*/
/*= Page toolbar items (all) =*/
/*=--------------------------=*/

.page-toolbar > * {
	margin-left: 1.5em;
}
.page-toolbar .button::before {
	font-family: var(--GW-Font-Awesome);
	font-size: 0.9em;
	padding-right: 4px;
}

/*=-------------------------------=*/
/*= Page toolbar items (specific) =*/
/*=-------------------------------=*/

.new-post::before {
	content: '\F067';
	font-weight: 900;
}
.new-private-message::before {
	content: '\F075';
	font-weight: 400;
}
.logout-button::before {
	content: '\F2F5';
	font-weight: 900;
}
.ignore-button::before {
	content: "\F070";
	font-weight: 400;
}
.unignore-button::before {
	content: "\F06E";
	font-weight: 400;
}
.rss::before {
	content: url('data:image/svg+xml;base64,<?php echo base64_encode(file_get_contents("assets/rss.svg")) ?>');
	display: inline-block;
	width: 0.8em;
	position: relative;
	top: 1px;
	margin: 0 2px 0 0;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 520px) {
	.page-toolbar {
		display: flex;
	}
	.page-toolbar > * {
		line-height: 1.15;
		padding: 8px 0 4px 0;
	}
	#content:not(.user-page) .page-toolbar {
		flex-direction: column-reverse;
		align-self: start;
	}
	#content.user-page .page-toolbar > * {
		text-align: center;
		margin-left: 1.5em;
	}
	#content.user-page .page-toolbar > *:not(.rss) {
		flex-basis: 25%;
	}
	#content.user-page .page-toolbar > .rss {
		flex-basis: 15%;
	}
	#content.user-page .page-toolbar * {
		text-transform: uppercase;
		font-size: 0.625rem;
	}
	#content.user-page .page-toolbar *::before {
		font-size: 1.375rem;
		display: block;
		padding: 0;
		margin: 0 auto;
	}
}

/*********************/
/* TOP PAGINATION UI */
/*********************/

#top-nav-bar {
	grid-row: 3;
	margin: 0.25em 0 0 0;
	padding: 1em 0 0 0;
	text-align: center;
	font-size: 1.25rem;
	display: flex;
	justify-content: center;
}
#top-nav-bar a {
	line-height: 1;
}
#top-nav-bar a.disabled {
	pointer-events: none;
	visibility: hidden;
}
#top-nav-bar .page-number {
	position: relative;
	display: inline-block;
	width: 1.5em;
}
#top-nav-bar .page-number-label {
	position: absolute;
	font-size: 0.5em;
	text-transform: uppercase;
	width: 100%;
	bottom: 90%;
	left: 0;
}
#top-nav-bar a::before {
	display: inline-block;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	#top-nav-bar {
		font-size: 1.75rem;
	}
}

/****************/
/* SUBLEVEL NAV */
/****************/

.sublevel-nav {
	text-align: center;
	display: flex;
	justify-content: center;
	margin: 1em 0 0 0;
}
#content > .sublevel-nav:not(.sort) {
	grid-row: 5;
	align-self: start;
}
#content.sequences-page > .sublevel-nav {
	grid-row: unset;
}
.sublevel-nav .sublevel-item {
	flex: 0 0 6em;
	padding: 0.125em 0.5em;
	font-size: 1.125rem;
}
.sublevel-nav .sublevel-item:active {
	transform: none;
}
.sublevel-nav .sublevel-item.selected {
	cursor: default;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	.sublevel-nav:not(.sort) {
		flex-wrap: wrap;
		width: calc(100vw - 200px);
	}
	.sublevel-nav:not(.sort) .sublevel-item {
		margin: 1px;
		flex-basis: 7em;
	}
}
@media only screen and (max-width: 720px) {
	.sublevel-nav:not(.sort) {
		width: calc(100vw - 200px);
	}
}
@media only screen and (max-width: 520px) {
	.sublevel-nav:not(.sort) {
		width: calc(100vw - 100px);
	}
	.sublevel-nav:not(.sort) .sublevel-item {
		font-size: 1rem;
	}
}

/***********************/
/* SORT ORDER SELECTOR */
/***********************/

.sublevel-nav.sort {
	position: relative;
	font-size: 0.75em;
}
#content > .sublevel-nav.sort {
	grid-row: 5 / span 2;
	justify-self: end;
	align-self: start;
	flex-flow: column;
}
#content.index-page > .sublevel-nav.sort {
	grid-row: 3 / span 1;
	justify-self: start;
	flex-flow: row;
}
#content.user-page > .sublevel-nav.sort {
	grid-row: 6;
}

.sublevel-nav.sort::before {
	content: "Sort";
	font-size: 0.75rem;
	position: absolute;
	top: 0;
	left: 0;
	width: 100%;

	font-weight: var(--GW-UI-font-weight-heavy);
}
.sublevel-nav.sort .sublevel-item {
	line-height: 1;
	font-size: 0.875rem;
	flex-basis: unset;

	font-family: var(--GW-UI-font);
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 720px) {
	#content.index-page > .sublevel-nav.sort {
		flex-flow: column;
		margin-left: 4px;
	}
}
@media only screen and (max-width: 520px) {
	#content.index-page > .sublevel-nav.sort {
		flex-flow: column;
		margin-left: 10px;
	}
}

/*******************************/
/* COMMENTS SORT MODE SELECTOR */
/*******************************/

.comments > .sublevel-nav.sort {
	margin: 1em auto 0 auto;
}
<?php fit_content(".comments > .sublevel-nav.sort"); ?>

/*******************************/
/* COMMENTS VIEW MODE SELECTOR */
/*******************************/

#comments-view-mode-selector {
	position: absolute;
	bottom: 30px;
	left: -40px;
	opacity: 0.6;
}
#comments-view-mode-selector:hover {
	opacity: 1.0;
}

/*=---------=*/
/*= Buttons =*/
/*=---------=*/

#comments-view-mode-selector a {
	display: block;
	font-family: var(--GW-Font-Awesome);
	font-size: 1.25rem;
	text-align: center;
	opacity: 0.4;
	padding: 0.25em;
	z-index: 1;
}
#comments-view-mode-selector a.threaded {
	transform: scaleY(-1);
	font-weight: 900;
}
#comments-view-mode-selector a.chrono {
	font-weight: 400;
}
#comments-view-mode-selector a.selected,
#comments-view-mode-selector a:hover {
	opacity: 1.0;
	text-decoration: none;
}
#comments-view-mode-selector a.selected {
	cursor: default;
}

/************/
/* ARCHIVES */
/************/

.archive-nav {
	margin: 1.25em 0.5em 0 0.5em;
	padding: 0.25em;
}
.archive-nav > * {
	display: flex;
}
.archive-nav *[class^='archive-nav-item'] {
	line-height: 1;
	flex: 1 1 5%;
	text-align: center;
	padding: 6px 4px 4px 4px;
	max-width: 8%;
}
<?php echo $firefox_exclusive; ?> {
	.archive-nav *[class^='archive-nav-item'] {
		padding: 5px 4px;
	}
}
.archive-nav span[class^='archive-nav-item'] {
	font-weight: var(--GW-UI-font-weight-heavy);
}
.archive-nav-days .archive-nav-item-day {
	font-size: 0.8em;
	padding: 7px 0 5px 0;
	max-width: 4%;
}
.archive-nav-days .archive-nav-item-day:first-child {
	flex-basis: 10%;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	div[class^='archive-nav-'] {
		flex-wrap: wrap;
		justify-content: flex-start;
	}
	.archive-nav *[class^='archive-nav-item'],
	.archive-nav *[class^='archive-nav-item']:first-child {
		padding: 10px;
		margin: 2px;
		max-width: unset;
		flex: 0 1 calc((100% / 8) - 4px);
	}
	.archive-nav .archive-nav-item-day,
	.archive-nav .archive-nav-item-day:first-child {
		flex-basis: calc((100% / 16) - 4px);
	}
	.archive-nav > *[class^='archive-nav-'] + *[class^='archive-nav-'] {
		margin-top: 8px;
		position: relative;
	}
	.archive-nav > *[class^='archive-nav-'] + *[class^='archive-nav-']::before {
		content: "";
		display: block;
		position: absolute;
		height: 1px;
		width: calc(100% + 8px);
		left: -4px;
		top: -4px;
	}
}
@media only screen and (max-width: 720px) {
	.archive-nav .archive-nav-item-day,
	.archive-nav .archive-nav-item-day:first-child {
		flex-basis: calc((100% / 12) - 4px);
	}
}
@media only screen and (max-width: 520px) {
	.archive-nav *[class^='archive-nav-item'],
	.archive-nav *[class^='archive-nav-item']:first-child {
		flex-basis: calc((100% / 5) - 4px);
	}
	.archive-nav .archive-nav-item-day,
	.archive-nav .archive-nav-item-day:first-child {
		flex-basis: calc((100% / 8) - 4px);
	}
}

