/*************************/
/* UI ELEMENTS CONTAINER */
/*************************/

#ui-elements-container {
	position: fixed;
	height: 100vh;
	top: 0;
	left: 0;
	right: 0;
	margin: auto;
	z-index: 10000;
	pointer-events: none;
}
#ui-elements-container > * {
	pointer-events: auto;
}
#post-nav-ui-toggle,
#appearance-adjust-ui-toggle {
	display: none;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 960px) {
	#ui-elements-container {
		height: unset;
		position: unset;
	}
	#ui-elements-container > div {
		position: fixed;
		visibility: hidden;
		z-index: 10000;
		opacity: 1.0;
	}

	#ui-elements-container > div[id$='-ui-toggle'] {
		visibility: visible;
		display: inline-block;
		border-radius: 50%;
		z-index: 10000;
	}
	#ui-elements-container > div[id$='-ui-toggle'] button,
	#theme-selector .theme-selector-close-button {
		font-family: var(--GW-Font-Awesome);
		font-weight: 900;
		font-size: 32px;
		padding: 10px;
		opacity: 0.8;
		-webkit-tap-highlight-color: transparent;
		transition: transform 0.2s ease;
	}
	#ui-elements-container > div[id$='-ui-toggle'] button::selection,
	#theme-selector .theme-selector-close-button::selection {
		background-color: transparent;
	}
	#ui-elements-container > div[id$='-ui-toggle'] button::-moz-focus-inner,
	#theme-selector .theme-selector-close-button::-moz-focus-inner {
		border: none;
	}
	#ui-elements-container > div[id$='-ui-toggle'] button.engaged {
		transform: rotate(-90deg);
		opacity: 1.0;
	}

	#post-nav-ui-toggle,
	#appearance-adjust-ui-toggle,
	#theme-selector .theme-selector-close-button {
		display: initial;
	}

	#appearance-adjust-ui-toggle {
		bottom: 10px;
		left: 10px;
	}

	#post-nav-ui-toggle {
		bottom: 10px;
		right: 10px;
	}
}

/******************/
/* THEME SELECTOR */
/******************/

#theme-selector {
	position: absolute;
	top: 3px;
	left: -41px;
	opacity: 0.4;
	display: table;
	max-width: 40px;
}
#theme-selector:hover {
	opacity: 1.0;
}
#theme-selector .theme-selector-close-button {
	display: none;
}

/*=----------------------=*/
/*= Theme select buttons =*/
/*=----------------------=*/

.theme-selector button {
	display: table-cell;
	width: 26px;
	height: 26px;
	padding: 5px;
	margin: 1px 7px 0 7px;
	color: transparent;
	background-size: 16px 16px;
	background-origin: content-box;
}
.theme-selector button,
.theme-selector button:hover,
.theme-selector button:active,
.theme-selector button:focus {
	text-shadow: none;
	color: transparent;
}
.theme-selector button:disabled {
	cursor: auto;
}

/*=----------------------------=*/
/*= Pre-rendered button images =*/
/*=----------------------------=*/
/*	(Each is just a capital letter A through whatever) */

.theme-selector button:nth-of-type(1) {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/theme_A.gif")) ?>');
}
.theme-selector button:nth-of-type(2) {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/theme_B.gif")) ?>');
}
.theme-selector button:nth-of-type(3) {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/theme_C.gif")) ?>');
}
.theme-selector button:nth-of-type(4) {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/theme_D.gif")) ?>');
}
.theme-selector button:nth-of-type(5) {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/theme_E.gif")) ?>');
}
.theme-selector button:nth-of-type(6) {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/theme_F.gif")) ?>');
}
.theme-selector button:nth-of-type(7) {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/theme_G.gif")) ?>');
}
.theme-selector button:nth-of-type(8) {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/theme_H.gif")) ?>');
}
.theme-selector button:nth-of-type(9) {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/theme_I.gif")) ?>');
}

/*=------------------------------=*/
/*= Theme select button tooltips =*/
/*=------------------------------=*/
/*	(with the name & description of the theme that each button selects) */

#theme-selector button {
	position: relative;
	z-index: 1;
}
#theme-selector button::before {
	content: attr(data-theme-name);
	position: absolute;
	top: 0;
	right: 100%;
	padding: 5px 6px 6px 6px;
	line-height: 1;
	width: 6em;
	text-align: right;
	z-index: 1;
	visibility: hidden;
}
#theme-selector:hover button::before {
	visibility: visible;
}
#theme-selector:hover ~ #theme-tweaker-toggle,
#theme-selector:active ~ #theme-tweaker-toggle {
	z-index: -1;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 1160px) {
	#theme-selector:hover::after {
		content: "";
		display: block;
		position: absolute;
		width: calc(6em - 7px);
		height: calc(100% + 2px);
		top: 0;
		left: calc(100% + 1px);
	}
	#theme-selector button::before {
		right: unset;
		left: 100%;
	}
}
@media only screen and (max-width: 1080px) {
	#theme-selector {
		top: 46px;
		left: -44px;
	}
}
@media only screen and (max-width: 1000px) {
	#theme-selector {
		left: -17px;
		top: 120px;
		padding: 3px 0;
		max-width: 32px;
	}
	#theme-selector button {
		margin: 1px 4px;
	}
}
@media only screen and (max-width: 960px) {
	#theme-selector {
		display: flex;
		flex-flow: column;
		width: calc(100vw - 20px);
		overflow: hidden;
		max-width: 360px;
		max-height: 0;
		top: calc(100% + 10px);
		left: 0;
		right: 0;
		margin: auto;
		padding: 0 0 64px 0;
		transition: 
			top 0.2s ease,
			max-height 0.2s ease,
			visibility 0.2s ease;
	}
	#theme-selector.engaged {
		visibility: visible;
		max-height: 1000px;
		top: 10px;
		z-index: 10001;
	}
	#theme-selector::before {
		content: "Select theme";
		white-space: nowrap;
		display: block;
		font-weight: var(--GW-UI-font-weight-heavy);
		font-size: 2rem;
		margin: 0.375em 1em 0.5em 1em;
		text-align: center;
	}
	#theme-selector button {
		width: calc(100% - 0.5em);
		background-repeat: no-repeat;
		padding: 1em 0.875em;
		margin: 1px 4px;
		line-height: 1;
		height: unset;
		position: relative;
	}
	#theme-selector button::after {
		content: attr(data-theme-description);
		white-space: nowrap;
		position: absolute;
		text-align: left;
		left: 2.5em;
		top: 1em;
	}
	#theme-selector button.selected::after {
		font-weight: var(--GW-UI-font-weight-heavy);
	}
	@media only screen and (max-height: 675px) {
		#theme-selector button {	
			padding: 0.875em;
		}
		#theme-selector button::after {
			top: 0.875em;
		}
	}
	#theme-selector .theme-selector-close-button {
		position: absolute;
		width: unset;
		background-color: transparent;
		top: 0;
		right: -3px;
	}
	#theme-selector .theme-selector-close-button,
	#theme-selector .theme-selector-close-button:focus,
	#theme-selector .theme-selector-close-button:active,
	#theme-selector .theme-selector-close-button:hover {
		box-shadow: none;
	}
}

/************************/
/* THEME TWEAKER TOGGLE */
/************************/

#theme-tweaker-toggle {
	position: absolute;
	top: 7px;
	left: -75px;
}
#theme-tweaker-toggle button {
	font-family: var(--GW-Font-Awesome);
	font-weight: 900;
	font-size: 1.25rem;
	opacity: 0.4;
	z-index: 1;
}
#theme-tweaker-toggle button:hover {
	opacity: 1.0;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 1080px) {
	#theme-tweaker-toggle {
		left: -44px;
		top: 2px;
	}
	#theme-tweaker-toggle button {
		height: 2em;
		width: 2em;
		padding: 7px;
	}
}
@media only screen and (max-width: 1000px) {
	#theme-tweaker-toggle {
		top: 70px;
		left: -21px;
	}
}
@media only screen and (max-width: 960px) {
	#theme-selector ~ #theme-tweaker-toggle {
		top: 100%;
	}
	#theme-selector ~ #theme-tweaker-toggle::after {
		content: "Open theme tweaker";
		position: absolute;
		font-size: 0.625em;
		white-space: nowrap;
		left: -50%;
		top: 100%;
	}
	#theme-selector.engaged ~ #theme-tweaker-toggle {
		visibility: visible;
		top: 530px;
		left: 0;
		right: 0;
		margin: auto;
		z-index: 11111;
		transition: 
			top 0.2s ease,
			visibility 0.2s ease;
	}
	@media only screen and (max-height: 675px) {
		#theme-selector.engaged ~ #theme-tweaker-toggle {	
			top: 492px;
		}
	}
	<?php fit_content("#theme-selector.engaged ~ #theme-tweaker-toggle"); ?>
	#theme-selector.engaged ~ #theme-tweaker-toggle button {
		opacity: 1.0;
	}
}

/******************/
/* WIDTH SELECTOR */
/******************/

#width-selector {
	position: absolute;
	top: 4px;
	right: -78px;
}
#width-selector button {
	width: 22px;
	height: 22px;
	padding: 6px;
	margin: 1px;
	overflow: hidden;
	background-repeat: no-repeat;
	background-size: 100%;
	background-origin: content-box;
}
#width-selector button,
#width-selector button:active,
#width-selector button:focus {
	text-shadow: none;
	color: transparent;
}
#width-selector button:disabled {
	cursor: auto;
}
#width-selector button.select-width-normal {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/normal.gif")) ?>');
}
#width-selector button.select-width-wide {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/wide.gif")) ?>');
}
#width-selector button.select-width-fluid {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/fluid.gif")) ?>');
}

/*	If the viewport is this narrow, then the different width settings do 
	nothing, so no reason to show the selector.
	*/
@media only screen and (max-width: 1220px) {
	#width-selector {
		display: none;
	}
}

/*=----------------=*/
/*= Hover tooltips =*/
/*=----------------=*/

#width-selector button::after {
	content: attr(data-name);
	position: absolute;
	display: block;
	left: 0;
	width: 100%;
	text-align: center;
	top: 56px;
	visibility: hidden;
}
#width-selector button.selected::after {
	content: attr(data-name) " (selected)";
}
#width-selector button:hover:not(:active)::after {
	visibility: visible;
}

<?php
global $content_width_settings;
foreach ($content_width_settings as $name => $setting) {
	echo "head.content-width-{$name} + body > * {\n	max-width: {$setting};\n}\n";
}
?>

/********************/
/* QUICK NAV WIDGET */
/********************/

#quick-nav-ui {
	position: absolute;
	right: -67px;
	bottom: 20px;
}
#quick-nav-ui a {
	font-family: var(--GW-Font-Awesome);
	font-weight: 900;
	font-size: 1.5rem;
	line-height: 1.7;
	text-align: center;
	display: block;
	width: 40px;
	height: 40px;
	margin: 10px 0 0 0;
}
#quick-nav-ui a[href='#comments'].no-comments {
	pointer-events: none;
}
#quick-nav-ui a {
	visibility: hidden;
}
#content.post-page ~ #ui-elements-container #quick-nav-ui a[href='#comments'] {
	visibility: visible;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 1080px) {
	#quick-nav-ui {
		right: -54px;
	}
}
@media only screen and (max-width: 1040px) {
	#quick-nav-ui {
		right: -49px;
	}
}
@media only screen and (max-width: 1020px) {
	#quick-nav-ui {
		right: -20px;
	}
}
@media only screen and (max-width: 960px) {
	#quick-nav-ui {
		max-width: 0px;
		transition:
			max-width 0.2s ease,
			visibility 0.2s ease;
		display: flex;
		right: 72px;
		bottom: 14px;
	}
	#quick-nav-ui.engaged {
		visibility: visible;
		max-width: 1000px;
	}
	#quick-nav-ui a {
		position: relative;
		margin: 2px;
	}
	#quick-nav-ui a + a {
		margin-left: 5px;
	}
	#quick-nav-ui a::after {
		position: absolute;
		top: calc(100% + 2px);
		font-size: 0.375rem;
		left: 0;
		right: 0;
		margin: auto;
		line-height: 1;
		padding: 2px;
		text-transform: uppercase;
		z-index: -1;

		font-family: var(--GW-UI-font);
		font-weight: var(--GW-UI-font-weight-heavy);
	}
	<?php fit_content("#quick-nav-ui a::after"); ?>
	#quick-nav-ui a[href='#top']::after {
		content: "Top";
		left: -1px;
	}
	#quick-nav-ui a[href='#comments']::after {
		content: "Comments";
	}
	#content.post-page:not(.individual-thread-page) ~ #ui-elements-container #quick-nav-ui a[href='#comments'] {
		visibility: hidden;
		transition: visibility 0.2s ease;
	}
	#content.post-page:not(.individual-thread-page) ~ #ui-elements-container #quick-nav-ui.engaged a[href='#comments'] {
		visibility: visible;
	}
	#quick-nav-ui a[href='#bottom-bar']::after {
		content: "Bottom";
	}
}

/**********************/
/* NEW COMMENT NAV UI */
/**********************/

#new-comment-nav-ui {
	position: absolute;
	right: -112px;
	bottom: 42px;
}
#new-comment-nav-ui > * {
	display: block;
	position: relative;
}
#new-comment-nav-ui.no-comments {
	display: none;
}

/*=--------------------=*/
/*= New comments count =*/
/*=--------------------=*/

#new-comment-nav-ui .new-comments-count {
	width: 2em;
	font-size: 1.25rem;
	line-height: 1.1;
	text-align: center;
	left: 1px;
	cursor: pointer;

	font-weight: var(--GW-UI-font-weight-heavy);
}
#new-comment-nav-ui .new-comments-count::selection {
	background-color: transparent;
}
#new-comment-nav-ui .new-comments-count::after {
	content: "NEW";
	display: block;
	font-size: 0.625rem;

	font-weight: var(--GW-UI-font-weight-heavy);
}

/*=-----------------------------------=*/
/*= Next/previous new comment buttons =*/
/*=-----------------------------------=*/

#new-comment-nav-ui .new-comment-sequential-nav-button {
	font-size: 1.75rem;
	font-family: var(--GW-Font-Awesome);
	font-weight: 900;
	width: 1.5em;
	z-index: 5001;
}
#new-comment-nav-ui .new-comment-previous {
	top: 8px;
}
#new-comment-nav-ui .new-comment-next {
	bottom: 6px;
}
#new-comment-nav-ui .new-comment-sequential-nav-button:disabled {
	cursor: auto;
	pointer-events: none;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 1160px) {
	#new-comment-nav-ui {
		bottom: 180px;
		right: -68px;
	}
}
@media only screen and (max-width: 1080px) {
	#new-comment-nav-ui {
		right: -55px;
	}
}
@media only screen and (max-width: 1040px) {
	#new-comment-nav-ui {
		right: -50px;
	}
}
@media only screen and (max-width: 1020px) {
	#new-comment-nav-ui {
		right: -21px;
	}
	#new-comment-nav-ui .new-comments-count::before {
		content: "";
		position: absolute;
		width: 100%;
		height: calc(100% + 45px);
		z-index: -1;
		left: 0;
		top: -22px;
	}
}
@media only screen and (max-width: 960px) {
	#new-comment-nav-ui {
		max-width: 0px;
		max-height: 0px;
		transition: 
			max-width 0.2s ease,
			max-height 0.2s ease,
			visibility 0.2s ease;
		display: flex;
		right: 78px;
		bottom: 70px;
	}
	#new-comment-nav-ui::before {
		content: "New Comments";
		position: absolute;
		bottom: 100%;
		font-size: 0.5625rem;
		left: 0;
		right: 0;
		margin: auto;
		padding: 2px 3px;
		text-transform: uppercase;
		z-index: -1;

		font-family: var(--GW-UI-font);
		font-weight: var(--GW-UI-font-weight-heavy);
	}
	<?php fit_content("#new-comment-nav-ui::before"); ?>
	#new-comment-nav-ui.engaged {
		visibility: visible;
		max-width: 1000px;
		max-height: 1000px;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button {
		top: unset;
		bottom: unset;
		padding: 2px 7px;
	}
	#new-comment-nav-ui .new-comment-sequential-nav-button.new-comment-previous {
		padding: 2px 7px 3px 7px;
	}
	#new-comment-nav-ui .new-comments-count {
		padding: 4px 0 5px 0;
	}
	#new-comment-nav-ui .new-comments-count::before {
		display: none;
	}
	#new-comment-nav-ui button::after {
		position: absolute;
		font-size: 0.375rem;
		left: 0;
		right: 0;
		margin: auto;
		line-height: 1;
		text-transform: uppercase;
		pointer-events: none;
		
		font-family: var(--GW-UI-font);
	}
	#new-comment-nav-ui button.new-comment-previous::after {
		content: "Previous";
		bottom: 5px;
	}
	#new-comment-nav-ui button.new-comment-next::after {
		content: "Next";
		top: 7px;
	}
}

/*******************/
/* HNS DATE PICKER */
/*******************/

#hns-date-picker {
	position: absolute;
	bottom: 72px;
	right: -253px;
	opacity: 0.6;
}
#hns-date-picker:hover, 
#hns-date-picker:focus-within {
	opacity: 1.0;
}
#hns-date-picker.no-comments {
	display: none;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 1440px) {
	#hns-date-picker {
		right: -81px;
		padding: 8px 10px 10px 10px;
		bottom: 62px;
		display: none;
	}
	#hns-date-picker::before {
		content: "";
		position: absolute;
		display: block;
		z-index: -1;
		height: calc(100% + 2px);
		top: -1px;
		left: -1px;
		width: 50%;
	}
}
@media only screen and (max-width: 1160px) {
	#hns-date-picker {
		bottom: 200px;
		right: -36px;
	}
	#hns-date-picker::before {
		width: calc(100% - 35px);
	}
}
@media only screen and (max-width: 1080px) {
	#hns-date-picker {
		right: -23px;
	}
	#hns-date-picker::before {
		width: calc(100% - 22px);
	}
}
@media only screen and (max-width: 1040px) {
	#hns-date-picker {
		right: -18px;
	}
	#hns-date-picker::before {
		width: calc(100% - 17px);
	}
}
@media only screen and (max-width: 1020px) {
	#hns-date-picker {
		right: 19px;
	}
	#hns-date-picker::before {
		width: 100%;
	}
}
@media only screen and (max-width: 960px) {
	#hns-date-picker {
		max-height: 0px;
		bottom: 132px;
		right: 62px;
		transition:
			max-height 0.2s ease,
			visibility 0.2s ease;
	}
	#hns-date-picker.engaged {
		visibility: visible;
		max-height: 1000px;
	}
	#hns-date-picker::before {
		width: calc(100% + 2px);
		border-width: 1px !important;
	}
}

/*=---------------=*/
/*= "Since" label =*/
/*=---------------=*/

#hns-date-picker span {
	display: block;
	font-size: 0.75rem;
	text-transform: uppercase;

	font-weight: var(--GW-UI-font-weight-heavy);
}

/*=--------------------=*/
/*= "Since" text field =*/
/*=--------------------=*/

#hns-date-picker input {
	margin-top: 1px;
	padding: 1px 3px;
	width: 140px;
	text-align: center;
	box-shadow: 0 0 0 1px transparent;
}

/************************/
/* ANTI-KIBITZER TOGGLE */
/************************/

#anti-kibitzer-toggle {
	position: absolute;
	right: -67px;
	bottom: 225px;
}
#anti-kibitzer-toggle button {
	display: block;
	width: 40px;
	height: 54px;
	padding: 0;
}
#anti-kibitzer-toggle button::before,
#anti-kibitzer-toggle button::after {
	font-family: var(--GW-Font-Awesome);
}
#anti-kibitzer-toggle button::before {
	content: "\F06E";
	display: block;
	font-size: 1.75em;
	font-weight: 400;
}
#anti-kibitzer-toggle button::after {
	content: "\F007\2004\F164";
	font-size: 0.875em;
	font-weight: 900;
}
#anti-kibitzer-toggle.engaged button::before {
	content: "\F070";
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 1160px) {
	#anti-kibitzer-toggle {
		bottom: 330px;
	}
}
@media only screen and (max-width: 1080px) {
	#anti-kibitzer-toggle {
		right: -54px;
	}
}
@media only screen and (max-width: 1040px) {
	#anti-kibitzer-toggle {
		right: -50px;
	}
}
@media only screen and (max-width: 1020px) {
	#anti-kibitzer-toggle {
		right: -20px;
	}
}
@media only screen and (max-width: 960px) {
	#theme-selector ~ #anti-kibitzer-toggle {
		top: 100%;
		bottom: unset;
		left: 0;
		right: 0;
		margin: auto;
		box-shadow: none;
		width: calc(100vw - 44px);
		max-width: 330px;
		text-align: right;
		pointer-events: none;
	}
	#theme-selector.engaged ~ #anti-kibitzer-toggle {
		visibility: visible;
		z-index: 11110;
		top: 530px;
		transition: 
			top 0.2s ease,
			visibility 0.2s ease;
	}
	@media only screen and (max-height: 675px) {
		#theme-selector.engaged ~ #anti-kibitzer-toggle {	
			top: 492px;
		}
	}
	#theme-selector.engaged ~ #anti-kibitzer-toggle button {
		pointer-events: auto;
		display: inline-block;
	}
}

/************************/
/* TEXT SIZE ADJUSTMENT */
/************************/

#text-size-adjustment-ui {
	position: absolute;
	top: 30px;
	right: -78px;
	opacity: 0.4;
}
#text-size-adjustment-ui:hover {
	opacity: 1.0;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 1220px) {
	#text-size-adjustment-ui {
		top: 4px;
	}
}
@media only screen and (max-width: 1080px) {
	#text-size-adjustment-ui {
		top: 112px;
		right: -30px;
	}
	#text-size-adjustment-ui button {
		display: block;
		position: relative;
	}
	#text-size-adjustment-ui button.increase {
		bottom: 48px;
	}
	#text-size-adjustment-ui button.decrease {
		top: 50px;
	}
	#text-size-adjustment-ui::after {
		display: none;
	}
}
@media only screen and (max-width: 1000px) {
	#text-size-adjustment-ui {
		right: -12px;
	}
}
@media only screen and (max-width: 960px) {
	#text-size-adjustment-ui {
		display: none;
	}
}

/* This doesn't work in Mozilla browsers, so hide it */
<?php echo $firefox_exclusive; ?> {
	#text-size-adjustment-ui {
		display: none;
	}
}

/*=---------=*/
/*= Buttons =*/
/*=---------=*/

#text-size-adjustment-ui button {
	font-weight: 900;
	font-family: var(--GW-Font-Awesome);
	font-size: 0.75rem;
	width: 24px;
	height: 24px;
	padding: 0;
}
#text-size-adjustment-ui button.default {
	font-family: inherit;
	font-size: 1.125rem;
	position: relative;
	top: 1px;

	font-weight: var(--GW-UI-font-weight-heavy);
}
#text-size-adjustment-ui button:disabled {
	opacity: 0.5;
}
#text-size-adjustment-ui button:disabled:hover {
	cursor: default;
}

/*=----------------=*/
/*= Hover tooltips =*/
/*=----------------=*/

#text-size-adjustment-ui::after {
	content: "Adjust text size";
	position: absolute;
	display: block;
	left: 0;
	width: 100%;
	text-align: center;
	top: 32px;
	visibility: hidden;
	font-size: 0.9em;
}
#text-size-adjustment-ui:hover::after {
	visibility: visible;
}

/*****************/
/* KEYBOARD HELP */
/*****************/

#keyboard-help-overlay {
	width: 100vw;
	height: 100vh;
	background-color: rgba(0,0,0,0.7);
	position: fixed;
	left: 0;
	top: 0;
	z-index: 5001;

	display: flex;
	justify-content: center;
	align-items: center;
	padding: 20px 30px 30px 20px;

	visibility: hidden;

	font-family: var(--GW-keyboard-help-overlay-font);
}

#keyboard-help-overlay .keyboard-help-container {
	background-color: #fff;
	filter: drop-shadow(4px 4px 2px #000);
	flex: 1 1 auto;
	max-width: 1500px;
	max-height: 100%;
	overflow-y: auto;
	position: relative;
}
#keyboard-help-overlay .keyboard-help-container h1 {
	text-align: center;
	border-bottom: 1px solid #ddd;
	margin: 0;
	padding: 10px 20px;
}
#keyboard-help-overlay .keyboard-help-container .note {
	margin: 0.5em auto;
	padding: 0 1em;
	width: fit-content;
}
#keyboard-help-overlay .keyboard-help-container .keyboard-shortcuts-lists {
	column-width: 21em;
	column-count: auto;
	column-gap: 1.5em;
	border-top: 1px solid #ddd;
	padding: 15px 20px;
}
#keyboard-help-overlay .keyboard-help-container ul {
	list-style-type: none;
	margin: 0;
	padding: 0;
	break-inside: avoid;
	white-space: nowrap;
}
#keyboard-help-overlay .keyboard-help-container ul:nth-of-type(n+2) {
	margin: 20px 0 0 0;
}
#keyboard-help-overlay .keyboard-help-container ul li.section {
	font-weight: bold;
	font-size: 1.125rem;
	break-after: avoid;
}
#keyboard-help-overlay .keyboard-help-container .keys {
	margin: 0 0.5em 0 0;
	min-width: 4.5em;
	display: inline-block;
}
#keyboard-help-overlay .keyboard-help-container .keys code {
	margin: 0 6px 0 0;
}
#keyboard-help-overlay .keyboard-help-container code {
	display: inline-block;
	background-color: #eee;
	border: 1px solid #ccc;
	padding: 3px 8px 4px 8px;
	margin: 0 1px;
}
#keyboard-help-overlay .keyboard-help-container code.ak {
	background-color: #ffeb83;
	border-color: #d4a500;
}
#keyboard-help-overlay .keyboard-help-container code.ak::before {
	content: "ak+";
	opacity: 0.3;
}

#nav-item-about button.open-keyboard-help {
	display: none;
}

/*	Show the “open keyboard help” button on the “About” tab only on wide
	(i.e., assumed-to-be-desktop) viewports.
	*/
@media only screen and (min-width: 961px) {
	#nav-item-about {
		position: relative;
		padding-right: 0.25em;
	}
	#nav-item-about button.open-keyboard-help {
		font-family: var(--GW-Font-Awesome);
		font-weight: 900;
		position: absolute;
		top: 0;
		right: 0;
		height: 100%;
		padding: 8px;
		display: initial;
		line-height: 1;
	}
}

#keyboard-help-overlay button.close-keyboard-help {
	position: absolute;
	right: 0;
	top: 0;
	font-family: var(--GW-Font-Awesome);
	font-size: 1.5rem;
	padding: 10px 12px;
}

/******************/
/* IMAGES OVERLAY */
/******************/
/* (To exclude images in posts from theme tweaks) */

#images-overlay {
	position: absolute;
	z-index: 1;
	left: 0;
	right: 0;
	margin: auto;
}

#images-overlay + #content .post-body img {
	visibility: hidden;
}

#images-overlay div {
	position: absolute;
}
#images-overlay div::after {
	content: "Click to enlarge";
	display: block;
	position: absolute;
	margin: auto;
	left: 0;
	right: 0;
	bottom: 10px;
	padding: 6px 10px;
	font-size: 1.25rem;
	background-color: rgba(0,0,0,0.6);
	color: #fff;
	border-radius: 5px;
	opacity: 0.0;
	transition: opacity 0.15s ease;
	pointer-events: none;
}
<?php fit_content("#images-overlay div::after"); ?>
#images-overlay div:hover::after {
	opacity: 1.0;
}

#images-overlay img {
	width: 100%;
}

