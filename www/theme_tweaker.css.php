/*****************/
/* THEME TWEAKER */
/*****************/

:root {
    --basilisk: url('data:image/png;base64,<?php echo base64_encode(file_get_contents("assets/basilisk.png")) ?>');
}

#theme-tweaker-ui {
	position: fixed;
	width: 100vw;
	height: 100vh;
	top: 0;
	left: 0;
	z-index: 9000;
	font-family: MS Sans Serif;
	font-size: 1rem;
	line-height: 1.15;
}
#theme-tweaker-ui::before {
	content: "";
	position: fixed;
	width: 100vw;
	height: 100vh;
	top: 0;
	left: 0;
	background-color: #000;
	opacity: 0.6;
}
#theme-tweaker-ui::after {
	content: "";
	position: fixed;
	top: 87px;
	right: calc((100% - 900px) / 2 + 26px);
	background-color: #bfb8bf;
	width: 18px;
	height: 16px;
	z-index: 2;
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/win95_close_widget.gif")) ?>');
	background-size: 100%;
	background-repeat: no-repeat;
	background-position: center center;
	box-shadow: 
		0 -1px 0 0 #fff8ff,
		-1px 0 0 0 #fff8ff,
		-1px -1px 0 0 #fff8ff,
		0 0 0 1px #7f787f,
		0 -1px 0 1px #dfd8df,
		-1px 0 0 1px #dfd8df,
		-1px -1px 0 1px #dfd8df,
		0 0 0 2px #030303;
	cursor: pointer;
}
#theme-tweaker-ui .main-theme-tweaker-window {
	position: fixed;
	z-index: 1;
	background-color: #bfb8bf;
	color: #000;
	box-shadow: 
		0 -1px 0 0 #fff8ff,
		-1px 0 0 0 #fff8ff,
		-1px -1px 0 0 #fff8ff,
		0 0 0 1px #7f787f,
		0 -1px 0 1px #dfd8df,
		-1px 0 0 1px #dfd8df,
		-1px -1px 0 1px #dfd8df,
		0 0 0 2px #030303;
	width: calc(900px - 40px);
	max-width: calc(100vw - 20px);
	max-height: calc(100vh - 160px);
	top: 80px;
	left: calc((100% - 900px) / 2 + 20px);
	padding: 30px 10px 10px 10px;
}
#theme-tweaker-ui .controls-container {
	overflow-y: scroll;
	max-height: calc(100vh - 320px);
	box-shadow: 
		0 -1px 0 0 #030303,
		-1px 0 0 0 #030303,
		-1px -1px 0 0 #030303,
		0 0 0 1px #dfd8df,
		0 -1px 0 1px #7f787f,
		-1px 0 0 1px #7f787f,
		-1px -1px 0 1px #7f787f,
		0 0 0 2px #fff8ff;
	padding: 16px;
	margin: 4px;
}

#theme-tweaker-ui h1 {
	font-size: inherit;
	font-weight: normal;
	margin: 0;
	position: absolute;
	background-color: #03037f;
	background-repeat: no-repeat;
	background-size: 20px;
	color: #fff8ff;
	padding: 4px 6px 3px 30px;
	background-position: 4px 50%;
	left: 2px;
	top: 2px;
	width: calc(100% - 4px);
	cursor: default;
	text-shadow: 1px 0 0 #fff;
	letter-spacing: 1px;
}
#theme-tweaker-ui .main-theme-tweaker-window h1 {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/win95_themes_icon.gif")) ?>');
}

#theme-tweaker-ui .current-theme {
	margin: 1em -8px 1.25em -8px;
	padding: 0 12px 1em 12px;
	box-shadow: 
		0 1px 0 #7f787f,
		0 2px 0 #fff8ff;
	cursor: default;
}
#theme-tweaker-ui .current-theme span {
	text-shadow: 1px 0 0 #000;
	letter-spacing: 1px;
}

#theme-tweaker-ui .theme-selector {
	position: absolute;
	top: 19px;
	left: 225px;
}
#theme-tweaker-ui .theme-selector button {
	width: 26px;
	height: 26px;
	display: inline-block;
	padding: 1px 0 0 1px;
	border: 4px solid #bfb8bf;
	margin: 8px 10px 8px 4px;
}
#theme-tweaker-ui .theme-selector button:active,
#theme-tweaker-ui .theme-selector button.selected {
	box-shadow: 
		0 -1px 0 0 #030303,
		-1px 0 0 0 #030303,
		-1px -1px 0 0 #030303,
		0 0 0 1px #dfd8df,
		0 -1px 0 1px #7f787f,
		-1px 0 0 1px #7f787f,
		-1px -1px 0 1px #7f787f,
		0 0 0 2px #fff8ff;
}

#theme-tweaker-ui div.section {
	border: 1px solid #7f787f;
	box-shadow:
		1px 1px 0 #fff8ff inset,
		0 1px 0 #fff8ff,
		1px 0 0 #fff8ff,
		1px 1px 0 #fff8ff;
	padding: 15px 10px 10px 12px;
	position: relative;
	margin: 0.5em 0 0.5em 0;
}
#theme-tweaker-ui div.section + div.section {
	margin: 1.5em 0 0.5em 0;
}
#theme-tweaker-ui div.section::before {
	content: attr(data-label);
	position: absolute;
	display: block;
	background-color: #bfb8bf;
	top: -0.5em;
	left: 1em;
	padding: 0 4px;
}

#theme-tweaker-ui #theme-tweak-section-sample-text {
	position: sticky;
	top: 8px;
	background-color: #bfb8bf;
	z-index: 1;
	box-shadow:
		1px 1px 0 #fff8ff inset,
		0 1px 0 #fff8ff,
		1px 0 0 #fff8ff,
		1px 1px 0 #fff8ff,
		0 -24px 0 1px #bfb8bf,
		0  2px 0 1px rgba(191, 184, 191, 1.0),
		0  4px 0 1px rgba(191, 184, 191, 0.875),
		0  6px 0 1px rgba(191, 184, 191, 0.75),
		0  8px 0 1px rgba(191, 184, 191, 0.625),
		0 10px 0 1px rgba(191, 184, 191, 0.5),
		0 12px 0 1px rgba(191, 184, 191, 0.375),
		0 14px 0 1px rgba(191, 184, 191, 0.25),
		0 16px 0 1px rgba(191, 184, 191, 0.125);
}
#theme-tweaker-ui #theme-tweak-section-sample-text .sample-text-container {
	display: table;
	background-color: #fff8ff;
	box-shadow:
		-1px -1px 0 #dfd8df inset,
		1px 1px 0 #030303 inset,
		0 -1px 0 #7f787f,
		-1px 0 0 #7f787f,
		-1px -1px 0 #7f787f,
		0 0 0 1px #fff8ff;
	height: 135px;
	width: 260px;
	padding: 1px;
	margin: 0 auto 0.5em auto;
}
/* This only needs to support Webkit because Firefox doesn’t support text size adjustment anyhow. */
@supports (min-width: fit-content) {
	#theme-tweaker-ui #theme-tweak-section-sample-text .sample-text-container {
		min-width: fit-content;
	}
}
#theme-tweaker-ui #theme-tweak-section-sample-text .sample-text {
	display: table-cell;
	width: 100%;
	text-align: center;
	vertical-align: middle;
}

#theme-tweaker-ui #theme-tweak-section-text-size-adjust {
	text-align: center;
}
#theme-tweaker-ui #theme-tweak-section-text-size-adjust button {
	margin: 6px 3px;
	padding: 0;
	height: 26px;
	width: 26px;
	background-repeat: no-repeat;
	background-position: center center;
	background-size: 100%;
}
#theme-tweaker-ui #theme-tweak-section-text-size-adjust .text-size-adjust-button.decrease {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/A_minus.gif")) ?>');
}
#theme-tweaker-ui #theme-tweak-section-text-size-adjust .text-size-adjust-button.default {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/A.gif")) ?>');
}
#theme-tweaker-ui #theme-tweak-section-text-size-adjust .text-size-adjust-button.increase {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/A_plus.gif")) ?>');
}
#theme-tweaker-ui #theme-tweak-section-text-size-adjust .text-size-adjust-button:disabled {
	cursor: default;
	opacity: 0.5;
}
/* This doesn't work in Mozilla browsers, so hide it */
@-moz-document url-prefix() {
	#theme-tweaker-ui #theme-tweak-section-text-size-adjust {
/* 
		height: 0;
		overflow: hidden;
		padding: 0;
		border: 0;
		margin: 0 0 -16px 0;
		visibility: hidden;
 */
	display: none;
	}
}

#theme-tweaker-ui div.section#theme-tweak-section-invert {
	padding: 23px 10px 15px 10px;
}

#theme-tweaker-ui input[type='checkbox'] {
	height: 0;
	width: 0;
	margin: 0;
	opacity: 0;
	pointer-events: none;
}
#theme-tweaker-ui input[type='checkbox'] + label {
	position: relative;
	padding: 0.35em 0.25em 0.25em 1.75em;
	cursor: pointer;
	line-height: 1.2;
}
#theme-tweaker-ui input[type='checkbox']:focus + label {
	outline: 1px dotted #000;
}
#theme-tweaker-ui input[type='checkbox'] + label::before {
	content: "";
	width: 1rem;
	height: 1rem;
	position: absolute;
	left: 0.25em;
	top: 5px;
	background-color: #fff8ff;
	box-shadow:
		-1px -1px 0 #dfd8df inset,
		1px 1px 0 #030303 inset,
		0 -1px 0 #7f787f,
		-1px 0 0 #7f787f,
		-1px -1px 0 #7f787f,
		0 0 0 1px #fff8ff;
	cursor: pointer;
}
#theme-tweaker-ui input[type='checkbox']:checked + label::before {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/win95_checkmark.gif")) ?>');	
	background-size: 14px;
	background-repeat: no-repeat;
	background-position: center center;
}

#theme-tweaker-ui input[type='range'] {
	width: 100%;
	position: relative;
	-webkit-appearance: none;
	width: 100%;
	height: 50px;
	margin: 0;
	padding: 0 3.5em 0 2.3em;
	background-color: transparent;
	border: none;
	cursor: pointer;
}
input[type='range']:focus {
	outline: 1px dotted #000;
}
input[type='range']::-webkit-slider-runnable-track {
	width: 100%;	
	height: 4px;
	box-sizing: content-box;
	box-shadow:
		-1px -1px 0 0 #fff8ff inset,
		1px 1px 0 #7f787f inset,
		-2px -2px 0 #dfd8df inset,
		2px 2px 0 #030303 inset;
	border-radius: 0;
	border-color: #bfb8bf;
	border-style: solid;
	border-width: 23px 0 23px 0;
}
input[type='range']::-webkit-slider-thumb {
	box-shadow: 
		0 -1px 0 0 #fff8ff,
		-1px 0 0 0 #fff8ff,
		-1px -1px 0 0 #fff8ff,
		0 0 0 1px #7f787f,
		0 -1px 0 1px #dfd8df,
		-1px 0 0 1px #dfd8df,
		-1px -1px 0 1px #dfd8df,
		0 0 0 2px #030303;
	border: none;
	height: 34px;
	width: 14px;
	border-radius: 0px;
	background: #bfb8bf;
	-webkit-appearance: none;
	margin-top: -16px;
}
input[type='range']::-moz-range-track {
	width: 100%;	
	height: 4px;
	box-sizing: content-box;
	box-shadow:
		-1px -1px 0 0 #fff8ff inset,
		1px 1px 0 #7f787f inset,
		-2px -2px 0 #dfd8df inset,
		2px 2px 0 #030303 inset;
	border-radius: 0;
	border-color: #bfb8bf;
	border-style: solid;
	border-width: 23px 0 23px 0;
}
input[type='range']::-moz-range-thumb {
	box-shadow: 
		0 -1px 0 0 #fff8ff,
		-1px 0 0 0 #fff8ff,
		-1px -1px 0 0 #fff8ff,
		0 0 0 1px #7f787f,
		0 -1px 0 1px #dfd8df,
		-1px 0 0 1px #dfd8df,
		-1px -1px 0 1px #dfd8df,
		0 0 0 2px #030303;
	border: 0px solid #000;
	height: 34px;
	width: 14px;
	border-radius: 0px;
	background: #bfb8bf;
}
input[type='range']::-ms-track {
	width: 100%;	
	height: 4px;
	box-sizing: content-box;
	box-shadow:
		-1px -1px 0 0 #fff8ff inset,
		1px 1px 0 #7f787f inset,
		-2px -2px 0 #dfd8df inset,
		2px 2px 0 #030303 inset;
	color: transparent;
	border-color: #bfb8bf;
	border-style: solid;
	border-width: 23px 0 23px 0;
}
input[type='range']::-ms-thumb {
	box-shadow: 
		0 -1px 0 0 #fff8ff,
		-1px 0 0 0 #fff8ff,
		-1px -1px 0 0 #fff8ff,
		0 0 0 1px #7f787f,
		0 -1px 0 1px #dfd8df,
		-1px 0 0 1px #dfd8df,
		-1px -1px 0 1px #dfd8df,
		0 0 0 2px #030303;
	border: none;
	height: 34px;
	width: 14px;
	border-radius: 0px;
	background: #bfb8bf;
}

#theme-tweaker-ui input[type='range']::before,
#theme-tweaker-ui input[type='range']::after {
	position: absolute;
	top: 1.05em;
	color: #666;
}
#theme-tweaker-ui #theme-tweak-control-saturate::before,
#theme-tweaker-ui #theme-tweak-control-brightness::before,
#theme-tweaker-ui #theme-tweak-control-contrast::before {
	content: "0%";
	left: 0.3em;
}
#theme-tweaker-ui #theme-tweak-control-saturate::after,
#theme-tweaker-ui #theme-tweak-control-brightness::after,
#theme-tweaker-ui #theme-tweak-control-contrast::after {
	content: "300%";
	right: 0.3em;
}
#theme-tweaker-ui #theme-tweak-control-hue-rotate::before {
	content: "0°";
	left: 0.9em;
}
#theme-tweaker-ui #theme-tweak-control-hue-rotate::after {
	content: "360°";
	right: 0.7em;
}
.theme-tweak-control-label {
	margin: 1em 3.45em 0 2.3em;
	padding: 0 2em 0 3.15em;
	text-align: center;
	position: relative;
	text-shadow: 1px 0 0 #000;
	letter-spacing: 1px;
}
.theme-tweak-control-label + .notch {
	position: absolute;
	box-shadow:
		-0.5px -0.5px 0 #dfd8df inset,
		0.5px 0.5px 0 #030303 inset,
		0 -0.5px 0 #7f787f,
		-1px 0 0 #7f787f,
		-1px -0.5px 0 #7f787f,
		0 0 0 0.5px #fff8ff;
	width: 1px;
	height: 1em;
	top: 4em;
	left: calc((100% - 5.8em) / 3 + 2.3em + 7px);
	cursor: pointer;
}
#theme-tweak-label-hue-rotate + .notch {
	left: calc(3.45em);
}
.theme-tweak-control-label + .notch::before {
	content: "";
	display: block;
	position: absolute;
	height: 100%;
	width: 1em;
	top: 0;
	left: -0.5em;
	background-color: transparent;
}

#theme-tweaker-ui .buttons-container {
	text-align: right;
	margin: 10px 3px 0 0;
}
#theme-tweaker-ui button {
	box-shadow: 
		0 -1px 0 0 #fff8ff,
		-1px 0 0 0 #fff8ff,
		-1px -1px 0 0 #fff8ff,
		0 0 0 1px #7f787f,
		0 -1px 0 1px #dfd8df,
		-1px 0 0 1px #dfd8df,
		-1px -1px 0 1px #dfd8df,
		0 0 0 2px #030303;
	margin: 0.5em 1px 0.5em 1em;
	width: 7em;
	padding: 7px 0 6px 0;
}
#theme-tweaker-ui .buttons-container button {
	color: inherit;
}
#theme-tweaker-ui button.default-button {
	padding: 6px 0 6px 0;
	box-shadow: 
		0 -1px 0 0 #fff8ff,
		-1px 0 0 0 #fff8ff,
		-1px -1px 0 0 #fff8ff,
		0 0 0 1px #7f787f,
		0 -1px 0 1px #dfd8df,
		-1px 0 0 1px #dfd8df,
		-1px -1px 0 1px #dfd8df,
		0 0 0 2px #030303,
		0 0 0 3px #030303;
}
#theme-tweaker-ui button:hover {
	text-shadow: none;
}
#theme-tweaker-ui button:active {
	transform: none;
	box-shadow:
		-1px -1px 0 #dfd8df inset,
		1px 1px 0 #030303 inset,
		0 -1px 0 #7f787f,
		-1px 0 0 #7f787f,
		-1px -1px 0 #7f787f,
		0 0 0 1px #fff8ff;
}
#theme-tweaker-ui .buttons-container button:active {
	padding: 7px 0 6px 0;
}
#theme-tweaker-ui button:focus {
	outline: 1px dotted #000;
}

#theme-tweaker-ui .minimize-button {
	width: 18px;
	height: 16px;
	position: absolute;
	top: -1px;
	right: 30px;
	background-size: 14px;
	background-repeat: no-repeat;
	background-position: center center;
}
#theme-tweaker-ui .minimize-button.minimize {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/win95_minimize_widget.gif")) ?>');	
}
#theme-tweaker-ui .minimize-button.maximize {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/win95_maximize_widget.gif")) ?>');	
}

#theme-tweaker-ui .help-button {
	width: 18px;
	height: 16px;
	position: absolute;
	top: -1px;
	right: 55px;
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/win95_help_widget.gif")) ?>');	
	background-size: 14px;
	background-repeat: no-repeat;
	background-position: center center;
}

#theme-tweaker-ui .reset-defaults-button {
	width: 10em;
	float: left;
}

#theme-tweaker-ui .controls-container::-webkit-scrollbar {
	width: 20px;
	background-color: #bfb8bf;
}
#theme-tweaker-ui .controls-container::-webkit-scrollbar-track {
	background-color: #fff8ff;
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/win95_scrollbar_track.gif")) ?>');	
}
 
#theme-tweaker-ui .controls-container::-webkit-scrollbar-thumb {
	background-color: #bfb8bf;
	box-shadow:
		-1px -1px 0 0 #030303 inset,
		1px 1px 0 0 #dfd8df inset,
		-1px -1px 0 1px #7f787f inset,
		0 0 0 2px #fff8ff inset;	
}

#theme-tweaker-ui .clippy-container {
	position: fixed;
	z-index: 1;
	background-color: #ffa;
	width: 13em;
	left: 2em;
	bottom: 12em;
	padding: 1em;
	border-radius: 8px;
	border: 1px solid #000;
	cursor: default;
}
@media only screen and (max-width: 1305px) {
	#theme-tweaker-ui .clippy-container {
		visibility: hidden;
	}
}
#theme-tweaker-ui .clippy-container::before {
	content: "";
	width: 0;
	height: 0;
	border-top: 42px solid #000;
	border-right: 42px solid transparent;
	position: absolute;
	bottom: -42px;
	right: 69px;
}
#theme-tweaker-ui .clippy-container::after {
	content: "";
	width: 0;
	height: 0;
	border-top: 40px solid #ffa;
	border-right: 40px solid transparent;
	position: absolute;
	bottom: -40px;
	right: 70px;
}
#theme-tweaker-ui .clippy-container:hover {
	opacity: 1.0;
}
#theme-tweaker-ui .clippy-container .hint {
	line-height: 1.3;
	color: #000;
}
#theme-tweaker-ui .clippy-container .clippy {
	width: 200px;
	height: 125px;
	background-image: var(--basilisk); 
	background-size: 100%;
	position: absolute;
	bottom: -150px;
	left: 0;
}
@media only screen and (max-width: 1305px) {
	#theme-tweaker-ui .clippy-container .clippy {
		visibility: visible;
		transform: scale(0.75) translate(-50px, 60px)
	}
}
@media only screen and (max-width: 1220px) {
	#theme-tweaker-ui .clippy-container .clippy {
		visibility: visible;
		transform: scale(0.625) translate(-90px, 100px)
	}
}
@media only screen and (max-width: 1140px) {
	#theme-tweaker-ui .clippy-container .clippy {
		visibility: visible;
		transform: scale(0.5) translate(-140px, 140px)
	}
}

#theme-tweaker-ui .help-window {
	width: 333px;
	background-color: #bfb8bf;
	position: fixed;
	z-index: 1;
	box-shadow: 
		0 -1px 0 0 #fff8ff,
		-1px 0 0 0 #fff8ff,
		-1px -1px 0 0 #fff8ff,
		0 0 0 1px #7f787f,
		0 -1px 0 1px #dfd8df,
		-1px 0 0 1px #dfd8df,
		-1px -1px 0 1px #dfd8df,
		0 0 0 2px #030303;
	top: 200px;
	left: calc((100% - 300px) / 2);
	padding: 40px 10px 10px 10px;
	pointer-events: auto;
}
#theme-tweaker-ui .help-window h1 {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/win95_help_icon.gif")) ?>');
}
#theme-tweaker-ui div.section#theme-tweak-section-clippy {
	padding: 23px 0 15px 10px;
}
#theme-tweaker-ui div.section#theme-tweak-section-clippy::before {
	z-index: 1;
}
#theme-tweaker-ui #theme-tweak-control-clippy + label::after {
	content: "";
	background-image: var(--basilisk); 
	background-size: 75px;
	background-repeat: no-repeat;
	background-position: center right;
	padding: 40px 85px 0 0;
	position: absolute;
	bottom: -2px;
	right: -86px;
	transform: scaleX(-1);
	display: inline-block;
}

#theme-tweaker-ui .clippy-close-button {
	position: absolute;
	top: 5px;
	right: 11px;
	width: 16px;
	height: 16px;
	border-radius: 8px;
	color: inherit;
	padding: 1px 0;
	box-shadow: 
		-1px -1px 0 #dfd8df inset,
		0 -1px 0 #7f787f,
		-1px 0 0 #7f787f,
		-1px -1px 0 #7f787f,
		0 0 0 1px #fff8ff,
		0 -1px 0 0 #fff8ff,
		-1px 0 0 0 #fff8ff,
		-1px -1px 0 0 #fff8ff,
		0 0 0 2px #7f787f,
		0 -2px 0 1px #dfd8df,
		-2px 0 0 1px #dfd8df,
		-1px -2px 0 1px #dfd8df,
		0 0 0 3px #030303;
}

/*========*/
/* MOBILE */
/*========*/

@media only screen and (max-width: 960px) {
	#theme-tweaker-ui {
		visibility: visible;
		z-index: 12000;
	}
	#theme-tweaker-ui::after {
		top: 10px;
		right: 8px;
	}
	#theme-tweaker-ui .main-theme-tweaker-window {
		max-width: unset;
		max-height: unset;
		left: 3px;
		top: 3px;
		width: calc(100% - 5px);
		height: calc(100% - 5px)
	}
	#theme-tweaker-ui .controls-container {
		max-height: calc(100% - 192px);
		padding: 0.75em;
	}
	#theme-tweaker-ui .current-theme {
		margin: 0.75em -8px 0 -8px;
		box-shadow: none;
		padding: 0 12px;
	}
	#theme-tweaker-ui .theme-selector {
		position: unset;
		margin: 0.5em 0 1em 0;
		white-space: nowrap;
	}
	#theme-tweaker-ui .clippy-container .clippy {
		transform: scale(0.375) translate(-215px, 215px);
	}

	#theme-tweaker-ui .controls-container {
		margin-right: -12px;
	}
	#theme-tweaker-ui .controls-container::-webkit-scrollbar {
		width: 32px;
	}

	@media only screen and (max-width: 369px) {
		#theme-tweaker-ui .theme-selector button {
			margin: 0.5em 0.25em 0.5em 0.5em;
		}
		#theme-tweaker-ui .reset-defaults-button {
			width: 9em;
		}
		#theme-tweaker-ui #theme-tweak-section-sample-text .sample-text-container {
			width: 12em;
		}
	}
	@media only screen and (max-width: 333px) {
		#theme-tweaker-ui .theme-selector button {
			margin: 0.5em 0.125em 0.5em 0.5em;
		}
	}
}