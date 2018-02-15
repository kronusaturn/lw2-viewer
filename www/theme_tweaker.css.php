/*****************/
/* THEME TWEAKER */
/*****************/

#theme-tweaker-toggle {
	position: fixed;
	top: 7px;
	left: calc((100% - 900px) / 2 - 75px);
}
#theme-tweaker-toggle button {
	font-family: Font Awesome;
	font-weight: 900;
	font-size: 1.25rem;
	color: #777;
	opacity: 0.4;
	z-index: 1;
}
#theme-tweaker-toggle button:hover {
	opacity: 1.0;
}

#theme-tweaker-ui {
	position: fixed;
	width: 100vw;
	height: 100vh;
	top: 0;
	left: 0;
	z-index: 1;
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
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("win95_close_widget.gif")) ?>');
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
#theme-tweaker-ui > div {
	position: fixed;
	z-index: 1;
	background-color: #bfb8bf;
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
	font-family: MS Sans Serif;
	font-size: 1rem;
	line-height: 1.15;
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
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("win95_themes_icon.gif")) ?>');
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
#theme-tweaker-ui .current-theme {
	margin: 1em -8px 1.25em -8px;
	padding: 0 12px 1em 12px;
	box-shadow: 
		0 1px 0 #7f787f,
		0 2px 0 #fff8ff;
}
#theme-tweaker-ui .current-theme span {
	text-shadow: 1px 0 0 #000;
	letter-spacing: 1px;
}

#theme-tweaker-ui div.section {
	border: 1px solid #7f787f;
	box-shadow:
		1px 1px 0 #fff8ff inset,
		0 1px 0 #fff8ff,
		1px 0 0 #fff8ff,
		1px 1px 0 #fff8ff;
	padding: 18px 10px 10px 12px;
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
#theme-tweaker-ui div.section#theme-tweak-section-invert {
	padding: 23px 10px 15px 14px;
}

#theme-tweaker-ui input[type='checkbox'] {
	display: none;
}
#theme-tweaker-ui input[type='checkbox'] + label {
	position: relative;
	padding: 0 0 0 1.5em;
	cursor: pointer;
	line-height: 1.2;
}
#theme-tweaker-ui input[type='checkbox'] + label::before {
	content: "";
	width: 1rem;
	height: 1rem;
	position: absolute;
	left: 0;
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
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("win95_checkmark.gif")) ?>');	
	background-size: 14px;
	background-repeat: no-repeat;
	background-position: center center;
}

#theme-tweaker-ui input[type='range'] {
	width: 100%;
	position: relative;
}
input[type=range] {
	-webkit-appearance: none;
	width: 100%;
	margin: 17px 0;
	padding: 0 3.15em 0 2em;
	background-color: transparent;
	border: none;
}
input[type=range]:focus {
	outline: none;
}
input[type=range]::-webkit-slider-runnable-track {
	width: 100%;
	height: 2px;
	cursor: pointer;
	box-shadow:
		-1px -1px 0 #dfd8df inset,
		1px 1px 0 #030303 inset,
		0 -1px 0 #7f787f,
		-1px 0 0 #7f787f,
		-1px -1px 0 #7f787f,
		0 0 0 1px #fff8ff;
	border-radius: 0px;
	border: none;
}
input[type=range]::-webkit-slider-thumb {
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
	cursor: pointer;
	-webkit-appearance: none;
	margin-top: -16px;
}
input[type=range]::-moz-range-track {
	width: 100%;
	height: 2px;
	cursor: pointer;
	box-shadow:
		-1px -1px 0 #dfd8df inset,
		1px 1px 0 #030303 inset,
		0 -1px 0 #7f787f,
		-1px 0 0 #7f787f,
		-1px -1px 0 #7f787f,
		0 0 0 1px #fff8ff;
	border-radius: 0px;
	border: none;
}
input[type=range]::-moz-range-thumb {
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
	cursor: pointer;
}
input[type=range]::-ms-track {
	width: 100%;
	height: 2px;
	cursor: pointer;
	border: none;
	color: transparent;
	box-shadow:
		-1px -1px 0 #dfd8df inset,
		1px 1px 0 #030303 inset,
		0 -1px 0 #7f787f,
		-1px 0 0 #7f787f,
		-1px -1px 0 #7f787f,
		0 0 0 1px #fff8ff;
}
input[type=range]::-ms-thumb {
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
	cursor: pointer;
}

#theme-tweaker-ui input[type='range']::before,
#theme-tweaker-ui input[type='range']::after {
	position: absolute;
	top: -0.35em;
	color: #666;
}
#theme-tweaker-ui #theme-tweak-control-saturate::before,
#theme-tweaker-ui #theme-tweak-control-brightness::before,
#theme-tweaker-ui #theme-tweak-control-contrast::before {
	content: "0%";
	left: 0;
}
#theme-tweaker-ui #theme-tweak-control-saturate::after,
#theme-tweaker-ui #theme-tweak-control-brightness::after,
#theme-tweaker-ui #theme-tweak-control-contrast::after {
	content: "300%";
	right: 0;
}
#theme-tweaker-ui #theme-tweak-control-hue-rotate::before {
	content: "0°";
	left: 0.6em;
}
#theme-tweaker-ui #theme-tweak-control-hue-rotate::after {
	content: "360°";
	right: 0.4em;
}
.theme-tweak-control-label {
	margin: 1.55em 3.15em 0 2em;
	padding: 0 2em 0 3.15em;
	text-align: center;
	position: relative;
	text-shadow: 1px 0 0 #000;
	letter-spacing: 1px;
}
#theme-tweak-label-saturate::before,
#theme-tweak-label-brightness::before,
#theme-tweak-label-contrast::before,
#theme-tweak-label-hue-rotate::before {
	content: "";
	display: block;
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
	top: -100%;
	left: calc(100% / 3 + 2px);
}
#theme-tweak-label-hue-rotate::before {
	left: 7px;
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
	padding: 7px 0 6px 0;
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
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("win95_minimize_widget.gif")) ?>');	
}
#theme-tweaker-ui .minimize-button.maximize {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("win95_maximize_widget.gif")) ?>');	
}

#theme-tweaker-ui .help-button {
	width: 18px;
	height: 16px;
	position: absolute;
	top: -1px;
	right: 55px;
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("win95_help_widget.gif")) ?>');	
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
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("win95_scrollbar_track.gif")) ?>');	
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
		opacity: 0.6;
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
}
#theme-tweaker-ui .clippy-container .hint img {
	vertical-align: text-bottom;
}
#theme-tweaker-ui .clippy-container .clippy {
	width: 200px;
	position: absolute;
	bottom: -150px;
	left: 0;
}
