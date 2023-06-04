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
.theme-tweaker-window {
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
	display: flex;
	flex-flow: column;
	align-items: stretch;
	justify-content: stretch;
	padding: 2px;
}

.theme-tweaker-window-title-bar {
	height: 25px;
	width: 100%;
	display: flex;
	background-color: #03037f;
}

.theme-tweaker-window-title {
	flex: 1 1 auto;
	color: #fff8ff;
	padding: 4px 6px 3px 30px;
	cursor: default;
	text-shadow: 1px 0 0 #fff;
	letter-spacing: 1px;
}
.theme-tweaker-window-title h1 {
	font-size: inherit;
	font-weight: normal;
	margin: 0;
}

.theme-tweaker-window-title-bar-buttons-container {
	display: flex;
	align-items: center;
	padding: 4px;
}

.theme-tweaker-window-title-bar-buttons-container button {
	width: 18px;
	height: 16px;
	background-size: 14px;
	background-repeat: no-repeat;
	background-position: center center;
	margin: 0 0 0 7px;
}

.theme-tweaker-window-title-bar-buttons-container .help-button {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/win95_help_widget.gif")) ?>');	
}

.theme-tweaker-window-title-bar-buttons-container .minimize-button.minimize {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/win95_minimize_widget.gif")) ?>');	
}
.theme-tweaker-window-title-bar-buttons-container .minimize-button.maximize {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/win95_maximize_widget.gif")) ?>');	
}

.theme-tweaker-window-title-bar-buttons-container .close-button {
	background-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/win95_close_widget.gif")) ?>');
	background-size: 100%;
	background-position: center bottom;
}

.theme-tweaker-window-content-view {
	padding: 12px;
	height: calc(100% - 25px);
	display: flex;
	flex-flow: column;
}

#theme-tweaker-ui .main-window {
	width: calc(900px - 40px);
	max-width: calc(100vw - 20px);
	max-height: calc(100vh - 160px);
	top: 80px;
	left: calc((100% - 900px) / 2 + 20px);
	height: 1200px;
}
#theme-tweaker-ui .theme-tweaker-window-title-bar {
	background-repeat: no-repeat;
	background-size: 20px;
	background-position: 4px 50%;
}
#theme-tweaker-ui .main-window .theme-tweaker-window-title-bar {
	background-image: url('data:image/gif;base64,R0lGODdhIAAgAOYAAAAAAAMDCwMD9AQDqwQDsgUDpAoDBAoDoqsDVKsDWq4DT7ADVrIDXf4DC/8D/QQEnAUE/qQET6QEUqQEW7AESvUEA/0EBAUFEPAFCPcF/wMImgMI/gMJBZsJWvIJBJCBmIuElH2Fj4WFlIiFfImFhoOGi4qGjI6GmpKGjoKHm5OHk4mIhoWKkoqKjIqKk4SLjIeLfY2LmX2NhYSOmoeOhoKQkQOkWKOkaqWlXaulXKSmTqSmUqqmYQqnYKmnVQWoXaWocZ+pbAiqVaOqVKSqTKmqVKyqWwOrVJ+rU6WrWrCuWcS+2Mi/wMy/082/zsrCzMrDxcLE1cXExLzFy77FxMTFzMzF1bfGwbzGvrfH18vIw7/J08HJu8PJxMPJzLTKyL3KzL7Kw73Lvs3Lyr3Mt8HM0LPRuf/2A/j38//3+fz+Cv3+9P7+BO//A/T/BPT/Cfb//vf/9///7P///wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAkAAHQALAAAAAAgACAAAAf/gHSCg4SFQ0NJOTqFjI2ORXNzaZEAjpaNRnBVbA4OV0oBl4w0KS4iLjI5c11unRlsOQYgLyUutS2VjSEHBQQDDz5zb1icGVo4AATKDwO/F441vgXNQ3BSPWFsUmxURgAFAHMBcwC5jC4azb2qP0c22WBqUkMAAfXl5oUmTV5cVGBG5hxpx0YMmypJUCCjUgWLlHyEWJiJFGnHHDZCtJ0Zo4NcmR0ArMypAnGQiCUU1yCZ84WNBQxb2HgB8IINFHpzooRqZGLMnDWRcFy0QJQbGzIH2/AwsOahoxdMJDAAOseiy6IQIESxidNpoxMdiIabg0STGi9sNkAQAOGJEWQ5/0sKYoGgQYUALSKp6sLGCZusbFbgXDNFLh0FRMVSBOBjh5EdRYrswBmJy05GExJboDknDjl7+Mql9HKZUIQGmy1U2AxnTuufFGN3/iC3roUAiQGwAEpVNtUdIMoVQiD2RQQLqMP1jk2VhxsAJkzkk6AawInjyV2klD1niJsSJrxgMbdALAgZcySg3sx9Dpo1RNjMMSCltbnTm0FESpBbhGw5QCShhjAAeEGJIJltlgACCUygAHHIjUVREWs8cRE5Y+VCgWqoeaDZegDMQAUVYYDhAxtZuHERAFSEUwl1FlwQWmi5GYBPDmyEcaEBLrhIR4IAuODCCCKw4AIICd5WwlEgAOQAg3xqiEbOBKgFUAIKjcBogQGEcADADfIVSImN5bzgCABkWlkIY89NcaApKpBgyS0tyMBIkzH4R44ofDJJBABR9NjnoAAEEaRhg94pXCAAOw==');
}
#theme-tweaker-ui .controls-container {
	overflow-y: scroll;
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
}

#theme-tweaker-ui .main-window .theme-select {
	display: flex;
	flex-flow: row wrap;
	align-items: center;
	margin: 0 -12px 1.25em -12px;
	padding: 3px 12px 13px 12px;
	box-shadow: 
		0 1px 0 #7f787f,
		0 2px 0 #fff8ff;
	cursor: default;
	user-select: none;
}
#theme-tweaker-ui .main-window .theme-select .current-theme {
	margin: 0;
	pointer-events: none;
}
#theme-tweaker-ui .main-window .theme-select .current-theme span {
	text-shadow: 1px 0 0 #000;
	letter-spacing: 1px;
}

#theme-tweaker-ui .main-window .theme-select .theme-selector {
	margin: 0 0 0 2em;
}
#theme-tweaker-ui .theme-selector button {
	width: 26px;
	height: 26px;
	display: inline-block;
	padding: 1px 0 0 1px;
	border: 4px solid #bfb8bf;
	margin: 0;
}
#theme-tweaker-ui .theme-selector button + button {
	margin: 0 0 0 14px;
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
	margin: 18px 0 0 0;
}
#theme-tweaker-ui button {
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
}
#theme-tweaker-ui .main-theme-tweaker-window > button {
	margin: 0.5em 0 0 0;
}
#theme-tweaker-ui .controls-container button,
#theme-tweaker-ui .buttons-container button,
#theme-tweaker-ui .help-window button {
	width: 7em;
	padding: 7px 0 6px 0;
}
#theme-tweaker-ui .buttons-container button {
	color: inherit;
}
#theme-tweaker-ui .buttons-container button + button {
	margin-left: 1em;
}
#theme-tweaker-ui button.default-button {
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

#theme-tweaker-ui .buttons-container .reset-defaults-button {
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
#theme-tweaker-ui .clippy-container .hint img {
	vertical-align: bottom;
	position: relative;
	bottom: 2px;
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
	pointer-events: auto;
}
#theme-tweaker-ui .help-window .theme-tweaker-window-title-bar {
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
	top: 8px;
	right: 8px;
	width: 16px;
	height: 16px;
	border-radius: 8px;
	color: inherit;
	padding: 1px 0;
	background-color: transparent;
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

#theme-tweaker-ui .main-window.minimized {
	width: 320px;
	height: 29px;
	overflow: hidden;
	top: 20px;
	right: 20px;
	left: auto;
}
#theme-tweaker-ui.main-window-minimized::after {
	top: 27px;
	right: 27px;
}
#theme-tweaker-ui.main-window-minimized::before {
	opacity: 0.0;
	height: 0;
}
#theme-tweaker-ui.main-window-minimized .clippy-container {
	opacity: 1.0;
}
#theme-tweaker-ui.main-window-minimized .clippy-container .hint span {
	color: #c00;
}
#theme-tweaker-ui.main-window-minimized {
	height: 0;
}

/*========*/
/* MOBILE */
/*========*/

@media only screen and (max-width: 1160px) {
	#theme-tweaker-ui {
		z-index: 12000;
	}
	#theme-tweaker-ui::after {
		top: 10px;
		right: 8px;
	}

	#theme-tweaker-ui .main-window {
		max-width: unset;
		max-height: unset;
		left: 3px;
		top: 3px;
		width: calc(100% - 5px);
		height: calc(100% - 5px)
	}

	#theme-tweaker-ui .main-window .theme-select {
		box-shadow: none;
		margin-bottom: 0;
	}
	#theme-tweaker-ui .main-window .theme-select .theme-selector {
		margin: 1em 0 0.625em 0;
		white-space: nowrap;
	}

	#theme-tweaker-ui .clippy-container .clippy {
		transform: scale(0.375) translate(-215px, 215px);
	}

	#theme-tweaker-ui .controls-container {
		padding: 0.75em;
		margin-right: -16px;
	}
	#theme-tweaker-ui .controls-container::-webkit-scrollbar {
		width: 32px;
	}
	/*	Compensating for Firefox mobile scroll bar nonsense.
	 */
	@-moz-document url-prefix() {
		#theme-tweaker-ui .controls-container {
			padding-right: 1.25em;
		}
		#theme-tweaker-ui input[type='range'] {
			padding: 0 0.5em 0 0.3em;
			margin: 0 2.5em 0 1.5em;
			width: calc(100% - 4em);
		}
		.theme-tweak-control-label {
			margin-right: 4.45em;
		}
		.theme-tweak-control-label + .notch {
			left: calc((100% - 4.8em) / 3 + 1.8em + 7px);
		}
		#theme-tweak-label-hue-rotate + .notch {
			left: calc(3em);
		}
	}

	#theme-tweaker-ui .main-window .buttons-container button:last-child {
		margin-top: 1em;
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