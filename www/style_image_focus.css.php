#content img:hover,
#images-overlay img:hover {
	filter: drop-shadow(0 0 3px #777);
	cursor: zoom-in;
}
#content img:active,
#images-overlay img:active {
	transform: scale(0.975);
}

#image-focus-overlay {
	position: fixed;
	top: 0;
	right: 0;
	bottom: 0;
	left: 0;
	display: none;
	cursor: zoom-out;
}
#image-focus-overlay::before {
	content: "";
	display: block;
	position: absolute;
	top: 0;
	right: 0;
	bottom: 0;
	left: 0;
	background-color: #000;
	opacity: 0.5;
	z-index: -1;
}
#image-focus-overlay.engaged {
	display: initial;
}

#image-focus-overlay img {
	margin: auto;
	position: absolute;
	left: 50%;
	top: 50%;
	transform: translateX(-50%) translateY(-50%);
}

#image-focus-overlay .help-overlay {
	position: absolute;
	display: flex;
	flex-flow: column;
	z-index: 2;
	font-size: 1.5rem;
	padding: 1em;
	border-radius: 10px;
	bottom: 1em;
	right: 1em;
	overflow: hidden;
	white-space: nowrap;
	color: transparent;
	visibility: hidden;
	transition: 
		visibility 1s ease,
		color 1s ease,
		background-color 1s ease,
		bottom 0.3s ease;
}
#image-focus-overlay .help-overlay:hover {
	max-width: 420px;
	max-height: 300px;
	background-color: rgba(0,0,0,0.85);
	color: #fff;
	visibility: visible;
	transition: 
		visibility 0.2s ease 0.3s,
		color 0.2s ease 0.3s,
		background-color 0.2s ease 0.3s;
}

#image-focus-overlay .help-overlay::after {
	content: "\F128";
	font-family: Font Awesome;
	font-weight: 900;
	font-size: 2rem;
	position: absolute;
	right: 0;
	bottom: 0;
	padding: 10px;
	color: #000;
	filter: drop-shadow(0 0 6px #fff);
	visibility: visible;
	opacity: 0.85;
	transition: 
		visibility 1s ease;
}
#image-focus-overlay .help-overlay:hover::after {
	visibility: hidden;
	transition: 
		visibility 0.2s ease 0.3s;
}

#image-focus-overlay .help-overlay p {
	margin: 0;
	text-indent: -2em;
	padding-left: 2em;
	max-width: 100%;
	overflow: hidden;
}
#image-focus-overlay .help-overlay p + p {
	margin: 0.75em 0 0 0;
}
#image-focus-overlay .help-overlay.hidden {
	bottom: -2em;
}

#image-focus-overlay .image-number {
	position: absolute;
	z-index: 2;
	font-size: 1.75rem;
	left: 1em;
	bottom: 1em;
	font-weight: 600;
	text-shadow:
		0 0 3px #fff,
		0 0 5px #fff,
		0 0 8px #fff,
		0 0 13px #fff;
	width: 1.5em;
	text-align: right;
	white-space: nowrap;
	transition: bottom 0.3s ease;
}
#image-focus-overlay .image-number::before {
	content: "#";
	opacity: 0.3;
}
#image-focus-overlay .image-number::after {
	content: " of " attr(data-number-of-images);
	opacity: 0.3;
}
#image-focus-overlay .image-number:hover::before,
#image-focus-overlay .image-number:hover::after {
	opacity: 1.0;
}
#image-focus-overlay .image-number.hidden {
	bottom: -1.25em;
}

#image-focus-overlay .slideshow-buttons {
	position: absolute;
	top: 0;
	left: 0;
	width: 100%;
	height: 100%;
	z-index: 1;
	display: flex;
	justify-content: space-between;
	pointer-events: none;
}
#image-focus-overlay .slideshow-buttons button {
	font-family: Font Awesome;
	font-weight: 900;
	font-size: 3rem;
	padding: 0.5em;
	color: #ddd;
	position: relative;
	left: 0;
	transition:
		left 0.3s ease;
	pointer-events: auto;
}
#image-focus-overlay .slideshow-buttons button::selection {
	background-color: transparent;
}
@media only screen and (hover: hover) {
	#image-focus-overlay .slideshow-buttons button:hover {
		background-color: rgba(0,0,0,0.1);
		color: #777;
	}
}
#image-focus-overlay .slideshow-buttons button:active {
	transform: none;
	color: #888;
}
#image-focus-overlay .slideshow-buttons button:disabled {
	text-shadow: none;
	background-color: transparent;
	color: #ddd;
	cursor: default;
	opacity: 0.4;
}
#image-focus-overlay .slideshow-button.previous.hidden {
	left: -1.75em;
}
#image-focus-overlay .slideshow-button.next.hidden {
	left: 1.75em;
}

.blurred {
	filter: blur(3px);
}
