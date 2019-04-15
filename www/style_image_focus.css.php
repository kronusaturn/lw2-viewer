/*=--------------=*/
/*= Hover styles =*/ 
/*=--------------=*/

#content img:hover,
#images-overlay img:hover {
	filter: drop-shadow(0 0 3px #777);
	cursor: zoom-in;
}
#content img:active,
#images-overlay img:active {
	transform: scale(0.975);
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
	background-color: rgba(0,0,0,0.75);
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

/*=---------=*/
/*= Overlay =*/
/*=---------=*/

#image-focus-overlay {
	position: fixed;
	top: 0;
	right: 0;
	bottom: 0;
	left: 0;
	z-index: 2;
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

/*=-------------------=*/
/*= Single-image mode =*/
/*=-------------------=*/

#image-focus-overlay:not(.slideshow) .image-number,
#image-focus-overlay:not(.slideshow) .slideshow-buttons {
	visibility: hidden;
}

/*=---------=*/
/*= Caption =*/
/*=---------=*/

#image-focus-overlay .caption {
	--GW-image-focus-caption-side-spacing: 4.5em;

	position: absolute;
	bottom: 0.75em;
	background-color: rgba(0,0,0,0.7);
	left: var(--GW-image-focus-caption-side-spacing);
	right: var(--GW-image-focus-caption-side-spacing);
	margin: auto;
	max-width: calc(100% - 2 * var(--GW-image-focus-caption-side-spacing));
	text-align: center;
	font-size: 1.375em;
	border-radius: 8px;
	z-index: 1;
	transition: 
		bottom 0.15s ease;
}
<?php fit_content("#image-focus-overlay .caption"); ?>
#image-focus-overlay .caption.hidden {
	bottom: -50%;
	transition: 
		bottom 0.5s ease;
}

#image-focus-overlay .caption p {
	margin: 1em 1.25em;
	color: #fff;
}

#image-focus-overlay .caption:not(:empty)::before {
	content: "";
	display: block;
	position: absolute;
	width: 100vw;
	height: calc(100% + 1.5em);
	z-index: -1;
	top: -0.75em;
	left: calc(-50vw + 50%);

}

/*=--------------=*/
/*= Help overlay =*/
/*=--------------=*/

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
	cursor: default;
	visibility: hidden;
	transition: 
		visibility 1s ease,
		color 1s ease,
		background-color 1s ease,
		bottom 0.3s ease;
}
#image-focus-overlay .help-overlay:hover {
	max-width: 24em;
	max-height: 14em;
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
	font-family: var(--GW-Font-Awesome);
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

/*=--------------=*/
/*= Slide number =*/
/*=--------------=*/

#image-focus-overlay .image-number {
	position: absolute;
	z-index: 2;
	font-size: 1.75rem;
	left: 1em;
	top: 1em;
	font-weight: var(--GW-UI-font-weight-heavy);
	text-shadow:
		0 0 3px #fff,
		0 0 5px #fff,
		0 0 8px #fff,
		0 0 13px #fff;
	width: 1.5em;
	text-align: right;
	white-space: nowrap;
	transition: top 0.2s ease;
}
#image-focus-overlay .image-number::before {
	content: "#";
}
#image-focus-overlay .image-number::after {
	content: " of " attr(data-number-of-images);
}
#image-focus-overlay .image-number.hidden {
	top: -1.25em;
}
@media only screen and (hover: hover) and (pointer: fine) {
	#image-focus-overlay .image-number::before,
	#image-focus-overlay .image-number::after {
		opacity: 0.3;
	}
	#image-focus-overlay .image-number:hover::before,
	#image-focus-overlay .image-number:hover::after {
		opacity: 1.0;
	}
}

/*=-------------------=*/
/*= Slideshow buttons =*/
/*=-------------------=*/

#image-focus-overlay .slideshow-buttons {
	position: absolute;
	top: 0;
	left: 0;
	width: 100%;
	height: 100%;
	z-index: 2;
	display: flex;
	justify-content: space-between;
	pointer-events: none;
}
#image-focus-overlay .slideshow-buttons button {
	font-family: var(--GW-Font-Awesome);
	font-weight: 900;
	font-size: 3rem;
	padding: 0.5em;
	color: #ddd;
	position: relative;
	left: 0;
	transition:
		left 0.3s ease;
	pointer-events: auto;
	user-select: none;
}
@media only screen and (hover: hover) and (pointer: fine) {
	#image-focus-overlay .slideshow-buttons button:hover {
		background-color: rgba(0,0,0,0.1);
		color: #777;
	}
}
@media not screen and (hover: hover) and (pointer: fine) {
	#image-focus-overlay .slideshow-buttons button {
		background-color: rgba(0,0,0,0.1);
		color: #fff;
		text-shadow: 
			0 0 1px #777;
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

/*=-----------------=*/
/*= Background blur =*/
/*=-----------------=*/

.blurred {
	filter: blur(3px);
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 960px) {
	#image-focus-overlay.engaged {
		visibility: visible;
	}
}
@media only screen and (max-width: 900px) {
	#image-focus-overlay .caption p {
		margin: 0.75em 1em;
	}
}
@media only screen and (max-width: 720px) {
	#image-focus-overlay .caption {
		font-size: 1.25em;
	}
}
@media only screen and (max-width: 640px) {
	#image-focus-overlay .caption {
		font-size: 1.125em;
		--GW-image-focus-caption-side-spacing: 5em;
	}
}

/*=--------------------------=*/
/*= Touch screen adjustments =*/
/*=--------------------------=*/

@media not screen and (hover: hover) and (pointer: fine) {
	#image-focus-overlay .caption {
		--GW-image-focus-caption-side-spacing: 0.75em;
	}
	#image-focus-overlay .help-overlay {
		display: none;
	}
}
