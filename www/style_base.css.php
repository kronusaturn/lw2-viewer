/***************/
/* BASE LAYOUT */
/***************/

html {
	box-sizing: border-box;
	font-size: 16px;
}
*, *::before, *::after {
	box-sizing: inherit;
}

/*=------=*/
/*= Body =*/
/*=------=*/

body {
	padding: 0;
	margin: 0;

	font-family: var(--GW-UI-font);
	background-color: var(--GW-body-background-color);
}
body::before {
	background-color: inherit;
	position: fixed;
	width: 100%;
	height: 100%;
}

body.no-scroll {
	overflow-y: scroll;
	position: fixed;
	width: 100%;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 960px) and (min-width: 901px) {
	body {
		overflow-x: hidden;
	}
}

/*=----------------------------=*/
/*= Immediate children of body =*/
/*=----------------------------=*/

body > * {
	width: calc(100% - 300px);
	min-width: 900px;
}
#content {
	margin: 0 auto;
	padding: 0;
	position: relative;
	overflow: visible;
	display: grid;
	grid-template-columns: 100%;
}
#content::before {
	content: "";
	position: absolute;
	top: 0;
	left: 0;
	width: 100%;
	height: 100%;
	z-index: -1;
	pointer-events: none;
	background-color: var(--GW-content-background-color);
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	#content,
	#images-overlay,
	#ui-elements-container {
		min-width: unset;
		width: unset;
	}
}

/*=---------=*/
/*= Content =*/
/*=---------=*/

#content > * {
	grid-column: 1;
}

/******/
/* UI */
/******/
/*	Removing browser default styling of various elements.
	*/

button,
input[type='submit'],
.button {
	font-family: inherit;
	font-size: inherit;
	cursor: pointer;
	border: none;
	border-radius: 0;
}
button,
input[type='submit'],
.button,
.button:visited {
	color: var(--GW-button-color);
	background-color: var(--GW-button-background-color);
}
button:hover,
input[type='submit']:hover,
.button:hover,
button:focus,
input[type='submit']:focus,
.button:focus {
	color: var(--GW-button-hover-color);
	background-color: var(--GW-button-hover-background-color);
}
button:active,
input[type='submit']:active,
.button:active {
	color: var(--GW-button-active-color);
	background-color: var(--GW-button-active-background-color);
}

input,
button,
textarea {
	-webkit-appearance: none;
	-moz-appearance: none;
	appearance: none;
}

input {
	font-family: inherit;
	font-size: inherit;
	font-weight: inherit;
}

a {
	text-decoration: none;
}

/*	On various input elements such as text fields and buttons, remove "blue glow" focus outlines on Macs, dotted black outlines in Firefox, etc.
	*/
:focus {
	outline: none;
}

/**************/
/* PRINT VIEW */
/**************/

@media only print {
	.nav-bar {
		visibility: hidden;
		max-height: 0;
		overflow: hidden;
	}
	#ui-elements-container {
		display: none;
	}
	#images-overlay {
		display: none;
	}
	#images-overlay + #content .post-body img {
		visibility: visible;
	}
	.comment-controls {
		display: none;
	}
	#comments-sort-mode-selector {
		display: none;
	}
	.post-meta .qualified-linking,
	.post-meta .lw2-link {
		display: none;
	}
	.comment-meta .permalink,
	.comment-meta .lw2-link,
	.comment-meta .comment-parent-link,
	.comment-meta .comment-minimize-button {
		display: none;
	}
	.vote {
		display: none;
	}
	.new-comment::before {
		display: none;
	}
	body #content::before {
		box-shadow: none;
	}
}
