/***************/
/* IMAGE FOCUS */
/***************/

function imageFocusSetup(imagesOverlayOnly = false) {
	GWLog("imageFocusSetup");

	if (typeof GW.imageFocus == "undefined")
		GW.imageFocus = {
			contentImagesSelector:	"#content img",
			overlayImagesSelector:	"#images-overlay img",
			focusedImageSelector:	"#content img.focused, #images-overlay img.focused",
			pageContentSelector:	"#content, #ui-elements-container > *:not(#image-focus-overlay), #images-overlay",
			shrinkRatio:			0.975,
			hideUITimerDuration:	1500,
			hideUITimerExpired:		() => {
				GWLog("GW.imageFocus.hideUITimerExpired");
				let currentTime = new Date();
				let timeSinceLastMouseMove = (new Date()) - GW.imageFocus.mouseLastMovedAt;
				if (timeSinceLastMouseMove < GW.imageFocus.hideUITimerDuration) {
					GW.imageFocus.hideUITimer = setTimeout(GW.imageFocus.hideUITimerExpired, (GW.imageFocus.hideUITimerDuration - timeSinceLastMouseMove));
				} else {
					hideImageFocusUI();
					cancelImageFocusHideUITimer();
				}
			}
		};

	// Create event listener for clicking on images to focus them.
	GW.imageClickedToFocus = (event) => {
		GWLog("GW.imageClickedToFocus");

		focusImage(event.target);

		unhideImageFocusUI();
		if (GW.mediaQueries.hover.matches) {
			// Set timer to hide the image focus UI.
			GW.imageFocus.hideUITimer = setTimeout(GW.imageFocus.hideUITimerExpired, GW.imageFocus.hideUITimerDuration);
		}
	};
	// Add the listener to each image in the overlay (i.e., those in the post).
	queryAll(GW.imageFocus.overlayImagesSelector).forEach(image => {
		image.addActivateEvent(GW.imageClickedToFocus);
	});
	// Accesskey-L starts the slideshow.
	(query(GW.imageFocus.overlayImagesSelector)||{}).accessKey = 'l';
	// Count how many images there are in the post, and set the “… of X” label to that.
	((query("#image-focus-overlay .image-number")||{}).dataset||{}).numberOfImages = queryAll(GW.imageFocus.overlayImagesSelector).length;
	if (imagesOverlayOnly) return;
	// Add the listener to all other content images (including those in comments).
	queryAll(GW.imageFocus.contentImagesSelector).forEach(image => {
		image.addActivateEvent(GW.imageClickedToFocus);
	});

	// Create the image focus overlay.
	let imageFocusOverlay = addUIElement(`<div id='image-focus-overlay'>
	<div class='help-overlay'>
		 <p><strong>Arrow keys:</strong> Next/previous image</p>
		 <p><strong>Escape</strong> or <strong>click</strong>: Hide zoomed image</p>
		 <p><strong>Space bar:</strong> Reset image size & position</p>
		 <p><strong>Scroll</strong> to zoom in/out</p>
		 <p>(When zoomed in, <strong>drag</strong> to pan; <br/><strong>double-click</strong> to close)</p>
	</div>
	<div class='image-number'></div>
	<div class='slideshow-buttons'>
		 <button type='button' class='slideshow-button previous' tabindex='-1' title='Previous image'>&#xf053;</button>
		 <button type='button' class='slideshow-button next' tabindex='-1' title='Next image'>&#xf054;</button>
	</div>
	<div class='caption'></div>
</div>`);
	imageFocusOverlay.dropShadowFilterForImages = " drop-shadow(10px 10px 10px #000) drop-shadow(0 0 10px #444)";

	// Activate the buttons.
	imageFocusOverlay.queryAll(".slideshow-button").forEach(button => {
		button.addActivateEvent(GW.imageFocus.slideshowButtonClicked = (event) => {
			GWLog("GW.imageFocus.slideshowButtonClicked");
			focusNextImage(event.target.hasClass("next"));
			event.target.blur();
		});
	});

	// UI starts out hidden.
	if (GW.mediaQueries.hover.matches) {
		hideImageFocusUI();
	}
}

function focusImage(imageToFocus) {
	GWLog("focusImage");

	// Clear “last-focused” class of last focused image.
	let lastFocusedImage = query("img.last-focused");
	if (lastFocusedImage) {
		lastFocusedImage.removeClass("last-focused");
		lastFocusedImage.removeAttribute("accesskey");
	}

	// Create the focused version of the image.
	imageToFocus.addClass("focused");
	let imageFocusOverlay = query("#image-focus-overlay");
	let clonedImage = imageToFocus.cloneNode(true);
	clonedImage.style = "";
	clonedImage.removeAttribute("width");
	clonedImage.removeAttribute("height");
	clonedImage.style.filter = imageToFocus.style.filter + imageFocusOverlay.dropShadowFilterForImages;

	// Add the image to the overlay.
	imageFocusOverlay.appendChild(clonedImage);
	imageFocusOverlay.addClass("engaged");

	// Set image to default size and position.
	resetFocusedImagePosition();

	// Blur everything else.
	queryAll(GW.imageFocus.pageContentSelector).forEach(element => {
		element.addClass("blurred");
	});

	// Add listener to zoom image with scroll wheel.
	window.addEventListener("wheel", GW.imageFocus.scrollEvent = (event) => {
		GWLog("GW.imageFocus.scrollEvent");

		event.preventDefault();

		let image = query("#image-focus-overlay img");

		// Remove the filter.
		image.savedFilter = image.style.filter;
		image.style.filter = "none";

		// Locate point under cursor.
		let imageBoundingBox = image.getBoundingClientRect();

		// Calculate resize factor.
		var factor = (image.height > 10 && image.width > 10) || event.deltaY < 0 ?
						1 + Math.sqrt(Math.abs(event.deltaY))/100.0 :
						1;

		// Resize.
		image.style.width = (event.deltaY < 0 ?
							(image.clientWidth * factor) :
							(image.clientWidth / factor))
							+ "px";
		image.style.height = "";

		// Designate zoom origin.
		var zoomOrigin;
		// Zoom from cursor if we’re zoomed in to where image exceeds screen,
		// AND the cursor is over the image.
		let imageSizeExceedsWindowBounds = (image.getBoundingClientRect().width > window.innerWidth || image.getBoundingClientRect().height > window.innerHeight);
		let zoomingFromCursor = imageSizeExceedsWindowBounds &&
								(imageBoundingBox.left <= event.clientX &&
								 event.clientX <= imageBoundingBox.right && 
								 imageBoundingBox.top <= event.clientY &&
								 event.clientY <= imageBoundingBox.bottom);
		// Otherwise, if we’re zooming OUT, zoom from window center; if we’re 
		// zooming IN, zoom from image center.
		let zoomingFromWindowCenter = event.deltaY > 0;
		if (zoomingFromCursor)
			zoomOrigin = { x: event.clientX, 
						   y: event.clientY };
		else if (zoomingFromWindowCenter)
			zoomOrigin = { x: window.innerWidth / 2, 
						   y: window.innerHeight / 2 };
		else
			zoomOrigin = { x: imageBoundingBox.x + imageBoundingBox.width / 2, 
						   y: imageBoundingBox.y + imageBoundingBox.height / 2 };

		// Calculate offset from zoom origin.
		let offsetOfImageFromZoomOrigin = {
			x: imageBoundingBox.x - zoomOrigin.x,
			y: imageBoundingBox.y - zoomOrigin.y
		}
		// Calculate delta from centered zoom.
		let deltaFromCenteredZoom = {
			x: image.getBoundingClientRect().x - (zoomOrigin.x + (event.deltaY < 0 ? offsetOfImageFromZoomOrigin.x * factor : offsetOfImageFromZoomOrigin.x / factor)),
			y: image.getBoundingClientRect().y - (zoomOrigin.y + (event.deltaY < 0 ? offsetOfImageFromZoomOrigin.y * factor : offsetOfImageFromZoomOrigin.y / factor))
		}
		// Adjust image position appropriately.
		image.style.left = parseInt(getComputedStyle(image).left) - deltaFromCenteredZoom.x + "px";
		image.style.top = parseInt(getComputedStyle(image).top) - deltaFromCenteredZoom.y + "px";
		// Gradually re-center image, if it’s smaller than the window.
		if (!imageSizeExceedsWindowBounds) {
			let imageCenter = { x: image.getBoundingClientRect().x + image.getBoundingClientRect().width / 2, 
								y: image.getBoundingClientRect().y + image.getBoundingClientRect().height / 2 }
			let windowCenter = { x: window.innerWidth / 2,
								 y: window.innerHeight / 2 }
			let imageOffsetFromCenter = { x: windowCenter.x - imageCenter.x,
										  y: windowCenter.y - imageCenter.y }
			// Divide the offset by 10 because we’re nudging the image toward
			// center, not jumping it there.
			image.style.left = Math.abs(imageOffsetFromCenter.x) < 10 ? 
							   windowCenter.x : 
							   parseInt(getComputedStyle(image).left) + imageOffsetFromCenter.x / 10 + "px";
			image.style.top = Math.abs(imageOffsetFromCenter.y) < 10 ? 
							  windowCenter.y : 
							  parseInt(getComputedStyle(image).top) + imageOffsetFromCenter.y / 10 + "px";
		}

		// Put the filter back.
		image.style.filter = image.savedFilter;

		// Set the cursor appropriately.
		setFocusedImageCursor();
	}, { passive: false });
	window.addEventListener("MozMousePixelScroll", GW.imageFocus.oldFirefoxCompatibilityScrollEvent = (event) => {
		GWLog("GW.imageFocus.oldFirefoxCompatibilityScrollEvent");

		event.preventDefault();
	});

	// If image is bigger than viewport, it’s draggable. Otherwise, click unfocuses.
	window.addEventListener("mouseup", GW.imageFocus.mouseUp = (event) => {
		GWLog("GW.imageFocus.mouseUp");

		window.onmousemove = '';

		// We only want to do anything on left-clicks.
		if (event.button != 0) return;

		// Don’t unfocus if click was on a slideshow next/prev button!
		if (event.target.hasClass("slideshow-button")) return;

		// We also don’t want to do anything if clicked on the help overlay.
		if (event.target.classList.contains("help-overlay") ||
			event.target.closest(".help-overlay"))
			return;

		let focusedImage = query("#image-focus-overlay img");
		if ((event.target == focusedImage || event.target.tagName == "HTML") && 
			(focusedImage.height >= window.innerHeight || focusedImage.width >= window.innerWidth)) {
			// If the mouseup event was the end of a pan of an overside image,
			// put the filter back; do not unfocus.
			focusedImage.style.filter = focusedImage.savedFilter;
		} else {
			unfocusImageOverlay();
			return;
		}
	});
	window.addEventListener("mousedown", GW.imageFocus.mouseDown = (event) => {
		GWLog("GW.imageFocus.mouseDown");

		event.preventDefault();

		let focusedImage = query("#image-focus-overlay img");
		if (focusedImage.height >= window.innerHeight || focusedImage.width >= window.innerWidth) {
			let mouseCoordX = event.clientX;
			let mouseCoordY = event.clientY;

			let imageCoordX = parseInt(getComputedStyle(focusedImage).left);
			let imageCoordY = parseInt(getComputedStyle(focusedImage).top);

			// Save the filter.
			focusedImage.savedFilter = focusedImage.style.filter;

			window.onmousemove = (event) => {
				// Remove the filter.
				focusedImage.style.filter = "none";
				focusedImage.style.left = imageCoordX + event.clientX - mouseCoordX + 'px';
				focusedImage.style.top = imageCoordY + event.clientY - mouseCoordY + 'px';
			};
			return false;
		}
	});

	// Double-click on the image unfocuses.
	clonedImage.addEventListener('dblclick', GW.imageFocus.doubleClick = (event) => {
		GWLog("GW.imageFocus.doubleClick");
		if (event.target.hasClass("slideshow-button")) return;

		unfocusImageOverlay();
	});

	// Escape key unfocuses, spacebar resets.
	document.addEventListener("keyup", GW.imageFocus.keyUp = (event) => {
		GWLog("GW.imageFocus.keyUp");

		let allowedKeys = [ " ", "Spacebar", "Escape", "Esc", "ArrowUp", "ArrowDown", "ArrowLeft", "ArrowRight", "Up", "Down", "Left", "Right" ];
		if (!allowedKeys.contains(event.key) || 
			getComputedStyle(query("#image-focus-overlay")).display == "none") return;

		event.preventDefault();

		switch (event.key) {
		case "Escape": 
		case "Esc":
			unfocusImageOverlay();
			break;
		case " ":
		case "Spacebar":
			resetFocusedImagePosition();
			break;
		case "ArrowDown":
		case "Down":
		case "ArrowRight":
		case "Right":
			if (query("#images-overlay img.focused")) focusNextImage(true);
			break;
		case "ArrowUp":
		case "Up":
		case "ArrowLeft":
		case "Left":
			if (query("#images-overlay img.focused")) focusNextImage(false);
			break;
		}
	});

	setTimeout(() => {
		// Prevent spacebar or arrow keys from scrolling page when image focused.
		togglePageScrolling(false);
	});

	// If the image comes from the images overlay, for the main post...
	if (imageToFocus.closest("#images-overlay")) {
		// Mark the overlay as being in slide show mode (to show buttons/count).
		imageFocusOverlay.addClass("slideshow");

		// Set state of next/previous buttons.
		let images = queryAll(GW.imageFocus.overlayImagesSelector);
		var indexOfFocusedImage = getIndexOfFocusedImage();
		imageFocusOverlay.query(".slideshow-button.previous").disabled = (indexOfFocusedImage == 0);
		imageFocusOverlay.query(".slideshow-button.next").disabled = (indexOfFocusedImage == images.length - 1);

		// Set the image number.
		query("#image-focus-overlay .image-number").textContent = (indexOfFocusedImage + 1);

		// Replace the hash.
		history.replaceState(null, null, "#if_slide_" + (indexOfFocusedImage + 1));
	} else {
		imageFocusOverlay.removeClass("slideshow");
	}

	// Set the caption.
	setImageFocusCaption();

	// Moving mouse unhides image focus UI.
	window.addEventListener("mousemove", GW.imageFocus.mouseMoved = (event) => {
		GWLog("GW.imageFocus.mouseMoved");

		let currentDateTime = new Date();
		if (!(event.target.tagName == "IMG" || event.target.id == "image-focus-overlay")) {
			cancelImageFocusHideUITimer();
		} else {
			if (!GW.imageFocus.hideUITimer) {
				unhideImageFocusUI();
				GW.imageFocus.hideUITimer = setTimeout(GW.imageFocus.hideUITimerExpired, GW.imageFocus.hideUITimerDuration);
			}
			GW.imageFocus.mouseLastMovedAt = currentDateTime;
		}
	});
}

function resetFocusedImagePosition() {
	GWLog("resetFocusedImagePosition");

	let focusedImage = query("#image-focus-overlay img");
	if (!focusedImage) return;

	let sourceImage = query(GW.imageFocus.focusedImageSelector);

	// Make sure that initially, the image fits into the viewport.
	let constrainedWidth = Math.min(sourceImage.naturalWidth, window.innerWidth * GW.imageFocus.shrinkRatio);
	let widthShrinkRatio = constrainedWidth / sourceImage.naturalWidth;
	var constrainedHeight = Math.min(sourceImage.naturalHeight, window.innerHeight * GW.imageFocus.shrinkRatio);
	let heightShrinkRatio = constrainedHeight / sourceImage.naturalHeight;
	let shrinkRatio = Math.min(widthShrinkRatio, heightShrinkRatio);
	focusedImage.style.width = (sourceImage.naturalWidth * shrinkRatio) + "px";
	focusedImage.style.height = (sourceImage.naturalHeight * shrinkRatio) + "px";

	// Remove modifications to position.
	focusedImage.style.left = "";
	focusedImage.style.top = "";

	// Set the cursor appropriately.
	setFocusedImageCursor();
}
function setFocusedImageCursor() {
	let focusedImage = query("#image-focus-overlay img");
	if (!focusedImage) return;
	focusedImage.style.cursor = (focusedImage.height >= window.innerHeight || focusedImage.width >= window.innerWidth) ? 
						 		'move' : '';
}

function unfocusImageOverlay() {
	GWLog("unfocusImageOverlay");

	// Remove event listeners.
	window.removeEventListener("wheel", GW.imageFocus.scrollEvent);
	window.removeEventListener("MozMousePixelScroll", GW.imageFocus.oldFirefoxCompatibilityScrollEvent);
	// NOTE: The double-click listener does not need to be removed manually,
	// because the focused (cloned) image will be removed anyway.
	document.removeEventListener("keyup", GW.imageFocus.keyUp);
	window.removeEventListener("mousemove", GW.imageFocus.mouseMoved);
	window.removeEventListener("mousedown", GW.imageFocus.mouseDown);
	window.removeEventListener("mouseup", GW.imageFocus.mouseUp);

	// Set accesskey of currently focused image (if it’s in the images overlay).
	let currentlyFocusedImage = query("#images-overlay img.focused");
	if (currentlyFocusedImage) {
		currentlyFocusedImage.addClass("last-focused");
		currentlyFocusedImage.accessKey = 'l';
	}

	// Remove focused image and hide overlay.
	let imageFocusOverlay = query("#image-focus-overlay");
	imageFocusOverlay.removeClass("engaged");
	removeElement(imageFocusOverlay.query("img"));

	// Un-blur content/etc.
	queryAll(GW.imageFocus.pageContentSelector).forEach(element => {
		element.removeClass("blurred");
	});

	// Unset “focused” class of focused image.
	query(GW.imageFocus.focusedImageSelector).removeClass("focused");

	setTimeout(() => {
		// Re-enable page scrolling.
		togglePageScrolling(true);
	});

	// Reset the hash, if needed.
	if (location.hash.hasPrefix("#if_slide_"))
		history.replaceState(null, null, "#");
}

function getIndexOfFocusedImage() {
	let images = queryAll(GW.imageFocus.overlayImagesSelector);
	var indexOfFocusedImage = -1;
	for (i = 0; i < images.length; i++) {
		if (images[i].hasClass("focused")) {
			indexOfFocusedImage = i;
			break;
		}
	}
	return indexOfFocusedImage;
}

function focusNextImage(next = true) {
	GWLog("focusNextImage");

	let images = queryAll(GW.imageFocus.overlayImagesSelector);
	var indexOfFocusedImage = getIndexOfFocusedImage();

	if (next ? (++indexOfFocusedImage == images.length) : (--indexOfFocusedImage == -1)) return;

	// Remove existing image.
	removeElement("#image-focus-overlay img");
	// Unset “focused” class of just-removed image.
	query(GW.imageFocus.focusedImageSelector).removeClass("focused");

	// Create the focused version of the image.
	images[indexOfFocusedImage].addClass("focused");
	let imageFocusOverlay = query("#image-focus-overlay");
	let clonedImage = images[indexOfFocusedImage].cloneNode(true);
	clonedImage.style = "";
	clonedImage.removeAttribute("width");
	clonedImage.removeAttribute("height");
	clonedImage.style.filter = images[indexOfFocusedImage].style.filter + imageFocusOverlay.dropShadowFilterForImages;
	imageFocusOverlay.appendChild(clonedImage);
	imageFocusOverlay.addClass("engaged");
	// Set image to default size and position.
	resetFocusedImagePosition();
	// Set state of next/previous buttons.
	imageFocusOverlay.query(".slideshow-button.previous").disabled = (indexOfFocusedImage == 0);
	imageFocusOverlay.query(".slideshow-button.next").disabled = (indexOfFocusedImage == images.length - 1);
	// Set the image number display.
	query("#image-focus-overlay .image-number").textContent = (indexOfFocusedImage + 1);
	// Set the caption.
	setImageFocusCaption();
	// Replace the hash.
	history.replaceState(null, null, "#if_slide_" + (indexOfFocusedImage + 1));
}

function setImageFocusCaption() {
	GWLog("setImageFocusCaption");
	var T = { }; // Temporary storage.

	// Clear existing caption, if any.
	let captionContainer = query("#image-focus-overlay .caption");
	Array.from(captionContainer.children).forEach(child => { child.remove(); });

	// Determine caption.
	let currentlyFocusedImage = query(GW.imageFocus.focusedImageSelector);
	var captionHTML;
	if ((T.enclosingFigure = currentlyFocusedImage.closest("figure")) && 
		(T.figcaption = T.enclosingFigure.query("figcaption"))) {
		captionHTML = (T.figcaption.query("p")) ? 
					  T.figcaption.innerHTML : 
					  "<p>" + T.figcaption.innerHTML + "</p>"; 
	} else if (currentlyFocusedImage.title != "") {
		captionHTML = `<p>${currentlyFocusedImage.title}</p>`;
	}
	// Insert the caption, if any.
	if (captionHTML) captionContainer.insertAdjacentHTML("beforeend", captionHTML);
}

function hideImageFocusUI() {
	GWLog("hideImageFocusUI");
	let imageFocusOverlay = query("#image-focus-overlay");
	imageFocusOverlay.queryAll(".slideshow-button, .help-overlay, .image-number, .caption").forEach(element => {
		element.addClass("hidden");
	});
}

function unhideImageFocusUI() {
	GWLog("unhideImageFocusUI");
	let imageFocusOverlay = query("#image-focus-overlay");
	imageFocusOverlay.queryAll(".slideshow-button, .help-overlay, .image-number, .caption").forEach(element => {
		element.removeClass("hidden");
	});
}

function cancelImageFocusHideUITimer() {
	clearTimeout(GW.imageFocus.hideUITimer);
	GW.imageFocus.hideUITimer = null;
}

function focusImageSpecifiedByURL() {
	GWLog("focusImageSpecifiedByURL");
	if (location.hash.hasPrefix("#if_slide_")) {
		registerInitializer('focusImageSpecifiedByURL', true, () => query("#images-overlay") != null, () => {
			let images = queryAll(GW.imageFocus.overlayImagesSelector);
			let imageToFocus = (/#if_slide_([0-9]+)/.exec(location.hash)||{})[1];
			if (imageToFocus > 0 && imageToFocus <= images.length) {
				focusImage(images[imageToFocus - 1]);

				unhideImageFocusUI();
				if (GW.mediaQueries.hover.matches) {
					// Set timer to hide the image focus UI.
					GW.imageFocus.hideUITimer = setTimeout(GW.imageFocus.hideUITimerExpired, GW.imageFocus.hideUITimerDuration);
				}
			}
		});
	}
}

/******************/
/* IMAGES OVERLAY */
/******************/

function generateImagesOverlay() {
	GWLog("generateImagesOverlay");
	// Don’t do this on the about page.
	if (query(".about-page") != null) return;

	// Remove existing, if any.
	removeElement("#images-overlay");

	// Create new.
	query("body").insertAdjacentHTML("afterbegin", "<div id='images-overlay'></div>");
	let imagesOverlay = query("#images-overlay");
	let imagesOverlayLeftOffset = imagesOverlay.getBoundingClientRect().left;
	queryAll(".post-body img").forEach(image => {
		image.removeAttribute("width");
		image.removeAttribute("height");
		image.removeAttribute("style");
		image.removeAttribute("class");

		let clonedImageContainer = document.createElement("div");
		let clonedImage = image.cloneNode(true);
		clonedImageContainer.appendChild(clonedImage);

		replicateImageStyle(image, clonedImage, imagesOverlayLeftOffset);

		imagesOverlay.appendChild(clonedImageContainer);
	});

	// Add the event listeners to focus each image.
	imageFocusSetup(true);
}

function recomputeImagesOverlayLayout() {
	GWLog("recomputeImagesOverlayLayout");

	let imagesOverlay = query("#images-overlay");
	if (imagesOverlay == null) return;

	let imagesOverlayLeftOffset = imagesOverlay.getBoundingClientRect().left;
	let overlayImages = imagesOverlay.queryAll("img");
	queryAll(".post-body img").forEach((image, index) => {
		replicateImageStyle(image, overlayImages[index], imagesOverlayLeftOffset);
	});
}

function replicateImageStyle(image, clonedImage, imagesOverlayLeftOffset) {
	GWLog("replicateImageStyle");

	clonedImage.style.borderStyle = getComputedStyle(image).borderStyle;
	clonedImage.style.borderColor = getComputedStyle(image).borderColor;
	clonedImage.style.borderWidth = Math.round(parseFloat(getComputedStyle(image).borderWidth)) + "px";

	let zoomLevel = parseFloat(GW.currentTextZoom);

	clonedImage.parentElement.style.top = image.getBoundingClientRect().top * zoomLevel - parseFloat(getComputedStyle(image).marginTop) + window.scrollY + "px";
	clonedImage.parentElement.style.left = image.getBoundingClientRect().left * zoomLevel - imagesOverlayLeftOffset + "px";
	clonedImage.parentElement.style.width = image.getBoundingClientRect().width * zoomLevel + "px";
	clonedImage.parentElement.style.height = image.getBoundingClientRect().height * zoomLevel + "px";
}
