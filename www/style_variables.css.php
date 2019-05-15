/*************/
/* VARIABLES */
/*************/

/*	Typography.
	*/
:root {
	--GW-Font-Awesome: 'Font Awesome', 'Font Awesome 5 Free';

	--GW-monospaced-fallback-font-stack: 'Source Code Pro', 'Menlo', 'Courier', 'Courier New', monospace;
	--GW-sans-serif-fallback-font-stack: 'Open Sans', 'Trebuchet MS', 'Verdana', 'Helvetica', 'Arial', sans-serif;
	--GW-serif-fallback-font-stack:  'PT Serif', 'Georgia', serif;

	--GW-monospaced-font: var(--GW-monospaced-fallback-font-stack);
	--GW-monospaced-font-weight: 400;
	
	--GW-Alignment-Forum-logotype-font: 'Concourse SmallCaps', 'Changa One';
	--GW-Alignment-Forum-logotype-font-weight: 600;

	--GW-UI-font: var(--GW-sans-serif-fallback-font-stack);
	--GW-UI-font-smallcaps: var(--GW-UI-font);
	--GW-UI-font-weight-light: 400;
	--GW-UI-font-weight-heavy: 700;

	--GW-nav-item-font-weight: var(--GW-UI-font-weight-heavy);

	--GW-keyboard-help-overlay-font: var(--GW-UI-font);

	--GW-theme-selector-tooltip-font: var(--GW-UI-font);

	--GW-body-text-font: var(--GW-serif-fallback-font-stack);
	--GW-body-text-font-weight: 400;

	--GW-editor-font: var(--GW-monospaced-font);
	--GW-editor-font-weight: var(--GW-monospaced-font-weight);

	--GW-post-listings-font: var(--GW-UI-font);
	--GW-post-listings-font-weight: var(--GW-UI-font-weight-heavy);
	--GW-listings-post-meta-font: var(--GW-UI-font);
	--GW-listings-post-meta-font-weight: 400;
	
	--GW-post-title-font: var(--GW-post-listings-font);
	--GW-post-title-font-weight: var(--GW-post-listings-font-weight);

	--GW-content-headings-font: var(--GW-post-listings-font);
	--GW-content-headings-font-weight: var(--GW-post-listings-font-weight);

	--GW-tables-font: var(--GW-monospaced-font);
	--GW-tables-font-weight: var(--GW-monospaced-font-weight);

	--GW-footnote-numbers-font-weight: 700;

	--GW-TOC-font: var(--GW-UI-font);
	--GW-TOC-heading-font-weight: var(--GW-UI-font-weight-heavy);

	--GW-link-post-link-font: var(--GW-UI-font);
	--GW-link-post-link-font-weight: var(--GW-UI-font-weight-heavy);

	--GW-comment-meta-author-font-weight: var(--GW-UI-font-weight-heavy);

	--GW-user-page-karma-font-weight: 700;

	--GW-login-page-tip-label-font-weight: 700;
}

/*	Layout.
	*/
:root {
	--GW-content-width: 0;

	--GW-content-side-padding: 50px;
	--GW-content-left-side-padding: var(--GW-content-side-padding);
	--GW-content-right-side-padding: var(--GW-content-side-padding);

	--GW-sequence-page-content-side-padding: 50px;
	--GW-sequence-page-content-left-side-padding: var(--GW-sequence-page-content-side-padding);
	--GW-sequence-page-content-right-side-padding: var(--GW-sequence-page-content-side-padding);

	--GW-user-page-content-side-padding: 30px;
	--GW-user-page-content-left-side-padding: var(--GW-user-page-content-side-padding);
	--GW-user-page-content-right-side-padding: var(--GW-user-page-content-side-padding);

	--GW-recent-comments-page-content-side-padding: 30px;
	--GW-recent-comments-page-content-left-side-padding: var(--GW-recent-comments-page-content-side-padding);
	--GW-recent-comments-page-content-right-side-padding: var(--GW-recent-comments-page-content-side-padding);

	--GW-conversation-page-content-side-padding: 30px;
	--GW-conversation-page-content-left-side-padding: var(--GW-conversation-page-content-side-padding);
	--GW-conversation-page-content-right-side-padding: var(--GW-conversation-page-content-side-padding);

	--GW-post-page-content-side-padding: 30px;
	--GW-post-page-content-left-side-padding: var(--GW-post-page-content-side-padding);
	--GW-post-page-content-right-side-padding: var(--GW-post-page-content-side-padding);

	--GW-post-side-padding: 30px;
	--GW-post-left-side-padding: var(--GW-post-side-padding);
	--GW-post-right-side-padding: var(--GW-post-side-padding);

	--GW-edit-post-page-content-side-padding: 30px;
	--GW-edit-post-page-content-left-side-padding: var(--GW-edit-post-page-content-side-padding);
	--GW-edit-post-page-content-right-side-padding: var(--GW-edit-post-page-content-side-padding);

	--GW-about-page-content-side-padding: var(--GW-post-page-content-side-padding);
	--GW-about-page-content-left-side-padding: var(--GW-post-page-content-side-padding);
	--GW-about-page-content-right-side-padding: var(--GW-post-page-content-side-padding);

	--GW-individual-thread-page-content-side-padding: 30px;
	--GW-individual-thread-page-content-left-side-padding: var(--GW-individual-thread-page-content-side-padding);
	--GW-individual-thread-page-content-right-side-padding: var(--GW-individual-thread-page-content-side-padding);

	--GW-comment-compact-height: auto;
	--GW-comment-compact-height-mobile: auto;
	--GW-comment-minimized-height: auto;
	--GW-comment-minimized-height-mobile: auto;
}
@media only screen and (max-width: 900px) {
	:root {
		--GW-content-side-padding: calc(100% / 45);
		--GW-sequence-page-content-side-padding: calc(100% / 36);
		--GW-user-page-content-side-padding: calc(100% / 30);
		--GW-recent-comments-page-content-side-padding: calc(100% / 30);
		--GW-conversation-page-content-side-padding: calc(100% / 30);
		--GW-post-page-content-side-padding: calc(100% / 30);
		--GW-post-side-padding: 0px;
		--GW-edit-post-page-content-side-padding: calc(100% / 30);
		--GW-individual-thread-page-content-side-padding: calc(100% / 30);
	}
}
@media only screen and (max-width: 520px) {
	:root {
		--GW-post-page-content-side-padding: 4px;
		--GW-post-side-padding: 6px;
	}
}

/*	Content column width.
	*/
:root head.content-width-normal + body {
	--GW-content-width: 900px;
}
:root head.content-width-wide + body {
	--GW-content-width: 1150px;
}
:root head.content-width-fluid + body {
	--GW-content-width: none;
}

/*	Page side padding.
	*/
:root #content {
	--GW-current-page-content-left-side-padding: var(--GW-content-left-side-padding);
	--GW-current-page-content-right-side-padding: var(--GW-content-right-side-padding);
}
:root #content.sequence-page {
	--GW-current-page-content-left-side-padding: var(--GW-sequence-page-content-left-side-padding);
	--GW-current-page-content-right-side-padding: var(--GW-sequence-page-content-right-side-padding);
}
:root #content.user-page {
	--GW-current-page-content-left-side-padding: var(--GW-user-page-content-left-side-padding);
	--GW-current-page-content-right-side-padding: var(--GW-user-page-content-right-side-padding);
}
:root #content.recent-comments-page {
	--GW-current-page-content-left-side-padding: var(--GW-recent-comments-page-content-left-side-padding);
	--GW-current-page-content-right-side-padding: var(--GW-recent-comments-page-content-right-side-padding);
}
:root #content.conversation-page {
	--GW-current-page-content-left-side-padding: var(--GW-conversation-page-content-left-side-padding);
	--GW-current-page-content-right-side-padding: var(--GW-conversation-page-content-right-side-padding);
}
:root #content.post-page {
	--GW-current-page-content-left-side-padding: var(--GW-post-page-content-left-side-padding);
	--GW-current-page-content-right-side-padding: var(--GW-post-page-content-right-side-padding);
}
:root #content.edit-post-page {
	--GW-current-page-content-left-side-padding: var(--GW-edit-post-page-content-left-side-padding);
	--GW-current-page-content-right-side-padding: var(--GW-edit-post-page-content-right-side-padding);
}
:root #content.about-page {
	--GW-current-page-content-left-side-padding: var(--GW-about-page-content-left-side-padding);
	--GW-current-page-content-right-side-padding: var(--GW-about-page-content-right-side-padding);
}
:root #content.individual-thread-page {
	--GW-current-page-content-left-side-padding: var(--GW-individual-thread-page-content-left-side-padding);
	--GW-current-page-content-right-side-padding: var(--GW-individual-thread-page-content-right-side-padding);
}

/*	Color scheme.
	*/
:root {
	--GW-body-background-color: transparent;
	--GW-content-background-color: var(--GW-body-background-color);

	--GW-body-text-color: #000;

	/*	UI elements.
		*/
	
	--GW-hyperlink-color: #00e;
	--GW-hyperlink-visited-color: var(--GW-hyperlink-color);
	--GW-hyperlink-hover-color: var(--GW-hyperlink-color);
	--GW-hyperlink-active-color: var(--GW-hyperlink-color);

	--GW-nav-bar-item-color: var(--GW-hyperlink-color);
	--GW-nav-bar-item-hover-color: var(--GW-hyperlink-hover-color);
	--GW-nav-bar-item-active-color: var(--GW-hyperlink-active-color);

	--GW-search-field-placeholder-color: #f00;

	--GW-button-color: var(--GW-hyperlink-color);
	--GW-button-hover-color: var(--GW-hyperlink-hover-color);
	--GW-button-active-color: var(--GW-hyperlink-active-color);

	--GW-button-background-color: transparent;
	--GW-button-hover-background-color: transparent;
	--GW-button-active-background-color: transparent;

	/*	Archive.
		*/

	--GW-archive-nav-item-color: var(--GW-hyperlink-color);
	--GW-archive-nav-item-hover-color: var(--GW-hyperlink-hover-color);
	--GW-archive-nav-item-active-color: var(--GW-hyperlink-active-color);

	/*	Comment thread navigation.
		*/

	--GW-comment-background-color-odd: var(--GW-body-background-color);
	--GW-comment-background-color-even: var(--GW-body-background-color);
	--GW-comment-background-color-target: var(--GW-body-background-color);

	--GW-comment-item-outline-color: transparent;
	--GW-comment-item-outline-width: 3px;

	--GW-comment-item-new-comment-outline-color: transparent;
	--GW-comment-item-focused-outline-color: transparent;
	--GW-comment-item-expanded-outline-color: var(--GW-comment-item-focused-outline-color);
	--GW-comment-item-higlight-color: transparent;
	--GW-comment-item-highlight-faint-color: transparent;

	--GW-comment-item-expanded-background-color: var(--GW-body-background-color);
	--GW-comment-item-expanded-box-shadow: none;

	--GW-comment-popup-background-color: var(--GW-comment-item-expanded-background-color);

	/*	Vote buttons.
		*/

	--GW-upvote-button-color: inherit;
	--GW-downvote-button-color: inherit;
}

/*	Sprites.
	*/
:root {
	--GW-comment-meta-icons-filled-sprites-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/comment_meta_icons_filled.gif")) ?>');
	--GW-comment-meta-icons-outline-sprites-image: url('data:image/gif;base64,<?php echo base64_encode(file_get_contents("assets/comment_meta_icons_outline.gif")) ?>');

	--GW-comment-meta-icons-normal-sprites: var(--GW-comment-meta-icons-outline-sprites-image);
	--GW-comment-meta-icons-hover-sprites: var(--GW-comment-meta-icons-filled-sprites-image);
}