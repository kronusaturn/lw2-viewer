/*************/
/* VARIABLES */
/*************/

/*	Typography.
	*/
:root {
	--GW-Font-Awesome: 'Font Awesome', 'Font Awesome 5 Free';
	--GW-monospaced-font: 'Inconsolata', 'Menlo', monospace;
	
	--GW-Alignment-Forum-logotype-font: 'Concourse SmallCaps', 'Changa One';
	--GW-Alignment-Forum-logotype-font-weight: 600;

	--GW-UI-font: 'Open Sans', 'Arial', sans-serif;
	--GW-UI-font-smallcaps: var(--GW-UI-font);
	--GW-UI-font-weight-light: 400;
	--GW-UI-font-weight-heavy: 700;

	--GW-keyboard-help-overlay-font: var(--GW-UI-font);

	--GW-body-text-font: 'PT Serif', 'Georgia', serif;
	--GW-body-text-font-weight: 400;

	--GW-editor-font: var(--GW-body-text-font);
	--GW-editor-font-weight: var(--GW-body-text-font-weight);

	--GW-post-listings-font: var(--GW-UI-font);
	--GW-post-listings-font-weight: var(--GW-UI-font-weight-heavy);
	--GW-listings-post-meta-font: var(--GW-UI-font);
	--GW-listings-post-meta-font-weight: 400;
	
	--GW-post-title-font: var(--GW-post-listings-font);
	--GW-post-title-font-weight: var(--GW-post-listings-font-weight);

	--GW-content-headings-font: var(--GW-post-listings-font);
	--GW-content-headings-font-weight: var(--GW-post-listings-font-weight);

	--GW-tables-font: var(--GW-monospaced-font);

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

	--GW-content-side-padding: 0;
	--GW-content-left-side-padding: var(--GW-content-side-padding);
	--GW-content-right-side-padding: var(--GW-content-side-padding);

	--GW-sequence-page-content-side-padding: 0;
	--GW-sequence-page-content-left-side-padding: var(--GW-sequence-page-content-side-padding);
	--GW-sequence-page-content-right-side-padding: var(--GW-sequence-page-content-side-padding);

	--GW-user-page-content-side-padding: 0;
	--GW-user-page-content-left-side-padding: var(--GW-user-page-content-side-padding);
	--GW-user-page-content-right-side-padding: var(--GW-user-page-content-side-padding);

	--GW-recent-comments-page-content-side-padding: 0;
	--GW-recent-comments-page-content-left-side-padding: var(--GW-recent-comments-page-content-side-padding);
	--GW-recent-comments-page-content-right-side-padding: var(--GW-recent-comments-page-content-side-padding);

	--GW-conversation-page-content-side-padding: 0;
	--GW-conversation-page-content-left-side-padding: var(--GW-conversation-page-content-side-padding);
	--GW-conversation-page-content-right-side-padding: var(--GW-conversation-page-content-side-padding);

	--GW-post-page-content-side-padding: 0;
	--GW-post-page-content-left-side-padding: var(--GW-post-page-content-side-padding);
	--GW-post-page-content-right-side-padding: var(--GW-post-page-content-side-padding);

	--GW-edit-post-page-content-side-padding: 0;
	--GW-edit-post-page-content-left-side-padding: var(--GW-edit-post-page-content-side-padding);
	--GW-edit-post-page-content-right-side-padding: var(--GW-edit-post-page-content-side-padding);

	--GW-about-page-content-side-padding: var(--GW-post-page-content-side-padding);
	--GW-about-page-content-left-side-padding: var(--GW-post-page-content-side-padding);
	--GW-about-page-content-right-side-padding: var(--GW-post-page-content-side-padding);

	--GW-individual-thread-page-content-side-padding: 0;
	--GW-individual-thread-page-content-left-side-padding: var(--GW-individual-thread-page-content-side-padding);
	--GW-individual-thread-page-content-right-side-padding: var(--GW-individual-thread-page-content-side-padding);

	--GW-post-side-padding: 0;
	--GW-post-left-side-padding: var(--GW-post-side-padding);
	--GW-post-right-side-padding: var(--GW-post-side-padding);

	--GW-comment-compact-height: auto;
	--GW-comment-compact-height-mobile: auto;
	--GW-comment-minimized-height: auto;
	--GW-comment-minimized-height-mobile: auto;
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
	
	--GW-hyperlink-color: #00e;
	--GW-hyperlink-visited-color: var(--GW-hyperlink-color);
	--GW-hyperlink-hover-color: var(--GW-hyperlink-color);
	--GW-hyperlink-active-color: var(--GW-hyperlink-color);

	--GW-nav-bar-item-color: var(--GW-hyperlink-color);
	--GW-nav-bar-item-hover-color: var(--GW-hyperlink-hover-color);
	--GW-nav-bar-item-active-color: var(--GW-hyperlink-active-color);

	--GW-button-color: var(--GW-hyperlink-color);
	--GW-button-hover-color: var(--GW-hyperlink-hover-color);
	--GW-button-active-color: var(--GW-hyperlink-active-color);

	--GW-button-background-color: transparent;
	--GW-button-hover-background-color: transparent;
	--GW-button-active-background-color: transparent;

	--GW-archive-nav-item-color: var(--GW-hyperlink-color);
	--GW-archive-nav-item-hover-color: var(--GW-hyperlink-hover-color);
	--GW-archive-nav-item-active-color: var(--GW-hyperlink-active-color);

	--GW-comment-background-color-odd: transparent;
	--GW-comment-background-color-even: transparent;
	--GW-comment-background-color-target: transparent;

	--GW-comment-item-expanded-background-color: transparent;
	--GW-comment-item-expanded-box-shadow: none;
	--GW-comment-item-expanded-outline-color: transparent;
	--GW-comment-item-expanded-outline: 3px solid var(--GW-comment-item-expanded-outline-color);

	--GW-comment-item-outline-color: transparent;

	--GW-new-comment-item-outline-color: transparent;

	--GW-comment-item-focused-background-color: var(--GW-comment-item-expanded-background-color);
	--GW-comment-item-focused-outline-color: var(--GW-comment-item-expanded-outline-color);
	--GW-comment-item-focused-outline: var(--GW-comment-item-expanded-outline);

	--GW-comment-popup-background-color: var(--GW-comment-item-focused-background-color);
}

