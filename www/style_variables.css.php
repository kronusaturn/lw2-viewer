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
	--GW-body-background-color: transparent;

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
	--GW-TOC-heading-font-weight: var(--GW-UI-font-weight-light);

	--GW-link-post-link-font: var(--GW-UI-font);
	--GW-link-post-link-font-weight: var(--GW-UI-font-weight-heavy);

	--GW-comment-meta-author-font-weight: var(--GW-UI-font-weight-heavy);

	--GW-user-page-karma-font-weight: 700;

	--GW-login-page-tip-label-font-weight: 700;
}

/*	Layout.
	*/
:root {
}

/*	Color scheme.
	*/
:root {
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
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

/*	Layout.
	*/
@media only screen and (max-width: 900px) {
	:root {
	}
}
@media only screen and (max-width: 520px) {
	:root {
	}
}

