/*************/
/* VARIABLES */
/*************/

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

	--GW-hyperlink-color: #00e;

	--GW-post-listings-font: var(--GW-UI-font);
	--GW-post-listings-font-weight: var(--GW-UI-font-weight-heavy);
	--GW-listings-post-meta-font: var(--GW-UI-font);
	--GW-listings-post-meta-font-weight: 400;
	
	--GW-post-title-font: var(--GW-post-listings-font);
	--GW-post-title-font-weight: var(--GW-post-listings-font-weight);

	--GW-content-headings-font: var(--GW-post-listings-font);

	--GW-tables-font: var(--GW-monospaced-font);

	--GW-footnote-numbers-font-weight: 700;

	--GW-TOC-font: var(--GW-UI-font);
	--GW-TOC-heading-font-weight: 400;

	--GW-link-post-link-font: var(--GW-UI-font);
	--GW-link-post-link-font-weight: 700;

	--GW-comment-meta-author-font-weight: var(--GW-UI-font-weight-heavy);

	--GW-user-page-karma-font-weight: 700;

	--GW-login-page-tip-label-font-weight: 700;
	
	--GW-content-side-padding: 30px;
}

/*=----------------------------=*/
/*= Viewport width adjustments =*/
/*=----------------------------=*/

@media only screen and (max-width: 900px) {
	:root {
		--GW-content-side-padding: 4px;
	}
}

