`<style id='theme-fade-transition'>
	body {
		transition:
			opacity 0.5s ease-out,
			background-color 0.3s ease-out;
	}
	body.transparent {
		background-color: #777;
		opacity: 0.0;
		transition:
			opacity 0.5s ease-in,
			background-color 0.3s ease-in;
	}
</style>`