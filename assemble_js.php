#!/usr/bin/env php
<?php

$js_components_dir = "www/js-components/";
$js_dir = "www/js/";

foreach (glob("www/*.js") as $js_file_name) {
	file_put_contents($js_dir.basename($js_file_name), process_file(file_get_contents($js_file_name)));
}

function process_file($js_file) {
	return preg_replace_callback("/{{{(.+?)}}}/", function ($m) {
		global $js_components_dir;
		return process_file(file_get_contents($js_components_dir."{$m[1]}.js"));
	}, $js_file);
}

?>