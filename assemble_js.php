#!/usr/bin/env php
<?php

$js_components_dir = "www/js-components/";
$js_dir = "www/js/";

$js_files = glob("www/*.js");
foreach ($js_files as $js_file_name) {
	$js_file = preg_replace_callback("/{{{(.+?)}}}/", function ($m) {
		global $js_components_dir;
		return file_get_contents($js_components_dir."{$m[1]}.js");
	}, file_get_contents($js_file_name));

	file_put_contents("{$js_dir}". basename($js_file_name), $js_file);
}

?>