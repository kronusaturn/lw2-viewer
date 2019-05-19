<?php

$js_components_dir = "www/js-components/";
$js_dir = "www/js/";

$js_file = preg_replace_callback("/{{{(.+?)}}}/", function ($m) {
	global $js_components_dir;
	return file_get_contents($js_components_dir."{$m[1]}.js");
}, file_get_contents($argv[1]));

file_put_contents("{$js_dir}". basename($argv[1]), $js_file);

?>