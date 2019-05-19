<?php

$js_components_dir = "www/js-components/";

echo preg_replace_callback("/{{{(.+?)}}}/", function ($m) {
	global $js_components_dir;
	return file_get_contents($js_components_dir."{$m[1]}.js");
}, file_get_contents($argv[1]));

?>