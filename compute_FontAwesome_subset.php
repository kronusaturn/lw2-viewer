<?php

$directories = [
	"www/",
	"www/accordius/",
	"www/ea/",
	"www/js-components/modules/",
	"www/js-components/parts/"
];
$patterns = [ 
	"*.js",
	"*.css.php"
];
$additional_files = [
	"lw2.lisp"
];
$characters = [ ];

function process_file($filename) {
	if (!file_exists($filename))
		return;

	global $characters;
	
	$contents = file_get_contents($filename);

	preg_match_all('/&#x(.{4})/', $contents, $matches);	
	$characters = array_merge($characters, $matches[1]);

	preg_match_all('/\\\(F.{3})/', $contents, $matches);
	$characters = array_merge($characters, $matches[1]);
}

foreach ($directories as $directory) {
	foreach ($patterns as $pattern) {
		$files = glob($directory.$pattern);
		foreach ($files as $file) {
			process_file($file);
		}
	}
}
foreach ($additional_files as $file) {
	process_file($file);
}

foreach ($characters as $key => $value) {
	$characters[$key] = strtoupper($value);
}
$characters = array_unique($characters);
sort($characters);
echo implode(",",$characters);
echo "\n";

?>