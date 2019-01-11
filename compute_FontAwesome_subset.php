<?php

$directories = [
	"www/",
	"www/accordius/",
	"www/ea/"
];
$files = [ 
	"script.js",
	"style.css.php",
	"style_mobile_additions.css.php",
	"theme_tweaker.css.php",
	"theme-brutalist.css.php",
	"theme-classic.css.php",
	"theme-default.css.php",
	"theme-grey.css.php",
	"theme-less.css.php",
	"theme-rts.css.php",
	"theme-ultramodern.css.php",
	"theme-zero.css.php",
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
	foreach ($files as $file) {
		process_file($directory.$file);
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