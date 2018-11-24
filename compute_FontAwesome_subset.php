<?php

$files = [ 
	"www/script.js",
	"www/style.css.php",
	"www/style_mobile_additions.css.php",
	"www/theme_tweaker.css.php",
	"www/theme-brutalist.css.php",
	"www/theme-classic.css.php",
	"www/theme-default.css.php",
	"www/theme-grey.css.php",
	"www/theme-less.css.php",
	"www/theme-rts.css.php",
	"www/theme-ultramodern.css.php",
	"www/theme-zero.css.php",
	"lw2.lisp"
];
$characters = [ ];

foreach ($files as $file) {
	$contents = file_get_contents("{$file}");

	preg_match_all('/&#x(.{4})/', $contents, $matches);	
	$characters = array_merge($characters, $matches[1]);
	
	preg_match_all('/\\\(F.{3})/', $contents, $matches);
	$characters = array_merge($characters, $matches[1]);
}

foreach ($characters as $key => $value) {
	$characters[$key] = strtoupper($value);
}
$characters = array_unique($characters);
sort($characters);
echo implode(",",$characters);
echo "\n";

?>