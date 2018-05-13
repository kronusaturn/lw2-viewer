<?php

if (!isset($argv[1]))
	die;
	
## Get command line arguments.
$stylesheet = file_get_contents($argv[1]);
$stylesheet = preg_replace("/\\/\\*.+?\\*\\//", "", $stylesheet);
// $stylesheet = preg_replace("/\\s+/", " ", $stylesheet);

$style_blocks = [ ];
preg_replace_callback("/([^{}]+?){([^}{]+?)}/", 'AggregateStyles', $stylesheet);
function AggregateStyles($m) {
	global $style_blocks;
	$selectors = explode(",", preg_replace("/,\\s+/", ",", trim($m[1])));
	$styles = explode(";", preg_replace([ "/\\s+/", "/;\\s+/" ], [ " ", ";" ], rtrim(trim($m[2]), ";")));
	$style_blocks[] = [ $selectors, $styles ];
	return $m[0];
}

// print_r($style_blocks);

foreach ($style_blocks as $block) {
	foreach ($block[0] as $selector) {
		foreach ($block[1] as $style) {
			list($property, $value) = explode(": ", $style);
			echo "$selector\t$property\t$value\n";
		}
	}
}

?>