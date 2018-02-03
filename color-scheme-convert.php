<?php

if (!isset($argv[1]))
	die;
	
$debug_enabled = false;

$stylesheet = file_get_contents($argv[1]);
echo preg_replace_callback("/(#[0-9abcdef]+)([,; ])/i", 'ProcessColorValue', $stylesheet);
function ProcessColorValue($m) {
	debug_log($m[1]);
	$m[1] = RGBToHex(XYZToRGB(LabToXYZ(CVT(XYZToLab(RGBToXYZ(HexToRGB($m[1])))))));
	debug_log("\n");

	return implode(array_slice($m,1));
}

/***********/
/* HELPERS */
/***********/

function debug_log($string) {
	global $debug_enabled;
	if ($debug_enabled)
		echo $string;
}

/******************/
/* TRANSFORMATION */
/******************/

## CVT = "Color Value Transform"
function CVT($Lab_value, $color_space = "Lab") {
	$Lab_value[0] = 100 - $Lab_value[0];
	debug_log("  →  Lab ".PCC($Lab_value));

	return $Lab_value;
}

/*********************/
/* FORMAT CONVERSION */
/*********************/

function HexToRGB($hexColorString) {
	if ($hexColorString[0] == '#')
		$hexColorString = substr($hexColorString,1);
	if (strlen($hexColorString) == 3)
		$hexColorString = preg_replace("/./","$0$0",$hexColorString);
	$components = str_split($hexColorString,2);
	foreach ($components as $i => $hexColor)
		$components[$i] = hexdec($hexColor);
	debug_log("  →  RGB ".PCC($components));
	return $components;
}

function RGBToHex($rgb_components) {
	foreach ($rgb_components as $i => $component) {
		$hex_value = dechex(round($component));
		if (strlen($hex_value) == 1)
			$hex_value = "0".$hex_value;
		$rgb_components[$i] = $hex_value;
	}
	$hexColorString = "#" . implode($rgb_components);
	$hexColorString = preg_replace("/([0-9abcdef])\\1([0-9abcdef])\\2([0-9abcdef])\\3/", "$1$2$3", $hexColorString);
	debug_log("  →  ".$hexColorString);
	return $hexColorString;
}

## PCC = "Print Color Components"
function PCC($components) {
	foreach ($components as $k => $v) {
		$components[$k] = round($v, 2);
	}
	return "( " . implode(", ", $components) . " )";
}

/**************************/
/* COLOR SPACE CONVERSION */
/**************************/

function RGBToXYZ($rgb_components) {
	foreach ($rgb_components as $i => $component) {
		$component /= 255.0;
		$rgb_components[$i] = ($component > 0.04045) ?
							  (pow((($component + 0.055) / 1.055), 2.4)) :
							  ($component / 12.92);
	}

	$var_R = $rgb_components[0] * 100.0;
	$var_G = $rgb_components[1] * 100.0;
	$var_B = $rgb_components[2] * 100.0;

	$X = $var_R * 0.4124 + $var_G * 0.3576 + $var_B * 0.1805;
	$Y = $var_R * 0.2126 + $var_G * 0.7152 + $var_B * 0.0722;
	$Z = $var_R * 0.0193 + $var_G * 0.1192 + $var_B * 0.9505;
	
	debug_log("  →  XYZ ".PCC([ $X, $Y, $Z ]));
	return [ $X, $Y, $Z ];
}

function XYZToLab($xyz_components) {
	$xyz_components[0] /= 95.047;
	$xyz_components[1] /= 100.000;
	$xyz_components[2] /= 108.883;

	foreach ($xyz_components as $i => $component) {
		$xyz_components[$i] = ($component > 0.008856) ?
							  (pow($component, (1.0/3.0))) :
							  ((7.787 * $component) + (16.0/116.0));
	}

	$var_X = $xyz_components[0];
	$var_Y = $xyz_components[1];
	$var_Z = $xyz_components[2];

	$L = (116.0 * $var_Y) - 16.0;
	$a = 500.0 * ($var_X - $var_Y);
	$b = 200.0 * ($var_Y - $var_Z);
	
	debug_log("  →  Lab ".PCC([ $L, $a, $b ]));
	return [ $L, $a, $b ];
}

function LabToXYZ($lab_components) {
	
	$var_Y = ($lab_components[0] + 16.0) / 116.0;
	$var_X = $lab_components[1] / 500.0 + $var_Y;
	$var_Z = $var_Y - $lab_components[2] / 200.0;
	$xyz_components = [ $var_X, $var_Y, $var_Z ];
	
	foreach ($xyz_components as $i => $component) {
		$xyz_components[$i] = (pow($component, 3) > 0.008856) ?
							  (pow($component, 3)) :
							  (($component - 16.0/116.0) / 7.787);
	}

	$xyz_components[0] *= 95.047;
	$xyz_components[1] *= 100.000;
	$xyz_components[2] *= 108.883;

	debug_log("  →  XYZ ".PCC($xyz_components));
	return $xyz_components;
}

function XYZToRGB($xyz_components) {
	$var_X = $xyz_components[0] / 100.0;
	$var_Y = $xyz_components[1] / 100.0;
	$var_Z = $xyz_components[2] / 100.0;

	$var_R = $var_X *  3.2406 + $var_Y * -1.5372 + $var_Z * -0.4986;
	$var_G = $var_X * -0.9689 + $var_Y *  1.8758 + $var_Z *  0.0415;
	$var_B = $var_X *  0.0557 + $var_Y * -0.2040 + $var_Z *  1.0570;
	
	$rgb_components = [ $var_R, $var_G, $var_B ];
	foreach ($rgb_components as $i => $component) {
		$component = ($component > 0.0031308) ?
					 (1.055 * pow($component, (1.0/2.4)) - 0.055) : 
					 (12.92 * $component);
		$rgb_components[$i] = min(max($component, 0.0), 1.0) * 255.0;
	}
	
	debug_log("  →  RGB ".PCC($rgb_components));
	return $rgb_components;
}

?>