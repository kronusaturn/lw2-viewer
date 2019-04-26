<?php
	header ('Content-type: text/css; charset=utf-8');

	$platform = @$argv[1] ?: 'Mac';

	function fit_content($selector, $property = "width", $line_prefix = "") {
		foreach (["-moz-fit-content", "fit-content"] as $pvalue) echo 
"{$line_prefix}@supports (width: {$pvalue}) {
{$line_prefix}	{$selector} {
{$line_prefix}		{$property}: {$pvalue};
{$line_prefix}	}
{$line_prefix}}
";
	}

	$firefox_exclusive = "@supports (-moz-user-focus: normal)";
?>

/*%%%%%%%%%%%%%%%*/
/*%% VARIABLES %%*/
/*%%%%%%%%%%%%%%%*/

<?php include("style_variables.css.php"); ?>

/*%%%%%%%%%%%%%%%%%*/
/*%% BASE STYLES %%*/
/*%%%%%%%%%%%%%%%%%*/

<?php include("style_base.css.php"); ?>

/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%% FLOATING UI ELEMENTS %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/

<?php include("style_floating_ui.css.php"); ?>

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%% SCROLLING UI ELEMENTS %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

<?php include("style_scrolling_ui.css.php"); ?>

/*%%%%%%%%%%%%%%%%%%%%%%*/
/*%% POSTS & COMMENTS %%*/
/*%%%%%%%%%%%%%%%%%%%%%%*/

<?php include("style_posts_comments.css.php"); ?>

/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%% COMMENTING & POSTING %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/

<?php include("style_commenting_posting.css.php"); ?>

/*%%%%%%%%%%%%%%%%%%%%%%*/
/*%% CONTENT ELEMENTS %%*/
/*%%%%%%%%%%%%%%%%%%%%%%*/

<?php include("style_content_elements.css.php"); ?>

/*%%%%%%%%%%%%%%%%*/
/*%% PAGE TYPES %%*/
/*%%%%%%%%%%%%%%%%*/

<?php include("style_page_types.css.php"); ?>

/*%%%%%%%%%%%%%%%%%*/
/*%% IMAGE FOCUS %%*/
/*%%%%%%%%%%%%%%%%%*/

<?php include("style_image_focus.css.php"); ?>

/*%%%%%%%%%%%*/
/*%% THEME %%*/
/*%%%%%%%%%%%*/

<?php if (isset($argv[2]) && preg_match("/\\.css(.php)?$/", $argv[2])) include($argv[2]); ?>

/*%%%%%%%%%%%%%%%*/
/*%% ADDITIONS %%*/
/*%%%%%%%%%%%%%%%*/

<?php

## TO BE IMPLEMENTED:
## This will be specified via command-line argument; but for now, we just 
## include all available additions (currently, only 'accordius').

$additions = [
	'accordius'
];

foreach ($additions as $addition) {
	$potential_includes = [
		"style.css.php",
		"style_mobile_additions.css.php"
	];
	foreach ($potential_includes as $include) {
		$include_path = "{$addition}/{$include}";
		if (file_exists($include_path))
			include ($include_path);
	}
}

?>
