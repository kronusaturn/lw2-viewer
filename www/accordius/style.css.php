/*************/
/* ACCORDIUS */
/*************/

/*======*/
/* TAGS */
/*======*/

#tags {
	order: 12;
	display: flex;
	flex-basis: 100%;
	justify-content: center;
	margin: 0;
	flex-flow: row wrap;
	align-items: flex-start;
}

<?php

if (isset($argv[2]) &&
	preg_match("/\\.css(.php)?$/", $argv[2]) && 
	file_exists("accordius/".$argv[2]))
	include("accordius/".$argv[2]);

?>