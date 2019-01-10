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
    list-style-type: none;
    justify-content: center;
    margin: 0;
}
#tags li a {
    display: inline-block;
}

<?php

if (isset($argv[2]) &&
	preg_match("/\\.css(.php)?$/", $argv[2]) && 
	file_exists("accordius/".$argv[2]))
	include("accordius/".$argv[2]);

?>