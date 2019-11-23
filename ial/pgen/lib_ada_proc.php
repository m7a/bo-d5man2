#!/usr/bin/php -f
section: 21
x-masysma-name: ada
keywords: ["ada", "ial_toplevel"]
x-masysma-redirect: rm-TOC.html
---
<?php
# normalize spaces to one space and remove parentheses
function remove_spaces($arg) {
	return preg_replace("/[()]/", "",
				preg_replace('/(\s|\xc2\xa0)+/', " ", $arg));
}

$doc = new DOMDocument();
$doc->loadHTML(file_get_contents("pages_ada/ada_att/rm-0-5.html"));
$divs = $doc->getElementsByTagName("div");
foreach($divs as $div) {
	if($div->getAttribute("class") !== "Index")
		continue;

	$title = NULL;
	$buf = "";

	foreach($div->childNodes as $child) {
		if($title == NULL) {
			$title = str_replace(" ", "_", trim(
					str_replace(", ", "_",
					remove_spaces($child->nodeValue))));
		} elseif($child->nodeType === XML_ELEMENT_NODE &&
						$child->tagName === "a") {
			# link
			$buf = str_replace(" ", "_", trim(preg_replace(
					"/( in | child of |subtype of )/", "",
					remove_spaces($buf))));
			# TODO z for now skip these strange entries with ,
			if($buf !== ",") {
				$tspl = implode("\", \"", explode(".", $title));
				if($tspl != $title && !empty($title))
					$tspl .= "\", \"".$title;
				$ttl = $title.($buf === ""? "": "/$buf");
				$btp = empty($buf)? "": ", \"$buf\"";
				echo(
				"section: 21\n".
				"x-masysma-name: ada/$ttl\n".
				"keywords: [\"ada\", \"$tspl\"$btp]\n".
				"x-masysma-redirect: ".
					$child->getAttribute("href")."\n---\n"
				);
			}
			$buf = "";
		} else {
			# not a link just take it for text
			$buf .= $child->nodeValue;
		}
	}
}
?>
