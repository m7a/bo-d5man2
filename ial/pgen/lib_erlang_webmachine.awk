#!/usr/bin/awk -f
{
	brackhere = index($0, "[[")
	if(brackhere != 0) {
		printf "%s", substr($0, 1, brackhere - 1)
		rest = substr($0, brackhere)
		brackhere = index(rest, "]]")
		if(brackhere != 0) {
			rest2 = substr(rest, 1, brackhere - 1)
			maxnum = index(rest2, "|")
			if(substr(rest2, 3, 1) == "/") {
				if(maxnum == 0) {
					maxnum = length(rest2)
				}
				maxnum = maxnum - 4
				printf "![](%s)", substr(rest2, 4, maxnum)
			} else if(maxnum == 0) {
				pagnam = substr(rest2, 3, length(rest2) - 2)
				pagfil = pagnam
				gsub(/ /, "-", pagfil)
				printf "[%s](%s.html)", pagnam, pagfil
			} else {
				replstr = substr(rest2, maxnum + 1)
				if(match(replstr, /^https?:/) == 0) {
					replstr = replstr ".html"
				}
				printf "[%s](%s)", substr(rest2, 3, maxnum - 3),
									replstr
			}
			print substr(rest, brackhere + 2)
		} else {
			print rest
		}
	} else {
		print
	}
}
