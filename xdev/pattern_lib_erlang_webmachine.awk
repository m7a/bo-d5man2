#!/usr/bin/awk -f
# TODO X NICE TRY BUT COMPLEXITY OF SHELL PART TOO LARGE. RETAIN SPLIT COMPONENTS!

# --------------------------------------------------------------------- outer --
BEGIN {
	if (ARGV[1] != "inner=1") {
		# https://unix.stackexchange.com/questions/228072
		query = "ps -ww -p $PPID -o cmd | tail -n 1"
		query | getline self_cmd
		close(query)
		outer_main(self_cmd)
		exit 0
	}
}

function outer_main(self_cmd) {
	phoenix_root = ENVIRON["MDVL_CI_PHOENIX_ROOT"]
	if (phoenix_root == "") {
		# This is not strictly correct but should work OK in most cases
		# to determine a position relative to the current scripts'
		# location
		query = sprintf("cd \"$(dirname \"%s\")/..\" && pwd",
				substr(self_cmd, index(self_cmd, " -f ") + 4))
		query | getline phoenix_root
		close(query)
	}

	system("! [ -d e_webmachine ] || rm -r e_webmachine; mkdir e_webmachine")
	system(sprintf("\"%s/co-maartifact/maartifact.pl\" extract ial_in_webmachine_wiki.git e_webmachine/cnt https://github.com/webmachine/webmachine.wiki.git", phoenix_root))

	printf("phoenix_root=<%s>\n", phoenix_root)
	printf("self=<%s>\n", self_cmd)
}
# --------------------------------------------------------------------- inner --
{
	brackhere = index($0, "[[")
	if(brackhere != 0) {
		printf "%s", substr($0, 1, brackhere - 1)
		rest      = substr($0, brackhere)
		brackhere = index(rest, "]]")
		if(brackhere != 0) {
			rest2  = substr(rest, 1, brackhere - 1)
			maxnum = index(rest2, "|")
			if(substr(rest2, 3, 1) == "/") {
				if(maxnum == 0)
					maxnum = length(rest2)
				maxnum = maxnum - 4
				printf "![](%s)", substr(rest2, 4, maxnum)
			} else if(maxnum == 0) {
				pagnam = substr(rest2, 3, length(rest2) - 2)
				pagfil = pagnam
				gsub(/ /, "-", pagfil)
				printf "[%s](%s.html)", pagnam, pagfil
			} else {
				replstr = substr(rest2, maxnum + 1)
				if(match(replstr, /^https?:/) == 0)
					replstr = replstr ".html"
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
