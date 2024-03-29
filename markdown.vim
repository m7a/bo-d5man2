" Vim syntax file
" Language:	Markdown
" Maintainer:	Linux-Fan <Ma_Sys.ma@web.de>
" LastChange:	2022/07
" Version:      2.0.3
"

syntax clear

" General
" =======

" Potential Bad Characters in TeX commands
" Happens when you are typing \epsilon too quickly...
hi D5ManInvalid ctermbg=DarkRed
syn match D5ManInvalid "\\[æ“¢ð€đŋħþſŧł„“]"

" Inline Formatting
" =================

" Inline Code
hi D5ManInlinedCode ctermfg=DarkMagenta
syn region D5ManInlinedCode start="`" end="`"

" Emphasis
hi D5ManEmphasis ctermfg=Yellow
syn region D5ManEmphasis start="\(^\|[^A-Za-z_]\)\zs_" end="_\ze\([^A-Za-z_]\|$\)"
hi D5ManBold cterm=bold ctermfg=White
syn region D5ManBold start="\(^\|[^A-Za-z_]\)\zs\*\*" end="\*\*\ze\([^A-Za-z_]\|$\)"

" Contained TeX markup $...$
hi D5ManTex ctermfg=LightBlue
syn region D5ManTex start="\$" end="\$" skip="\\\$" contains=D5ManInvalid

" Links
hi D5ManLink ctermfg=DarkGreen
syn match D5ManLink "<[a-z0-9]\+:\/\/.*>"
syn match D5ManLink "\[[^]]\+\]([^ ]\+)"

" Top-Level
" =========

" Bullet and Numbered Lists
hi D5ManListMarker ctermfg=DarkYellow
syn match D5ManListMarker "^\S\(\S\| \)*$\n^:   \ze\S\|^\s\+\(\*\|[0-9]\+\.\)\ze\s"

" Table
hi D5ManTableSepline ctermfg=DarkGray
syn match D5ManTableSepline "^--\+\(  --\+\)*$"

" Top-Level Sections (equals-sign underlined sections)
hi D5ManSection cterm=bold ctermfg=Yellow
syn match D5ManSection "^.*$\n^\(=\+\|=\+\)$"

" Second-Level Sections (## )
hi D5ManHeadingSecond cterm=bold ctermfg=Cyan
syn match D5ManHeadingSecond "^## .*$"

" Third-Level Sections (### )
hi D5ManHeadingThird cterm=bold ctermfg=Red
syn match D5ManHeadingThird "^### .*$"

" Tilde-Delimited Code Blocks
syn region D5ManRegionCode start=/^\~\~\~\({\.[a-z0-9_]\+}\)\?$/ end=/^\~\~\~$/
hi D5ManRegionCode ctermfg=Gray

" Meta and KV
" ===========

hi D5ManMeta ctermfg=LightBlue
hi D5ManMetaKV cterm=bold ctermfg=LightBlue
syn match D5ManMetaKV contained "^section\ze: [0-9]\+$"
syn match D5ManmetaKV contained "^x-masysma-name\ze: \"\?[a-z0-9A-Z/_.]\+\"\?$"
syn match D5ManMetaKV contained "^\(title\|author\|keywords\|x-masysma-copyright\|x-masysma-version\|x-masysma-repository\|x-masysma-website\|x-masysma-redirect\|x-masysma-web-priority\|x-masysma-web-changefreq\|x-masysma-download\|x-masysma-see-also\|x-masysma-news-rss\|x-masysma-news-image\)\ze: .*$"
syn match D5ManMetaKV contained "^date\ze: [0-9]\{4\}\(\/[0-9]\{2\}\(\/[0-9]\{2\}\( [0-9]\{2\}:[0-9]\{2\}\(:[0-9]\{2\}\)\?\)\?\)\?\)\?$"
syn match D5ManMetaKV contained "^lang\ze: [a-z]\{2\}-[A-Z]\{2\}$"
syn match D5ManMetaKV contained "^x-masysma-owned\ze: [01]$"
syn match D5ManMetaKV contained "^x-masysma-news-skip\ze: [01]$"
syn match D5ManMetaKV contained "^x-masysma-task-type\ze: \(long\|short\|subtask\|periodic\)\(\s\+#.*\)\?$"
syn match D5ManMetaKV contained "^x-masysma-task-priority\ze: \(red\|green\|black\|white\|yellow\|purple\|delayed\|considered\)\(\s\+#.*\)\?$"
syn match D5ManMetaKV contained "^toc\ze: [01]$"
syn region D5ManMeta start=/^---$/ end=/^---$/ contains=D5ManMetaKV fold

" Meta folding
hi Folded cterm=bold ctermfg=LightBlue
setlocal foldmethod=syntax
setlocal foldlevel=0

" High Priority/Other
" ===================

" Trailing Spaces
hi D5ManSpc ctermbg=DarkGray
syn match D5ManSpc "\s\+$"

" Forced Spaces
hi D5ManForcedSpace ctermbg=Gray
syn match D5ManForcedSpace " "
hi D5ManForcedHalfSpace ctermbg=Cyan
syn match D5ManForcedHalfSpace " "

" Commands
" ========

" inspired by D5Man Legacy (v1) VIM plugin but in a very reduced fashion.

command D5U call D5ManDBUpdate()

function D5ManDBUpdate()
	let section = D5ManGetKey("section")
	let name    = D5ManGetKey("x-masysma-name")
	let name2   = substitute(name, "\"", "", "g")
	let url     = "http://127.0.0.1:7450/page/".name2."(".section.")"
	if exists("g:d5man_api_url")
		let url = g:d5man_api_url."/page/".name2."(".section.")"
	endif
	call system("wget --post-data= -O/dev/null ".shellescape(url))
endfunction

function D5ManGetKey(k)
	return substitute(getline(search("^".a:k.":\\s\\+.*")),
						\ "^".a:k.":\\s\\+", "", "")
endfunction
