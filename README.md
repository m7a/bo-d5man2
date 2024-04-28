---
section: 32
x-masysma-name: d5man2
title: Ma_Sys.ma D5Man 2
date: 2019/12/06 13:16:32
lang: en-US
author: ["Linux-Fan, Ma_Sys.ma (Ma_Sys.ma@web.de)"]
keywords: ["d5man", "d5man2", "d5manapi", "d5mantui"]
x-masysma-version: 2.1.0
x-masysma-repository: https://www.github.com/m7a/bo-d5man2
x-masysma-website: https://masysma.net/32/d5man2.xhtml
x-masysma-owned: 1
x-masysma-copyright: (c) 2019--2024 Ma_Sys.ma <info@masysma.net>.
---
Overview
========

Ma_Sys.ma D5Man 2 is a set of programs and auxiliary resources intended to build
a locally run knowledge base. It consists of the following individual
components:

`d5mantui`
:   _Terminal User Interface_ as an interactive means to query for D5Man pages.
    See `d5mantui` for details.
d5manexport
:   Multiple programs to export a special instance of pandoc's Markdown by
    invoking the `pandoc` utility (not part of D5Man). Two export scripts are
    supplied: `d5manexportpdf` exports a single page to a printable PDF
    document; `d5manexporthtml` exports a whole directory structure of D5Man
    pages to a specified directory.
Auxiliary Resources
:   The package provides a syntax file `markdown.vim` as a replacement for VIM's
    default Markdown style. This style makes sections more visible and is
    loosely inspired by the style used in D5Man Legacy.

This version “D5Man 2” replaces D5Man Legacy which was an attempt to achieve
a similar outcome but in a much more sophisticated manner leading to excessive
complexity.

D5Man Structure and Concepts
============================

D5Man is centered around the concept of _D5Man pages_: A D5Man page is a text
file written in a subset of pandoc's Markdown together with a minimum amount of
metadata in YAML format and with optional attachment files. The details of the
format are described under _D5Man Format 2_.

## Filesystem Structure

Two ways of organizing D5Man pages on file systems are recommended:

 1. Document-Root: A directory structure which consists only of D5Man pages
    (or other data processable by the D5Man API Server). On the top-level there
    is one directory per _section_ and the respective sections contain
    one ore multiple D5Man documents each.
 2. Program-Root: A directory structure which consists of programs'
    repositories. Here, pages are represented by the files named `README.md`
    in the subdirectories of the root.

A third option is _detached_, that is a directory unknown to the D5Man programs
which contains one or more pages. Such files are not found by the D5Man API
Server but can still be converted to PDF.

### Example for a Document-Root Structure: Ma_Sys.ma Website

	/rr
	 |
	 +-- 31/
	 |    |
	 |    +-- web_about_att/
	 |    |    |
	 |    |    +-- ...
	 |    |
	 |    +-- web_news_att/
	 |    |    |
	 |    |    +-- ...
	 |    |
	 |    +-- ...
	 |    |
	 |    +-- web_about.md
	 |    |
	 |    +-- web_news.md
	 |    |
	 |    +-- ...
	 |
	 +-- ...

### Example for a Program-Root Structure: Ma_Sys.ma Repositories

	/rr
	 |
	 +-- bo-adler32/
	 |    |
	 |    +-- README.md
	 |    |
	 |    +-- ...
	 |
	 +-- bo-big/
	 |    |
	 |    +-- big4_att/
	 |    |    |
	 |    |    +-- screenshot3.png
	 |    |
	 |    +-- README.md
	 |    |
	 |    +-- ...
	 |
	 +-- ...

## Concept

D5Man is designed as a sort of personal Wiki that can have a published part. To
achieve the separation between published and private parts, different _sections_
are used. Each page is assigned a section, which by convention is a number of
two digits (i.e. ranges from 10 to 99).

 * Pages are created, edited and viewed in a text editor which is by default
   configured to be [VIM](https://www.vim.org/).
 * If ready for the Internet, pages are exported to XHTML and can then be
   uploaded to any webspace. Alternatively, if pages are to be printed, they
   can be exported to PDF. All export uses [Pandoc](https://www.pandoc.org/)
   internally.
 * The D5Man User Interface runs in a terminal emulator. This way, all
   interactive parts are available from the commandline. Additionally, for
   “browsing” large pieces of information (like e.g. an API documentation),
   a web-based interface is desirable. Thus there is also a read-only
   web-based interface for exactly that purpose called IAL
   (_Information and Links_) as a separate program, see [ial(32)](ial.xhtml).

A typical workflow for creating a page is as follows:

 1. Create an empty page by copying from a template file (`d5man2.md`) to a
    file in a section directory or repository's `README.md`.
 2. Populate the file with information.
 3. Optionally: Export the file to target format of choice and print it or
    upload it to the Internet.

To edit or recall a page, enter a prefix of the page's name in `d5mantui` and
once it appears in the search results, open it by pressing ENTER.

## Benefits

Compared to other personal Wiki approaches, D5Man provides the following
set of advantages:

 * _Terminal-only workflow_ possible. This allows for good integration into
   an environment where most applications in use are also running in terminals.
 * Full control over _individual files_: D5Man does not write to the documents
   below the “roots” except for creating new pages. Apart from that, writing is
   the sole responsibility of the text editor. Additionally, pages exist as
   dedicated files allowing existing backup procedures to be effective for
   saving D5Man files as well.
 * _Partial Publishing_. It is possible to publish only a subset of the
   actually present documents as to distinguish between public and private
   information. Use of different sections for this purpose makes the distinction
   clear at all times.
 * _Balanced Markup_ Language. D5Man Legacy proposed an own syntax. While it
   is superior in certain aspects, it turned out to be too difficult to parse
   correctly. Thus D5Man 2 uses a thoughtfully crafted subset of Pandoc's
   Markdown which ensures compatibility with printed and web-based formats and
   provides reasonably well-readable and easily editable source files.
 * _Minimality_. After a failed attempt to develop a “large” system for the
   purpose, D5Man 2 stays minimal. At the core (Perl and Erlang parts), its
   source code is less than 1000 lines of code!

## Alternatives

There are countless approaches to do _static blogs_ or _personal wikis_.

### Static website generation with Markdown

Close-to-comprehensive list: <https://www.staticgen.com/>, some candiates:
[Hugo](https://gohugo.io/), [Jekyll](https://jekyllrb.com/)

### Local Wikis

 * Dedicated wiki: [DokuWiki](https://www.dokuwiki.org/dokuwiki)
 * [Fossil SCM](https://www.fossil-scm.org/home/doc/trunk/www/index.wiki)
   integrates a Wiki and Issue Tracker storing all data in an SQLite database.
 * [EMACS Org-Mode](https://orgmode.org/)

### Using VIM as a personal notekeeping application or Wiki

 * with help files:
   <https://vim.fandom.com/wiki/Add_your_note_files_to_Vim_help>,
   <https://vim.fandom.com/wiki/Keep_a_to-do_memo_file_with_links_as_in_Vim_help>
 * with vimwiki: <https://github.com/vimwiki/vimwiki>,
   <https://github.com/lervag/wiki.vim>
 * <https://github.com/tomtom/vikibase_vim>

D5Man Format 2
==============

If you are interested in the old D5Man Legacy format, see
[d5man/legacy(32)](d5man_legacy.xhtml). Here, a selected subset of the elements
from the rich syntax described in the Pandoc documentation is presented in order
to obtain a sensible subset. Of course, there is no technical restriction for
sticking to this subset.

D5Man's text format is expected to always be read and written in UTF-8 encoding.

## Metadata

A D5Man 2 Document begins with a header of following form:

~~~{.yaml}
---
section: 32
x-masysma-name: d5man2
title: Ma_Sys.ma D5Man 2
lang: en-US
author: ["Linux-Fan, Ma_Sys.ma (Ma_Sys.ma@web.de)"]
keywords: ["d5man", "d5man2", "d5manapi", "d5mantui", "ial"]
---
~~~

This header which follows YAML syntax is called the _metadata_ in D5Man. It
is a set of key-value assignments of form `key: value`. All fields which do
not have any special meaning for Pandoc are prefixed `x-masysma-` as to indicate
that they are additional fields used for D5Man. The use of the fields is as
follows:

`section` (required)
:   Defines the _section_ this page is part of. It is not really used in its
    Pandoc meaning (which would be the section for an actual manpage exported
    from the document), but the concept of D5Man manpages and actual manpages
    is similar to some extent (both provide textual information).
`title` (required)
:   Defines a document title (in legacy D5Man called `description`) which
    is a single large heading to go above the document.
`lang` (optional)
:   Gives the language in which the document (and/or its metadata) are written.
`author` (optional)
:   Gives a list of authors. Syntax `["Linux-Fan, Ma_Sys.ma..."]` creates a
    YAML list with just one element in the example above.
`keywords` (required)
:   A list of keywords (also in YAML syntax) to assign to the page. Note that
    D5Man API search querys consider only `x-masysma-name`, `section` and
    `keywords` and matches case-sensitively against prefixes. It is thus
    often useful to provide sensible subsets of the page's name in the
    `keywords` section. In legacy D5Man, this was called `tags`.
`date` (optional)
:   Specifies the date of document creation in `YYYY/MM/DD HH:ii:ss` format.
`toc` (optional)
:   Controls the generation of a table of contents for PDF exports
    (processed by pandoc only).
`x-masysma-name` (required)
:   Determines the page's name. For newly created pages, it is recommended to
    chose names satisfying the regex `[a-z0-9_/]+`. Other names are
    supported, but may not contain any whitespace or other characters that
    are uncommon in file names processed by scripts (except for `/`).
    For _Document-Root_ organizazion, the file name should be the page's
    name with `/` replaced by `_` and an additional `.md` suffix. For tasks,
    suffix `.hot` should be used.
`x-masysma-task-priority` (required and only allowed for tasks)
:   Specifies the priority of this task (cf. section _D5Man TUI Task
    Management_). Allowed values are the following:
    red, green, black, white, yellow, purple, delayed, considered.
`x-masysma-task-type` (required and only allowed for tasks)
:   Specifies the type fo this task (cf. section _D5Man TUI Task Management_).
    Allowed values are the following: long, short, subtask, periodic.
`x-masysma-version`, `x-masysma-copyright` (optional)
:   Specifies a version and copyright for the document (and the program it is
    describing). Format and use of these fields are entirely up to the user.
`x-masysma-repository` (optional)
:   Provides a link to the source code repository associated with the document
    and/or the software it describes.
`x-masysma-website` (optional)
:   Provides a link to the respective page on the (Ma_Sys.ma) Website. This
    allows e.g. Github users to find the Website which provides a
    correctly exported (i.e. readable) version of the distorted view that
    Github creates out of D5Man's Markdown files.
`x-masysma-owned` (optional)
:   If present, this enables the inclusion of Ma_Sys.ma Logo and Icons in
    exported PDF files. Of course, the logos can also be replaced by different
    ones for local usage. Or one can leave out this key to avoid the
    use of logos in the export results altogether.
`x-masysma-redirect` (optional)
:   This field either gives an absolute URL (`https://...`) or a file name.
    In case a file name is given, the given file (relative to the attachment
    directory) is opened instead of opening the page when running from
    D5Man TUI. All pages available through IAL need to supply this field.
`x-masysma-download` (optional)
:   Specify an URL for downloading a file (used for Website generation).
`x-masysma-web-priority`, `x-masysma-web-changefreq` (optional)
:   Defines a priority (0.0--1.0) and a change frequency (monthly, weekly etc.)
    to be used in sitemaps generated during the XHTML export.
    Default is priority=0.4, changefreq=monthly.
`x-masysma-expires` (optional)
:   Expiry date. Same format as `date`. The meaning of this field is up to
    the user's interpretation.

### Section Structure

The section structure used by the Ma_Sys.ma is as given in the following table.

Sec  Short Description
---  --------------------------------------------------------------------------
11   Documentation in the style of a classical man-page.
21   IAL as generated from documentation
22   IAL hand-added files
23   IAL internal
31   Website pages providing general website content (navigation, license, ...).
32   Documentation for current Ma_Sys.ma developments (programs, scripts, etc.)
33   Legacy (_currently unused_)
34   Creative section with Mods and Stories
35   not public: UNI notes
37   Blog, Knowledge Base, self-contained pages, other public notes
42   not public: user notes
43   not public: tasks

## Attachments

By convention, images included in the document are stored in a directory
called the same as the page's name with (`/` replaced by `_`) and a suffix
`_att` (instead of `.md`).

For instance, this `README.md` has name `d5man2` thus the attachments would
be stored in a directory `d5man2_att` next to the file. For page `d5man/legacy`,
attachments go to `d5man_legacy_att` etc.

Additionally, images which are supplied in vector formats (SVG or PDF) are
included by their file name _without extension_. This allows the LaTeX
export to use a PDF file and the XHTML export to use a SVG file without
changing the source file. Finally, D5Man's XHTML export also instantiates
a simple automatic conversion from PDF to SVG in order to avoid storing
redundant vector graphics in the attachments directory.

Unlike legacy D5Man, an explicit list of all files attached is no longer needed
to be declared in the documents themselves.

## Top-Level Structure

Documents consist of the leading metadata block (see _Metadata_) followed by
a D5Man document which consists of headings, lists, tables, code and paragraphs.

## Headings

D5Man proposes three levels of headings.
The top-level headings are underlined by equals signs.
The second-level headings are prefixed by `## ` (hash-hash-space).
The third-level headings are prefixed by `### ` (hash-hash-hash-space).
The following code shows all the heading styles.

~~~{.markdown}
Top-Level Heading
=================

Top-Level (e.g. introductory) content.

## Second-Level Heading

Second-Level Content

### Third-Level Heading aka. List Title

Inner Content / End of example.
~~~

## Lists

D5Man has numbered, unnumbered and definition lists. Legacy D5Man also
proposed _pro and contra_-style lists which are as of now not retained in
D5Man 2. Unnumberd list items are prefixed by an asterisk (`*`),
numbered lists are prefixed by the item's number followed by a dot
(`1.`, `2.`, etc.) and description lists' contents are prefixed by a
`:` at the beginning of the first line of the description list's content.
Note that for description lists, the offset from the left has to be exactly
four characters wide (`:   ` / colon-space-space-space on the first line;
`    ` / space-space-space-space on the second line onwards). Here are
examples for the respective list types.

~~~
Description List
:   This is the term being described.
    This is the second line of the term being described.
Second Description List Item
:   Another item to be described.

 1. First Item of a numbered list:
    This item has an additional line in source code.
 2. Second
 3. Third
 4. Fourth

 * Item 1 of the unnumberd list has
   two lines in source code.
 * Item 2 has a single line.
    * Nested Item a
    * Nested Item b
 * Item 3
~~~

## Tables

Two distinct notations exist for tables: Tables with headings and without
headings. All tables start from the first character in the line and leave two
spaces between columns.

For tables with headings, there is a single dashed line below the individual
headings. Example:

~~~{.markdown}
Caption 1   Caption 2
----------  ------------
Inner Cell  Inner Cell 2
Other Cell  Last Cell
~~~

For tables without headings, the same dashed lines are created and put above
and below the respective table. Leaving out the captions, the table from before
becomes this:

~~~{.markdown}
----------  ------------
Inner Cell  Inner Cell 2
Other Cell  Last Cell
----------  ------------
~~~

## Code

Top-level code can be declared by either indenting the code with at least a
single tab character or by enclosing it in lines with three tilde characters
(`~~~`).

Example for tilde-based code section (the source code uses indentation to
make this appear as code in the output document):

	~~~
	code content
	~~~

The tilde-based notation allows for a programming language to be declared by
replacing the first `~~~` with `~~~{.language}` where `language` is replaced by
a programming language name as known to `pandoc`. Examples include `c`,
`markdown` and `java`.

Alternatively, here is the indented variant (the source code uses tilde
symbols to make this appear as code in the output document):

~~~
	code content
~~~

## Paragraphs and Inline Formatting

Paragraphs are just regular text separated by two newlines. Throughout the
document's text, it is possible to use _inline formatting_ to place emphasis,
links etc. It is described in the following.

Code
:   By using backtick-quotation, inline code can be expressed
    (`` ` ``code`` ` `` displays `code`). Escaping backticks inline requires
    them to be sourrounded by more backticks and space. See
    [stackoverflow.com/82718](https://meta.stackexchange.com/questions/82718/)
    for details.
Emphasis
:   Like the legacy D5Man format, Markdown supports emphasis by surrounding the
    text to be emphasized with underscores e.g. `_emphasized_` yields
    _emphasized_.
Superset and subset
:   Putting something in an index works by adding tilde symbols (`~`) around
    the part to be lowered, e.g. `H~2~O` for H~2~O. Elevating parts of a
    word is possible by surrounding it with hat symbols (`^`) e.g.
    `10^2^` for 10^2^
Links
:   Links to URLs or other pages are of format `[shortcut name](URL)`
    e.g. `[Example Page](http://www.example.com/)` gives
    [Example Page](http://www.example.com/).
    If a link is given by URL only, it is given in angled-brackets like this:
    `<http://www.example.com>` gives <http://www.example.com>.
    To link to another D5Man page, its XHTML name needs to be given:
    `[d5man/legacy(32)](d5man_legacy.xhtml)` gives
    [d5man/legacy(32)](d5man_legacy.xhtml). By convention, the link to another
    page is labelled by that page's name followed by its section in parentheses.
    To link to pages in other sections, one needs to prefix `../SECTION` to
    the link's target due to the D5Man directory structure being organized in
    sections (even if it was originally a Program-Root structure, D5Man export
    always generates files as if they were part of a Document-Root structure).
    Note that unlike in legacy D5Man, links are expected to only work for the
    XHTML export. Navigating the hypertext directly inside the editor is no
    longer a supported use case.
Math
:   Inline Math is only supported for the PDF exports and can be expressed by
    LaTeX' single-dollar notation, e.g. `$\binom{1}{1}$` becomes
    $\binom{1}{1}$.

For qotation and symbols, legacy D5Man used some automatic replacement
logic. With the new version, this feature is no longer available, thus the use
of UTF-8 symbols is suggested. On some Linux systems, quotation is easily
available by [ALTGR]-[V] (`„`), [ALTGR]-[B] (`“`) and [ALTGR]-[N] (`”`).

Forced spaces (aka. non-breaking spaces) can be inserted by using the respective
unicode symbols. As described by
[Thomas Peklak](https://coderwall.com/p/07mtla/insert-non-breaking-space-in-vim),
a single non-breaking space can be entered in VIM by pressing
[CTRL]-[K] [SPACE] [SPACE]. Similarly, a forced half-space can be entered by
using the sequence [CTRL]-[V] [U] [2] [0] [2] [F].

Arrows are best inserted by using their UTF-8 symbols. The paragraph below
shows a few examples, see <http://xahlee.info/comp/unicode_arrows.html> for
a more comprehensive treatise.

Arrows: ← → ↑ ↓ ⇐ ⇒ ⇔

## Images

The general syntax for images is `![CAPTION](FILE)`. By convention, `FILE` is
given relative to the page's file and if it is associated directly to the page,
then it is placed in a directory with the page's name concatenated with a
trailing `_att`.

For instance, an attachment `test.png` for this very page would be loaded by
specifying `![Test](d5man2_att/test.png)`.

Note that for `.svg` and `.pdf` files the extension of the image file name is
normally not given in order to allow an automatic detection by pandoc/LaTeX to
take place.

Compiling and Installing D5Man 2
================================

D5Man 2 requires an Erlang OTP runtime and a suitable Perl interpreter as well
as a selection of Perl modules. A declaration of all dependencies for an
installation on a Debian stable system can be found in file `build.xml`.

Only the Erlang-based `d5mantui` requires external dependencies and needs to be
compiled, all other D5Man 2 components are scripts and run without compilation
or further processing. As a build dependency, `mdvl-cecho` (or alternatively
`cecho` as commented-out in `rebar.config`) is required. You can get
the build instructions for package `mdvl-cecho` here:
<https://github.com/m7a/lp-cecho>.

Prior to attempting to run D5Man TUI, adjust its config file at
`d5mantui4/config/sys.config` to refer to your local paths!

To compile the individual parts, it might be sufficient to call `make`
in directory `d5mantui4`. After this has succeeded, all components are on disk.
To generate an installable Debian package, `ant` and the usual build tools for
Debian packages are required. One can then build the package by invoking
`ant package` in the repository's top-level directory.

`d5mantui`
==========

The D5Man Terminal User Interface (TUI) displays query results interactively in
the terminal while typing the query. Additionally, it contains functions to
build a basic _Task Management_ system on top of D5Man.

## Synopsis

	d5mantui [query]

## Configuration

D5Man TUI can be configured through the `sys.config` file. The default
configuration looks as follows:

~~~{.erlang}
[{d5mantui4, [
	{db_roots,       ["/data/main/119_man_rr", "/data/main/120_mdvl_rr"]},
	{command_editor, ["/usr/bin/vim"]},
	{newpage_root,   "/data/main/119_man_rr"}
]},
{kernel, [{error_logger, {file, "/tmp/d5mantui4.log"}}]}
].
~~~

_Users must change this config prior to using D5man TUI_!

The meaning of the properties is as follows:

`db_roots`
:   Declares a list of directories to consult for D5Man pages. They can be
    either in _Document-Root_ or _Program-Root_ organization. All the pages
    found below the respective roots will be available for querying.
`newpage_root`
:   Specifies the directory in which new pages are created.
`command_editor`
:   Configures the Editor to use. It is recommended to use a terminal-based
    editor which creates a new instance upon invocation of this command.
`error_logger` file
:   Configures a log to write errors to. In event of a D5Man TUI crash, please
    delete this file, reproduce the issue and then send the contents of this
    log file to the Ma_Sys.ma for analysis.

## Usage

The screen could e.g. look as follows:

~~~
─────────────────────────────[ Ma_Sys.ma D5Man Terminal User Interface 4.0.0 ]──

 [                                                                            ]

─────────────────────────────────────────────────────────────[ Query Results ]──

   11 adler32         31 web/news                32 kbdcheck
   11 bin2bmp         31 web/programs            32 lz4_ada
   11 ma_capsblinker  32 big4                    32 ma_inventory
   11 ma_open_cl_info 32 blake3_ada              32 masysmaci/build
   11 ma_sitecopy     32 bruteforce3             32 masysmaci/main
   11 maartifact      32 conf-cli                32 masysmaci/pkgsync
   11 maerct          32 conf-gui                32 matrix_screensaver
   11 mahalt          32 d5man/legacy            32 maxbupst
   11 maloadmon       32 decode_girocode         32 megasync
   11 syssheet        32 gamuhr                  32 pressed_keys
   31 keysigning      32 i3bar                   32 progress
   31 web/about       32 ial                     32 scanning
   31 web/creative    32 image_viewer            32 screenindex
   31 web/gpl         32 internet-enable-disable 32 shellscripts
   31 web/knowledge   32 java-nostalgic-tools    32 ssd-optimization
   31 web/main        32 jmbb                    32 tar_ada

 1       2New    3       4All    5       6Docs   7Tasks  8-DLY   9-Sub   0Quit 
~~~

The first line is a prompt where the user can enter any query that will be used
to find pages. The list below displays the search results and can be scrolled
with [UP] and [DOWN] arrows on the keyboard. Upon pressing [ENTER], the selected
page is opened.

All commandline arguments to `d5manqtui` are treated as an input for the
prompt. If the initial query (as given on the commandline) yields exactly one
result, the TUI will not be displayed and the respective page will be opened
directly.

Function keys can be used as described in the following subsections.

### [F2] -- New Page/Task

One can press [F2] to create a new page. To do this, the input at the prompt
needs to be in format `SECTION NAME` i.e. the new page's section followed by its
name. D5Man TUI then clears the prompt.

In case of a regular page, it directly asks for a space-separated list of
tags to assign to the page. Common tags are displayed in the _Query Results_
while this mode is active. After pressing [ENTER], a predefined template is
copied to a new file and opened it in the configured editor.

If SECTION is set to 43 then metadata about the task is also queried from the
prompt (press [ENTER] to continue) and afterwards a task rather than a page is
be created.

This function only supports Document-Root organization for creating new files.

### [F4] -- All

Displays all results for the given query (default). Queries will return tasks
as well as documents depending on which matches the input string.

### [F6] -- Docs

Hides tasks from the result list.

## [F7], [F8], [F9] -- D5Man TUI Task Management

Since package version 1.0.54, D5Man provides some basic means to manage
“tasks”, i.e. TODO lists or issues or the like. The idea behind this scheme is
to leverage the Markdown format and D5Man's querying capabilities for management
of tasks. Two distinct dimensions are considered to organize tasks:

 1. The _task type_ specifies if this task is long, short, periodic or a
    subtask. The order of display is: periodic, then long, then short.
 2. The _task priority_ specifies how important a task is by assinging colors
    (purple, red, yellow, green, black, white). The use of the colors is up to
    the user. Instead of a color a task can also be in state _considered_ or
    _delayed_: Considered means that it should be kept in mind but is not
    assigned any priority (think: very low priority) and _delayed_ means that it
    may be hidden because it is not expected to be worked on any time soon
    (e.g. not in the current week).

D5Man assings the file extension `.hot` to task files to distinguish them from
documents and all tasks are placed and expected to be in section 43. D5Man
automatically finds tasks in Document-Root structures' section 43. Also, it
scans the directories in Program-Root structures for `TODO.hot` files and adds
them to the tasks to consider.

The `x-masysma-name` field is expected to be set to a short identifier (e.g.
code and number or similar) whereas the `title` is expected to summarize the
matter of the task.

### [F8] -- DLY

By default, tasks of priority _delayed_ are displayed. [F8] is a tristate toggle
that displays the action that is going to happen when pressing it:

 1. `8-DLY` -- First press [F8] to hide (-) the delayed tasks from the view
    and display only non-delayed tasks.
 2. `8=DLY` -- Press [F8] again to display only (=) delayed tasks
 3. `8+DLY` -- Press [F8] again to add non-delayed tasks (+) to the view again
    (returns to the initial state).

### [F9] -- Subtasks

By default, tasks of type _subtask_  are displayed. [F9] is a tristate toggle
that displays the action that is going to happen when pressing it:

 1. `9-Sub` -- First press [F9] to hide (-) the subtasks from the view and
    display only long/short/periodic ones.
 2. `9=Sub` -- Press [F9] again to display only (=) subtasks.
 3. `9+Sub` -- Press [F9] again to add non-subtasks (+) to the view again
    (returns to the initial state).

## Change History

Before the current revision (D5Man TUI 4), there was an implementation
consisting of a daemon (`d5manapi`) and a client (`d5mantui` perl script / v.3).
This had some advantages but was mostly much more complex than the current
implementation.

The new D5Man TUI 4 is close to a rewrite of the old functionality and thus
doesn't run fully stable yet. See `xdev/d5mantui3.pl` and `xdev/d5manapi` for
the old variants of these components.

`d5manexportpdf`
================

## Name

`d5manexportpdf` -- Script to export D5Man 2 Pandoc Markdown to PDF

## Synopsis

	d5manexportpdf INPUT.md

## Description

This invokes `pandoc` on the provided filename `INPUT.md` and writes the export
result to `INPUT.md.pdf` (i.e. adds extension `.pdf` to the input file name).
Note that due to hardcoded paths, this script only works if D5Man 2 is installed
(e.g. as a Debian package).

The script deliberately contains almost no logic at all.
This allows it to be ported to other scripting languages like Windows Batch.
Additionally, a “regular” pandoc invocation can serve as a fallback if D5Man 2
is not available.

## Example

	d5manexportpdf README.md

This should produce a nicely readable PDF for any instructions supplied as
part of Ma_Sys.ma repositories.

## Troubleshooting export issues

If the export fails during the invocation of `pdflatex`, it will most likely
generate a meaningless error message. Here is a regex for finding unicode
chars which might not be supported:

	/[^\x00-\x7F]

Source: <https://stackoverflow.com/questions/16987362/how-to-get-vim-to-highlight-non-ascii-characters>

`d5manexporthtml`
=================

## Name

`d5manexporthtml` -- Export D5Man 2 roots to multiple XHTML pages

## Synopsis

	d5manexporthtml -o DESTDIR -i ROOT[,ROOT...] [-s SECTION[,SECTION...]] [-m PDF2SVG] [-u URLPREFIX] [-- PANDOCOPTIONS...]

## Description

D5Man 2's HTML export exports a selection of sections from (optionally multiple)
root directories to a given output directory structure. The output structure
resembles a Document-Root structure independently of whether the given `ROOT`
directories are Document-Root or Program-Root organized.

In addition to the exported XHTML pages, a `sitemap.xml` and `.htaccess` files
are generated to allow hosting the result structure online. PDF attachments are
automatically converted to SVG.

## Options

`-o DESTDIR`
:   Configures the output directory to be `DESTDIR`
`-i ROOT[,ROOT...]`
:   Configures a comma-separated list of input directories.
    (As a result, it is currently impossible to process directories which
    contain comma as part of their name)
`-s SECTION[,SECTION...]`
:   Specifies a list of sections to export.
    If this is not given, the default value of 11,31,32,33,34,37,38,39 will
    be used.
`-m PDF2SVG`
:   Specifies the path to a `pdf2svg` executable. On Debian systems, package
    `pdf2svg` can be installed and then the default `/usr/bin/pdf2svg` will
    be sufficient. In other cases, it might be necessary to create an auxiliary
    script that invokes Inkscape or another tool capable of converting PDF to
    SVG. The `pdf2svg` is expected to take the input PDF file as its first
    parameter and the output SVG file as its second parameter.
`-u URLPREFIX`
:   Defines a prefix to be used for sitemap generation. By default, it is set
    to `&masysma_url_prefix;` which will most likely _not work_. In case the
    generated sitemap is of interest, this parameter needs to be given and
    have an URL value. The Ma_Sys.ma Website uses
    `-u https://masysma.lima-city.de`, for instance.
`-- PANDOCOPTIONS...`
:   After the double dashes, an arbitrary number of pandoc options can be
    given which are passed directly to the `pandoc` command. Most users will
    want to specify a `--template=...` here in order to obtain a nicely
    formatted page up to their liking.

## Ma_Sys.ma Variables

Using this script, the invocation of `pandoc` is passed the following additional
variables:

`x-masysma-source`
:   Set to the Markdown source code file name for the current page.
`x-masysma-meta-revised`
:   Set to the pages last modification in UTC (`YYYY-mm-dd HH:ii:ss`)
`x-masysma-revised-human`
:   Set to the pages last modification in local timezone (`YYYY/mm/dd HH:ii:ss`)

## Examples

As an example, consider downloading some of the Ma_Sys.ma Repositories into
a common directory tree to obtain a structure as shown above in section
_Example for a Program-Root Structure: Ma_Sys.ma Repositories_.

Then, you could create an XHTML export of their contents as follows:

	$ mkdir /tmp/test
	$ d5manexporthtml -o /tmp/test -i /rr

Below the output directory `/tmp/test`, this will create a document-root
structure of output files like this:

	/tmp/test
	 |
	 +-- 11/
	 |    |
	 |    +-- maloadmon_att/
	 |    |    |
	 |    |    +-- ...
	 |    |
	 |    +-- ...
	 |
	 +-- 32/
	 |    |
	 |    +-- d5man2.md
	 |    |
	 |    +-- d5man2.xhtml
	 |    |
	 |    +-- ...
	 |
	 +-- ...

Opening `d5man2.xhtml` one can see the XHTML representation of this very page,
it might look as follows:

![Excerpt from exporting this very page to XHTML (beginning of the page
shown)](d5man2_att/exportpreview)

Without further options, exporting uses the template supplied with pandoc.
If you want to use this for your own purposes, it makes sense to derive an own
template for customization.
