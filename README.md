---
section: 32
x-masysma-name: d5man2
title: Ma_Sys.ma D5Man 2
date: 2019/12/06 13:16:32
lang: en-US
author: ["Linux-Fan, Ma_Sys.ma (Ma_Sys.ma@web.de)"]
keywords: ["d5man", "d5man2", "d5manapi", "d5mantui", "ial"]
x-masysma-version: 2.0.0
x-masysma-repository: https://www.github.com/m7a/bo-d5man2
x-masysma-website: https://masysma.lima-city.de/32/d5man2.xhtml
x-masysma-owned: 1
x-masysma-copyright: |
  Copyright (c) 2019, 2020 Ma_Sys.ma.
  For further info send an e-mail to Ma_Sys.ma@web.de.
---
Overview
========

Ma_Sys.ma D5Man 2 is a set of programs and auxiliary resources intended to build
a locally run knowledge base. It consists of the following individual
components:

`d5manapi`
:   _Application Programming Interface_ providing access to page metadata by
    means of a REST (Representional State Transfer) interface.
    See `d5manapi` for details.
`d5mantui`
:   _Terminal User Interface_ as an interactive means to query for D5Man pages.
    See `d5mantui` for details.
d5manexport
:   Multiple programs to export a special instance of pandoc's Markdown by
    invoking the `pandoc` utility (not part of D5Man). Two export scripts are
    supplied: `d5manexportpdf` exports a single page to a printable PDF
    document; `d5manexporthtml` exports a whole directory structure of D5Man
    pages to a specified directory.
IAL (Information and Links)
:   The D5Man API allows for integration of pages which are not in D5Man's
    Pandoc Markdown format but e.g. HTML by specifying separate files with
    D5Man-style metadta for such existing files.
    See _Information and Links (IAL)_ for details.
Auxiliary Resources
:   The package provides a syntax file `markdown.md` as a replacement for VIM's
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

### Example for a Document-Root Structure: IAL

	/rr
	 |
	 +-- 21/
	 |    |
	 |    +-- ada_att/
	 |    |    |
	 |    |    +-- ...
	 |    |
	 |    +-- ant_att/
	 |    |    |
	 |    |    +-- ...
	 |    |
	 |    +-- ada.yml
	 |    |
	 |    +-- ant.yml
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
   (_Information and Links_).

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
As documentation is often available online, there is less and less need for
the functionality provided by _Information and Links_. The following lists
some alternative softwares to cover individual aspects of D5Man. There does not
seem to be a comprehensive substitute with all the benefits, though.

### Local Wikis

 * Dedicated wiki: [DokuWiki](https://www.dokuwiki.org/dokuwiki)
 * [Fossil SCM](https://www.fossil-scm.org/home/doc/trunk/www/index.wiki)
   integrates a Wiki and Issue Tracker storing all data in an SQLite database.
 * [EMACS Org-Mode](https://orgmode.org/)
 * Using VIM as a personal notekeeping application or Wiki:
    * with help files:
      <https://vim.fandom.com/wiki/Add_your_note_files_to_Vim_help>,
      <https://vim.fandom.com/wiki/Keep_a_to-do_memo_file_with_links_as_in_Vim_help>
    * with vimwiki: <https://github.com/vimwiki/vimwiki>,
      <https://github.com/lervag/wiki.vim>
    * <https://github.com/tomtom/vikibase_vim>

### Static website generation with Markdown

Close-to-comprehensive list: <https://www.staticgen.com/>, some candiates:
[Hugo](https://gohugo.io/), [Jekyll](https://jekyllrb.com/)

### Offline Documentation Management (IAL Alternative)

<https://zealdocs.org/>

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
    name with `/` replaced by `_` and an additional `.md` suffix.
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

Only the Erlang-based `d5manapi` requires external dependencies and needs to be
compiled, all other D5Man 2 components are scripts and run without compilation
or further processing. By providing `erlang.mk` along with `d5manapi`,
compilation should automatically download all dependencies if a working
Erlang OTP runtime can be found.

To compile the individual parts, it might be sufficient to call `make`
in directory `d5manapi`. If this succeeds, all components are already on disk.
To generate an installable Debian package, `ant` and the usual build tools for
Debian packages are required. One can then build the package by invoking
`ant package` in the repository's top-level directory.

`d5manapi`
==========

The D5Man API server loads metadata for all pages into RAM and provides a
REST API to query the respective metadata.

## Configuration

`erlang.mk` builds a script `d5manapi_release` to run the server which can be
invoked as follows:

	bin/d5manapi_release foreground [-config CONFIG]

Here, `CONFIG` refers to an optional configuration file. Default values for
the configuration can be found in `d5manapi/rel/sys.config` and are as follows:

~~~{.erlang}
[{d5manapi, [
	{ip, {127, 0, 0, 1}},
	{port, 7450},
	{redirect_url_prefix, "http://127.0.0.1:7450/rrman/"},
	{fs, #{
		rrman => "/data/main/300t399_man_rr",
		ial   => "/data/main/400t699_mdvl_rr/bo-d5man2/ial/home",
		local => "/data/main/400t699_mdvl_rr/br-ial-local"
	}},
	{db_roots, [
		"/data/main/300t399_man_rr",
		"/data/main/400t699_mdvl_rr"
	]}
]}].
~~~

The lines with `ip`, `port` and `redirect_url_prefix` configure the server's
address. For local usage, it is highly recommended to set `ip` to the defined
`127.0.0.1`.

The other parts of the configuration most likely require changes for local
use. They are dividied into `fs` and `db_roots` which can be described as
follows:

`fs`
:   Provides an association of server paths to local paths. This essentially
    makes the D5Man API server serve static files. For instance in this
    configuration, file `/data/main/300t399_man_rr/21/ada_att/rm-0-1.html` is
    available thorugh the server at
    `http://127.0.0.1:7450/rrman/21/ada_att/rm-0-1.html`.
`db_roots`
:   Declares a list of directories to consult for D5Man pages. They can be
    either in _Document-Root_ or _Program-Root_ organization. All the pages
    found below the respective roots will be available for querying.

Note that for Linux usage, script `d5manapi/aux/d5manapi` is provided. It
invokes the server automatically detecting the presence of a configuration file
in `$HOME/.mdvl/d5man/d5manapi.conf`. Additionally, a systemd unit
`d5manapi.service` is provided. It is intended to be installed as a
user-service. See `d5manapi.service` for details.

## Usage

Once configured, `d5manapi` can be started and awaits connections from other
D5Man components (i.e. `d5mantui` or IAL).

The API currently exposes a single endpoint called `query`. It can be invoked
as follows:

	curl http://127.0.0.1:7450/query/

Without any actual query string, this will return all elements in the database
up to the default limit of 100. To configure a different limit, use header
`x-masysma-limit` e.g. as follows:

	curl -H "x-masysma-limit: 4" http://127.0.0.1:7450/query/

This query returns four elements from the database. Set the limit to 0 to
return the entire database (can be large...)

To send a query string, use it as path:

	curl http://127.0.0.1:7450/query/31%20web

This sends query `31 web` to the server which returns all pages in section
`31` which match query string `web`.

Currently, the API always outputs XML. An example output from the API can look
as follows:

~~~{.xml}
<?xml version="1.0" encoding="UTF-8"?>
<d5man>
	<page>
		<meta>
			<kv k="file" v="/data/main/300t399_man_rr/21/erlang.yml"/>
			<kv k="section" v="21"/>
			<kv k="name" v="erlang/snmp_user_based_sm_mib:delete_user/1"/>
			<kv k="tags" v="erlang snmp_user_based_sm_mib:delete_user/1 snmp_user_based_sm_mib delete_user/1"/>
			<kv k="redirect" v="http://127.0.0.1:7450/rrman/21/erlang_att/lib/snmp-5.2.12/doc/html/snmp_user_based_sm_mib.html#delete_user-1"/>
		</meta>
	</page>
	<page>
		<meta>
			<kv k="file" v="/data/main/300t399_man_rr/21/erlang.yml"/>
			<kv k="section" v="21"/>
			<kv k="name" v="erlang/snmp_user_based_sm_mib:delete_user/1"/>
			<kv k="tags" v="erlang snmp_user_based_sm_mib:delete_user/1 snmp_user_based_sm_mib delete_user/1"/>
			<kv k="redirect" v="http://127.0.0.1:7450/rrman/21/erlang_att/lib/snmp-5.2.12/doc/html/snmp_user_based_sm_mib.html#delete_user-1"/>
		</meta>
	</page>
</d5man>
~~~

The format is a little “complicated” for being mostly compatible with D5Man
Legacy page files. It consists of a single `d5man` element which contains
separate `page` elements for each page. In case of this API, each `page`
contains exactly (and only) one `meta` element which in turn contains the
actual metadata in form of `kv` (key-value) elements. Metadata `file`, `section`
and `name` are expected to be always present. `tags` contains a space-separated
list of tags obtained from `keywords` declarations in the files. In case a
page is not expected to be opened directly, `redirect` indicates the page to
open instead.

The example XML from above shows metadata as can be generated by script
`ial/pgen/pages_erlang.sh` for the Erlang documentation.

`d5mantui`
==========

The D5Man Terminal User Interface (TUI) is a special-purpose client for the
D5Man API. It displays query results interactively in the terminal while
typing the query.

## Configuration

D5Man TUI can be configured by providing a suitable XML property file. In the
XML format, the default configuration looks as follows:

~~~{.xml}
<?xml version="1.0" encoding="UTF-8"?>
<properties>
	<entry key="d5man.ui.command.editor">vim</entry>
	<entry key="d5man.ui.command.browser">firefox</entry>
	<entry key="d5man.ui.newpage.root">/data/main/300t399_man_rr</entry>
	<entry key="d5man.api.url">http://127.0.0.1:7450/</entry>
</properties>
~~~

The syntax is a subset of Java's XML properties (initially, `d5mantui` was
intended to be a Java client).

For very simple installations (where no new pages are going to be created, e.g.
when using IAL only), the defaults may be sufficient. In other cases, the
`d5man.ui.newpage.root` needs to be changed to point to the Document-Root
to place newly created pages in. The other properties should be
self-explanatory.

To find the XML file, D5Man TUI looks in environment variable `$D5MAN_CONF_UI`
and if that is absent, attempts to load file
`$HOME/.mdvl/d5man/d5mantui_properties.xml`.

## Usage

The screen could e.g. look as follows:

	> erlang
	<o> 21 erlang/erl_syntax:receive_expr_action/1
	< > 21 erlang/snmpm:which_agents/0
	< > 21 erlang/snmpm_mpd:generate_msg/5
	< > 21 erlang/common_test:Module:suite/0
	< > 21 erlang/wxListCtrl:setItemData/3
	< > 21 erlang/sys:remove/2
	< > 21 erlang/snmpa_conf:append_target_params_config/2
	< > 21 erlang/leex:tokens/3
	< > 21 erlang/wxStyledTextCtrl:startStyling/3
	< > 21 erlang/snmpa_network_interface:get_log_type/1
	< > 21 erlang/wxLocale:getString/5
	< > 21 erlang/sys:replace_state/2
	< > 21 erlang/gl:clear/1
	< > 21 erlang/wxPopupTransientWindow:destroy/1
	< > 21 erlang/unicode:characters_to_nfkd_list/1
	< > 21 erlang/gl:map2d/10
	< > 21 erlang/gl:getProgramInfoLog/2
	< > 21 erlang/io:parse_erl_form/3
	< > 21 erlang/snmpa_error_io:config_err/2
	< > 21 erlang/wxStyledTextCtrl:wordPartLeft/1
	< > 21 erlang/erlang:fun_info/2
	< > 21 erlang/gl:getHandleARB/1
		2 New                                                           0 Exit

The first line is a prompt where the user can enter any query that will be sent
to D5Man API. The list below displays the search results and can be scrolled
with [UP] and [DOWN] arrows on the keyboard. Upon pressing [ENTER], the selected
page is opened.

All commandline arguments to `d5manqtui` are treated as an input for the
prompt. If the initial query (as given on the commandline) yields exactly one
result, the TUI will not be displayed and the respective page will be opened
directly.

Additionally, one can press [F2] to create a new page. To do this, the input
at the prompt needs to be in format `SECTION NAME` i.e. the new page's section
followed by its name. [F2] will then copy a predefined template to a new file
and open it in the configured editor. Note that this function only supports
Document-Root organization for creating new files.

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

Information and Links (IAL)
===========================

_Information and Links_ provides a system for storing and using (potentially
large) pieces of documentation offline. For this purpose, IAL 1.5 integrates
with D5Man 2 by providing a _web interface_ and a set of _scripts_ contained
in the following directory structure in the repository:

`ial/pgen` (scripts)
:   Provides scripts to prepare existing pieces of documentation for use with
    IAL.

`ial/home` (web interface)
:   Contains a “template” to use as a web interface to IAL.
    The implementation is mostly contained in JavaScript file `ial15.js` with
    the remainder of the files serving as a GUI skeleton.

Assumptions
:   By default, IAL assumes that all pages to be considered for IAL are in
    sections 21 (for automatically generated pages) and 22 (for hand-crafted
    pages) respectively.

## Scripts

The scripts provided as part of the repository require the `co-maartifact`
repository for [maartifact(11)](../11/maartifact.xhtml) to be present next to
`bo-d5man2`.

Invoking the individual `pages_...sh` scripts then creates directories with
files suitable for copying into an appropriate document-root D5Man 2 structure.
Each script may have additional requirements/features which are documented at
the beginning of its source code.

## Web Interface

Assumptions
:   The web interface assumes that all IAL pages contain a `x-masysma-redirect`
    field to identify the HTML page or website to open. This makes webbrowser
    integration easy at the expense of not allowing “regular” D5Man 2 pages to
    be visible in the web interface.

_TODO Provide an example screen and usage howto, explain how API serves the pages_
_TODO Provide a script to copy/update resource directories_
