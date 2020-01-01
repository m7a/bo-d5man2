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
x-masysma-owned: 1
x-masysma-copyright: |
  Copyright (c) 2019, 2020 Ma_Sys.ma.
  For further info send an e-mail to Ma_Sys.ma@web.de.
---
WARNING: EXPERIMENTAL CODE
==========================

**THIS IS HIGHLY EXPERIMENTAL AND UNDER DEVELOPMENT**

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
:   _Terminal User Interface_ as an interactive means to query the D5Man API.
    Currently, it provides a keyword-based search (only).
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

_TODO ADD SOME EXAMPLES_

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

 1. Create an empty page by copying from a template file to a file in a
    section directory or repository's `README.md`
    _TODO PROVIDE SUCH TEMPLATE FILE_
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
   below the “roots”. This is the sole responsibility of the text editor.
   Additionally, pages exist as dedicated files allowing existing backup
   procedures to be effective for saving D5Man files as well.
 * _Partial Publishing_. It is possible to publish only a subset of the
   actually present documents as to distinguish between public and private
   information. Use of different sections for this purpose makes the distinction
   clear at all times.
 * _Balanced Markup_ Language. D5Man Legacy proposed an own syntax. While it
   is superior in certain aspects, it turned out to be too difficult to parse
   correctly. Thus D5Man 2 uses a thoughtfully crafted subset of Pandoc's
   Markdown which ensures compatibility with printed and web-based formats and
   provides reasonably well-readable and easily editable source files.

## Alternatives

_TODO PROVIDE A LIST OF ALTERNATIVE SOFTWARE WHICH SERVES SIMILAR PURPOSES_

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
`x-masysma-name` (required)
:   Determines the page's name. For newly created pages, it is recommended to
    chose names satisfying the regex `[a-z0-9_/]+`. Other names are
    supported, but may not contain any whitespace or other characters that
    are uncommon in file names processed by scripts (except for `/`).
    For _Document-Root_ organizazion, the file name should be the page's
    name with `/` replaced by `_` and an additional `.md` suffix.
`x-masysma-version`, `x-masysma-copyright` (optional)
:   Specifies a version and copyright for the document (and the program it is
    describing). Fromat and use of these fields are entirely up to the user.
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
`x-masysma-web-priority`, `x-masysma-web-changefreq` (optional)
:   Defines a priority (0.0--1.0) and a change frequency (monthly, weekly etc.)
    to be used in sitemaps generated during the XHTML export.
    Default is priority=0.4, changefreq=monthly.

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
a simple automatic conversion from PDF to SVG in order to avoid that redundant
vector images need to be stored in the attachments directory.

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
`    ` space-space-space-space on the second line onwards). Here are
examples for the respective list types.

~~~{.markdown}
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

_TODO ASTAT_

## Code

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
[Thomas Keklak](https://coderwall.com/p/07mtla/insert-non-breaking-space-in-vim),
a single non-breaking space can be entered in VIM by pressing
[CTRL]-[K] [SPACE] [SPACE]. Similarly, a forced half-space can be entered by
using the sequence [CTRL]-[V] [U] [2] [0] [2] [F].

_TODO ARROWS_

## Images

_TODO_

Compiling and Installing D5Man 2
================================

`d5manapi`
==========

`d5mantui`
==========

`d5manexportpdf`
================

`d5manexporthtml`
=================

	USAGE d5manexport -o DESTDIR -i ROOT[,ROOT...] -s SECTION[,SECTION...] -u URLPREFIX [-m PDF2SVG] [-- PANDOCOPTIONS...]

Information and Links (IAL)
===========================

TODO Needs to copy resource directories
