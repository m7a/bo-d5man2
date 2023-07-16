#!/usr/bin/perl
# Ma_Sys.ma D5Man Export 2.1.5, Copyright (c) 2019, 2022, 2023 Ma_Sys.ma.
# For further info send an e-mail to Ma_Sys.ma@web.de.

use strict;
use warnings FATAL => 'all';
use autodie;

use File::stat;                     # switch stat's array to key-value hash
require File::Basename;
require YAML::Tiny;                 # libyaml-tiny-perl
require Getopt::Std;
require File::Path;
require File::Copy;
require File::Copy::Recursive;      # libfile-copy-recursive-perl
require XML::RSS;                   # libxml-rss-perl
require DateTime::Format::Strptime; # libdatetime-format-strptime-perl

use Data::Dumper 'Dumper'; # debug only

# -- getopt conf --
$Getopt::Std::STANDARD_HELP_VERSION = 1;
$Getopt::Std::STANDARD_HELP_VERSION = 1; # avoid only used once warning

sub VERSION_MESSAGE {
	open my $s0fil, "<:encoding(UTF-8)", $0;
	while(<$s0fil>) {
		next if /^#!/;
		last unless /^# /;
		print substr $_, 2;
	}
	close $s0fil;
}

sub HELP_MESSAGE {
	print "\nUSAGE d5manexporthtml -o DESTDIR -i ROOT[,ROOT...] ".
			"[-s SECTION[,SECTION...]] [-u URLPREFIX] ".
			"[-n NEWSP] [-m PDF2SVG] [-- PANDOCOPTIONS...]\n";
}

# -- read arguments --
my %options;
Getopt::Std::getopts("o:i:s:u:n:m:", \%options);

if(not exists $options{o}) {
	print STDERR "Argument -o is required and needs to specify DESTDIR.\n";
	exit(1);
}
if(not exists $options{i}) {
	print STDERR "Argument -i is needed and needs to specify at ".
							"least one ROOT.\n";
	exit(1);
}

my $urlprefix      = $options{u} // "&masysma_url_prefix;";
my $exportsecstr   = $options{s} // "11,31,32,33,34,37,38,39";
my $pdf2svg        = $options{m} // "/usr/bin/pdf2svg";
my @exportsections = split /[, ]/, $exportsecstr;
my %exportmap;
undef @exportmap{@exportsections};

my @roots   = split /,/, $options{i};
my $destdir = $options{o};

my @pandocopts;
while(((scalar @ARGV) ne 0)) {
	push @pandocopts, $ARGV[0];
	shift @ARGV;
}

# -- create output dir + open sitemap file --
mkdir($destdir) if(not -d $destdir);
open my $stream_sitemap, '>:encoding(UTF-8)', $destdir."/sitemap.xml";
print $stream_sitemap <<~"EOF";
	<?xml version="1.0" encoding="UTF-8"?>
	<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
	EOF

# -- delete data from last export --
for my $section (@exportsections) {
	File::Path::remove_tree("$destdir/$section") if(-d "$destdir/$section");
}

sub create_attachment_htaccess {
	my $attachdest = shift;
	open my $hta, '>:encoding(UTF-8)', $attachdest."/.htaccess";
	print $hta <<~"EOF";
		Deny from all
		<Files *>
			Order deny,allow
			Allow from all
		</Files>
		EOF
	close $hta;
}

# -- process roots --
my %all_page_metadata;
my $look_for_newsp = (exists $options{n});
my $newsp_found = undef;

for my $root (@roots) {
	while(glob("'$root/??/*.md' '$root/?[po]-*/README.md'")) {
		my $filepath = $_;

		# -- read file metadata --
		# Parse the YAML because it is needed later anyways
		open my $fh, '<:encoding(UTF-8)', $filepath;
		my @yaml_lines;
		while(my $line = <$fh>) {
			last if(($line =~ /^---/) and ($#yaml_lines ge 0));
			push @yaml_lines, $line;
		}
		my $yaml = YAML::Tiny::Load(join "", @yaml_lines);

		my $mtime = stat($filepath)->mtime;
		my $section = $yaml->{section};
		my $namepart = $yaml->{"x-masysma-name"};
		my $namestr = "$namepart($section)";
		$namepart =~ tr/\//_/;
		my $secdestdir = $destdir."/".$section;

		# if its the news page, process it.
		if($look_for_newsp and $options{n} eq $namestr) {
			my @content_lines = <$fh>;
			$newsp_found = {
				section    => $section,
				namepart   => $namepart,
				secdestdir => $secdestdir,
				lines      => [@content_lines],
				title      => $yaml->{title},
				lang       => $yaml->{lang},
				copyright  => $yaml->{"x-masysma-copyright"},
				image      => $yaml->{"x-masysma-news-image"},
				url => "$urlprefix/$section/$namepart.xhtml",
			};
		}

		close $fh;

		next if not exists $exportmap{$section};

		# -- establish output directory --
		if(not -d $secdestdir) {
			mkdir($secdestdir);
			open my $hta, '>:encoding(UTF-8)', $secdestdir.
								"/.htaccess";
			print $hta <<~"EOF";
				Deny from all
				AddType application/xhtml+xml .xhtml
				AddType text/plain .md
				AddCharset UTF-8 .xhtml
				AddCharset UTF-8 .md
				<Files *.xhtml>
					Order deny,allow
					Allow from all
				</Files>
				<Files *.md>
					Order deny,allow
					Allow from all
				</Files>
				EOF
			close $hta;
		}

		# -- get modification time --
		my ($tS, $tM, $tH, $td, $tm, $tY) = gmtime($mtime);
		$tY += 1900;
		$tm += 1;
		my $date_meta = sprintf("%04d-%02d-%02d %02d:%02d:%02d",
						$tY, $tm, $td, $tH, $tM, $tS);
		my ($tSG, $tMG, $tHG, $tdG, $tmG, $tYG) = localtime($mtime);
		$tYG += 1900;
		$tmG += 1;
		my $date_ger = sprintf("%04d/%02d/%02d %02d:%02d:%02d",
					$tYG, $tmG, $tdG, $tHG, $tMG, $tSG);

		# -- prepare parameters --
		# find alternative highlight styles with
		# pandoc --list-highlight-styles
		my @params = (
			"-s",
			"-t", "html4",
			"--highlight-style=monochrome",
			"--default-image-extension=svg",
			"-V", "x-masysma-source=$namepart.md",
			"-V", "x-masysma-meta-revised=$date_meta",
			"-V", "x-masysma-revised-human=$date_ger",
			"-f", "markdown+compact_definition_lists+".
				"tex_math_single_backslash+link_attributes",
			"--shift-heading-level-by=1",
			"--mathml",
			"-o", "$secdestdir/$namepart.xhtml",
		);
		push @params, @pandocopts;
		push @params, $filepath;

		# -- call export --
		system("pandoc", @params);

		# -- copy source --
		File::Copy::copy($filepath, "$secdestdir/$namepart.md");

		# -- copy attachments --
		my $attdirnam = $namepart."_att";
		my $attachments = File::Basename::dirname($filepath).
								"/".$attdirnam;
		my $attachdest = $secdestdir."/".$attdirnam;
		if(-d $attachments) {
			File::Copy::Recursive::dircopy($attachments,
								$attachdest);
			# -- convert pdfs without associated svg --
			while(glob("'$attachdest/*.pdf'")) {
				my $attf = $_;
				my ($attnam, $_path, $_suffix) =
						File::Basename::fileparse($attf,
								qr".[^.]+$");
				my $attsvg = "$attachdest/$attnam.svg";
				next if (-f $attsvg);
				system($pdf2svg, $attf, $attsvg);
			}

			# -- add htaccess allow --
			create_attachment_htaccess($attachdest);
		}

		# -- append to sitemap --
		my $date_fmt = sprintf("%04d-%02d-%02dT%02d:%02d:%02dZ",
						$tY, $tm, $td, $tH, $tM, $tS);
		my $web_priority = defined($yaml->{"x-masysma-web-priority"})?
				$yaml->{"x-masysma-web-priority"}: 0.4;
		my $web_changefreq =
				defined($yaml->{"x-masysma-web-changefreq"})?
				$yaml->{"x-masysma-web-changefreq"}: "monthly";
		my $url_loc = "$urlprefix/$section/$namepart.xhtml";
		print $stream_sitemap <<~"EOF";
			<url>
				<loc>$url_loc</loc>
				<lastmod>$date_fmt</lastmod>
				<priority>$web_priority</priority>
				<changefreq>$web_changefreq</changefreq>
			</url>
			EOF

		# -- append to metadata --
		$all_page_metadata{$namestr} = {
			section => $section,
			author  => $yaml->{author},
			title   => $yaml->{title},
			lang    => $yaml->{lang},
			url_loc => $url_loc,
			skip    => defined($yaml->{"x-masysma-news-skip"}),
		};
	}
}

sub htmlspecialchars {
	my $str = shift;
	$str =~ s/&/&amp;/g; 
	$str =~ s/"/&quot;/g; 
	$str =~ s/'/&apos;/g; 
	$str =~ s/</&lt;/g; 
	$str =~ s/>/&gt;/g; 
	return $str;
}

# -- process news page --
if(defined($newsp_found)) {
	my $current_item = undef;
	my $current_content = "";

	my $rss = XML::RSS->new(version => "2.0");
	$rss->add_module(
		prefix => "atom",
		uri    => "http://www.w3.org/2005/Atom"
	);
	$rss->channel(
		title       => $newsp_found->{title},
		description => $newsp_found->{title},
		link        => $newsp_found->{url},
		language    => $newsp_found->{lang},
		copyright   => $newsp_found->{copyright},
		docts       => "https://www.rssboard.org/rss-specification",
		atom        => { link => { rel => "self", href => $urlprefix.
				"/".$newsp_found->{section}."/".
				$newsp_found->{namepart}."_att/rss.xml" } }
	);
	$rss->image(
		title => $newsp_found->{title},
		url   => $newsp_found->{image},
		link  => $newsp_found->{url},
	);

	for my $line (@{$newsp_found->{lines}}) {
		my $has_item = defined($current_item);
		my $proc_item = undef;

		if($line =~ /^[0-9]{2}\.[0-9]{2}\.[0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2}/) {
			$proc_item = $current_item;
			chomp($line);
			$current_item = $line;
		} elsif($line =~ /^:?\s+([^\s].*)$/ and
						defined($current_item)) {
			$current_content .= $1." ";
		} elsif($has_item) {
			$proc_item = $current_item;
			$current_item = undef;
		}

		if(defined($proc_item)) {
			my @mentioned_pages = $current_content =~
					/\[([a-z0-9\.\/_-]+\([0-9]+\))\]/g;
			for my $page (@mentioned_pages) {
				my $metadata = $all_page_metadata{$page};
				next if($metadata->{skip});

				# Now this is the entry to process.
				my @author_if_def = ();
				if(defined($metadata->{author})) {
					my $author = join(", ",
							$metadata->{author});
					$author =~ s/(^\["|"\]$)//g;
					@author_if_def = (author => $author);
				}
				# remove inner links as we link this via RSS
				$current_content =~ s/\[([a-z0-9\.\/_-]+\([0-9]+\))\]\([^\s]+\)/$1/g;
				my $pubdate = DateTime::Format::Strptime->new(
					pattern   => "%d.%m.%Y %H:%M:%S",
					time_zone => "local"
				)->parse_datetime($proc_item);
				$pubdate->set_time_zone("UTC");
				# Cannot directly pass the object. Although it
				# is supported, it produces the wrong date
				# format for FSS2.0. If this is fixed upstream,
				# it should be possible to delete this
				# statement.
				$pubdate = $pubdate->strftime(
						"%a, %d %b %Y %H:%M:%S %z");
				# XML::RSS already does htmlentities _once_, but
				# we actually need to do it _twice_ because the
				# CDATA content of the element is interpreted
				# as HTML and this makes some of our notation
				# in text content invalid. E.g. if an item
				# has description = "See <https://example.com>"
				# then it needs to encode to
				# "See &amp;lt;https://example.gom&amp;gt;"
				my $description =
					htmlspecialchars($current_content);
				$rss->add_item(
					title       => $metadata->{title},
					description => $description,
					link        => $metadata->{url_loc},
					guid        => $metadata->{url_loc},
					pubDate     => $pubdate,
				);
			}
			$current_content = "";
		}
	}

	my $target_dir = $newsp_found->{secdestdir}."/".
						$newsp_found->{namepart}."_att";
	if(not -d $target_dir) {
		mkdir($target_dir);
		create_attachment_htaccess($target_dir);
	}
	$rss->save($target_dir."/rss.xml");
}

print $stream_sitemap "</urlset>\n";
close $stream_sitemap;
