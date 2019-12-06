#!/usr/bin/perl
# Ma_Sys.ma D5Man Export 2.0.0, Copyright (c) 2019 Ma_Sys.ma.
# For further info send an e-mail to Ma_Sys.ma@web.de.

use strict;
use warnings FATAL => 'all';
use autodie;

use File::stat; # switch stat from numeric to OO
require File::Basename;
require YAML::Tiny; # libyaml-tiny-perl
#use Data::Dumper 'Dumper'; # debug only

my @exportsections = qw(11 31 32 33 34 37 38 39);

if($#ARGV lt 1) {
	print("USAGE d5manexport DESTDIR ROOT... -- [PANDOCOPTIONS...]\n");
	exit(1);
}

my %exportmap;
undef @exportmap{@exportsections};

# -- read arguments --
my $destdir = $ARGV[0];
shift @ARGV;

my @roots;
my @pandocopts;
while(((scalar @ARGV) ne 0) and ($ARGV[0] ne "--")) {
	push @roots, $ARGV[0];
	shift @ARGV;
}
if((scalar @ARGV) ne 0) {
	shift @ARGV; # assert that it is --
	while(((scalar @ARGV) ne 0)) {
		push @pandocopts, $ARGV[0];
		shift @ARGV;
	}
}

mkdir($destdir) if(not -d $destdir);
open my $stream_sitemap, '>:encoding(UTF-8)', $destdir."/sitemap.tpl.xml";
print $stream_sitemap <<~"EOF";
	<?xml version="1.0" encoding="UTF-8"?>
	<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
	EOF

# -- process roots --
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
		close $fh;
		my $yaml = YAML::Tiny::Load(join "", @yaml_lines);

		my $mtime = stat($filepath)->mtime;
		my $section = $yaml->{section};
		my $namepart = $yaml->{"x-masysma-name"};
		$namepart =~ tr/\//_/;
		next if not exists $exportmap{$section};

		# -- establish output directory --
		my $secdestdir = $destdir."/".$section;
		mkdir($secdestdir) if(not -d $secdestdir);

		# -- prepare parameters --
		my @params = (
			"-s",
			"-t", "html4",
			"-f", "markdown+compact_definition_lists+".
						"tex_math_single_backslash",
			"-o", $secdestdir."/".$namepart.".xhtml",
		);
		push @params, @pandocopts;
		push @params, $filepath;

		# -- call export --
		system("pandoc", @params);

		# -- append to sitemap --
		my ($tS, $tM, $tH, $td, $tm, $tY) = gmtime($mtime);
		$tY += 1900;
		$tm += 1;
		my $date_fmt = sprintf("%04d-%02d-%02dT%02d:%02d:%02dZ",
						$tY, $tm, $td, $tH, $tM, $tS);
		my $web_priority = defined($yaml->{"x-masysma-web-priority"})?
				$yaml->{"x-masysma-web-priority"}: 0.4;
		my $web_changefreq =
				defined($yaml->{"x-masysma-web-changefreq"})?
				$yaml->{"x-masysma-web-changefreq"}: "monthly";
		print $stream_sitemap <<~"EOF";
			<url>
			<loc>&masysma_url_prefix;/$section/$namepart.xhtml</loc>
			<lastmod>$date_fmt</lastmod>
			<priority>$web_priority</priority>
			<changefreq>$web_changefreq</changefreq>
			</url>
			EOF
	}
}

print $stream_sitemap "</urlset>\n";
close $stream_sitemap;
