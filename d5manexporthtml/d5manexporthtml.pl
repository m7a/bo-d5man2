#!/usr/bin/perl
# Ma_Sys.ma D5Man Export 2.0.0, Copyright (c) 2019 Ma_Sys.ma.
# For further info send an e-mail to Ma_Sys.ma@web.de.

use strict;
use warnings FATAL => 'all';
use autodie;

use File::stat; # switch stat's array to name-accessable structure.
require File::Basename;
require YAML::Tiny; # libyaml-tiny-perl
require Getopt::Std;
require File::Path;
require File::Copy;
require File::Copy::Recursive; # libfile-copy-recursive-perl
#use Data::Dumper 'Dumper'; # debug only

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
			"[-m PDF2SVG] [-- PANDOCOPTIONS...]\n";
}

# -- read arguments --
my %options;
Getopt::Std::getopts("o:i:s:u:m:", \%options);

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
		my @params = (
			"-s",
			"-t", "html4",
			"--default-image-extension=svg",
			"-V", "x-masysma-source=$namepart.md",
			"-V", "x-masysma-meta-revised=$date_meta",
			"-V", "x-masysma-revised-human=$date_ger",
			"-f", "markdown+compact_definition_lists+".
				"tex_math_single_backslash+link_attributes",
			"--base-header-level=2",
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
			open my $hta, '>:encoding(UTF-8)', $attachdest.
								"/.htaccess";
			print $hta <<~"EOF";
				Deny from all
				<Files *>
					Order deny,allow
					Allow from all
				</Files>
				EOF
			close $hta;
		}

		# -- append to sitemap --
		my $date_fmt = sprintf("%04d-%02d-%02dT%02d:%02d:%02dZ",
						$tY, $tm, $td, $tH, $tM, $tS);
		my $web_priority = defined($yaml->{"x-masysma-web-priority"})?
				$yaml->{"x-masysma-web-priority"}: 0.4;
		my $web_changefreq =
				defined($yaml->{"x-masysma-web-changefreq"})?
				$yaml->{"x-masysma-web-changefreq"}: "monthly";
		print $stream_sitemap <<~"EOF";
			<url>
			<loc>$urlprefix/$section/$namepart.xhtml</loc>
			<lastmod>$date_fmt</lastmod>
			<priority>$web_priority</priority>
			<changefreq>$web_changefreq</changefreq>
			</url>
			EOF
	}
}

print $stream_sitemap "</urlset>\n";
close $stream_sitemap;
