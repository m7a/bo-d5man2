#!/usr/bin/perl
# Ma_Sys.ma D5Man Terminal UI 3.1.3, Copyright (c) 2020, 2022 Ma_Sys.ma.
# For further info send an e-mail to Ma_Sys.ma@web.de.

use strict;
use warnings FATAL => 'all';
use autodie;
use threads;

require REST::Client;           # librest-client-perl
require URI::Encode;            # liburi-encode-perl
require XML::DOM;               # libxml-dom-perl
require Curses::UI;             # libcurses-ui-perl
require Curses::UI::TextEditor; # (in same package)
require DateTime;               # libdatetime-perl
require File::HomeDir;          # libfile-homedir-perl
require Thread::Queue;          # (perl-modules-5.24)

#use Data::Dumper; # DEBUG ONLY

#---------------------------------------------------------------------[ Conf ]--
my $haveenv = defined($ENV{D5MAN_CONF_UI});
my $homef = File::HomeDir::home()."/.mdvl/d5man/d5mantui_properties.xml";
my %properties = (
	"d5man.ui.command.editor" =>
		"vim -c \"let g:d5man_api_url=\\\"\${d5man.api.url}\\\"\"",
	"d5man.ui.command.browser" => "firefox",
	"d5man.ui.newpage.root" => "/data/main/119_man_rr",
	"d5man.api.url" => "http://127.0.0.1:7450",
);
if($haveenv or -f $homef) {
	my $conffile = $haveenv? $ENV{D5MAN_CONF_UI}: $homef;
	my $domparser2 = new XML::DOM::Parser;
	my $document = $domparser2->parseFile($conffile);
	my $entries = $document->getElementsByTagName("entry");
	for(my $i = 0; $i < $entries->getLength(); $i++) {
		my $k = $entries->item($i)->getAttribute("key");
		my $v = $entries->item($i)->getData();
		$properties{$k} = $v;
	}
}

my $command_editor;
my $command_browser;
my $target_root;
my $api_url;

for my $k (keys %properties) {
	my $v = $properties{$k};
	for my $i (keys %properties) {
		my $search = "\${$i}";
		my $replace = $properties{$i};
		$v =~ s/\Q$search\E/\Q$replace\E/g;
	}
	if($k eq "d5man.ui.command.editor") {
		$command_editor = $v;
	} elsif($k eq "d5man.ui.command.browser") {
		$command_browser = $v;
	} elsif($k eq "d5man.api.url") {
		$api_url = $v;
	} elsif($k eq "d5man.ui.newpage.root") {
		$target_root = $v;
	}
}

# color name -> [fg, bg]
my %COLOR_TABLE = (
	"red"     => ["white", "red"],
	"green"   => ["black", "green"],
	"black"   => ["black", "white"],
	"white"   => ["white", "blue"],
	"yellow"  => ["black", "yellow"],
	"purple"  => ["white", "magenta"],
	"delayed" => ["blue",  "black"],
);

#-----------------------------------------------------[ Custom Listbox Class ]--
{
	package Curses::UI::ColoredListbox;
	use strict;
	use Curses;
	use Curses::UI::Listbox;

	use vars qw($VERSION @ISA @EXPORT);

	$VERSION = '1.0';
	@ISA = qw(Curses::UI::Listbox);

	my %color_assignment = ();

	# $1 idx
	# $2 fg
	# $3 bg
	sub set_item_color() {
		my $co = $Curses::UI::color_object;
		$color_assignment{$_[1]} = $co->get_color_pair($_[2], $_[3]);
	}

	sub clear_item_colors() {
		%color_assignment = ();
	}

	sub text_draw(;$) {
		my ($this, $y, $prefix_len, $label) = @_;
		if(defined($color_assignment{$y})) {
			$this->{-canvasscr}->attron(COLOR_PAIR(
							$color_assignment{$y}));
		}
		# copied from parent component
		$this->{-canvasscr}->addstr($y, $prefix_len, 
					" " x ($this->canvaswidth-$prefix_len));
		$this->SUPER::text_draw($y, $prefix_len, $label);
		if(defined($color_assignment{$y})) {
			$this->{-canvasscr}->attroff(COLOR_PAIR(
							$color_assignment{$y}));
		}
	}
};
$INC{"Curses/UI/ColoredListbox.pm"} = 1;

#------------------------------------------[ Preliminary Argument Processing ]--
my $task_filter = "";

if($#ARGV >= 0 and $ARGV[0] =~ /^--/) {
	my $option = shift @ARGV;
	if($option eq "--documents-only") {
		$task_filter = "no";
	} elsif($option eq "--board" or $option eq "--delayed" or
						$option eq "--subtask") {
		$task_filter = substr($option, 2);
	} else {
		print("USAGE $0 [--documents-only|--board|--delayed|--subtask]".
								" [query]\n");
		exit(0);
	}
}

#------------------------------------------------------------------[ Backend ]--
my $queue_search_requests = Thread::Queue->new();
my $queue_search_results  = Thread::Queue->new();

my $thread = threads->create(sub {
	my $client = REST::Client->new();
	my $domparser = new XML::DOM::Parser;
	while(1) {
		# discard all except for newest
		my $query; # {in => "query string", max => 400}
		do {
			$query = $queue_search_requests->dequeue();
			return if($query eq "TERM"); # poison-pill termination
		} while(defined($queue_search_requests->peek()));
		$client->GET($api_url."/query/".URI::Encode::uri_encode(
				$query->{in}, { encode_reserved => 1 }).
				($query->{task_filter} ne ""?
				"?task=".$query->{task_filter}: ""));
		my $resp = [];
		if($client->responseCode eq "200") {
			my $doc = $domparser->parse($client->responseContent());
			my $pages = $doc->getElementsByTagName("meta");
			for(my $i = 0; ($i < $pages->getLength()) and
						($i < $query->{max}); $i++) {
				my $kv = $pages->item($i)->
						getElementsByTagName("kv");
				my $curresp = {};
				for(my $j = 0; $j < $kv->getLength(); $j++) {
					my $ckv = $kv->item($j);
					my $k = $ckv->getAttribute("k");
					$curresp->{$k} = $ckv->getAttribute("v")
					if($k eq "name" or $k eq "section" or
					$k eq "redirect" or $k eq "file" or
					$k eq "title" or $k eq "task-type" or
					$k eq "task-priority");
				}
				push @{$resp}, $curresp;
			}
		} else {
			push @{$resp}, {
				section => -1,
				name => "(error".$client->responseCode.")"
			};
		}
		# [{section => 32, name => "test", redirect => ..}, ...]
		$queue_search_results->enqueue($resp);
	}
});

#--------------------------------------------------------------[ First Query ]--
# If a parameter is given through ARGV, perform a first query before
# initializing the TUI part because this will allow for operation independent
# of window size!

my $initial_query = ($#ARGV >= 0)? join(" ", @ARGV): "";
$queue_search_requests->enqueue({in => $initial_query, max => 200,
						task_filter => $task_filter});
my @initial_results = @{$queue_search_results->dequeue()};
my $search_result = ($#initial_results eq 0 and
	$initial_results[0]->{section} ne -1)? $initial_results[0]: undef;

#----------------------------------------------------------------------[ TUI ]--
sub invoke_tui {
	my $result = undef;
	my $page_metadata = {};
	my $curses = Curses::UI->new(-clear_on_exit => 1, -mouse_support => 0,
					-color_support => 1);
	my $scrh = $curses->height();
	my $curses_window = $curses->add("win1", "Window", -border => 0);
	my $num_search_entries = $scrh - 2;
	$curses_window->add("label", "Label", -text => "> ");
	my $curses_listbox = $curses_window->add("list", "ColoredListbox",
			-radio => 1, -y => 1, -values => ["(Not connected)"],
			-height => $num_search_entries,
			-labels => { 1 => "(Not connected)" }, -selected => 0);
	my @labels = (undef, "New", undef, "All", undef, "Docs", "Board", "W+1",
							"Tasks", "Exit");
	for (my $i = 1; $i < scalar @labels; $i++) {
		my $digit = $i % 10;
		my $aidx = $i - 1;
		$curses_window->add("label_$i", "Label",
					-text => "$digit $labels[$aidx]",
					-y => $scrh - 1, -x => $aidx * 8)
					if(defined($labels[$aidx]));
	}

	my $last_query = $initial_query;
	my $last_task_filter = $task_filter;
	my $change_callback = sub {
		my $cii = shift;
		my $newquery = $cii->get();
		return if($newquery eq $last_query and
					$task_filter eq $last_task_filter);
		$queue_search_requests->enqueue({
			in => $newquery,
			max => $num_search_entries,
			task_filter => $task_filter
		});
		$last_query = $newquery;
		$last_task_filter = $task_filter;
	};

	my $curses_input = $curses_window->add("input", "TextEntry", -x => 2,
			-text => $initial_query, -pos => length($initial_query),
			-onchange => $change_callback);
	$curses_input->set_binding(sub {
		# enter press (open existing)
		my @items = $curses_listbox->get();
		if($#items >= 0 and defined($items[0]) and
					defined($page_metadata->{$items[0]})) {
			$result = $page_metadata->{$items[0]};
			$curses->mainloopExit();
		}
	}, Curses::UI::TextEditor::KEY_ENTER());
	$curses_input->set_binding(sub {
		# up (go upwards, select)
		$curses_listbox->process_bindings("k");
		$curses_listbox->process_bindings("1");
	}, Curses::UI::TextEditor::KEY_UP());
	$curses_input->set_binding(sub {
		# down (go downwards, select)
		$curses_listbox->process_bindings("j");
		$curses_listbox->process_bindings("1");
	}, Curses::UI::TextEditor::KEY_DOWN());
	$curses_input->set_binding(sub {
		# F2 -- new page (marker section for create new)
		$result = {section => -2, createnew => $curses_input->get()};
		$curses->mainloopExit();
	}, 266);
	$curses_input->set_binding(sub {
		# F4 -- back to default view
		$task_filter = "";
	}, 268);
	$curses_input->set_binding(sub {
		# F6 -- display only documents
		$task_filter = "no";
	}, 270);
	$curses_input->set_binding(sub {
		# F7 -- display board
		$task_filter = "board";
	}, 271);
	$curses_input->set_binding(sub {
		# F8 -- display delayed
		$task_filter = "delayed";
	}, 272);
	$curses_input->set_binding(sub {
		# F9 -- display subtasks
		$task_filter = "subtask";
	}, 273);
	$curses_input->set_binding(sub {
		# F10 -- exit (no result)
		$curses->mainloopExit();
	}, 274);
	$curses_input->focus();

	my $displayresults = sub {
		my $results_to_proc = shift;
		undef %{$page_metadata};
		my $values = [];
		my $i = 0;
		$curses_listbox->clear_item_colors();
		for my $entry (@{$results_to_proc}) {
			my $label = $entry->{section}." ".$entry->{name};
			if(length($label) < 15 and defined($entry->{title})) {
				$label = sprintf("%-14s %s", $label,
							$entry->{title});
			}
			if(defined($entry->{"task-priority"}) and
			defined($COLOR_TABLE{$entry->{"task-priority"}})) {
				my $tble = $COLOR_TABLE{$entry->{
							"task-priority"}};
				$curses_listbox->set_item_color($i, $tble->[0],
							$tble->[1]);
			}
			$page_metadata->{$label} = $entry;
			push @{$values}, $label;
			$i++;
		}
		$curses_listbox->values($values);
		$curses_listbox->process_bindings("1");
		$curses_listbox->draw();
		$curses_input->focus();
	};

	my $updateresults = sub {
		my $results_to_proc = undef;
		while(1) {
			my $curr = $queue_search_results->peek();
			last if not defined($curr);
			$queue_search_results->dequeue();
			$results_to_proc = $curr;
		}
		$displayresults->($results_to_proc)
						if(defined($results_to_proc));
	};

	# Although it would be intuitive to use the callback, the timer works
	# much more reliably in case only the filter is changed. Interestingly,
	# having both of the callbacks seems to provide the most responsive
	# experience although it is a little unexpected that it matters to have
	# both of them...
	$curses->add_callback("updateresults", $updateresults);
	$curses->set_timer("timer_updateresults", $updateresults, 0.1);

	$displayresults->(\@initial_results);
	$curses->mainloop();
	# We need to clear the TUI s.t. the terminal will not be messed up.
	$curses->leave_curses();
	return $result;
}

$search_result = invoke_tui if(not defined($search_result));
$queue_search_requests->enqueue("TERM");
$thread->join();

#-----------------------------------------------------------[ Process result ]--

exit(0) if(not defined($search_result) or $search_result->{section} eq -1);

if($search_result->{section} eq -2) {
	# create new page
	my $raw = $search_result->{createnew};
	my $idx = index($raw, " ");
	exit(0) if($idx eq -1); # invalid input. ignore silently
	my $section = substr($raw, 0, $idx);
	my $name = substr($raw, $idx + 1);
	my $filename = $name;
	$filename =~ s/\//_/g;
	my $dir = $target_root."/".$section;
	my $is_task = ($section eq "43");
	my $path = $dir."/".$filename.($is_task? ".hot": ".md");
	my $localtz = DateTime::TimeZone->new(name => 'local');
	my $dateobj = DateTime->now(time_zone => $localtz);
	my $date = $dateobj->strftime("%Y/%m/%d %H:%M:%S");
	my $year = $dateobj->strftime("%Y");
	mkdir($dir) if(not -d $dir);
	if(-f $path) {
		print("Error: File already exists: $path. Not overwriting.\n");
		exit(1);
	}
	open(my $fd, ">:encoding(UTF-8)", $path);
	if($is_task) {
		print $fd <<~EOF;
			---
			section: 43
			x-masysma-name: "$name"
			title: Template Issue Summary
			date: $date
			lang: en-US
			keywords: ["task"]
			x-masysma-task-type: subtask
			x-masysma-task-priority: considered
			---
			Task Overview
			=============

			This text can be upgraded as needed.

			x-masysma-task-type
			:   long | short | subtask | periodic
			x-masysma-task-priority
			:   red | green | black | white | yellow | purple |
			    delayed | considered

			2022/01/01
			==========

			Task Step. This text is never changed after being added.
			EOF
	} else {
		print $fd <<~EOF;
			---
			section: $section
			x-masysma-name: $name
			title: Template Page Title
			date: $date
			lang: en-US
			author: ["Linux-Fan, Ma_Sys.ma (info\@masysma.net)"]
			keywords: ["key", "word"]
			x-masysma-version: 1.0.0
			x-masysma-repository: https://www.github.com/m7a/...
			x-masysma-website: https://masysma.net/$section/$filename.xhtml
			x-masysma-owned: 1
			x-masysma-copyright: (c) $year Ma_Sys.ma <info\@masysma.net>.
			---
			Template
			========

			D5Man 2 Template file.
			Edit metadata, delete template, start writing.
			EOF
	}
	close($fd);
	exec $command_editor." \"".$path."\"";
} elsif(defined($search_result->{redirect})) {
	# open in web browser
	exec $command_browser." \"".$search_result->{redirect}."\"";
	# Should the browser start process be the same as the running browser,
	# it might make sense to fork here for GUI browsers?
	#my $child = fork();
	#if($child != 0) {
	#	exec $command_browser, ($search_result->{redirect});
	#}
} else {
	# open in editor
	exec $command_editor." \"".$search_result->{file}."\"";
}
