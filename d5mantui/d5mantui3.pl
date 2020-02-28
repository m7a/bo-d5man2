#!/usr/bin/perl
# Ma_Sys.ma D5Man Terminal UI 3, Copyright (c) 2020 Ma_Sys.ma.
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

#---------------------------------------------------------------------[ Conf ]--
my $command_editor  = "vim";
my $command_browser = "firefox";
my $target_root     = "/data/main/man/rr";
my $api_url         = "http://127.0.0.1:7450";

my $haveenv = defined($ENV{D5MAN_CONF_UI});
my $homef = File::HomeDir::home()."/.mdvl/d5man/d5naui_properties.xml";
if($haveenv or -f $homef) {
	my $conffile = $haveenv? $ENV{D5MAN_CONF_UI}: $homef;
	my $domparser2 = new XML::DOM::Parser;
	my $document = $domparser2->parseFile($conffile);
	my $entries = $document->getElementsByTagName("entry");
	for(my $i = 0; $i < $entries->getLength(); $i++) {
		my $k = $entries->item($i)->getAttribute("key");
		my $v = $entries->item($i)->getData();
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
				$query->{in}, { encode_reserved => 1 }));
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
					$k eq "redirect" or $k eq "file");
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
$queue_search_requests->enqueue({in => $initial_query, max => 200});
my @initial_results = @{$queue_search_results->dequeue()};
my $search_result = ($#initial_results eq 0 and
	$initial_results[0]->{section} ne -1)? $initial_results[0]: undef;

#----------------------------------------------------------------------[ TUI ]--
sub invoke_tui {
	my $result = undef;
	my $page_metadata = {};
	my $curses = Curses::UI->new(-clear_on_exit => 1, -mouse_support => 0);
	my $scrh = $curses->height();
	my $curses_window = $curses->add("win1", "Window", -border => 0);
	my $num_search_entries = $scrh - 2;
	$curses_window->add("label", "Label", -text => "> ");
	my $curses_listbox = $curses_window->add("list", "Listbox", -radio => 1,
			-y => 1, -values => ["(Not connected)"],
			-height => $num_search_entries,
			-labels => { 1 => "(Not connected)" }, -selected => 0);
	$curses_window->add("label2", "Label", -text => "2 New",
						-y => $scrh - 1, -x => 8);
	$curses_window->add("label3", "Label", -text => "0 Exit",
						-y => $scrh - 1, -x => 72);
	my $last_query = $initial_query;
	my $curses_input = $curses_window->add(
		"input", "TextEntry", -x => 2, -text => $initial_query,
		-pos => length($initial_query),
		-onchange => sub {
			my $cii = shift;
			my $newquery = $cii->get();
			return if $newquery eq $last_query;
			$queue_search_requests->enqueue({in => $newquery,
						max => $num_search_entries});
			$last_query = $newquery;
		}
	);
	$curses_input->set_binding(sub {
		# enter press (open existing)
		my @items = $curses_listbox->get();
		$result = $page_metadata->{$items[0]} if $#items >= 0 and
					defined($page_metadata->{$items[0]});
		$curses->mainloopExit();
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
		# F10 -- exit (no result)
		$curses->mainloopExit();
	}, 274);
	$curses_input->focus();
	my $displayresults = sub {
		my $results_to_proc = shift;
		undef %{$page_metadata};
		my $values = [];
		for my $entry (@{$results_to_proc}) {
			my $label = $entry->{section}." ".$entry->{name};
			$page_metadata->{$label} = $entry;
			push @{$values}, $label;
		}
		$curses_listbox->values($values);
		$curses_listbox->process_bindings("1");
		$curses_listbox->draw();
		$curses_input->focus();
	};
	$curses->add_callback("updateresults", sub {
		my $results_to_proc;
		while(1) {
			my $curr = $queue_search_results->peek();
			return if not defined($curr);
			$queue_search_results->dequeue();
			$results_to_proc = $curr;
		}
		$displayresults->($results_to_proc);
	});
	$displayresults->(\@initial_results);
	$curses->mainloop();
	# TODO z bad hack
	# We need to clear the TUI s.t. the terminal will not be messed up.
	# Problem is: If calling exec, the destructor is never called (?) and
	# the terminal remains messed up after vim or firefox return. Thus,
	# call the destructor early (bad practice) to clean terminal state...
	$curses->DESTROY();
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
	my $path = $dir."/".$filename.".md";
	my $localtz = DateTime::TimeZone->new(name => 'local');
	my $dateobj = DateTime->now(time_zone => $localtz);
	my $date = $dateobj->strftime("%Y/%m/%d %H:%M:%S");
	my $year = $dateobj->strftime("%Y");
	mkdir($dir) if(not -d $dir);
	open(my $fd, ">:encoding(UTF-8)", $path);
	print $fd <<~EOF;
		section: $section
		x-masysma-name: $name
		title: Template Page Title
		date: $date
		lang: en-US
		author: ["Linux-Fan, Ma_Sys.ma (Ma_Sys.ma\@web.de)"]
		keywords: ["key", "word"]
		x-masysma-version: 1.0.0
		x-masysma-repository: https://www.github.com/m7a/...
		x-masysma-website: https://masysma.lima-city.de/$section/$filename.xhtml
		x-masysma-owned: 1
		x-masysma-copyright: |
		  Copyright (c) $year Ma_Sys.ma.
		  For further info send an e-mail to Ma_Sys.ma\@web.de.
		---
		Template
		========

		D5Man 2 Template file. Edit metadata, delete template, start writing.
		EOF
	close($fd);
	exec $command_editor, ($path);
} elsif(defined($search_result->{redirect})) {
	# open in web browser
	exec $command_browser, ($search_result->{redirect});
	# Should the browser start process be the same as the running browser,
	# it might make sense to fork here for GUI browsers?
	#my $child = fork();
	#if($child != 0) {
	#	exec $command_browser, ($search_result->{redirect});
	#}
} else {
	# open in editor
	exec $command_editor, ($search_result->{file});
}
