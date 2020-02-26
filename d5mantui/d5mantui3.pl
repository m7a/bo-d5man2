#!/usr/bin/perl
# Ma_Sys.ma D5Man Terminal UI 3, Copyright (c) 2020 Ma_Sys.ma.
# For further info send an e-mail to Ma_Sys.ma@web.de.
use strict;
use warnings FATAL => 'all';
use autodie;
use threads;

require REST::Client;           # librest-client-perl
require URI::Encode;            # liburi-encode-perl
require Curses::UI;             # libcurses-ui-perl
require Curses::UI::TextEditor; # (in same package)
require XML::DOM;               # libxml-dom-perl
require Thread::Queue;

#---------------------------------------------------------------------[ Conf ]--
my $command_editor  = "vim";
my $command_browser = "firefox";
my $api_url         = "http://127.0.0.1:7450";
# TODO z LOAD CONF IF EXISTENT (XML PROPERTY FORMAT) ~/.mdvl/d5man/d5manui_properties.xml OR env D5MAN_CONF_UI

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
		$client->GET($api_url."/query/".
					URI::Encode::uri_encode($query->{in}));
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

# If a parameter is given thorugh ARGV, perform a first query before
# initializing the TUI part because this will allow for operation independent
# of window size!

# TODO N_IMPL

#----------------------------------------------------------------------[ TUI ]--
my $curses = Curses::UI->new(-clear_on_exit => 1, -mouse_support => 0);
my $scrh = $curses->height();
my $curses_window = $curses->add("win1", "Window", -border => 0);
my $num_search_entries = $scrh - 2;
$curses_window->add("label", "Label", -text => "> ");
my $curses_listbox = $curses_window->add("list", "Listbox", -radio => 1,
			-y => 1, -values => ["(Not connected)"],
			-height => $num_search_entries,
			-labels => { 1 => "(Not connected)" }, -selected => 0);
$curses_window->add("label2", "Label", -text => "2 New", -y => $scrh - 1,
								-x => 8);
$curses_window->add("label3", "Label", -text => "0 Exit", -y => $scrh - 1,
								-x => 72);
my $curses_input = $curses_window->add(
	"input", "TextEntry", -x => 2,
	-onchange => sub {
		my $cii = shift;
		$queue_search_requests->enqueue({in => $cii->get(),
						max => $num_search_entries});
	}
);
$curses_input->set_binding(sub {
	# enter press
	# TODO N_IMPL
	$curses->mainloopExit();
}, Curses::UI::TextEditor::KEY_ENTER());
$curses_input->set_binding(sub {
	# F2 -- new page
	# TODO N_IMPL
	$curses->mainloopExit();
}, 266);
$curses_input->set_binding(sub {
	# F10 -- exit
	$curses->mainloopExit();
}, 274);
$curses_input->focus();
$curses->add_callback("updateresults", sub {
	my $results_to_proc;
	while(1) {
		my $curr = $queue_search_results->peek();
		last if not defined($curr);
		$queue_search_results->dequeue();
		$results_to_proc = $curr;
	}
	my $values = [];
	for my $entry (@{$results_to_proc}) {
		push @{$values}, $entry->{section}." ".$entry->{name};
	}
	$curses_listbox->values($values);
	$curses_listbox->draw();
});
$curses->mainloop();

$queue_search_requests->enqueue("TERM");
$thread->join();
