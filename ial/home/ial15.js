'use strict';

/*
 * Ma_Sys.ma IAL 1.5, Copyright (c) 2019 Ma_Sys.ma.
 * For further info send an e-mail to Ma_Sys.ma@web.de.
 *
 * originally dates back to "Informationen und Links" (ial) Javascript Parts
 * (2011, 2012, 2013, 2017)
 */

var ial15_last_query_id = 0;

function ial15_query(query) {
	var req = new XMLHttpRequest();
	ial15_last_query_id++;
	var my_id = ial15_last_query_id;
	req.onreadystatechange = function() {
		if(req.readyState === 4 && ial15_last_query_id === my_id)
			ial15_set_links(document.getElementById(
						"ial15_search_results_out"),
						req.responseXML, true);
	}
	req.open("GET", "/query/" + query.replace("\/", "%2f"));
	req.send();
}

function ial15_locallinks() {
	var req = new XMLHttpRequest();
	req.onreadystatechange = function() {
		if(req.readyState === 4) {
			if(req.responseXML == null)
				return;
			var el = document.getElementById("locall");
			ial15_clear_children(el);
			el.appendChild(req.responseXML.documentElement);
		}
	}
	req.open("GET", "/local/links.xml");
	req.send();
}

function ial15_fixed_content() {
	var elements = document.getElementById("leftmost").
						getElementsByTagName("ul");
	for(var i = 0; i < elements.length; i++)
		ial15_fixed_content_query(elements[i]);
}

function ial15_fixed_content_query(element) {
	var req = new XMLHttpRequest();
	req.onreadystatechange = function() {
		if(req.readyState === 4)
			ial15_set_links(element, req.responseXML, false);
	}
	req.open("GET", "/query/21%20" + element.getAttribute("id"));
	req.send();
}

function ial15_set_links(dout, doc, gen_current) {
	if(doc == null)
		return;

	ial15_clear_children(dout);
	var elements = doc.getElementsByTagName("meta");
	for(var i = 0; i < elements.length; i++) {
		// process record
		var rec = new Object();
		var sub = elements[i].getElementsByTagName("kv");
		for(var j = 0; j < sub.length; j++)
			rec[sub[j].getAttribute("k")] =
						sub[j].getAttribute("v");

		var id = rec["name"] + "(" + rec["section"] + ")";

		// TODO z optional might want to add filter capabilities here

		if(rec["redirect"] == undefined) {
			console.log("Record " + id + " has no redirect.");
			continue; // skip fishy item
		}

		var link_target;
		if(rec["redirect"].startsWith("http://") ||
					rec["redirect"].startsWith("https://"))
			link_target = rec["redirect"];
		else
			link_target = "file://" + rec["file"].substring(0,
				rec["file"].lastIndexOf('.')) + "_att/" +
				rec["redirect"];


		// generate HTML
		var entry = document.createElement("li");
		if(i == 0 && gen_current)
			entry.setAttribute("id", "ial15_entry_current");
		var alink = document.createElement("a");
		alink.setAttribute("href", link_target);
		alink.setAttribute("target", "_blank");
		alink.textContent = id;
		entry.append(alink);
		dout.appendChild(entry);
	}
}

function ial15_clear_children(dout) {
	while(dout.hasChildNodes())
		dout.removeChild(dout.firstChild);
}

function ial15_get_input() {
	return document.getElementById("ial15_query_in_perm").value +
				document.getElementById("ial15_query_in").value;
}

function ial15_keypress(event) {
	if(event != null && (event.keyCode == 38 || event.keyCode == 40)) {
		var current = document.getElementById("ial15_entry_current");
		if(current == null)
			return;
		var dst;
		var dout = document.getElementById("ial15_search_results_out");
		switch(event.keyCode) {
		case 38: // up
			dst = current.previousSibling == null?
					dout.lastChild: current.previousSibling;
			break;
		case 40: // down
			dst = current.nextSibling == null?
					dout.firstChild: current.nextSibling;
			break;
		}
		current.removeAttribute("id");
		dst.setAttribute("id", "ial15_entry_current");
	} else if(event != null && event.keyCode == 13) {
		ial15_open();
	} else {
		ial15_query(ial15_get_input());
	}
	// ignore for now
	return true;
}

function ial15_open() {
	var current = document.getElementById("ial15_entry_current");
	if(current != null) {
		var target = current.getElementsByTagName("a")[0].
							getAttribute("href");
		window.open(target);
		document.getElementById("ial15_query_in").value = "";
		ial15_focus_input();
		ial15_query(ial15_get_input());
	}
	return false;
}

function ial15_focus_input() {
	document.getElementById("ial15_query_in").focus();
}

ial15_query(ial15_get_input()); // initialize non-empty
ial15_focus_input();
ial15_locallinks();
ial15_fixed_content();
