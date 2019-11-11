package ma.d5man.ui;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.Files;
import java.util.Properties;
import java.net.URL;

/*
	Default configuration

	<?xml version="1.0" encoding="UTF-8"?>
	<!DOCTYPE properties SYSTEM "http://java.sun.com/dtd/properties.dtd">

	<properties>
		<entry key="d5man.ui.command.editor">vim</entry>
		<entry key="d5man.ui.command.browser">firefox</entry>
		<entry key="d5man.api.url">http://127.0.0.1:7450/</entry>
	</properties>
 */

class Config {

	final String commandEditor;
	final String commandBrowser;
	final URL    api;

	Config() throws IOException {
		// Locations to check for properties
		String confStr = System.getenv("D5MAN_CONF_UI");
		Path conf;
		if(confStr == null) {
			conf = Paths.get(System.getProperty("user.home"),
				".mdvl", "d5man", "d5manui_properties.xml").
				toAbsolutePath();
			if(!Files.exists(conf))
				conf = null;
		} else {
			conf = Paths.get(confStr);
		}

		Properties pr = new Properties();
		if(conf != null) {
			try(InputStream is = Files.newInputStream(conf)) {
				// Design Decision: We are using XML properties
				// here because they default to UTF-8 which is
				// used throughout the rest of D5Man
				pr.loadFromXML(is);
			}
		}
		commandEditor  = pr.getProperty("d5man.ui.command.editor",
								"vim");
		commandBrowser = pr.getProperty("d5man.ui.command.browser",
								"firefox");
		api = new URL(pr.getProperty("d5man.api.url",
						"http://127.0.0.1:7450/"));
	}

}
