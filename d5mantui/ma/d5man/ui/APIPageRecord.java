package ma.d5man.ui;

import java.nio.file.Path;

class APIPageRecord {

	int section = -1;
	String name;
	Path file;
	String[] tags = new String[0];
	String redirect;

	@Override
	public String toString() {
		return String.format("%2d %s", section, name == null?
								"null": name);
	}

}
