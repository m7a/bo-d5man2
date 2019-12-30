package ma.d5man.ui;

import java.io.IOException;
import java.util.function.Consumer;
import java.nio.file.Paths;

import jexer.TApplication;
import jexer.TExceptionDialog;

class PageOpener implements Consumer<APIPageRecord> {

	private final Application parent;
	private final String commandEditor;
	private final String commandBrowser;

	PageOpener(Application parent, String commandEditor,
							String commandBrowser) {
		super();
		this.parent = parent;
		this.commandEditor = commandEditor;
		this.commandBrowser = commandBrowser;
	}

	@Override
	public void accept(APIPageRecord rec) {
		try {
			openPageRecord(rec);
		} catch(Exception ex) {
			new TExceptionDialog(parent, ex);
		}
	}

	private void openPageRecord(APIPageRecord rec) throws IOException {
		parent.shutdown();

		if(rec.redirect != null) {
			// This has a redirect, means it is to be viewn in the
			// browser. Need to decide if this request is a relative
			// path to a local file (file:///) or an absolute URL
			// (http://)
			String fn = rec.file.getFileName().toString();
			int extpos = fn.lastIndexOf('.');
			String urlToCall =
				// absolute URL
				(rec.redirect.startsWith("http://") ||
					rec.redirect.startsWith("https://"))?
				rec.redirect:
				// relative URL
				("file://" + (rec.file.getParent().resolve(
				fn.substring(0, extpos) + "_att").resolve(
				rec.redirect)).toAbsolutePath().toString());
			Runtime.getRuntime().exec(new String[] { commandBrowser,
								urlToCall });
		} else {
			// https://stackoverflow.com/questions/29733038
			ProcessBuilder proc = new ProcessBuilder(new String[] {
				commandEditor, rec.file.toString()
			});
			proc.inheritIO();
			try {
				proc.start().waitFor();
			} catch(InterruptedException ex) {
				ex.printStackTrace();
				System.exit(1);
			}
		}
	}

}
