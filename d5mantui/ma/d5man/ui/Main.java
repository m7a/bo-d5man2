package ma.d5man.ui;

import java.io.UnsupportedEncodingException;
import java.io.IOException;
import java.net.URL;
import jexer.TExceptionDialog;

// TODO NEXT FEATURES
// [ ] Colorscheme like old D5Man (red, white, black)
// [ ] Use async gui update and process query in background.
//     Need to cancel unnecessary/superseded queries and await
//     finaly query result prior to processing enter.
// [ ] Find system lib, do not provide a symlink but a description that one
//     can add lib there for windows world...
public class Main {

	public static void main(String[] args) {
		StringBuilder argsq = new StringBuilder();
		for(int i = 0; i < args.length; i++) {
			if(i != 0)
				argsq.append(' ');
			argsq.append(args[i]);
		}

		Application application;
		try {
			application = new Application();
		} catch(UnsupportedEncodingException ex) {
			System.err.println("Failed to initialize application.");
			ex.printStackTrace();
			System.exit(1);
			return;
		}

		Config config;
		try {
			config = new Config();
		} catch(IOException ex) {
			new TExceptionDialog(application, ex);
			System.exit(1);
			return;
		}

		PageOpener pageOpener = new PageOpener(application,
				config.commandEditor, config.commandBrowser);

		try {
			new ClientUIWindow(
				application,
				new APIConnection(config.api),
				argsq.toString(),
				pageOpener
			);
			application.run();
		} catch(Exception ex) {
			new TExceptionDialog(application, ex);
			System.exit(1);
		}
	}

}
