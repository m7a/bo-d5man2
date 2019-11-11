package ma.d5man.ui;

import java.io.UnsupportedEncodingException;
import jexer.TApplication;
import jexer.menu.TMenu;
import jexer.event.TMenuEvent;

class Application extends TApplication {

	private static final int ID_EXIT = 2000;

	Application() throws UnsupportedEncodingException {
		super(BackendType.XTERM);

		TMenu file = addMenu("&File");
		file.addItem(ID_EXIT, "E&xit");
	}

	@Override
	protected boolean onMenu(TMenuEvent menu) {
		switch(menu.getId()) {
		case ID_EXIT:
			exit();
			return true;
		default:
			return false;
		}
	}

}
