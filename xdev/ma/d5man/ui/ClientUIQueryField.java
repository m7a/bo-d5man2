package ma.d5man.ui;

import jexer.TWidget;
import jexer.TField;
import jexer.TKeypress;
import jexer.event.TKeypressEvent;

class ClientUIQueryField extends TField {

	private final ClientUIWindow clientUI;

	ClientUIQueryField(ClientUIWindow parent, String qInitial) {
		super(parent, 6, 0, 43, false, qInitial);
		clientUI = parent;
	}

	@Override
	public void onKeypress(TKeypressEvent ev) {
		switch(ev.getKey().getKeyCode()) {
		case TKeypress.UP:
			clientUI.selectEntry(-1);
			break;
		case TKeypress.DOWN:
			clientUI.selectEntry(1);
			break;
		default:
			super.onKeypress(ev);
			clientUI.performQuery();
		}
	}

}
