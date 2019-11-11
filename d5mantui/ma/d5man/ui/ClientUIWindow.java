package ma.d5man.ui;

import java.util.Arrays;

import jexer.*;
import jexer.event.TKeypressEvent;
import jexer.event.TResizeEvent;

import java.util.List;
import java.util.ArrayList;
import java.util.function.Consumer;

class ClientUIWindow extends TWindow {

	private final TApplication parent;
	private final APIConnection conn;
	private final Consumer<APIPageRecord> pageOpener;

	private final TField queryInput;
	private final TList queryResults;
	private List<APIPageRecord> queryResultRecords;

	ClientUIWindow(TApplication parent, APIConnection conn,
			String qInitial, Consumer<APIPageRecord> pageOpener) {
		super(parent, "Ma_Sys.ma D5Man Main Screen", 50, 20);
		this.parent = parent;
		this.conn = conn;
		this.pageOpener = pageOpener;
		addLabel("Query", 0, 0);
		queryInput = new ClientUIQueryField(this, qInitial);
		addLabel("Results", 0, 4);
		queryResults = addList(Arrays.asList(new String[] { "..." }),
								0, 6, 48, 14);
		maximize();
		activate();
		queryInput.activate();
		if(performQuery() == 1)
			applySelectedQuery();
	}

	@Override
	public void onResize(TResizeEvent ev) {
		super.onResize(ev);
		queryInput.setWidth(ev.getWidth() - 2);
		queryResults.setWidth(ev.getWidth() - 2);
		queryResults.setHeight(ev.getHeight() - 8);
		queryResults.reflowData();
	}

	/** @return number of query results */
	int performQuery() {
		try {
			ArrayList<String> list = new ArrayList<String>();
			queryResultRecords = conn.query(queryInput.getText());
			for(APIPageRecord rec: queryResultRecords)
				list.add(rec.toString());
			queryResults.setList(list);
			selectEntry(0);
			return list.size();
		} catch(RuntimeException ex) {
			new TExceptionDialog(parent, ex);
			return 0;
		}
	}

	void selectEntry(int dx) {
		int sz = queryResults.getList().size();
		if(sz != 0) {
			int cursel = queryResults.getSelectedIndex() == -1? 0:
						queryResults.getSelectedIndex();
			queryResults.setSelectedIndex((cursel + dx) % sz);
		}
	}

	private void applySelectedQuery() {
		int sidx = queryResults.getSelectedIndex();
		if(sidx != -1)
			pageOpener.accept(queryResultRecords.get(sidx));
	}

	@Override
	public void onKeypress(TKeypressEvent ev) {
		switch(ev.getKey().getKeyCode()) {
		case TKeypress.ENTER:
			applySelectedQuery();
			break;
		default:
			super.onKeypress(ev);
		}
	}

}
