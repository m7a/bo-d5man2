package ma.d5man.ui;

import java.net.URL;
import java.net.URLConnection;
import java.net.URLEncoder;
import java.net.MalformedURLException;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

class APIConnection {

	private final URL url; 

	private final DocumentBuilder docBuilder;

	APIConnection(URL url) throws ParserConfigurationException {
		this.url = url;
		this.docBuilder = DocumentBuilderFactory.newInstance().
							newDocumentBuilder();
	}

	/*
		<kv k="file" v="/data/main/man/x_dev/work/0renewal/d5man_root/21/javadoc.yml"/>
		<kv k="section" v="21"/>
		<kv k="name" v="javadoc/javax.swing.JPanel"/>
		<kv k="tags" v="JPanel javadoc javax.swing"/>
		<kv k="redirect" v="jpanel.html"/>
	*/
	List<APIPageRecord> query(String query) {
		ArrayList<APIPageRecord> rv = new ArrayList<APIPageRecord>();
		Document queryResult;
		try {
			queryResult = get("query/" + URLEncoder.encode(query,
						"UTF-8").replace("+", "%20"));
		} catch(UnsupportedEncodingException ex) {
			throw new RuntimeException(ex);
		}
		NodeList lst = queryResult.getElementsByTagName("meta");
		for(int i = 0; i < lst.getLength(); i++) {
			NodeList kvs = ((Element)lst.item(i)).
						getElementsByTagName("kv");
			APIPageRecord rec = new APIPageRecord();
			for(int j = 0; j < kvs.getLength(); j++) {
				Element kv = (Element)kvs.item(j);
				String v = kv.getAttribute("v");
				switch(kv.getAttribute("k")) {
				case "section":  rec.section =
						     Integer.parseInt(v); break;
				case "file":     rec.file = Paths.get(v); break;
				case "name":     rec.name = v;            break;
				case "tags":     rec.tags = v.split(" "); break;
				case "redirect": rec.redirect = v;        break;
				default:
					throw new RuntimeException(
						"Unknown kv: k=\"" +
						kv.getAttribute("k") +
						"\", v=\"" + v + "\""
					);
				}
			}
			rv.add(rec);
		}
		return rv;
	}

	private Document get(String path) {
		try {
			URL queryResource = new URL(url, path);
			URLConnection conn = queryResource.openConnection();
			try(InputStream in = conn.getInputStream()) {
				return docBuilder.parse(in);
			}
		} catch(Exception ex) {
			throw new RuntimeException(ex);
		}
	}

}
