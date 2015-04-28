/**
 * 
 */
package test;


import com.gargoylesoftware.htmlunit.BrowserVersion;
import com.gargoylesoftware.htmlunit.BrowserVersionFeatures;
import com.gargoylesoftware.htmlunit.PluginConfiguration;
import com.gargoylesoftware.htmlunit.WebClient;
import com.gargoylesoftware.htmlunit.html.HtmlPage;



/**
 * @author Raimbault Juste <br/> <a href="mailto:juste.raimbault@polytechnique.edu">juste.raimbault@polytechnique.edu</a>
 *
 */
public class TestFinalDom {

	
	 private static BrowserVersion firefox17WithUptoDateFlash = new BrowserVersion(
			 BrowserVersion.FIREFOX_31.getApplicationName(),
			 BrowserVersion.FIREFOX_31.getApplicationVersion(),
			 BrowserVersion.FIREFOX_31.getUserAgent(),
			 BrowserVersion.FIREFOX_31.getBrowserVersionNumeric(),
			 new BrowserVersionFeatures[]{
				 BrowserVersionFeatures.JS_APPLET_OBJECT,
				 BrowserVersionFeatures.STYLESHEET_HREF_EXPANDURL,
				 BrowserVersionFeatures.STYLESHEET_HREF_STYLE_NULL
			 }
	);
	 
	
		    
		private static final PluginConfiguration plugin1 = new PluginConfiguration(
		            "Shockwave Flash",
		            "Shockwave Flash 11.4 r402",
		            "NPSWF32_11_4_402_287.dll");
		static {	    
		plugin1.getMimeTypes().add(new PluginConfiguration.MimeType(
		            "application/x-shockwave-flash", 
		            "Adobe Flash movie",
		            "swf"));
		
		firefox17WithUptoDateFlash.getPlugins().add(plugin1);
	}
	
	public static void test1(){
		try{
			WebClient client = new WebClient();
			client.getBrowserVersion().getPlugins().add(plugin1);
			HtmlPage startPage = client.getPage("https://www.google.com/maps/dir/48.8785864,2.3540918/48.8762067,2.359198/?vector=1");
			System.out.println(startPage.asXml());
		}catch(Exception e){e.printStackTrace();}
	}
		
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		test1();

	}

}
