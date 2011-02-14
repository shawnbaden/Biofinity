package bootstrap.liftweb

import edu.unl.biofinity.api.{model => Model}
import edu.unl.biofinity.api.{service => Service}
import edu.unl.biofinity.site.FeedManager
import edu.unl.biofinity.site.TwitterBioBlitzImporterScheduler
import edu.unl.biofinity.site.snippet.Global

import _root_.java.sql.{DriverManager,Connection}

import _root_.javax.sql.{DataSource}

import net.liftweb._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.js._
import net.liftweb.http.js.JE._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.S._
import net.liftweb.mapper._
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import net.liftweb.util._
import net.liftweb.util.Helpers._

/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
	def boot {
		val signedIn = If(
			() => Model.User.signedIn_?,
			() => RedirectResponse(Service.Authentication.redirectURL)
		)
		val signedInAsAdministrator = If(
			() => Model.User.signedIn_? && Model.User.currentUser.userType == Model.UserType.Administrator,
			() => RedirectResponse("/")
		)
		val signOutIfRestricted = If(
			() => {
				if (Model.User.signedIn_? && Model.User.currentUser.is.userType == Model.UserType.Restricted) {
					Model.User.currentUser(null)
					Model.User.currentGroup(null)
				}
				true
			},
			() => RedirectResponse("/")
		)
		
		val labSignedIn = If(
			() => Model.User.signedIn_? && Model.User.groupMember_?,
			() => {
				if ( !Model.User.signedIn_? ) {
					RedirectResponse("/lab/info")
				} else {
					if (Model.User.pendingRequests_?) {
						RedirectResponse("/lab/settings")
					} else {
						RedirectResponse("/lab/new")
					}
				}
			}
		)
		val labSignedOut = If(
			() => !Model.User.signedIn_?,
			() => RedirectResponse("/lab")
		)

		val entries =
			Menu(Loc("Home",                        "index" :: Nil, "Home",                                               signOutIfRestricted)) ::
			Menu(Loc("Administration Home",         "admin" :: "index" :: Nil, "Administration Home",                     signedInAsAdministrator)) ::
			Menu(Loc("Data Classification",         "data" :: "classification" :: Nil, "Data Classification",             signOutIfRestricted)) ::
			Menu(Loc("Data Event",                  "data" :: "event" :: Nil, "Data Event",                               signOutIfRestricted)) ::
			Menu(Loc("Data Group",                  "data" :: "group" :: Nil, "Data Group",                               signOutIfRestricted)) ::
			Menu(Loc("Data Home",                   "data" :: "index" :: Nil, "Data Home",                                signOutIfRestricted)) ::
			Menu(Loc("Data Location",               "data" :: "location" :: Nil, "Data Location",                         signOutIfRestricted)) ::
			Menu(Loc("Data Map",                    "data" :: "map" :: Nil, "Data Map",                                   signOutIfRestricted)) ::
			Menu(Loc("Data Occurrence",             "data" :: "occurrence" :: Nil, "Data Occurrence",                     signOutIfRestricted)) ::
			Menu(Loc("Data Search",                 "data" :: "search" :: Nil, "Data Search",                             signOutIfRestricted)) ::
			Menu(Loc("Data Taxon",                  "data" :: "taxon" :: Nil, "Data Taxon",                               signOutIfRestricted)) ::
			Menu(Loc("HDZ Ampibian Create",         "HDZ" :: "amphibian" :: "create" :: Nil, "HDZ Ampibian Create",       signOutIfRestricted, signedIn)) ::
			Menu(Loc("HDZ Ampibian Home",           "HDZ" :: "amphibian" :: "index" :: Nil, "HDZ Ampibian Home",          signOutIfRestricted)) ::
			Menu(Loc("HDZ Ampibian Success",        "HDZ" :: "amphibian" :: "success" :: Nil, "HDZ Ampibian Success",     signOutIfRestricted, signedIn)) ::
			Menu(Loc("Info Additional",             "info" :: "additional" :: Nil, "Additional Info",                     signOutIfRestricted)) ::
			Menu(Loc("Info BioBlitz",               "info" :: "bioblitz" :: Nil, "BioBlitz",                              signOutIfRestricted)) ::
			Menu(Loc("Info Framework",              "info" :: "framework" :: Nil, "Framework",                            signOutIfRestricted)) ::
			Menu(Loc("Info Home",                   "info" :: "index" :: Nil, "Info Home",                                signOutIfRestricted)) ::
			Menu(Loc("Info People",                 "info" :: "people" :: Nil, "People",                                  signOutIfRestricted)) ::
			Menu(Loc("Info Publications",           "info" :: "publications" :: Nil, "Publications",                      signOutIfRestricted)) ::
			Menu(Loc("Lab Classification",          "lab" :: "classification" :: Nil, "Lab Classification",               labSignedIn)) ::
			Menu(Loc("Lab Classify Occurrence",     "lab" :: "classify-occurrence" :: Nil, "Lab Classify Occurrence",     labSignedIn)) ::
			Menu(Loc("Lab Create",                  "lab" :: "create" :: Nil, "Lab Create",                               signedIn)) ::
			Menu(Loc("Lab Create Classification",   "lab" :: "create-classification" :: Nil, "Lab Create Classification", labSignedIn)) ::
			Menu(Loc("Lab Create Occurrence",       "lab" :: "create-occurrence" :: Nil, "Lab Create Occurrence",         labSignedIn)) ::
			Menu(Loc("Lab Event",                   "lab" :: "event" :: Nil, "Lab Event",                                 labSignedIn)) ::
			Menu(Loc("Lab Group",                   "lab" :: "group" :: Nil, "Lab Group",                                 labSignedIn)) ::
			Menu(Loc("Lab Home",                    "lab" :: "index" :: Nil, "Lab Home",                                  labSignedIn)) ::
			Menu(Loc("Lab Info",                    "lab" :: "info" :: Nil, "Lab Info",                                   labSignedOut)) ::
			Menu(Loc("Lab Join",                    "lab" :: "join" :: Nil, "Lab Join",                                   signedIn)) ::
			Menu(Loc("Lab Location",                "lab" :: "location" :: Nil, "Lab Location",                           labSignedIn)) ::
			Menu(Loc("Lab Map",                     "lab" :: "map" :: Nil, "Lab Map",                                     labSignedIn)) ::
			Menu(Loc("Lab New",                     "lab" :: "new" :: Nil, "Lab New",                                     signedIn)) ::
			Menu(Loc("Lab Occurrence",              "lab" :: "occurrence" :: Nil, "Lab Occurrence",                       labSignedIn)) ::
			Menu(Loc("Lab Search",                  "lab" :: "search" :: Nil, "Lab Search",                               labSignedIn)) ::
			Menu(Loc("Lab Settings",                "lab" :: "settings" :: Nil, "Lab Settings",                           signedIn)) ::
			Menu(Loc("Lab Classified Taxon",        "lab" :: "classified-taxon" :: Nil, "Lab Classified Taxon",           labSignedIn )) ::
			Menu(Loc("Lab Taxon",                   "lab" :: "taxon" :: Nil, "Lab Taxon",                                 labSignedIn )) ::
			Menu(Loc("User Home",                   "user" :: "index" :: Nil, "User Home",                                signOutIfRestricted, signedIn)) ::
			Menu(Loc("User Info",                   "user" :: "info" :: Nil, "User Info",                                 signOutIfRestricted, signedIn)) ::
			Menu(Loc("User Preferences",            "user" :: "preferences" :: Nil, "User Preferences",                   signOutIfRestricted, signedIn)) ::
			Menu(Loc("User Services",               "user" :: "services" :: Nil, "User Services",                         signOutIfRestricted, signedIn)) ::
			Menu(Loc("Wiki Home",                   "wiki" :: "index" :: Nil, "Wiki Home",                                signOutIfRestricted)) ::
			Nil
		LiftRules.setSiteMap(SiteMap(entries:_*))
		
		DB.defineConnectionManager(DefaultConnectionIdentifier, DevDbProvider)
		
		Schemifier.schemify(true, Schemifier.infoF _,
			Model.AdditionalProperty,
			Model.AdditionalPropertyBundle,
			Model.Classification,
			Model.ClassifiedTaxon,
			Model.Event,
			Model.Group,
			Model.GroupPost,
			Model.GroupRequest,
			Model.GroupUser,
			Model.GroupUserRequest,
			Model.Location,
			Model.MediaFile,
			Model.MediaFileBundle,
			Model.MobileDevice,
			Model.Occurrence,
			Model.Person,
			Model.Post,
			Model.Taxon,
			Model.TaxonomyNode,
			Model.Source,
			Model.TransformedMediaFile,
			Model.User
    	)
		
		LiftRules.addToPackages("edu.unl.biofinity.site")
		LiftRules.maxMimeSize = 10 * 1024 * 1024
		LiftRules.maxMimeFileSize = LiftRules.maxMimeSize
		LiftRules.useXhtmlMimeType = false

		TwitterBioBlitzImporterScheduler.start()
		
		LiftRules.dispatch.prepend{
			case Req("crossdomain" :: Nil, "xml", GetRequest) => () => 
				Full(XmlResponse(
					<cross-domain-policy>
						<allow-access-from domain="*.cooliris.com" secure="false" />
					</cross-domain-policy>
				))
			case Req("news" :: Nil, "rss", GetRequest) => () =>
				Full(PlainTextResponse("<?xml version=\"1.0\"?>" + Global.renderRSSNews().mkString, ("Content-Type", "application/rss+xml") :: Nil, 200))
			case Req("data" :: "media" :: Nil, "rss", GetRequest) => () => FeedManager.generateFeed
		}
		LiftRules.dispatch.append {
			case r @ Req("bioblitz"                                         :: _, _, _) =>           () => Service.BioBlitz.redirectToGroup(r)
			case r @ Req("BioBlitz"                                         :: _, _, _) =>           () => Service.BioBlitz.redirectToGroup(r)
			case r @ Req("bioblitz-info"                                    :: _, _, _) =>           () => Full(RedirectResponse("/info/bioblitz"))
			case r @ Req("service" :: "authentication" :: "submitRequest"   :: _, _, _) =>           () => Service.Authentication.submitRequest(r)
			case r @ Req("service" :: "authentication" :: "processResponse" :: _, _, _) =>           () => Service.Authentication.processResponse(r)
			case r @ Req("service" :: "authentication" :: "signOut"         :: _, _, _) =>           () => Service.Authentication.signOut(r)
			case r @ Req("service" :: "classification" :: "read"            :: _, _, GetRequest) =>  () => Service.Classification.read(r)
			case r @ Req("service" :: "event"          :: "read"            :: _, _, GetRequest) =>  () => Service.Event.read(r)
			case r @ Req("service" :: "location"       :: "locationsJSON"   :: _, _, GetRequest) =>  () => Service.Location.locationsJSON(r)
			case r @ Req("service" :: "location"       :: "read"            :: _, _, GetRequest) =>  () => Service.Location.read(r)
			case r @ Req("service" :: "media-file"     :: "read"            :: _, _, GetRequest) =>  () => Service.MediaFile.read(r)
			case r @ Req("service" :: "mobile-device"  :: "read"            :: _, _, GetRequest) =>  () => Service.MobileDevice.read(r)
			case r @ Req("service" :: "occurrence"     :: "create"          :: _, _, PostRequest) => () => Service.Occurrence.create(r)
			case r @ Req("service" :: "occurrence"     :: "occurrencesCSV"  :: _, _, GetRequest) =>  () => Service.Occurrence.occurrencesCSV(r)
			case r @ Req("service" :: "occurrence"     :: "occurrencesKML"  :: _, _, GetRequest) =>  () => Service.Occurrence.occurrencesKML(r)
			case r @ Req("service" :: "occurrence"     :: "read"            :: _, _, GetRequest) =>  () => Service.Occurrence.read(r)
			case r @ Req("service" :: "search"         :: "occurrencesJSON" :: _, _, GetRequest) =>  () => Service.Search.occurrencesJSON(r)
			case r @ Req("service" :: "taxon"          :: "read"            :: _, _, GetRequest) =>  () => Service.Taxon.read(r)
			case r @ Req("service" :: "taxon"          :: "names"           :: _, _, GetRequest) =>  () => Service.Taxon.names(r)
			case r @ Req("service" :: "taxon"          :: "occurrences"     :: _, _, GetRequest) =>  () => Service.Taxon.occurrences(r)
			case r @ Req("service" :: "taxon"          :: "occurrencesJSON" :: _, _, GetRequest) =>  () => Service.Taxon.occurrencesJSON(r)
			case r @ Req("service" :: "taxon"          :: "ranks"           :: _, _, GetRequest) =>  () => Service.Taxon.ranks(r)
			case r @ Req("service" :: "user"           :: "read"            :: _, _, GetRequest) =>  () => Service.User.read(r)
		}
	}
}

/* this should be reconfigured to use JNDI to get a java.sql.Datasource for connections */
object DevDbProvider extends ConnectionManager {
	Class.forName("com.mysql.jdbc.jdbc2.optional.MysqlDataSource")
	
	def newConnection(name : ConnectionIdentifier) = { 
		try {
			Full(DriverManager.getConnection("jdbc:mysql://localhost:3306/Biofinity", "Biofinity", "Biofinity"))
		} catch { 
			case e: Exception => e.printStackTrace; Empty
		}
	}
	
	def releaseConnection(conn: Connection) = {
		conn.close()
	}
}