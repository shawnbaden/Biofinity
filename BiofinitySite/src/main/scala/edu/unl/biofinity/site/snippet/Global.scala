package edu.unl.biofinity.site.snippet

import edu.unl.biofinity.api.{model => Model}
import edu.unl.biofinity.api.{service => Service}

import java.text.NumberFormat
import java.text.SimpleDateFormat
import java.util.Date

import net.liftweb._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.js._
import net.liftweb.http.js.JE._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.S._
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.util.Helpers._

import org.joda.time.DateTime

import scala.util._
import scala.xml._

class Global {

	private val menuPages: List[(String, String)] =
		("/", "Home") ::
		("/lab/", if ( Model.User.currentGroup.is != null ) { Model.User.currentGroup.is.name+" Database"} else {"Lab Database"}) ::
		("/data/", "Web Database") ::
		("/wiki/", "Intelligent Wiki") ::
		Nil

	def renderSearchForm(xhtml: NodeSeq): NodeSeq = {
		if (S.uri.startsWith("/wiki")) {
			<form method="get" action="/wiki" style="display: inline-block;">{ xhtml }</form>
		} else if (null != Model.User.currentGroup.is) {
			<form method="post" action="/lab/search" style="display: inline-block;">{ xhtml }</form>
		} else {
			<form method="post" action="/data/search" style="display: inline-block;">{ xhtml }</form>
		}
	}

	def renderMenu(xhtml: NodeSeq): NodeSeq = { 
		val uri = S.uri
		menuPages.flatMap { s => 
			bind(
				"g",
				xhtml,
				"menuItem" -> {
					<li class={if (uri.equals(s._1)) "active" else if (!(s._1.equals("/")) & (uri.startsWith(s._1)) ) "active" else "" }>
						<a href={s._1}>{s._2}</a>
					</li>
				}
			)
		}
	}
	
	def renderLabMenu(xhtml: NodeSeq): NodeSeq = {
		if (Model.User.groupMember_? && Model.User.currentUser.is.userType != Model.UserType.Restricted) {
			<div id="labmenuitem" onclick="toggleLabOptions()">
				My Labs
				<div id="labmenuoptions">
					<lift:embed what="lab/options"/>
				</div>
			</div>
		} else {
			Text("")
		}
	}
	
 	def renderSignIn = {
		val redirect =
			if ( S.uri.startsWith("/lab")) {
				"/lab/"
			} else if (S.uri.toUpperCase.startsWith("/HDZ/AMPHIBIAN")) {
				"/HDZ/amphibian"
			} else {
				S.uri
			}
		
		/* the profile pages should be exempt from login redirection */
		if ( redirect.indexOf("/user") < 0 ) Service.Authentication.redirectURL(redirect)
		
		if (!Model.User.signedIn_?) {
			<div id="loginitem">
				<form method="post" action="/service/authentication/submitRequest">
					<input type="hidden" name="endpointUrl" value="https://www.google.com/accounts/o8/id"/>
					<a href="#" onclick="parentNode.submit()">Log In</a>
				</form>
			</div>
		} else {
			if (Model.User.currentUser.is.userType == Model.UserType.Restricted) {
				Text("")
			} else {
				<div id="accountitem" onclick="toggleAccountOptions()">
					Account
					<div id="account_options">
					<ul>
					{
						val name = (Model.User.currentUser.person.obj openOr Model.Person).fullName
						val displayName =
							if (null == name || "" == name) {
								"User Account"
							} else {
								name
							}
					<li> {
						if (Model.User.currentUser.is.userType == Model.UserType.Restricted) {
							{displayName}
						} else {
							<a href="/user/">Manage Account</a>
						}
					} </li>
					}
						<li><a href="/service/authentication/signOut">Log Out</a></li>
					</ul>
					</div>
				</div>
			}
		}
	}
	
	private val infoMenuPages: List[(String, String)] =
		("/info/", "Overview") ::
		("/info/people", "People") ::
		("/info/framework", "Framework") ::
		("/info/publications", "Publications") ::
		("/info/additional", "Additional Resources") ::
		Nil
	
	def renderInfoMenu(xhtml: NodeSeq): NodeSeq = { 
		val uri = S.uri
		infoMenuPages.flatMap { s => 
			bind(
				"i",
				xhtml,
				"menuItem" -> {
					<li>
						<a class={if (uri.equals(s._1)) "active" else "" } href={s._1}>{s._2}</a>
					</li>
				}
			)
		}
	}
	
	def renderPosts(xhtml: NodeSeq): NodeSeq = {
		val post: Model.Post = Model.Post.find(S.param("ID") openOr -1) openOr null
		val posts: List[Model.Post] = post match {
			case (null) => {
				Model.Post.findAll().sort((a,b) => a.published.compareTo(b.published) > 0)
			}
			case _ => {
				List(post)
			}
		}
		posts.flatMap(post =>
			bind(
				"post",
				xhtml,
				"Title" -> <span class="news_title">{post.title}</span>,
				"Description" -> post.description
			)
		)
	}

	def renderWikiFrame(xhtml: NodeSeq): NodeSeq = {
		val query = S.param("search-query") openOr ""
		val pageId = S.param("WikiPageID") openOr ""
		val host = S.hostName + ":8080"
		val path = "/BiofinityWiki"
		val searchParams = "?mode=search&terms=" + query
		val wikiBaseURL = "http://" + host + path

		if (query == "") {
			val url = 
				if (pageId != "")
					wikiBaseURL + "?mode=view&pageid=" + pageId
				else 
					wikiBaseURL

			<iframe id="WikiFrame" src={url} scrolling="yes" frameborder="0" style="overflow: visible; width: 100%; height: 800px;" />
		} else {
			val wikiSearchURL = wikiBaseURL + searchParams
			<iframe id="WikiFrame" src={wikiSearchURL} scrolling="yes" frameborder="0" style="overflow: visible; width: 100%; height: 800px;" />
		}
	}
}

object Global {
	val NUMBER_FORMAT = NumberFormat.getInstance()
	
	val ISO_8601_DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd")
	
	val ISO_8601_2004_E = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")
	
	val RFC_3339_DATE_FORMAT = new SimpleDateFormat("dd MMM yyyy HH:mm:ss Z")
	
	val US_DATE_FORMAT = new SimpleDateFormat("MM/dd/yyyy")
	
	val HOUR_DATE_FORMAT = new SimpleDateFormat("h")
	
	val MINUTE_DATE_FORMAT = new SimpleDateFormat("mm")
	
	val AM_PM_DATE_FORMAT = new SimpleDateFormat("a")

	val FRIENDLY_DATE_FORMAT = new SimpleDateFormat("MMMMM d, yyyy h:mm aaa")
	
	val FRIENDLY_DATE_FORMAT_NO_TIME = new SimpleDateFormat("MMMMM d, yyyy")
	
	def renderRSSNews(): Node = {
		val rfc3339 = new SimpleDateFormat("dd MMM yyyy HH:mm:ss Z");
		val now: Date = new Date()
		val posts = Model.Post.findAll(By_>(Model.Post.published, new DateTime(now).minusDays(30).toDate())).sort((a,b) => a.published.compareTo(b.published) > 0)
		<rss version="2.0">
			<channel>
				<title>The Biofinity Project</title>
				<link>{S.hostAndPath.substring(0, S.hostAndPath.length - S.contextPath.length)}</link>
				<description>Recent news about The Biofinity Project.</description>
				<language>en-us</language>
				<pubDate>{rfc3339.format(now)}</pubDate>
				<lastBuildDate>{rfc3339.format(now)}</lastBuildDate>
				<docs>{S.hostAndPath}/news.rss</docs>
				<ttl>5</ttl>
				{for (post <- posts) yield
				<item>
					<title>{post.title}</title>
					<link>{S.hostAndPath}/info/news?ID={post.entityID}</link>
					<description>{post.description}</description>
					<pubDate>{rfc3339.format(post.published.is)}</pubDate>
				</item>
				}
			</channel>
		</rss>
	}
}