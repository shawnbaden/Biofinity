package edu.unl.biofinity.api.service

import edu.unl.biofinity.api.{model => Model}

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

import _root_.org.openid4java.consumer.{ConsumerManager, VerificationResult};
import _root_.org.openid4java.discovery.DiscoveryInformation;
import _root_.org.openid4java.message.{AuthRequest, ParameterList, Message, MessageExtension};
import _root_.org.openid4java.message.ax.{FetchRequest, FetchResponse, AxMessage};

object Authentication {
	val mgr: ConsumerManager = new ConsumerManager
	object openIDDiscoveryInformation extends RequestVar[DiscoveryInformation](null)
	object redirectURL extends SessionVar[String]("/")
	
	def submitRequest(r: Req) = {
		val endpoint = r.params("endpointUrl")(0)
		
		// get the authentication service endpoint url response
		val discoveries = mgr.discover(endpoint)
		val info: DiscoveryInformation = mgr.associate(discoveries)
		openIDDiscoveryInformation(info)
		
		// get the authentication request
		val req: AuthRequest = mgr.authenticate(info, S.hostAndPath+"/service/authentication/processResponse")
		
		// add the exchange attributes to request
		val fetch_req: FetchRequest = FetchRequest.createFetchRequest();
		fetch_req.addAttribute("FirstName", "http://axschema.org/namePerson/first", true);
		fetch_req.addAttribute("LastName", "http://axschema.org/namePerson/last", true);
		fetch_req.addAttribute("Email", "http://schema.openid.net/contact/email", true);
		
		fetch_req.setCount("FirstName", 1)
		fetch_req.setCount("LastName", 1)
		fetch_req.setCount("Email", 1)
		
		req.addExtension(fetch_req);
		
		Log.info("Request URL: "+req.getDestinationUrl(true))
 
		// generate the authentication request URL and redirect to it
		Full(RedirectResponse(req.getDestinationUrl(true), S responseCookies :_*))
	} 
	
	def processResponse(r: Req) = {
		val paramMap = new java.util.HashMap[String, String]
		r.params.foreach(e => paramMap.put(e._1, e._2.firstOption getOrElse null))
		
		val response =	new ParameterList(paramMap);
		var redirectPath: String = "/"
		
		val result: VerificationResult = mgr.verify(S.hostAndPath+"/service/authentication/processResponse", response, openIDDiscoveryInformation)
		
		if ( result.getVerifiedId != null ) {
			try {
				val authRes: Message = result.getAuthResponse;
				val msgExt: MessageExtension = authRes.getExtension(AxMessage.OPENID_NS_AX);
				
				if (msgExt.isInstanceOf[FetchResponse]) {
					val fetchResp: FetchResponse = msgExt.asInstanceOf[FetchResponse]
					val openID = result.getVerifiedId.getIdentifier
					var user: Model.User = Model.User.find(By(Model.User.openID, openID)) openOr null
					if (null == user) {
						val email = fetchResp.getAttributeValue("Email")
						user = Model.User.find(By(Model.User.email, email)) openOr null
						if (null == user) {
							val person = Model.Person.create
							person.firstName(fetchResp.getAttributeValue("FirstName"))
							person.lastName(fetchResp.getAttributeValue("LastName"))
							person.save
							user = Model.User.create
							user.person(person)
							user.openID(openID)
							user.email(email)
							user.save
						} else {
							user.openID(openID)
							user.save
						}
					}
					
					redirectPath = signIn(openID)
				}
			} catch {
				case e: Exception => Log.info("could not get attribute exchange message"); e.printStackTrace
			}
		}
		
		// redirect to wiki login service
		val host = S.hostName + ":8080"
		//val host = "localhost:8080"
		val encodedOpenID = Helpers.urlEncode(Model.User.currentUser.is.openID.is.toString)
		val encodedRedirectURL = Helpers.urlEncode(S.hostAndPath+redirectPath)
		val wikiLoginURL = "http://" + host + "/BiofinityWikiServer/resources/authentication/biologin?openId=" + encodedOpenID + "&returnPage=" + encodedRedirectURL
		Log.info("Wiki Login URL="+wikiLoginURL)

		Full(RedirectResponse(wikiLoginURL, S responseCookies :_*))		
	}
	
	def signOut(r: Req) = {
		Model.User.currentUser(null)
		Model.User.currentGroup(null)
		
		// redirect to wiki logout service
		val host = S.hostName + ":8080"
		//val host = "localhost:8080"
		val wikiLogoutURL = "http://" + host + "/BiofinityWikiServer/resources/authentication/logout?returnPage=" + Helpers.urlEncode(S.hostAndPath+redirectURL)
		Log.info("Wiki Logout URL="+wikiLogoutURL)

		Full(RedirectResponse(wikiLogoutURL, S responseCookies :_*))
	}
	
	private def signIn(openID: String): String = {
		val user: Model.User = Model.User.find(By(Model.User.openID, openID)) openOr null
		if (null != user) {
			Model.User.currentUser(user)
			if (Model.User.groupMember_?) {
				Model.User.currentGroup(Model.User.currentUser.groups.first)
			} else {
				Model.User.currentGroup(null)
			}
		}
		
		redirectURL
	}
}