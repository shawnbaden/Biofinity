package edu.unl.biofinity.api.controller

import edu.unl.biofinity.api.{model => Model}

import java.net.{URLConnection, URL}
import java.sql.ResultSet

import net.liftweb.http._
import net.liftweb.mapper._
import net.liftweb.util._

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpMethod;
import org.apache.commons.httpclient.methods.PutMethod;
import org.apache.commons.httpclient.methods.StringRequestEntity;

import scala.xml._ 

object WikiPage {
	def create(wikiPageType: String, entityID: Long, sourceID: Long): Long = {
		val xml = "<dataPage><type>" + wikiPageType + "</type><entityId>" + entityID + "</entityId><sourceId>" + sourceID + "</sourceId><authorId>" + Model.User.currentUser.is.entityID + "</authorId></dataPage>"
		
		try {
			val client = new HttpClient();
			val method = new PutMethod("http://localhost:8080/BiofinityWikiServer/resources/datapages");
			
			val request = new StringRequestEntity(xml, null, "UTF-8")
			method.setRequestEntity(request)
			method.setRequestHeader("Content-type", "application/xml")
			
			client.setConnectionTimeout(10000);
			client.setTimeout(10000);
			
			client.executeMethod(method);
			
			val headerAsString = method.getResponseHeader("Location").toString
			val entityIDAsString: String  = headerAsString.substring(headerAsString.lastIndexOf("=") + 1, headerAsString.length).trim
			entityIDAsString.toLong
		} catch {
			case e : Exception => -1
		}
	}
}