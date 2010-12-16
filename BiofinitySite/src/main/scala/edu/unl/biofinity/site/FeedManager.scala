package edu.unl.biofinity.site

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

import scala.xml._

import java.sql.PreparedStatement
import java.text.SimpleDateFormat

object FeedManager {
	val item_template: Node =
		<item>
			<title><i:title/></title>
			<media:description><i:description/></media:description>
			<i:content/>
				<i:thumbnail/>
			<link><i:link/></link>
		</item>
		
	val response_template:Node =
		<channel>
			<m:photos/>
		</channel>
	
	def generateFeed: Box[LiftResponse] = {
		val taxon: Model.Taxon = Model.Taxon.find(S.param("ID") openOr "-1") openOr null
		if (null == taxon) {
			Full(
				PlainTextResponse(
					"<?xml version=\"1.0\"?><rss version=\"2.0\" xmlns:media=\"http://search.yahoo.com/mrss/\" xmlns:atom=\"http://www.w3.org/2005/Atom\">" +
					bind(
						"m",
						response_template,
						"photos" -> Text(""),
						"videos" -> Text("")
					).toString +
					"</rss>"
				)
			)
		} else {
			val group: Boolean = !S.param("Group").isEmpty
			val classifications = Model.Classification.findAllByPreparedStatement({database =>
				val baseSQL = "select c.entity_id, c.source_id, c.name from classifications c inner join taxonomy_nodes n on n.classification_id = c.entity_id inner join taxons t on n.taxon_id = t.entity_id inner join sources s on c.source_id = s.entity_id"
				if (!group) {
					val preparedStatement: PreparedStatement = database.connection.prepareStatement(baseSQL + " where n.leaf = 1 and n.taxon_id = ? and s.source_type = 0")
					preparedStatement.setLong(1, taxon.entityID)
					preparedStatement
				} else {
					val preparedStatement: PreparedStatement = database.connection.prepareStatement(baseSQL + " where n.leaf = 1 and n.taxon_id = ? and (c.source_id = ? or c.source_id = ?)")
					preparedStatement.setLong(1, taxon.entityID)
					preparedStatement.setLong(2, Model.User.currentGroup.is.publicSource.obj.open_!.entityID)
					preparedStatement.setLong(3, Model.User.currentGroup.is.privateSource.obj.open_!.entityID)
					preparedStatement
				}
			}).removeDuplicates

			val searchParameters: String = {
				if (0 < classifications.length) {
					"machine_tags=" + classifications.map(classification => {
						val name = classification.name.is
						Log.info(name)
						val nameParts = name.split(" ")
						Log.info(nameParts.length.toString)
						val queryType =
							if (nameParts.length < 3) {
								"%22taxonomy:binomial="
							} else {
								"%22taxonomy:trinomial="
							}
						queryType + nameParts.reduceRight((l, r) => l + "%20" + r) + "%22"
					}).reduceRight((l, r) => l + "," + r)
				} else {
					"machine_tags=taxonomy:" + taxon.rank.is.toLowerCase + "=" + taxon.name.is.toLowerCase
				}
			}
			val searchURL: String = "http://api.flickr.com/services/rest/?format=rest&api_key=94ad2185cd9fd855e9cb684b308b9f8f&method=flickr.photos.search&tag_mode=any&machine_tag_mode=any&" + searchParameters
			val response: Elem = XML.load(searchURL)
			Full(
				PlainTextResponse(
					"<?xml version=\"1.0\"?><rss version=\"2.0\" xmlns:media=\"http://search.yahoo.com/mrss/\" xmlns:atom=\"http://www.w3.org/2005/Atom\">" +
					bind(
						"m",
						response_template,
						"photos" -> convertFlickrStream(response)
					) +
					"</rss>"
				)
			)
		}
	}
	
	private def convertFlickrStream(flickr: Elem): NodeSeq = {
		(flickr \\ "photo").flatMap{ node: Node => {bind("i", item_template, "title" -> node \ "@title", 
									"description" -> node \ "@title",
									"link" -> ("http://www.flickr.com/photos/" + (node \ "@owner").toString + "/" + (node \ "@id").toString),
							 					"content" -> <media:content url={"http://farm" + node \ "@farm" + ".static.flickr.com/" + node \ "@server" + "/" + node \ "@id" + "_" + node \ "@secret" + "_m.jpg"}/>,
							 					"thumbnail" -> <media:thumbnail url={"http://farm" + node \ "@farm" + ".static.flickr.com/" + node \ "@server" + "/" + node \ "@id" + "_" + node \ "@secret" + "_m.jpg"}/>
		) } }
	}
}