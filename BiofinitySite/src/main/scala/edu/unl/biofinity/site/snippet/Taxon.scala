package edu.unl.biofinity.site.snippet

import edu.unl.biofinity.api.{model => Model}
import edu.unl.biofinity.api.{service => Service}

import java.sql.PreparedStatement
import java.sql.ResultSet
import java.text.SimpleDateFormat

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

class Taxon {
	def init: NodeSeq = {
		Model.Taxon.currentTaxon(Model.Taxon.find(S.param("ID").openOr(S.param("TaxonID").openOr(-1))) openOr null)
		
		Text("")
	}
	
	def renderTaxon(xhtml: NodeSeq): NodeSeq = {
		var children: List[Model.Taxon] = null
		
		def initChildren() = {
		  	if (null == children) {
				children = {
					val baseSQL = "select t.entity_id, t.name, t.rank from taxonomy_nodes n inner join taxons t on n.taxon_id = t.entity_id inner join classifications c on n.classification_id = c.entity_id inner join sources s on c.source_id = s.entity_id"
					if (null == Model.Taxon.currentTaxon.is) {
						Model.Taxon.findAllByPreparedStatement({ db =>
							if (S.attr("group").isEmpty) {
								val preparedStatement: PreparedStatement = db.connection.prepareStatement(baseSQL + " where s.source_type = 0                    and n.root = 1 group by t.entity_id order by t.name")
								preparedStatement
							} else {
								val preparedStatement: PreparedStatement = db.connection.prepareStatement(baseSQL + " where (c.source_id = ? or c.source_id = ?) and n.root = 1 group by t.entity_id order by t.name")
								preparedStatement.setLong(1, (Model.User.currentGroup.is.publicSource.obj openOr Model.Source).entityID)
								preparedStatement.setLong(2, (Model.User.currentGroup.is.privateSource.obj openOr Model.Source).entityID)
								preparedStatement
							}
						})
					} else {
						Model.Taxon.findAllByPreparedStatement({ db =>
							if (S.attr("group").isEmpty) {
								val preparedStatement: PreparedStatement = db.connection.prepareStatement(baseSQL + " where s.source_type = 0                    and n.entity_id in (select child_id from taxonomy_nodes where taxon_id = ?) group by t.entity_id order by t.name") 
								preparedStatement.setString(1, Model.Taxon.currentTaxon.entityID.toString)
								preparedStatement
							} else {
								val preparedStatement: PreparedStatement = db.connection.prepareStatement(baseSQL + " where (c.source_id = ? or c.source_id = ?) and n.entity_id in (select child_id from taxonomy_nodes where taxon_id = ?) group by t.entity_id order by t.name") 
								preparedStatement.setLong(1, (Model.User.currentGroup.is.publicSource.obj openOr Model.Source).entityID)
								preparedStatement.setLong(2, (Model.User.currentGroup.is.privateSource.obj openOr Model.Source).entityID)
								preparedStatement.setString(3, Model.Taxon.currentTaxon.entityID.toString)
								preparedStatement
							}
						})
					}
				}
			}
		}
		
		def renderChildren(xhtml: NodeSeq) = {
		  	initChildren
			if (null != children && children.length > 0) {
				bind(
					"children",
					xhtml,
					"List" -> {(nodeSeq: NodeSeq) => {
						val childrenNodeSeq: NodeSeq = children.sort((s,t) => t.name.compareTo(s.name) > 0).flatMap(child => {
							bind(
								"child",
								nodeSeq,
								"NameLink" -> <a href={"taxon?ID=" + child.entityID}>{child.name}</a>,
								"Rank" -> child.rank
							)
						})
						childrenNodeSeq
					}}
				)
			} else {
				Text("")
			}
		}
		
		if (null == Model.Taxon.currentTaxon.is) {
			bind(
				"taxon",
				xhtml,
				"Children" -> {(nodeSeq: NodeSeq) => {
					renderChildren(nodeSeq)
				}},
				"Classifications" -> Text(""),
				"DownloadCSVLink" -> Text(""),
				"DownloadKMLLink" -> Text(""),
				"ID" -> Text(""),
				"MapLink" -> Text(""),
				"Name" -> Text("root"),
				"NameLink" -> Text(""),
				"Occurrences" -> Text(""),
				"Rank" -> Text("Taxonomy")
			)
		} else {
			var classifications: List[Model.Classification] = null
			
			def initClassifications() = {
				if (null == classifications) {
					classifications = Model.Classification.findAllByPreparedStatement({database =>
						if (S.attr("group").isEmpty) {
							val preparedStatement: PreparedStatement = database.connection.prepareStatement("select c.entity_id, c.name, c.source_id from classifications c inner join taxonomy_nodes n on n.classification_id = c.entity_id inner join taxons t on n.taxon_id = t.entity_id inner join sources s on c.source_id = s.entity_id where n.leaf = 1 and n.taxon_id = ? and s.source_type = 0")
							preparedStatement.setLong(1, Model.Taxon.currentTaxon.entityID)
							preparedStatement
						} else {
							val preparedStatement: PreparedStatement = database.connection.prepareStatement("select c.entity_id, c.name, c.source_id from classifications c inner join taxonomy_nodes n on n.classification_id = c.entity_id inner join taxons t on n.taxon_id = t.entity_id inner join sources s on c.source_id = s.entity_id where n.leaf = 1 and n.taxon_id = ? and (s.entity_id = ? or s.entity_id = ?)")
							preparedStatement.setLong(1, Model.Taxon.currentTaxon.entityID)
							preparedStatement.setLong(2, (Model.User.currentGroup.is.publicSource.obj openOr Model.Source).entityID)
							preparedStatement.setLong(3, (Model.User.currentGroup.is.privateSource.obj openOr Model.Source).entityID)
							preparedStatement
						}
					})
				}
			}
			
			bind(
				"taxon",
				xhtml,
				"Children" -> {(nodeSeq: NodeSeq) => {
					renderChildren(nodeSeq)
				}},
				"Classifications" -> {(nodeSeq: NodeSeq) => {
					initClassifications
					if (null != classifications && classifications.length > 0) {
						bind(
							"classifications",
							nodeSeq,
							"List" -> {(listNodeSeq: NodeSeq) => {
								val classificationsNodeSeq: NodeSeq = classifications.flatMap(classification => {
									Model.Classification.currentClassification(classification)
									new Classification().renderClassification(listNodeSeq)
								})
								Model.Classification.currentClassification(null)
								classificationsNodeSeq
							}}
						)
					} else {
						Text("")
					}
				}},
				"DownloadCSVLink" -> {(nodeSeq: NodeSeq) => {
					val taxon = Model.Taxon.currentTaxon.is
					val group = !S.attr("group").isEmpty
					
					def setOccurrences() = {
						var classificationIDs: List[Long] = List()
						DB.use(DefaultConnectionIdentifier) {connection =>
							val baseSQL = "select c.entity_id from classifications c inner join taxonomy_nodes n on n.classification_id = c.entity_id inner join taxons t on n.taxon_id = t.entity_id inner join sources s on c.source_id = s.entity_id"
							var fullSQL = ""
							if (!group) {
								fullSQL = baseSQL + " where n.taxon_id = ? and s.source_type = 0"
							} else {
								fullSQL = baseSQL + " where n.taxon_id = ? and (c.source_id = ? or c.source_id = ?)"
							}
							
							DB.prepareStatement(fullSQL, connection) { preparedStatement =>
							  	preparedStatement.setLong(1, taxon.entityID)
							  	if (group) {
							  		preparedStatement.setLong(2, Model.User.currentGroup.is.publicSource.obj.open_!.entityID)
									preparedStatement.setLong(3, Model.User.currentGroup.is.privateSource.obj.open_!.entityID)
							  	}
								val results: ResultSet = preparedStatement.executeQuery()
								while (results.next()) {
									val entityID = results.getLong("ENTITY_ID")
									classificationIDs = classificationIDs ::: List(entityID)
								}
							}
						}
						val classicationIDsAsString: String = classificationIDs.map(_.toString).foldLeft[String]("-1")(_ + "," + _)
						var occurrenceIDs: List[Long] = List()
						DB.use(DefaultConnectionIdentifier) {connection =>
							val baseSQL = "select o.entity_id from occurrences o inner join classifications c on o.classification_id = c.entity_id inner join sources s on o.source_id = s.entity_id"
							var fullSQL = ""
							if (!group) {
								fullSQL = baseSQL + " where o.classification_id in (" + classicationIDsAsString + ") and s.source_type = 0"
							} else {
								fullSQL = baseSQL + " where o.classification_id in (" + classicationIDsAsString + ") and (o.source_id = ? or o.source_id = ?)"
							}
							
							DB.prepareStatement(fullSQL, connection) { preparedStatement =>
							  	if (group) {
							  		preparedStatement.setLong(1, Model.User.currentGroup.is.publicSource.obj.open_!.entityID)
									preparedStatement.setLong(2, Model.User.currentGroup.is.privateSource.obj.open_!.entityID)
							  	}
								val results: ResultSet = preparedStatement.executeQuery()
								while (results.next()) {
									val entityID = results.getLong("ENTITY_ID")
									occurrenceIDs = occurrenceIDs ::: List(entityID)
								}
							}
						}
						Model.Occurrence.currentOccurrenceIDs(occurrenceIDs)
					}
					
					SHtml.link("/service/occurrence/occurrencesCSV", () => setOccurrences, nodeSeq)
				}},
				"DownloadKMLLink" -> {(nodeSeq: NodeSeq) => {
					val taxon = Model.Taxon.currentTaxon.is
					val group = !S.attr("group").isEmpty

					def setOccurrences() = {
						var classificationIDs: List[Long] = List()
						DB.use(DefaultConnectionIdentifier) {connection =>
							val baseSQL = "select c.entity_id from classifications c inner join taxonomy_nodes n on n.classification_id = c.entity_id inner join taxons t on n.taxon_id = t.entity_id inner join sources s on c.source_id = s.entity_id"
							var fullSQL = ""
							if (!group) {
								fullSQL = baseSQL + " where n.taxon_id = ? and s.source_type = 0"
							} else {
								fullSQL = baseSQL + " where n.taxon_id = ? and (c.source_id = ? or c.source_id = ?)"
							}

							DB.prepareStatement(fullSQL, connection) { preparedStatement =>
							  	preparedStatement.setLong(1, taxon.entityID)
							  	if (group) {
							  		preparedStatement.setLong(2, Model.User.currentGroup.is.publicSource.obj.open_!.entityID)
									preparedStatement.setLong(3, Model.User.currentGroup.is.privateSource.obj.open_!.entityID)
							  	}
								val results: ResultSet = preparedStatement.executeQuery()
								while (results.next()) {
									val entityID = results.getLong("ENTITY_ID")
									classificationIDs = classificationIDs ::: List(entityID)
								}
							}
						}
						val classicationIDsAsString: String = classificationIDs.map(_.toString).foldLeft[String]("-1")(_ + "," + _)
						var occurrenceIDs: List[Long] = List()
						DB.use(DefaultConnectionIdentifier) {connection =>
							val baseSQL = "select o.entity_id from occurrences o inner join classifications c on o.classification_id = c.entity_id inner join sources s on o.source_id = s.entity_id"
							var fullSQL = ""
							if (!group) {
								fullSQL = baseSQL + " where o.classification_id in (" + classicationIDsAsString + ") and s.source_type = 0"
							} else {
								fullSQL = baseSQL + " where o.classification_id in (" + classicationIDsAsString + ") and (o.source_id = ? or o.source_id = ?)"
							}

							DB.prepareStatement(fullSQL, connection) { preparedStatement =>
							  	if (group) {
							  		preparedStatement.setLong(1, Model.User.currentGroup.is.publicSource.obj.open_!.entityID)
									preparedStatement.setLong(2, Model.User.currentGroup.is.privateSource.obj.open_!.entityID)
							  	}
								val results: ResultSet = preparedStatement.executeQuery()
								while (results.next()) {
									val entityID = results.getLong("ENTITY_ID")
									occurrenceIDs = occurrenceIDs ::: List(entityID)
								}
							}
						}
						Model.Occurrence.currentOccurrenceIDs(occurrenceIDs)
					}

					SHtml.link("/service/occurrence/occurrencesKML", () => setOccurrences, nodeSeq)
				}},
				"ExternalLinks" -> {(nodeSeq: NodeSeq) => {
					bind(
						"external-link",
						nodeSeq,
						"BOLD" -> {(nodeSeq: NodeSeq) => <a href={"http://www.boldsystems.org/views/taxbrowser.php?taxon=" + Model.Taxon.currentTaxon.is.name}>{nodeSeq}</a>},
						"EOLList" -> {(nodeSeq: NodeSeq) => {
							try {
								val results: Elem = XML.load("http://www.eol.org/search.xml?q=" + Model.Taxon.currentTaxon.is.name)
								val linksNodeSeq: NodeSeq = (results \\ "taxon-concept").flatMap((node: Node) => {
									bind(
										"external-link",
										nodeSeq,
										"EOL" -> {
											<a href={"http://www.eol.org/pages/" + (node \\ "id").first.text}>
											{ (node \\ "canonical-form").first.text }
											</a>
										}
									)
								})
								linksNodeSeq
							} catch {case e:Exception => Text("")}
						}},
						"GBIF" -> {(nodeSeq: NodeSeq) => <a href={"http://data.gbif.org/search/" + Model.Taxon.currentTaxon.is.name}>{nodeSeq}</a>},
						"Lifemapper" -> {(nodeSeq: NodeSeq) => <a href={"http://www.lifemapper.org/species/" + Model.Taxon.currentTaxon.is.name}>{nodeSeq}</a>},
						"Microscope" -> {(nodeSeq: NodeSeq) => <a href={"http://starcentral.mbl.edu/microscope/portal.php?pagetitle=classification&SEARCHNAME=" + Model.Taxon.currentTaxon.is.name}>{nodeSeq}</a>},
						"NCBI" -> {(nodeSeq: NodeSeq) => <a href={"http://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?name=" + Model.Taxon.currentTaxon.is.name}>{nodeSeq}</a>},
						"ShowEOLLinks" -> {(nodeSeq: NodeSeq) => <a href="javascript: showEOLLinks();">{nodeSeq}</a>},
						"TOL" -> {(nodeSeq: NodeSeq) => <a href={"http://tolweb.org/tree?group=" + Model.Taxon.currentTaxon.is.name}>{nodeSeq}</a>},
						"UBIO" -> {(nodeSeq: NodeSeq) => <a href={"http://www.ubio.org/browser/search.php?search_all=" + Model.Taxon.currentTaxon.is.name}>{nodeSeq}</a>}
					)
				}},
				"ID" -> Model.Taxon.currentTaxon.is.entityID,
				"MapLink" -> {(nodeSeq: NodeSeq) => {<a href={"map?TaxonID=" + Model.Taxon.currentTaxon.is.entityID}>{nodeSeq}</a>}},
				"MediaURL" -> {
					val group: String =
						if (S.attr("group").isEmpty) {
							""
						} else {
							"&Group=true"
						}
					Text(S.hostAndPath + "/data/media.rss?ID=" + Model.Taxon.currentTaxon.is.entityID + group)
				},
				"Name" -> Model.Taxon.currentTaxon.is.name,
				"NameLink" -> <a href={"taxon?ID=" + Model.Taxon.currentTaxon.is.entityID}>{Model.Taxon.currentTaxon.is.name}</a>,
				"Occurrences" -> {(nodeSeq: NodeSeq) => {
					initClassifications
					
					val occurrences = classifications.flatMap(_.occurrences).filter(occurrence => Source.sourceFilter(occurrence.source.obj.openOr(null)))
					if (null != occurrences && occurrences.length > 0) {
						bind(
							"occurrences",
							nodeSeq,
							"List" -> {(listNodeSeq: NodeSeq) => {
								val occurrencesNodeSeq: NodeSeq = occurrences.flatMap(occurrence => {
									Model.Occurrence.currentOccurrence(occurrence)
									new Occurrence().renderOccurrence(listNodeSeq)
								})
								Model.Occurrence.currentOccurrence(null)
								occurrencesNodeSeq
							}}
						)
					} else {
						Text("")
					}
				}},
				"Rank" -> Model.Taxon.currentTaxon.is.rank
			)
		}
	}
	
	def renderScripts(xhtml: NodeSeq): NodeSeq = {
		def showEOLLinks() = {
			SetHtml("taxon-external-links-EOL", <lift:embed what="taxon-external-links-EOL" />)
		}
		
		Script(
			Function(
				"showEOLLinks",
				Nil,
				SHtml.ajaxInvoke(showEOLLinks)._2
			)
		)
	}
	
	def renderOccurrencesJSON: NodeSeq = {
		val taxon: String =
			if (null == Model.Taxon.currentTaxon.is) {
				""
			} else {
				"TaxonID=" + Model.Taxon.currentTaxon.is.entityID.toString
			}
		val group: String =
			if (S.attr("group").isEmpty) {
				""
			} else {
				"&Group=true"
			}
		<script type="text/javascript" src={S.hostAndPath + "/service/taxon/occurrencesJSON?" + taxon + group}></script>
	}
}