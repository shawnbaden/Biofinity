package edu.unl.biofinity.api.service

import edu.unl.biofinity.api.{model => Model}

import java.sql.PreparedStatement
import java.sql.ResultSet
import java.sql.SQLException

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

object Taxon {
	def read(r: Req): Box[LiftResponse] = {
		val taxon: Model.Taxon = Model.Taxon.find(S.param("ID") openOr -1) openOr null
		
		val mobileDevice: Model.MobileDevice = Model.MobileDevice.find(S.param("MobileDeviceID") openOr -1) openOr null
		val user: Model.User =
			if (null == mobileDevice) {
				null
			} else {
				mobileDevice.user.obj openOr null
			}
		val group: Model.Group =
			if (null == user) {
				null
			} else {
				val groups = user.groups
				if (groups.length > 0) {
					groups.first
				} else {
					null
				}
			}
		
		val children: List[Model.Taxon] = {
			val baseSQL = "select t.entity_id, t.name, t.rank from taxonomy_nodes n inner join taxons t on n.taxon_id = t.entity_id inner join classifications c on n.classification_id = c.entity_id inner join sources s on c.source_id = s.entity_id"
			if (null == taxon) {
				Model.Taxon.findAllByPreparedStatement({ db =>
					if (null == group) {
						val preparedStatement: PreparedStatement = db.connection.prepareStatement(baseSQL + " where s.source_type = 0                    and n.root = 1 group by t.entity_id order by t.name")
						preparedStatement
					} else {
						val preparedStatement: PreparedStatement = db.connection.prepareStatement(baseSQL + " where (c.source_id = ? or c.source_id = ?) and n.root = 1 group by t.entity_id order by t.name")
						preparedStatement.setLong(1, (group.publicSource.obj openOr Model.Source).entityID)
						preparedStatement.setLong(2, (group.privateSource.obj openOr Model.Source).entityID)
						preparedStatement
					}
				})
			} else {
				Model.Taxon.findAllByPreparedStatement({ db =>
					if (null == group) {
						val preparedStatement: PreparedStatement = db.connection.prepareStatement(baseSQL + " where s.source_type = 0                    and n.entity_id in (select child_id from taxonomy_nodes where taxon_id = ?) group by t.entity_id order by t.name") 
						preparedStatement.setString(1, taxon.entityID.toString)
						preparedStatement
					} else {
						val preparedStatement: PreparedStatement = db.connection.prepareStatement(baseSQL + " where (c.source_id = ? or c.source_id = ?) and n.entity_id in (select child_id from taxonomy_nodes where taxon_id = ?) group by t.entity_id order by t.name") 
						preparedStatement.setLong(1, (group.publicSource.obj openOr Model.Source).entityID)
						preparedStatement.setLong(2, (group.privateSource.obj openOr Model.Source).entityID)
						preparedStatement.setString(3, taxon.entityID.toString)
						preparedStatement
					}
				})
			}
		}
		
		def renderChildren(children: List[Model.Taxon]): NodeSeq = {
			children.flatMap(child => {
				<Taxon ID={child.entityID.toString} Rank={child.rank.toString} HasOccurrences={ if (child.occurrences.length > 0) "true" else "false"}>{child.name}</Taxon>
			}) 
		}
		
		val response: Node =
			if (null == taxon) {
				<Taxon>
					{renderChildren(children)}
				</Taxon>
			} else {
				<Taxon ID={taxon.entityID.toString} Name={taxon.name.toString} Rank={taxon.rank.toString}>
					{ renderChildren(children) }
				</Taxon>
			}
		
		Full(XmlResponse(response))
	}
	
	def occurrences(r: Req): Box[LiftResponse] = {
		val taxon: Model.Taxon = Model.Taxon.find(S.param("ID") openOr -1) openOr null
		
		val mobileDevice: Model.MobileDevice = Model.MobileDevice.find(S.param("MobileDeviceID") openOr -1) openOr null
		val user: Model.User =
			if (null == mobileDevice) {
				null
			} else {
				mobileDevice.user.obj openOr null
			}
		Model.User.currentUser(user)
		val group: Model.Group =
			if (null == user) {
				null
			} else {
				val groups = user.groups
				if (groups.length > 0) {
					groups.first
				} else {
					null
				}
			}
		
		if (null == taxon) {
			Full(XmlResponse(<Occurrences />))
		} else {
			def generateChildren(nodes: List[Model.Occurrence]): NodeSeq = {
				nodes.flatMap(s => {
					val source = s.source.obj.open_!
					val event = s.event.obj.open_!
					val location = event.location.obj.open_!
					
					<Occurrence id={s.entityID.toString} sex={s.sex.is} lifeStage={s.lifeStage.is} preparations={s.preparations.is}>
						<Details>{s.details.is}></Details>
						<Source id={source.entityID.is.toString} name={source.name.is}></Source>
						<CollectionEvent latitude={location.latitude.is.toString} longitude={location.longitude.is.toString} elevation={location.verbatimElevation.is.toString}>
							<Start>{event.date.is.toString}</Start>
							<End>{event.endDate.is.toString}</End>
							<FieldNotes>{event.fieldNotes.is}</FieldNotes>
							<Images>
								{
									val mediaFileBundle = s.mediaFileBundle.obj openOr null
									if (null != mediaFileBundle) {
										generateImageUrls(s.mediaFileBundle.obj.open_!.mediaFiles)
									}
								}
							</Images>
						</CollectionEvent>
					</Occurrence>
				})
			}
			
			def generateImageUrls(images: List[Model.MediaFile]): NodeSeq = {
				images.flatMap(s => <Url>{S.hostAndPath+"/service/media-file/read?ID="+s.entityID}</Url>)
			}
			
			val response: Node = {
				<Occurrences taxonID={taxon.entityID.toString} taxonName={taxon.name.toString} taxonRank={taxon.rank.toString}>
					{
						if (null == group) {
							generateChildren(taxon.occurrences.filter(_.source.obj.open_!.sourceType == Model.SourceType.Public))
						} else {
							generateChildren(taxon.occurrences.filter(_.source.obj.open_!.group == group))
						}
					}
				</Occurrences>
			}
			
			Full(XmlResponse(response))
		}
	}
	
	def occurrencesJSON(r: Req): Box[LiftResponse] = {
		val taxon = Model.Taxon.find(S.param("TaxonID") openOr -1) openOr null
		val group: Boolean = !S.param("Group").isEmpty

		if (group && !Model.User.groupSignedIn_?) {
			Full(NotFoundResponse())
		} else {
			Model.Taxon.currentTaxon(taxon)
			
			var baseSQL = "select c.entity_id, c.source_id from classifications c inner join taxonomy_nodes n on n.classification_id = c.entity_id inner join taxons t on n.taxon_id = t.entity_id inner join sources s on c.source_id = s.entity_id"
			val classicationIDs: String =
				if (null == Model.Taxon.currentTaxon.is) {
					null
				} else {
					val classifications = Model.Classification.findAllByPreparedStatement({database =>
						if (!group) {
							val preparedStatement: PreparedStatement = database.connection.prepareStatement(baseSQL + " where n.taxon_id = ? and s.source_type = 0")
							preparedStatement.setLong(1, Model.Taxon.currentTaxon.entityID)
							preparedStatement
						} else {
							val preparedStatement: PreparedStatement = database.connection.prepareStatement(baseSQL + " where n.taxon_id = ? and (c.source_id = ? or c.source_id = ?)")
							preparedStatement.setLong(1, Model.Taxon.currentTaxon.entityID)
							preparedStatement.setLong(2, Model.User.currentGroup.is.publicSource.obj.open_!.entityID)
							preparedStatement.setLong(3, Model.User.currentGroup.is.privateSource.obj.open_!.entityID)
							preparedStatement
						}
					}).removeDuplicates
					classifications.map(_.entityID.toString).foldLeft[String]("-1")(_ + "," + _)
				}
			var occurrences: List[JsExp] = List()
			DB.use(DefaultConnectionIdentifier) {connection =>
				if (null == Model.Taxon.currentTaxon.is) {
					baseSQL = "select o.entity_id, c.name, l.latitude, l.longitude from occurrences o inner join classifications c on o.classification_id = c.entity_id inner join sources s on o.source_id = s.entity_id inner join events e on o.event_id = e.entity_id inner join locations l on e.location_id = l.entity_id where"
				} else {
					baseSQL = "select o.entity_id, c.name, l.latitude, l.longitude from occurrences o inner join classifications c on o.classification_id = c.entity_id inner join sources s on o.source_id = s.entity_id inner join events e on o.event_id = e.entity_id inner join locations l on e.location_id = l.entity_id where o.classification_id in (" + classicationIDs + ") and"
				}
				val fullSQL =
					if (!group) {
						baseSQL + " s.source_type = 0"
					} else {
						baseSQL + " (o.source_id = ? or o.source_id = ?)"
					}
				DB.prepareStatement(fullSQL, connection) { preparedStatement =>
					if (group) {
						preparedStatement.setLong(1, Model.User.currentGroup.is.publicSource.obj.open_!.entityID)
						preparedStatement.setLong(2, Model.User.currentGroup.is.privateSource.obj.open_!.entityID)
					}
					val results: ResultSet = preparedStatement.executeQuery()
					while (results.next()) {
						val entityID = results.getLong("ENTITY_ID")
						val classificationName = try {
							results.getString("NAME")
						} catch {
							case e:SQLException => "Unclassified"
						}
						val latitude = results.getDouble("LATITUDE")
						val longitude = results.getDouble("LONGITUDE")
						if (latitude != 0.0 && longitude != 0.0) {
							occurrences = occurrences ::: List(
								JsObj(
									("latitude", Str(latitude.toString)),
									("longitude", Str(longitude.toString)),
									("title", Str(classificationName)),
									("targetURL", Str("")),
									("ID", Str(entityID.toString))
								)
							)
						}
					}
				}
			}
			val markersJSON = JsArray(occurrences:_*)
			
			Full(
				PlainTextResponse(
					JsCrVar("occurrencesJSON", markersJSON),
					List(),
					200
				)
			)
		}
	}
	private def fail(status: Int): Box[LiftResponse] = { 
		Full(InMemoryResponse(Array(), List(), List(), status))
	}
}