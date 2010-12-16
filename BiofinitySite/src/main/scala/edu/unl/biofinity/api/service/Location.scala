package edu.unl.biofinity.api.service

import edu.unl.biofinity.api.{model => Model}

import java.sql.PreparedStatement
import java.sql.ResultSet

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

object Location {
	def read(r: Req): Box[LiftResponse] = {
		val location: Model.Location = Model.Location.find(S.param("ID") openOr -1) openOr null

		if (null == location) {
			Full(NotFoundResponse(""))
		} else {
			if (location.canRead_?) {
				Full(
					XmlResponse(
<dwr:DarwinRecordSet
	xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
	xsi:schemaLocation="http://rs.tdwg.org/dwc/dwcrecord/ http://rs.tdwg.org/dwc/xsd/tdwg_dwc_classes.xsd"
	xmlns:dcterms="http://purl.org/dc/terms/"
	xmlns:dwc="http://rs.tdwg.org/dwc/terms/"
	xmlns:dwr="http://rs.tdwg.org/dwc/dwcrecord/">
	<dcterms:Location>
		<dwc:continent>{location.continent.is}</dwc:continent>
		<dwc:country>{location.country.is}</dwc:country>
		<dwc:county>{location.county.is}</dwc:county>
		<dwc:island>{location.island.is}</dwc:island>
		<dwc:islandGroup>{location.islandGroup.is}</dwc:islandGroup>
		<dwc:latitude>{location.latitude.is.toString}</dwc:latitude>
		<dwc:locality>{location.locality.is}</dwc:locality>
		<dwc:longitude>{location.longitude.is.toString}</dwc:longitude>
		<dwc:locationID>{"urn:lsid:biofinity.unl.edu:Location:" + location.entityID.is.toString}</dwc:locationID>
		<dwc:stateProvince>{location.stateProvince.is}</dwc:stateProvince>
		<dwc:verbatimElevation>{location.verbatimElevation.is}</dwc:verbatimElevation>
		<dwc:waterBody>{location.waterBody.is}</dwc:waterBody>
	</dcterms:Location>
</dwr:DarwinRecordSet>
					)
				)
			} else {
				Full(UnauthorizedResponse(""))
			}
		}
	}
	
	def locationsJSON(r: Req): Box[LiftResponse] = {
		if (!S.param("Group").isEmpty && !Model.User.groupSignedIn_?) {
			Full(NotFoundResponse())
		} else {
			var baseSQL = "select l.entity_id, l.latitude, l.longitude from locations l inner join sources s on l.source_id = s.entity_id"
			val locations =
				Model.Location.findAllByPreparedStatement({database =>
					if (S.param("Group").isEmpty) {
						val preparedStatement: PreparedStatement = database.connection.prepareStatement(baseSQL + " where s.source_type = 0")
						preparedStatement
					} else {
						val preparedStatement: PreparedStatement = database.connection.prepareStatement(baseSQL + " where (l.source_id = ? or l.source_id = ?)")
						preparedStatement.setLong(1, Model.User.currentGroup.is.publicSource.obj.open_!.entityID)
						preparedStatement.setLong(2, Model.User.currentGroup.is.privateSource.obj.open_!.entityID)
						preparedStatement
					}
				}).removeDuplicates.filter(location => {location.latitude != 0.0 && location.longitude != 0.0})
			
			val locationsJSON = JsArray(
				locations.map(location => {
					JsObj(
						("latitude", Str(location.latitude.toString)),
						("longitude", Str(location.longitude.toString)),
						("targetURL", Str({"javascript: selectLocation(" + location.entityID.toString + ")"})),
						("ID", Str(location.entityID.toString))
					)
				}):_*
			)
			
			Full(
				PlainTextResponse(
					JsCrVar("locationsJSON", locationsJSON),
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