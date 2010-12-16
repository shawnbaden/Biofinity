package edu.unl.biofinity.api.service

import edu.unl.biofinity.api.{controller => Controller}
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

object Search {
	def occurrencesJSON(r: Req): Box[LiftResponse] = {
		if (null == Controller.Occurrence.occurrenceIDs.is) {
			Full(NotFoundResponse())
		} else {
			Log.info(Controller.Occurrence.occurrenceIDs.is.length.toString)
			var occurrencesJS: List[JsExp] = List()
			val simpleOccurrences: List[Model.SimpleOccurrence] = Controller.SimpleOccurrence.search(Controller.Occurrence.occurrenceIDs.is)
			simpleOccurrences.foreach(simpleOccurrence => {
				if (simpleOccurrence.latitude != 0.0 && simpleOccurrence.longitude != 0.0) {							
					occurrencesJS = occurrencesJS ::: List(
						JsObj(
							("latitude", Str(simpleOccurrence.latitude.toString)),
							("longitude", Str(simpleOccurrence.longitude.toString)),
							("title", Str(simpleOccurrence.classificationName)),
							("targetURL", Str("")),
							("ID", Str(simpleOccurrence.occurrenceID.toString))
						)
					)
				}
			})
			val markersJSON = JsArray(occurrencesJS:_*)
			
			Controller.Occurrence.occurrenceIDs(List())
			
			Full(
				PlainTextResponse(
					JsCrVar("occurrencesJSON", markersJSON),
					List(),
					200
				)
			)
		}
	}
}