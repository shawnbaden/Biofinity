package edu.unl.biofinity.api.controller

import edu.unl.biofinity.api.{model => Model}

import java.io.BufferedWriter
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.OutputStreamWriter
import java.sql.ResultSet
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
import collection.immutable.Queue
import xml.{Text, Node, NodeSeq}

object SimpleOccurrence {
	def search(occurrenceIDs: List[Long]): List[Model.SimpleOccurrence] = {
		var simpleOccurrences: List[Model.SimpleOccurrence] = List()
		
		DB.use(DefaultConnectionIdentifier) {connection =>
			val occurrenceIDsAsString: String = occurrenceIDs.map(_.toString).foldLeft[String]("-1")(_ + "," + _)
			val fullSQL = "SELECT l.country, c.name, e.date, l.latitude, l.locality, l.longitude, mf.entity_id, mf.file_type, o.entity_id, o.recorded_by, l.state_province, l.verbatim_elevation FROM occurrences o INNER JOIN events e ON o.event_id = e.entity_id INNER JOIN locations l ON e.location_id = l.entity_id LEFT OUTER JOIN classifications c ON o.classification_id = c.entity_id LEFT OUTER JOIN media_files mf ON o.media_file_bundle_id = mf.media_file_bundle_id WHERE o.entity_id IN (" + occurrenceIDsAsString + ") GROUP BY o.entity_id"
			
			var valueDate: Date = null
			var valueDouble = 0.0
			var valueString = ""
			
			DB.prepareStatement(fullSQL, connection) { preparedStatement =>
				val results: ResultSet = preparedStatement.executeQuery()
				while (results.next()) {
					val simpleOccurrence = new Model.SimpleOccurrence()
					
					valueString = results.getString("COUNTRY")
					if (null != valueString) {
						simpleOccurrence.country = valueString
					}
					
					valueString = results.getString("NAME")
					if (null != valueString) {
						simpleOccurrence.classificationName = valueString
					}
					
					valueDate = results.getDate("DATE")
					if (null != valueDate) {
						simpleOccurrence.eventDate = valueDate
					}
					
					valueDouble = results.getDouble("LATITUDE")
					if (null != valueDouble) {
						simpleOccurrence.latitude = valueDouble
					}
					
					valueString = results.getString("LOCALITY")
					if (null != valueString) {
						simpleOccurrence.locality = valueString
					}
					
					valueDouble = results.getDouble("LONGITUDE")
					if (null != valueDouble) {
						simpleOccurrence.longitude = valueDouble
					}
					
					simpleOccurrence.mediaFileID = results.getLong("MF.ENTITY_ID")
					
					valueString = results.getString("FILE_TYPE")
					if (null != valueString) {
						simpleOccurrence.mediaFileType = valueString
					}
					
					simpleOccurrence.occurrenceID = results.getLong("O.ENTITY_ID")
					
					valueString = results.getString("RECORDED_BY")
					if (null != valueString) {
						simpleOccurrence.recordedBy = valueString
					}
					
					valueString = results.getString("STATE_PROVINCE")
					if (null != valueString) {
						simpleOccurrence.stateProvince = valueString
					}
					
					valueString = results.getString("VERBATIM_ELEVATION")
					if (null != valueString) {
						simpleOccurrence.verbatimElevation = valueString
					}
					
					simpleOccurrences = simpleOccurrences :+ simpleOccurrence
				}
			}
		}
		
		simpleOccurrences
	}
	
	def sort(simpleOccurrences: List[Model.SimpleOccurrence], sortBy: String, sortByDescending: Boolean): List[Model.SimpleOccurrence] = {
		var sortedSimpleOccurrences: List[Model.SimpleOccurrence] = List()
		
		if (null != simpleOccurrences && null != sortBy) {
			if (sortBy.equals("")) {
			} else if (sortBy.equals("Country")) {
				if (sortByDescending) {
					sortedSimpleOccurrences = simpleOccurrences.sort((a, b) => a.country.compareToIgnoreCase(b.country) > 0)
				} else {
					sortedSimpleOccurrences = simpleOccurrences.sort((a, b) => b.country.compareToIgnoreCase(a.country) > 0)
				}
			} else if (sortBy.equals("EventDate")) {
				if (sortByDescending) {
					sortedSimpleOccurrences = simpleOccurrences.sort((a, b) => {
						if (null == a.eventDate && null == b.eventDate) {
							false
						} else if (null == a.eventDate) {
							false
						} else if (null == b.eventDate) {
							true
						} else {
							a.eventDate.after(b.eventDate)
						}
					})
				} else {
					sortedSimpleOccurrences = simpleOccurrences.sort((a, b) => {
						if (null == a.eventDate && null == b.eventDate) {
							true
						} else if (null == a.eventDate) {
							true
						} else if (null == b.eventDate) {
							false
						} else {
							b.eventDate.after(a.eventDate)
						}
					})
				}
			} else if (sortBy.equals("Locality")) {
				if (sortByDescending) {
					sortedSimpleOccurrences = simpleOccurrences.sort((a, b) => a.locality.compareToIgnoreCase(b.locality) > 0)
				} else {
					sortedSimpleOccurrences = simpleOccurrences.sort((a, b) => b.locality.compareToIgnoreCase(a.locality) > 0)
				}
			} else if (sortBy.equals("OccurrenceID")) {
				if (sortByDescending) {
					sortedSimpleOccurrences = simpleOccurrences.sort((a, b) => a.occurrenceID > b.occurrenceID)
				} else {
					sortedSimpleOccurrences = simpleOccurrences.sort((a, b) => b.occurrenceID > a.occurrenceID)
				}
			} else if (sortBy.equals("RecordedBy")) {
				if (sortByDescending) {
					sortedSimpleOccurrences = simpleOccurrences.sort((a, b) => a.recordedBy.compareToIgnoreCase(b.recordedBy) > 0)
				} else {
					sortedSimpleOccurrences = simpleOccurrences.sort((a, b) => b.recordedBy.compareToIgnoreCase(a.recordedBy) > 0)
				}
			} else if (sortBy.equals("StateProvince")) {
				if (sortByDescending) {
					sortedSimpleOccurrences = simpleOccurrences.sort((a, b) => a.stateProvince.compareToIgnoreCase(b.stateProvince) > 0)
				} else {
					sortedSimpleOccurrences = simpleOccurrences.sort((a, b) => b.stateProvince.compareToIgnoreCase(a.stateProvince) > 0)
				}
			} else if (sortBy.equals("VerbatimElevation")) {
				if (sortByDescending) {
					sortedSimpleOccurrences = simpleOccurrences.sort((a, b) => a.verbatimElevationAsDouble > b.verbatimElevationAsDouble)
				} else {
					sortedSimpleOccurrences = simpleOccurrences.sort((a, b) => b.verbatimElevationAsDouble > a.verbatimElevationAsDouble)
				}
			}
		}
		
		sortedSimpleOccurrences
	}
}