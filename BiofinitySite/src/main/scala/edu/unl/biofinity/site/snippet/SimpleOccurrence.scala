package edu.unl.biofinity.site.snippet

import edu.unl.biofinity.api.{controller => Controller}
import edu.unl.biofinity.api.{model => Model}
import edu.unl.biofinity.api.{service => Service}

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

import scala.xml._

class SimpleOccurrence {
	def renderSimpleOccurrence(xhtml: NodeSeq): NodeSeq = {
		if (null == Model.SimpleOccurrence.currentSimpleOccurrence.is) {
			Text("")
		} else {
			bind(
				"simple-occurrence",
				xhtml,
				"Country" -> Model.SimpleOccurrence.currentSimpleOccurrence.is.country,
				"ClassificationName" -> Model.SimpleOccurrence.currentSimpleOccurrence.is.classificationName,
				"EventDate" -> {
					if (null == Model.SimpleOccurrence.currentSimpleOccurrence.is.eventDate) {
						""
					} else {
						Model.SimpleOccurrence.currentSimpleOccurrence.is.eventDate.toString
					}
				},
				"Latitude" -> Model.SimpleOccurrence.currentSimpleOccurrence.is.latitude.toString,
				"Locality" -> Model.SimpleOccurrence.currentSimpleOccurrence.is.locality,
				"Longitude" -> Model.SimpleOccurrence.currentSimpleOccurrence.is.longitude.toString,
				"MediaFileLink" -> {(nodeSeq: NodeSeq) => {
					if (0 < Model.SimpleOccurrence.currentSimpleOccurrence.is.mediaFileID && Model.MediaFile.isImage(Model.SimpleOccurrence.currentSimpleOccurrence.is.mediaFileType)) {
						val widthParam = try {
							val width = BindHelpers.attr("width").first.open_!.toInt
							
							if (1 > width) {
								""
							} else {
								"&Width=" + width.toString
							}
						} catch { case e: Exception => "" }
						
						<a href={"occurrence?ID=" + Model.SimpleOccurrence.currentSimpleOccurrence.is.occurrenceID.toString}>
							<img src={"/service/media-file/read?ID=" + Model.SimpleOccurrence.currentSimpleOccurrence.is.mediaFileID.toString + widthParam} />
						</a>
					} else {
						Text("")
					}
				}},
				"ID" -> Model.SimpleOccurrence.currentSimpleOccurrence.is.occurrenceID.toString,
				"IDLink" -> <a href={"occurrence?ID=" + Model.SimpleOccurrence.currentSimpleOccurrence.is.occurrenceID.toString}>{Model.SimpleOccurrence.currentSimpleOccurrence.is.occurrenceID.toString}</a>,
				"RecordedBy" -> Model.SimpleOccurrence.currentSimpleOccurrence.is.recordedBy,
				"StateProvince" -> Model.SimpleOccurrence.currentSimpleOccurrence.is.stateProvince,
				"VerbatimElevation" -> Model.SimpleOccurrence.currentSimpleOccurrence.is.verbatimElevation
			)
		}
	}
	
	def renderSimpleOccurrences(xhtml: NodeSeq): NodeSeq = {
		if (null != Model.SimpleOccurrence.currentSimpleOccurrences.is && 0 < Model.SimpleOccurrence.currentSimpleOccurrences.is.length) {
			var currentSortBy: String = ""
			var currentSortByDescending: Boolean = false
			
			val occurrenceIDs: List[Long] = Model.SimpleOccurrence.currentSimpleOccurrences.is.map(_.occurrenceID)
			
			def render(): NodeSeq = {
				bind(
					"simple-occurrences",
					xhtml,
					"Count" -> Model.SimpleOccurrence.currentSimpleOccurrences.is.length,
					"DownloadCSVLink" -> {(nodeSeq: NodeSeq) => {
						SHtml.link("/service/occurrence/occurrencesCSV", () => Model.Occurrence.currentOccurrenceIDs(occurrenceIDs), nodeSeq)
					}},
					"DownloadKMLLink" -> {(nodeSeq: NodeSeq) => {
						SHtml.link("/service/occurrence/occurrencesKML", () => Model.Occurrence.currentOccurrenceIDs(occurrenceIDs), nodeSeq)
					}},
					"EmptyText" -> Text(""),
					"List" -> {(nodeSeq: NodeSeq) => {
						val simpleOccurrencesNodeSeq: NodeSeq = Model.SimpleOccurrence.currentSimpleOccurrences.is.flatMap(simpleOccurrence => {
							Model.SimpleOccurrence.currentSimpleOccurrence(simpleOccurrence)
							renderSimpleOccurrence(nodeSeq)
						})
						Model.SimpleOccurrence.currentSimpleOccurrence(null)
						simpleOccurrencesNodeSeq
					}},
					"MapLink" -> {(nodeSeq: NodeSeq) => {
						SHtml.link("map", () => Controller.Occurrence.occurrenceIDs(occurrenceIDs), nodeSeq)
					}},
					"SortByCountryLink" -> {(nodeSeq: NodeSeq) => sortByLink(nodeSeq, "Country")},
					"SortByEventDateLink" -> {(nodeSeq: NodeSeq) => sortByLink(nodeSeq, "EventDate")},
					"SortByLocalityLink" -> {(nodeSeq: NodeSeq) => sortByLink(nodeSeq, "Locality")},
					"SortByOccurrenceIDLink" -> {(nodeSeq: NodeSeq) => sortByLink(nodeSeq, "OccurrenceID")},
					"SortByRecordedByLink" -> {(nodeSeq: NodeSeq) => sortByLink(nodeSeq, "RecordedBy")},
					"SortByStateProvinceLink" -> {(nodeSeq: NodeSeq) => sortByLink(nodeSeq, "StateProvince")},
					"SortByVerbatimElevationLink" -> {(nodeSeq: NodeSeq) => sortByLink(nodeSeq, "VerbatimElevation")}
				)
			}
			
			def sortByLink(linkTextNodeSeq: NodeSeq, sortBy: String) = {
				val link = SHtml.a(() =>
					{
						val sortByDescending = 
							if (sortBy.equals(currentSortBy)) {
								!currentSortByDescending
							} else {
								false
							}
						
						sort(sortBy, sortByDescending)
						SetHtml("simple-occurrences", render)
					},
					linkTextNodeSeq
				)
				
				if (sortBy.equals(currentSortBy)) {
					val directionLink =
						if (currentSortByDescending) {
							"/resources/images/down.gif"
						} else {
							"/resources/images/up.gif"
						}
					
					<span>{link}<img src={directionLink} /></span>
				} else {
					link
				}
			}
			
			def sort(sortBy: String, sortByDescending: Boolean) = {
				currentSortBy = sortBy
				currentSortByDescending = sortByDescending
				Model.SimpleOccurrence.currentSimpleOccurrences(
					Controller.SimpleOccurrence.sort(
						Model.SimpleOccurrence.currentSimpleOccurrences.is,
						currentSortBy,
						currentSortByDescending
					)
				)
			}
			
			if (!S.attr("sortBy").isEmpty) {
				sort(S.attr("sortBy").get, S.attr("sortByDescending").isDefined)
			}
			
			render
		} else {
			var emptyTextNodeSeq: NodeSeq = Text("")
			
			val nodeSeq: NodeSeq = bind(
				"simple-occurrences",
				xhtml,
				"Count" -> Text("0"),
				"DownloadCSVLink" -> Text(""),
				"DownloadKMLLink" -> Text(""),
				"EmptyText" -> {(nodeSeq: NodeSeq) => {
					emptyTextNodeSeq = nodeSeq
					nodeSeq
				}},
				"List" -> Text(""),
				"SortByCountryLink" -> {(nodeSeq: NodeSeq) => nodeSeq},
				"SortByEventDateLink" -> {(nodeSeq: NodeSeq) => nodeSeq},
				"SortByLocalityLink" -> {(nodeSeq: NodeSeq) => nodeSeq},
				"SortByOccurrenceIDLink" -> {(nodeSeq: NodeSeq) => nodeSeq},
				"SortByRecordedByLink" -> {(nodeSeq: NodeSeq) => nodeSeq},
				"SortByStateProvince" -> {(nodeSeq: NodeSeq) => nodeSeq},
				"SortByVerbatimElevation" -> {(nodeSeq: NodeSeq) => nodeSeq}
			)
			
			emptyTextNodeSeq
		}
	}
}