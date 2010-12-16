package edu.unl.biofinity.site.snippet

import edu.unl.biofinity.api.{controller => Controller}
import edu.unl.biofinity.api.{model => Model}

import java.text.ParseException
import java.text.SimpleDateFormat
import java.util.Calendar
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

class Event {
	def init: NodeSeq = {
		val event = Model.Event.find(S.param("ID") openOr -1) openOr null
		if (null != event) {
			val source: Model.Source = event.source.obj openOr Model.Source
			if (!S.attr("group").isEmpty) {
				if (source == Model.User.currentGroup.privateSource.obj.openOr(null) || source == Model.User.currentGroup.publicSource.obj.openOr(null)) {
					Model.Event.currentEvent(event)
				}
			} else {
				if (source.public_?) {
					Model.Event.currentEvent(event)
				}
			}
		}
		Text("")
	}
 	
	def renderEvent(xhtml: NodeSeq): NodeSeq = {
		if (null == Model.Event.currentEvent.is) {
			Text("")
		} else {
			def initLocation() = {
				Model.Location.currentLocation(Model.Event.currentEvent.is.location.obj openOr null)
			}
			
			def initSource() = {
				Model.Source.currentSource(Model.Event.currentEvent.is.source.obj openOr null)
			}
			
			if ((!S.attr("group").isEmpty && !S.attr("update").isEmpty && Model.Event.currentEvent.canUpdate_?) || !S.attr("updateExplicit").isEmpty) {
				var dateValue = ""
				var hourValue = "12"
				var minuteValue = "0"
				var isPM = false
				
				if (null != Model.Event.currentEvent.is.date.is) {
					dateValue = Global.US_DATE_FORMAT.format(Model.Event.currentEvent.is.date.is)
					hourValue = Global.HOUR_DATE_FORMAT.format(Model.Event.currentEvent.is.date.is)
					minuteValue = Global.MINUTE_DATE_FORMAT.format(Model.Event.currentEvent.is.date.is)
					isPM =
						if (Global.AM_PM_DATE_FORMAT.format(Model.Event.currentEvent.is.date.is).equals("PM")) {
							true
						} else {
							false
						}
				}
				
				def updateDate() = {
					if (null != Model.Event.currentEvent.is.date.is) {
						val date = Model.Event.currentEvent.is.date.get
						if (isPM && !hourValue.equals("12")) {
							date.setHours(hourValue.toInt + 12)
						} else if (!isPM && hourValue.equals("12")) {
							date.setHours(0)
						} else {
							date.setHours(hourValue.toInt)
						}
						date.setMinutes(minuteValue.toInt)
						Model.Event.currentEvent.is.date(null)
						Model.Event.currentEvent.is.date(date)
					}
				}
				
				def setDate(value:String): JsCmd = {
					try {
						if (value.equals("")) {
							Model.Event.currentEvent.is.date(null)
						} else {
							val date: Date = Global.US_DATE_FORMAT.parse(value)
							Model.Event.currentEvent.is.date(date)
							updateDate()
						}
						JsCmds.Noop
					} catch {
						case e:ParseException => Alert("The date is not formatted correctly: MM/DD/YYYY")
					}
				}
				
				def setHours(value: String): JsCmd = {
					hourValue = value
					updateDate()
					JsCmds.Noop
				}
				
				def setMinutes(value: String): JsCmd = {
					minuteValue = value
					updateDate()
					JsCmds.Noop
				}
				
				def setPM(value: String): JsCmd = {
					if (value.equals("PM")) {
						isPM = true
					} else {
						isPM = false
					}
					updateDate()
					JsCmds.Noop
				}
				
				val hours = "12" :: "1" :: "2" :: "3" :: "4" :: "5" :: "6" :: "7" :: "8" :: "9" :: "10" :: "11" :: Nil
				val minutes = ("00" :: "01" :: "02" :: "03" :: "04" :: "05" :: "06" :: "07" :: "08" :: "09" :: Nil) ::: (10 to 59).toList
				def save(): JsCmd = {
					Model.Event.currentEvent.is.save
					SetHtml("event-details", <lift:embed what="/lab/event-details"/>)
				}
				
				SHtml.ajaxForm(
					bind(
						"event",
						xhtml,
						"AdditionalPropertyBundle" -> {(nodeSeq: NodeSeq) => {
							var additionalPropertyBundle = Model.Event.currentEvent.additionalPropertyBundle.obj openOr null
							if (null == additionalPropertyBundle) {
								additionalPropertyBundle = Model.AdditionalPropertyBundle.create
								additionalPropertyBundle.save
								Model.Event.currentEvent.is.additionalPropertyBundle(additionalPropertyBundle)
								Model.Event.currentEvent.is.save
								Model.Event.currentEvent(Model.Event.find(Model.Event.currentEvent.is.entityID) openOr null)
							}
							Model.AdditionalPropertyBundle.currentAdditionalPropertyBundle(additionalPropertyBundle)
							new AdditionalPropertyBundle().renderAdditionalPropertyBundle(nodeSeq)
						}},
						"AdditionalPropertyBundleWarning" -> {(nodeSeq: NodeSeq) => {
							val additionalPropertyBundle = Model.Event.currentEvent.additionalPropertyBundle.obj openOr null
							val groupAdditionalPropertyBundle = Model.User.currentGroup.is.eventAdditionalPropertyBundle.obj openOr null
							
							if (Model.AdditionalPropertyBundle.contains(additionalPropertyBundle, groupAdditionalPropertyBundle)) {
								Text("")
							} else {
								def ensureContains(): JsCmd = {
									Model.AdditionalPropertyBundle.ensureContains(additionalPropertyBundle, groupAdditionalPropertyBundle)
									SetHtml("event-additional-property-bundle", <lift:embed what="/lab/event-additional-property-bundle-update"/>)
								}
								
								bind(
									"additional-property-bundle-warning",
									nodeSeq,
									"EnsureContainsLink" -> {(nodeSeq: NodeSeq) => SHtml.ajaxButton(nodeSeq, () => ensureContains)}
								)
							}
						}},
						"CancelLink" -> {(nodeSeq: NodeSeq) => <a href="javascript: cancelEventDetailsUpdate();">{nodeSeq}</a>},
						"DeleteLink" -> Text(""),
						"Date" -> SHtml.ajaxText(dateValue, value => {setDate(value)}, "id"->"event-date"),
						"DateFormatted" -> Text(""),
						"DateFormattedNoTime" -> Text(""),
						"Hour" -> SHtml.ajaxSelect(hours.map(i => (i.toString, i.toString)), Full(hourValue), value => {setHours(value)}),
						"Minute" -> SHtml.ajaxSelect(minutes.map(i => (i.toString, i.toString)), Full(minuteValue), value => {setMinutes(value)}),
						"PM" -> SHtml.ajaxSelect(("AM" :: "PM" :: Nil).toList.map(i => (i.toString, i.toString)), if (isPM) {Full("PM")} else {Full("AM")}, value => {setPM(value)}),
						"EditLink" -> Text(""),
						//"EndDate" -> Text(Global.ISO_8601.format(Model.Event.currentEvent.get.endDate.is)),
						"Habitat" -> SHtml.ajaxText(Model.Event.currentEvent.is.habitat, value => {Model.Event.currentEvent.is.habitat(value); JsCmds.Noop}),
						"FieldNotes" -> SHtml.ajaxText(Model.Event.currentEvent.is.fieldNotes, value => {Model.Event.currentEvent.is.fieldNotes(value); JsCmds.Noop}),
						"FieldNumber" -> SHtml.ajaxText(Model.Event.currentEvent.is.fieldNumber, value => {Model.Event.currentEvent.is.fieldNumber(value); JsCmds.Noop}),
						"ID" -> Model.Event.currentEvent.is.entityID,
						"Location" -> {(nodeSeq: NodeSeq) => {
							initLocation
							new Location().renderLocation(nodeSeq)
						}},
						"Remarks" -> SHtml.ajaxText(Model.Event.currentEvent.is.remarks, value => {Model.Event.currentEvent.is.remarks(value); JsCmds.Noop}),
						"SamplingEffort" -> SHtml.ajaxText(Model.Event.currentEvent.is.samplingEffort, value => {Model.Event.currentEvent.is.samplingEffort(value); JsCmds.Noop}),
						"SamplingProtocol" -> SHtml.ajaxText(Model.Event.currentEvent.is.samplingProtocol, value => {Model.Event.currentEvent.is.samplingProtocol(value); JsCmds.Noop}),
						"SaveLink" -> {(nodeSeq: NodeSeq) => SHtml.ajaxButton(nodeSeq, () => save)},
						"Source" -> {(nodeSeq: NodeSeq) => {
							initSource
							new Source().renderSource(nodeSeq)
						}},
						"VerbatimDate" -> SHtml.ajaxText(Model.Event.currentEvent.is.verbatimDate, value => {Model.Event.currentEvent.is.verbatimDate(value); JsCmds.Noop}),
						"ViewLink" -> Text(""),
						"WikiPageIDLink" -> Text("")
					) ++
					Script(renderInitDatePickerJavaScriptCommand)
				)
			} else {
				val dateValue =
					if (null == Model.Event.currentEvent.is.date.is) {
						""
					} else {
						Global.RFC_3339_DATE_FORMAT.format(Model.Event.currentEvent.is.date.is)
					}
				bind(
					"event",
					xhtml,
					"AdditionalPropertyBundle" -> {(nodeSeq: NodeSeq) => {
						Model.AdditionalPropertyBundle.currentAdditionalPropertyBundle(Model.Event.currentEvent.additionalPropertyBundle.obj openOr null)
						new AdditionalPropertyBundle().renderAdditionalPropertyBundle(nodeSeq)
					}},
					"AdditionalPropertyBundleWarning" -> Text(""),
					"CancelLink" -> Text(""),
					"DeleteLink" -> Text(""),
					"Date" -> Text(dateValue),
					"DateFormatted" -> {
						if (null == Model.Event.currentEvent.is.date.is) {
							Text("")
						} else {
							Text(Global.FRIENDLY_DATE_FORMAT.format(Model.Event.currentEvent.is.date.is))
						}
					},
					"DateFormattedNoTime" -> {
						if (null == Model.Event.currentEvent.is.date.is) {
							Text("")
						} else {
							Text(Global.FRIENDLY_DATE_FORMAT_NO_TIME.format(Model.Event.currentEvent.is.date.is))
						}
					},
					"EditLink" -> {(nodeSeq: NodeSeq) => {
						if (!S.attr("group").isEmpty && Model.Event.currentEvent.canUpdate_?) {
							<a href="javascript: showEventDetailsUpdate();">{nodeSeq}</a>
						} else {
							Text("")
						}
					}},
					//"EndDate" -> Text(Global.RFC_3339.format(Model.Event.currentEvent.get.endDate.is)),
					"FieldNotes" -> Model.Event.currentEvent.get.fieldNotes,
					"FieldNumber" -> Model.Event.currentEvent.get.fieldNumber,
					"Habitat" -> Model.Event.currentEvent.get.habitat,
					"ID" -> Model.Event.currentEvent.get.entityID,
					"Location" -> {(nodeSeq: NodeSeq) => {
						initLocation
						new Location().renderLocation(nodeSeq)
					}},
					"Remarks" -> Model.Event.currentEvent.get.remarks,
					"SamplingEffort" -> Model.Event.currentEvent.get.samplingEffort,
					"SamplingProtocol" -> Model.Event.currentEvent.get.samplingProtocol,
					"SaveLink" -> Text(""),
					"Source" -> {(nodeSeq: NodeSeq) => {
						initSource
						new Source().renderSource(nodeSeq)
					}},
					"VerbatimDate" -> Model.Event.currentEvent.get.verbatimDate,
					"ViewLink" -> {(nodeSeq: NodeSeq) => {
						<a href={"event?ID=" + Model.Event.currentEvent.is.entityID}>{nodeSeq}</a>
					}},
					"WikiPageIDLink" -> {(nodeSeq: NodeSeq) => {
						val wikiPageID = Model.Event.currentEvent.is.wikiPageID.is
						if (0 < wikiPageID) {
							<a href={"/wiki?WikiPageID=" + wikiPageID}>{wikiPageID}</a>
						} else {
							if (!S.attr("group").isEmpty) {
								def create() = {
									val newWikiPageID = Controller.WikiPage.create("event", Model.Event.currentEvent.is.entityID, Model.Event.currentEvent.is.source.obj.open_!.entityID)
									Model.Event.currentEvent.is.wikiPageID(newWikiPageID)
									Model.Event.currentEvent.is.save
									JsCmds.Run("location.reload(true)")
								}
								
								bind(
									"wiki-page",
									nodeSeq,
									"CreateLink" -> {(nodeSeq: NodeSeq) => {SHtml.a(() => create, nodeSeq)}}
								)
							} else {
								Text("")
							}
						}
					}}
				)
			}
		}
	}
  	
	def renderScripts: NodeSeq = {
		def showEventDetails() = {
			SetHtml("event-details", <lift:embed what="lab/event-details" />)
		}
		
		def showEventDetailsUpdate() = {
			SetHtml("event-details", <lift:embed what="lab/event-details-update" />) &
			renderInitDatePickerJavaScriptCommand
		}
		
		def cancelEventDetailsUpdate() = {
			Model.Event.currentEvent(Model.Event.find(Model.Event.currentEvent.is.entityID) openOr null)
			
			SetHtml("event-details", <lift:embed what="lab/event-details" />)
		}
		
		def showEventAdditionalPropertyBundle() = {
			SetHtml("event-additional-property-bundle", <lift:embed what="lab/event-additional-property-bundle" />)
		}
		
		def showEventAdditionalPropertyBundleUpdate() = {
			SetHtml("event-additional-property-bundle", <lift:embed what="lab/event-additional-property-bundle-update" />)
		}
		
		Script(
			Function(
				"showEventDetails",
				Nil,
				SHtml.ajaxInvoke(showEventDetails)._2
			)
		) ++
		Script(
			Function(
				"showEventDetailsUpdate",
				Nil,
				SHtml.ajaxInvoke(showEventDetailsUpdate)._2
			)
		) ++
		Script(
			Function(
				"cancelEventDetailsUpdate",
				Nil,
				SHtml.ajaxInvoke(cancelEventDetailsUpdate)._2
			)
		) ++
		Script(
			Function(
				"showEventAdditionalPropertyBundle",
				Nil,
				SHtml.ajaxInvoke(showEventAdditionalPropertyBundle)._2
			)
		) ++
		Script(
			Function(
				"showEventAdditionalPropertyBundleUpdate",
				Nil,
				SHtml.ajaxInvoke(showEventAdditionalPropertyBundleUpdate)._2
			)
		)
	}
	
	def renderInitDatePickerJavaScriptCommand: JsCmd = {
	  	Call(
			"initDatePicker",
			Str("event-date")
		)
	}
	
	def renderStats(xhtml: NodeSeq): NodeSeq = {
		if (null == Model.Event.currentEvent.is) {
			return xhtml
		} else {
			val occurrenceCount = Model.Occurrence.count(By(Model.Occurrence.event, Model.Event.currentEvent.entityID))
			if (S.attr("multiple").isEmpty || (!S.attr("multiple").isEmpty && occurrenceCount > 1)) {
				bind(
					"event",
					xhtml,
					"Occurrences" -> Global.NUMBER_FORMAT.format(occurrenceCount)
				)
			} else {
				Text("")
			}
		}
	}
}