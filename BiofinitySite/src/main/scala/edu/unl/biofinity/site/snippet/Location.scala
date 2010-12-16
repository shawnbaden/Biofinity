package edu.unl.biofinity.site.snippet

import edu.unl.biofinity.api.{controller => Controller}
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
import net.liftweb.widgets.autocomplete._

import scala.xml._

class Location {
	def init: NodeSeq = {
		val location = Model.Location.find(S.param("ID") openOr -1) openOr null
		if (null != location) {
			val source: Model.Source = location.source.obj openOr Model.Source
			if (!S.attr("group").isEmpty) {
				if (source == Model.User.currentGroup.privateSource.obj.openOr(null) || source == Model.User.currentGroup.publicSource.obj.openOr(null)) {
					Model.Location.currentLocation(location)
				}
			} else {
				if (source.public_?) {
					Model.Location.currentLocation(location)
				}
			}
		}
		Text("")
	}
	
	def renderLocation(xhtml: NodeSeq): NodeSeq = {
		if (null == Model.Location.currentLocation.is) {
			Text("")
		} else {
			//var currentLocation = Model.Location.currentLocation.is
			Map.markerLatitude(Model.Location.currentLocation.is.latitude)
			Map.markerLongitude(Model.Location.currentLocation.is.longitude)
			
			def initSource() = {
				Model.Source.currentSource(Model.Location.currentLocation.is.source.obj openOr null)
			}
			
			if ((!S.attr("group").isEmpty && !S.attr("update").isEmpty && Model.Location.currentLocation.canUpdate_?) || !S.attr("updateExplicit").isEmpty) {
				Location.latitudeValue(Model.Location.currentLocation.is.latitude.toString)
				Location.latitudeDMSDegreesValue(Map.convertDDtoDMSDegrees(Model.Location.currentLocation.is.latitude).toString)
				Location.latitudeDMSMinutesValue(Map.convertDDtoDMSMinutes(Model.Location.currentLocation.is.latitude).toString)
				Location.latitudeDMSSecondsValue(Map.convertDDtoDMSSeconds(Model.Location.currentLocation.is.latitude).toString)
				Location.latitudeDMSDirectionValue(if (Model.Location.currentLocation.is.latitude.is < 0.0) "S" else "N")
				Location.longitudeValue(Model.Location.currentLocation.is.longitude.toString)
				Location.longitudeDMSDegreesValue(Map.convertDDtoDMSDegrees(Model.Location.currentLocation.is.longitude).toString)
				Location.longitudeDMSMinutesValue(Map.convertDDtoDMSMinutes(Model.Location.currentLocation.is.longitude).toString)
				Location.longitudeDMSSecondsValue(Map.convertDDtoDMSSeconds(Model.Location.currentLocation.is.longitude).toString)
				Location.longitudeDMSDirectionValue(if (Model.Location.currentLocation.is.longitude.is < 0.0) "W" else "E")
				
				def updateLocation(showAlert: Boolean): JsCmd = {
					try {
						val latitude = Location.latitudeValue.is.toDouble
						if (latitude < -90.0 || latitude > 90.0) {
							if (showAlert) {
								return Alert("Latitude must be between -90.0 and 90.0.")
							}
						} else {
							Model.Location.currentLocation.is.latitude(latitude)
							Map.markerLatitude(latitude)
							Location.latitudeValue(latitude.toString)
							Location.latitudeDMSDegreesValue(Map.convertDDtoDMSDegrees(latitude).toString)
							Location.latitudeDMSMinutesValue(Map.convertDDtoDMSMinutes(latitude).toString)
							Location.latitudeDMSSecondsValue(Map.convertDDtoDMSSeconds(latitude).toString)
							Location.latitudeDMSDirectionValue(if (latitude < 0.0) "S" else "N")
						}
					} catch {
						case e:NumberFormatException => {
							if (showAlert) {
								return Alert("Latitude must be a number between -90.0 and 90.0.")
							}
						}
					}
					
					try {
						val longitude = Location.longitudeValue.is.toDouble
						if (longitude < -180.0 || longitude > 180.0) {
							if (showAlert) {
								return Alert("Longitude must be between -180.0 and 180.0.")
							}
						} else {
							Model.Location.currentLocation.is.longitude(longitude)
							Map.markerLongitude(longitude)
							Location.longitudeValue(longitude.toString)
							Location.longitudeDMSDegreesValue(Map.convertDDtoDMSDegrees(longitude).toString)
							Location.longitudeDMSMinutesValue(Map.convertDDtoDMSMinutes(longitude).toString)
							Location.longitudeDMSSecondsValue(Map.convertDDtoDMSSeconds(longitude).toString)
							Location.longitudeDMSDirectionValue(if (longitude < 0.0) "W" else "E")
						}
					} catch {
						case e:NumberFormatException => {
							if (showAlert) {
								return Alert("Longitude must be a number between -180.0 and 180.0.")
							}
						}
					}
					
					Call("moveMarker", JsVar(Location.latitudeValue.is), JsVar(Location.longitudeValue.is)) &
					SetValById("location-latitude-degrees", Location.latitudeDMSDegreesValue.is) &
					SetValById("location-latitude-minutes", Location.latitudeDMSMinutesValue.is) &
					SetValById("location-latitude-seconds", Location.latitudeDMSSecondsValue.is) &
					SetValById("location-latitude-direction", Location.latitudeDMSDirectionValue.is) &
					SetValById("location-longitude-degrees", Location.longitudeDMSDegreesValue.is) &
					SetValById("location-longitude-minutes", Location.longitudeDMSMinutesValue.is) &
					SetValById("location-longitude-seconds", Location.longitudeDMSSecondsValue.is) &
					SetValById("location-longitude-direction", Location.longitudeDMSDirectionValue.is)
				}
				
				def updateDMSLocation(showAlert: Boolean): JsCmd = {
					try {
						val latitudeDMSDegrees = Location.latitudeDMSDegreesValue.is.toInt
						if (latitudeDMSDegrees < 0 || latitudeDMSDegrees > 90) {
							if (showAlert) {
								return Alert("Latitude degrees must be between 0 and 90.")
							}
						}
					} catch {
						case e:NumberFormatException => {
							if (showAlert) {
								return Alert("Latitude degrees must be a whole number between 0 and 90.")
							}
						}
					}
					
					try {
						val latitudeDMSMinutes = Location.latitudeDMSMinutesValue.is.toInt
						if (latitudeDMSMinutes < 0 || latitudeDMSMinutes > 60) {
							if (showAlert) {
								return Alert("Latitude minutes must be between 0 and 60.")
							}
						}
					} catch {
						case e:NumberFormatException => {
							if (showAlert) {
								return Alert("Latitude minutes must be a number between 0 and 60.")
							}
						}
					}
					
					try {
						val latitudeDMSSeconds = Location.latitudeDMSSecondsValue.is.toDouble
						if (latitudeDMSSeconds < 0.0 || latitudeDMSSeconds > 60.0) {
							if (showAlert) {
								return Alert("Latitude seconds must be between 0 and 60.0.")
							}
						}
					} catch {
						case e:NumberFormatException => {
							if (showAlert) {
								return Alert("Latitude seconds must be a number between 0 and 60.0.")
							}
						}
					}
					
					try {
						val longitudeDMSDegrees = Location.longitudeDMSDegreesValue.is.toInt
						if (longitudeDMSDegrees < 0 || longitudeDMSDegrees > 180) {
							if (showAlert) {
								return Alert("Longitude degrees must be between 0 and 180.")
							}
						}
					} catch {
						case e:NumberFormatException => {
							if (showAlert) {
								return Alert("Longitude degrees must be a number between 0 and 90.")
							}
						}
					}
					
					try {
						val longitudeDMSMinutes = Location.longitudeDMSMinutesValue.is.toInt
						if (longitudeDMSMinutes < 0 || longitudeDMSMinutes > 60) {
							if (showAlert) {
								return Alert("Longitude minutes must be between 0 and 60.")
							}
						}
					} catch {
						case e:NumberFormatException => {
							if (showAlert) {
								return Alert("Longitude minutes must be a number between 0 and 60.")
							}
						}
					}
					
					try {
						val longitudeDMSSeconds = Location.longitudeDMSSecondsValue.is.toDouble
						if (longitudeDMSSeconds < 0.0 || longitudeDMSSeconds > 60.0) {
							if (showAlert) {
								return Alert("Longitude seconds must be between 0.0 and 60.0.")
							}
						}
					} catch {
						case e:NumberFormatException => {
							if (showAlert) {
								return Alert("Longitude seconds must be a number between 0.0 and 60.0.")
							}
						}
					}
					try {
						var latitude = Map.convertDMStoDD(Location.latitudeDMSDegreesValue.is.toInt, Location.latitudeDMSMinutesValue.is.toInt, Location.latitudeDMSSecondsValue.is.toDouble)
						if ("S" == Location.latitudeDMSDirectionValue.is) {
							latitude = latitude * -1
						}
						Location.latitudeValue(latitude.toString)
						Model.Location.currentLocation.is.latitude(latitude)
						Map.markerLatitude(latitude)
						var longitude = Map.convertDMStoDD(Location.longitudeDMSDegreesValue.is.toInt, Location.longitudeDMSMinutesValue.is.toInt, Location.longitudeDMSSecondsValue.is.toDouble)
						if ("W" == Location.longitudeDMSDirectionValue.is) {
							longitude = longitude * -1
						}
						Location.longitudeValue(longitude.toString)
						Model.Location.currentLocation.is.longitude(longitude)
						Map.markerLongitude(longitude)
					} catch {
						case e:NumberFormatException => {
						}
					}
					
					Call("moveMarker", JsVar(Location.latitudeValue.is), JsVar(Location.longitudeValue.is)) &
					SetValById("location-latitude", Location.latitudeValue.is) &
					SetValById("location-longitude", Location.longitudeValue.is)
				}
				
				def validateAndSetPositiveDouble(value: String, setDouble: Double => Unit): JsCmd = {
					try {
						val doubleValue = value.toDouble
						if (doubleValue < 0.0) {
							Alert("The number cannot be negative.")
						} else {
							setDouble(doubleValue)
							JsCmds.Noop
						}
					} catch {
						case e:NumberFormatException => Alert("The value must be a non-negative number.")
					}
				}
				
				/*
				def getCounties(current: String, limit: Int): Seq[String] = {
					val counties = Model.Location.findAll(By(Model.Location.source, currentLocation.source.is)).map(_.county.is).filter(_.toLowerCase().startsWith(current))
					currentLocation.county(current.toUpperCase)
					counties
				}
				*/
				
				def save(): JsCmd = {
					Model.Location.currentLocation.is.save
					Map.markerDraggable(false)
					
					SetHtml("location-details", <lift:embed what="/lab/location-details"/>) &
					new Map().renderMapJavaScriptCommand &
					new Map().renderMarkerJavaScriptCommand
				}
				
				SHtml.ajaxForm(
					bind(
						"location",
						xhtml,
						"CancelLink" -> {(nodeSeq: NodeSeq) => <a href="javascript: cancelLocationDetailsUpdate();">{nodeSeq}</a>},
						"Continent" -> SHtml.ajaxText(Model.Location.currentLocation.is.continent, value => {Model.Location.currentLocation.is.continent(value); JsCmds.Noop}),
						"Country" -> SHtml.ajaxText(
							Model.Location.currentLocation.is.country,
							value => {Model.Location.currentLocation.is.country(value); JsCmds.Noop},
							"id"->"location-country"
						),
						"County" -> SHtml.ajaxText(
							Model.Location.currentLocation.is.county,
							value => {Model.Location.currentLocation.is.county(value); JsCmds.Noop},
							"id"->"location-county"
						),/*-> AutoComplete(
								Model.Location.currentLocation.is.county,
								getCounties _,
								_ => ()
						),*/
						"DeleteLink" -> Text(""),
						"EditLink" -> Text(""),
						"ID" -> Model.Location.currentLocation.is.entityID,
						"Island" -> SHtml.ajaxText(Model.Location.currentLocation.is.island, value => {Model.Location.currentLocation.is.island(value); JsCmds.Noop}),
						"IslandGroup" -> SHtml.ajaxText(Model.Location.currentLocation.is.islandGroup, value => {Model.Location.currentLocation.is.islandGroup(value); JsCmds.Noop}),
						"Latitude" -> SHtml.ajaxText(
							Location.latitudeValue.is,
							value => {
								val showAlert = (value != "" && value != Location.latitudeValue.is)
								Location.latitudeValue(value)
								updateLocation(showAlert)
							},
							"id"->"location-latitude"
						),
						"LatitudeDMSDegrees" -> SHtml.ajaxText(
							Location.latitudeDMSDegreesValue.is,
							value => {
								val showAlert = (value != "" && value != Location.latitudeDMSDegreesValue.is)
								Location.latitudeDMSDegreesValue(value)
								updateDMSLocation(showAlert)
							},
							"id"->"location-latitude-degrees"
						),
						"LatitudeDMSMinutes" -> SHtml.ajaxText(
							Location.latitudeDMSMinutesValue.is,
							value => {
								val showAlert = (value != "" && value != Location.latitudeDMSMinutesValue.is)
								Location.latitudeDMSMinutesValue(value)
								updateDMSLocation(showAlert)
							},
							"id"->"location-latitude-minutes"
						),
						"LatitudeDMSSeconds" -> SHtml.ajaxText(
							Location.latitudeDMSSecondsValue.is,
							value => {
								val showAlert = (value != "" && value != Location.latitudeDMSSecondsValue.is)
								Location.latitudeDMSSecondsValue(value)
								updateDMSLocation(showAlert)
							},
							"id"->"location-latitude-seconds"
						),
						"LatitudeDMSDirection" -> SHtml.ajaxSelect(
							("N" :: "S" :: Nil).toList.map(i => (i.toString, i.toString)),
							Full(Location.latitudeDMSDirectionValue.is),
							value => {
								val showAlert = (value != "" && value != Location.latitudeDMSDirectionValue.is)
								Location.latitudeDMSDirectionValue(value)
								updateDMSLocation(showAlert)
							},
							"id"->"location-latitude-direction"
						),
						"Locality" -> SHtml.ajaxText(
							Model.Location.currentLocation.is.locality,
							value => {Model.Location.currentLocation.is.locality(value); JsCmds.Noop},
							"id"->"location-locality"
						),
						"Longitude" -> SHtml.ajaxText(
							Location.longitudeValue.is,
							value => {
								val showAlert = (value != "" && value != Location.longitudeValue.is)
								Location.longitudeValue(value)
								updateLocation(showAlert)
							},
							"id"->"location-longitude"
						),
						"LongitudeDMSDegrees" -> SHtml.ajaxText(
							Location.longitudeDMSDegreesValue.is,
							value => {
								val showAlert = (value != "" && value != Location.longitudeDMSDegreesValue.is)
								Location.longitudeDMSDegreesValue(value)
								updateDMSLocation(showAlert)
							},
							"id"->"location-longitude-degrees"
						),
						"LongitudeDMSMinutes" -> SHtml.ajaxText(
							Location.longitudeDMSMinutesValue.is,
							value => {
								val showAlert = (value != "" && value != Location.longitudeDMSMinutesValue.is)
								Location.longitudeDMSMinutesValue(value)
								updateDMSLocation(showAlert)
							},
							"id"->"location-longitude-minutes"
						),
						"LongitudeDMSSeconds" -> SHtml.ajaxText(
							Location.longitudeDMSSecondsValue.is,
							value => {
								val showAlert = (value != "" && value != Location.longitudeDMSSecondsValue.is)
								Location.longitudeDMSSecondsValue(value)
								updateDMSLocation(showAlert)
							},
							"id"->"location-longitude-seconds"
						),
						"LongitudeDMSDirection" -> SHtml.ajaxSelect(
							("E" :: "W" :: Nil).toList.map(i => (i.toString, i.toString)),
							Full(Location.longitudeDMSDirectionValue.is),
							value => {
								val showAlert = (value != "" && value != Location.longitudeDMSDirectionValue.is)
								Location.longitudeDMSDirectionValue(value)
								updateDMSLocation(showAlert)
							},
							"id"->"location-longitude-direction"
						),
						"LookupLocationLink" -> {(nodeSeq: NodeSeq) => <a href="javascript: lookupLocation();">{nodeSeq}</a>},
						"MaximumDepthInMeters" -> SHtml.ajaxText(
							Model.Location.currentLocation.is.maximumDepthInMeters.toString,
							value => {
								def setDouble(double: Double): Unit = {
									Model.Location.currentLocation.is.maximumDepthInMeters(double)
								}
								validateAndSetPositiveDouble(value, setDouble)
							}
						),
						"MaximumDistanceAboveSurfaceInMeters" -> SHtml.ajaxText(
							Model.Location.currentLocation.is.maximumDistanceAboveSurfaceInMeters.toString,
							value => {
								def setDouble(double: Double): Unit = {
									Model.Location.currentLocation.is.maximumDistanceAboveSurfaceInMeters(double)
								}
								validateAndSetPositiveDouble(value, setDouble)
							}
						),
						"MaximumElevationInMeters" -> SHtml.ajaxText(
							Model.Location.currentLocation.is.maximumElevationInMeters.toString,
							value => {
								def setDouble(double: Double): Unit = {
									Model.Location.currentLocation.is.maximumElevationInMeters(double)
								}
								validateAndSetPositiveDouble(value, setDouble)
							}
						),
						"Municipality" -> SHtml.ajaxText(
							Model.Location.currentLocation.is.municipality,
							value => {Model.Location.currentLocation.is.municipality(value); JsCmds.Noop},
							"id"->"location-municipality"
						),
						"MinimumDepthInMeters" -> SHtml.ajaxText(
							Model.Location.currentLocation.is.minimumDepthInMeters.toString,
							value => {
								def setDouble(double: Double): Unit = {
									Model.Location.currentLocation.is.minimumDepthInMeters(double)
								}
								validateAndSetPositiveDouble(value, setDouble)
							}
						),
						"MinimumDistanceAboveSurfaceInMeters" -> SHtml.ajaxText(
							Model.Location.currentLocation.is.minimumDistanceAboveSurfaceInMeters.toString,
							value => {
								def setDouble(double: Double): Unit = {
									Model.Location.currentLocation.is.minimumDistanceAboveSurfaceInMeters(double)
								}
								validateAndSetPositiveDouble(value, setDouble)
							}
						),
						"MinimumElevationInMeters" -> SHtml.ajaxText(
							Model.Location.currentLocation.is.minimumElevationInMeters.toString,
							value => {
								def setDouble(double: Double): Unit = {
									Model.Location.currentLocation.is.minimumElevationInMeters(double)
								}
								validateAndSetPositiveDouble(value, setDouble)
							}
						),
						"Remarks" -> SHtml.ajaxText(Model.Location.currentLocation.is.remarks, value => {Model.Location.currentLocation.is.remarks(value); JsCmds.Noop}),
						"SaveLink" -> {(nodeSeq: NodeSeq) => SHtml.ajaxButton(nodeSeq, () => save)},
						"Source" -> {(nodeSeq: NodeSeq) => {
							initSource
							new Source().renderSource(nodeSeq)
						}},
						"StateProvince" -> SHtml.ajaxText(
							Model.Location.currentLocation.is.stateProvince,
							value => {Model.Location.currentLocation.is.stateProvince(value); JsCmds.Noop},
							"id"->"location-state-province"
						),
						"VerbatimCoordinates" -> SHtml.ajaxText(Model.Location.currentLocation.is.verbatimCoordinates, value => {Model.Location.currentLocation.is.verbatimCoordinates(value); JsCmds.Noop}),
						"VerbatimDepth" -> SHtml.ajaxText(Model.Location.currentLocation.is.verbatimDepth, value => {Model.Location.currentLocation.is.verbatimDepth(value); JsCmds.Noop}),
						"VerbatimElevation" -> SHtml.ajaxText(
							Model.Location.currentLocation.is.verbatimElevation,
							value => {Model.Location.currentLocation.is.verbatimElevation(value); JsCmds.Noop},
							"id"->"location-verbatim-elevation"
						),
						"ViewLink" -> Text(""),
						"WaterBody" -> SHtml.ajaxText(Model.Location.currentLocation.is.waterBody, value => {Model.Location.currentLocation.is.waterBody(value); JsCmds.Noop}),
						"WikiPageIDLink" -> Text("")
					)
				)
			} else {
				bind(
					"location",
					xhtml,
					"CancelLink" -> Text(""),
					"Continent" -> Model.Location.currentLocation.is.continent,
					"Country" -> Model.Location.currentLocation.is.country,
					"County" -> Model.Location.currentLocation.is.county,
					"DeleteLink" -> Text(""),
					"EditLink" -> {(nodeSeq: NodeSeq) => {
						if (!S.attr("group").isEmpty && Model.Location.currentLocation.canUpdate_?) {
							<a href="javascript: showLocationDetailsUpdate();">{nodeSeq}</a>
						} else {
							Text("")
						}
					}},
					"ID" -> Model.Location.currentLocation.get.entityID,
					"Island" -> Model.Location.currentLocation.is.island,
					"IslandGroup" -> Model.Location.currentLocation.is.islandGroup,
					"Latitude" -> Model.Location.currentLocation.is.latitude,
					"Locality" -> Model.Location.currentLocation.is.locality,
					"Longitude" -> Model.Location.currentLocation.is.longitude,
					"LookupLocationLink" -> Text(""),
					"MaximumDepthInMeters" -> Model.Location.currentLocation.is.maximumDepthInMeters,
					"MaximumDistanceAboveSurfaceInMeters" -> Model.Location.currentLocation.is.maximumDistanceAboveSurfaceInMeters,
					"MaximumElevationInMeters" -> Model.Location.currentLocation.is.maximumElevationInMeters,
					"MinimumDepthInMeters" -> Model.Location.currentLocation.is.minimumDepthInMeters,
					"MinimumDistanceAboveSurfaceInMeters" -> Model.Location.currentLocation.is.minimumDistanceAboveSurfaceInMeters,
					"MinimumElevationInMeters" -> Model.Location.currentLocation.is.minimumElevationInMeters,
					"Municipality" -> Model.Location.currentLocation.is.municipality,
					"Remarks" -> Model.Location.currentLocation.is.remarks,
					"SaveLink" -> Text(""),
					"Source" -> {(nodeSeq: NodeSeq) => {
						initSource
						new Source().renderSource(nodeSeq)
					}},
					"StateProvince" -> Model.Location.currentLocation.is.stateProvince,
					"VerbatimCoordinates" -> Model.Location.currentLocation.is.verbatimCoordinates,
					"VerbatimDepth" -> Model.Location.currentLocation.is.verbatimDepth,
					"VerbatimElevation" -> Model.Location.currentLocation.is.verbatimElevation,
					"ViewLink" -> {(nodeSeq: NodeSeq) => {
						<a href={"location?ID=" + Model.Location.currentLocation.is.entityID}>{nodeSeq}</a>
					}},
					"WaterBody" -> Model.Location.currentLocation.is.waterBody,
					"WikiPageIDLink" -> {(nodeSeq: NodeSeq) => {
						val wikiPageID = Model.Location.currentLocation.is.wikiPageID.is
						if (0 < wikiPageID) {
							<a href={"/wiki?WikiPageID=" + wikiPageID}>{wikiPageID}</a>
						} else {
							if (!S.attr("group").isEmpty) {
								def create() = {
									val newWikiPageID = Controller.WikiPage.create("location", Model.Location.currentLocation.is.entityID, Model.Location.currentLocation.is.source.obj.open_!.entityID)
									Model.Location.currentLocation.is.wikiPageID(newWikiPageID)
									Model.Location.currentLocation.is.save
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
		def showLocationDetails() = {
			Map.markerDraggable(false)
			
			SetHtml("location-details", <lift:embed what="lab/location-details" />) &
			new Map().renderMapJavaScriptCommand &
			new Map().renderMarkerJavaScriptCommand
		}
		
		def showLocationDetailsUpdate() = {
			Map.markerDraggable(true)
			
			SetHtml("location-details", <lift:embed what="lab/location-details-update" />) &
			new Map().renderMapJavaScriptCommand &
			new Map().renderMarkerJavaScriptCommand
		}
		
		def cancelLocationDetailsUpdate() = {
			Model.Location.currentLocation(Model.Location.find(Model.Location.currentLocation.is.entityID) openOr null)
			Map.markerDraggable(false)
			
			SetHtml("location-details", <lift:embed what="lab/location-details" />) &
			new Map().renderMapJavaScriptCommand &
			new Map().renderMarkerJavaScriptCommand
		}
		
		def updateLocation(location: Any): JsCmd = {
			location match {
				case map: scala.collection.immutable.Map[String, String] => {
					val latitude = map.get("latitude").get.toDouble
					val longitude = map.get("longitude").get.toDouble
					
					Model.Location.currentLocation.is.latitude(latitude)
					Model.Location.currentLocation.is.longitude(longitude)
					Map.markerLatitude(latitude)
					Map.markerLongitude(longitude)
					
					Location.latitudeValue(latitude.toString)
					Location.latitudeDMSDegreesValue(Map.convertDDtoDMSDegrees(latitude).toString)
					Location.latitudeDMSMinutesValue(Map.convertDDtoDMSMinutes(latitude).toString)
					Location.latitudeDMSSecondsValue(Map.convertDDtoDMSSeconds(latitude).toString)
					Location.latitudeDMSDirectionValue(if (latitude < 0.0) "S" else "N")
					Location.longitudeValue(longitude.toString)
					Location.longitudeDMSDegreesValue(Map.convertDDtoDMSDegrees(longitude).toString)
					Location.longitudeDMSMinutesValue(Map.convertDDtoDMSMinutes(longitude).toString)
					Location.longitudeDMSSecondsValue(Map.convertDDtoDMSSeconds(longitude).toString)
					Location.longitudeDMSDirectionValue(if (longitude < 0.0) "W" else "E")
					
					SetValById("location-latitude", Location.latitudeValue.is) &
					SetValById("location-latitude-degrees", Location.latitudeDMSDegreesValue.is) &
					SetValById("location-latitude-minutes", Location.latitudeDMSMinutesValue.is) &
					SetValById("location-latitude-seconds", Location.latitudeDMSSecondsValue.is) &
					SetValById("location-latitude-direction", Location.latitudeDMSDirectionValue.is) &
					SetValById("location-longitude", Location.longitudeValue.is) &
					SetValById("location-longitude-degrees", Location.longitudeDMSDegreesValue.is) &
					SetValById("location-longitude-minutes", Location.longitudeDMSMinutesValue.is) &
					SetValById("location-longitude-seconds", Location.longitudeDMSSecondsValue.is) &
					SetValById("location-longitude-direction", Location.longitudeDMSDirectionValue.is)
				}
				case _ => {
					Noop
				}
			}
		}
		
		Script(
			Function(
				"showLocationDetails",
				Nil,
				SHtml.ajaxInvoke(showLocationDetails)._2
			)
		) ++
		Script(
			Function(
				"showLocationDetailsUpdate",
				Nil,
				SHtml.ajaxInvoke(showLocationDetailsUpdate)._2
			)
		) ++
		Script(
			Function(
				"cancelLocationDetailsUpdate",
				Nil,
				SHtml.ajaxInvoke(cancelLocationDetailsUpdate)._2
			)
		) ++
		Script(
			Function(
				"updateLocation",
				"latitude" :: "longitude" :: Nil,
				SHtml.jsonCall(JsObj("latitude" -> JsVar("latitude"), "longitude" -> JsVar("longitude")), updateLocation _)._2
			)
		)
	}
	
	def renderEvents(xhtml: NodeSeq): NodeSeq = {
		if (null == Model.Location.currentLocation.is) {
			Text("")
		} else {
			bind(
				"location",
				xhtml,
				"events" -> {
					(nodeSeq: NodeSeq) =>
						Model.Location.currentLocation.is.events.filter(event => Source.sourceFilter(event.source.obj.openOr(null))).flatMap(event => bind(
							"event",
							nodeSeq,
							"Date" -> event.date,
							"Habitat" -> event.habitat,
							"ID" -> event.entityID, 
							"SelectLink" -> {(nodeSeq: NodeSeq) => {
								<a href={"javascript: selectEvent(" + event.entityID + ");"}>{nodeSeq}</a>
							}}
						))
				}
			)

		}
	}
	
	def renderLocationsJSON: NodeSeq = {
		val group: String =
			if (S.attr("group").isEmpty) {
				""
			} else {
				"?Group=true"
			}
		<script type="text/javascript" src={S.hostAndPath + "/service/location/locationsJSON" + group}></script>
	}
	
	def renderLocationsOnMap: NodeSeq = {
		Script(renderLocationsOnMapJavaScriptCommand)
	}
	
	def renderLocationsOnMapJavaScriptCommand: JsCmd = {
		Call(
			"addMarkers",
			JsVar("locationsJSON"),
			JsTrue
		)
	}
}

object Location {
	object latitudeValue extends RequestVar[String]("")
	object latitudeDMSDegreesValue extends RequestVar[String]("")
	object latitudeDMSMinutesValue extends RequestVar[String]("")
	object latitudeDMSSecondsValue extends RequestVar[String]("")
	object latitudeDMSDirectionValue extends RequestVar[String]("")
	object longitudeValue extends RequestVar[String]("")
	object longitudeDMSDegreesValue extends RequestVar[String]("")
	object longitudeDMSMinutesValue extends RequestVar[String]("")
	object longitudeDMSSecondsValue extends RequestVar[String]("")
	object longitudeDMSDirectionValue extends RequestVar[String]("")
}