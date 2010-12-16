package edu.unl.biofinity.site.snippet

import edu.unl.biofinity.api.{model => Model}

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

class HDZ {
	def initCreateOccurrence: NodeSeq = {
		val source: Model.Source = Model.Source.find(By(Model.Source.uniqueID, "HDZ_AMPHIBIAN_PRIVATE")) openOr null
		if (null == source) {
			S.redirectTo("/HDZ/amphibian")
		} else {
			Model.Occurrence.currentOccurrence(Model.Occurrence.create.source(source).occurrenceType(Model.OccurrenceType.HumanObservation))
			Model.Event.currentEvent(Model.Event.create.source(source))
			Model.Location.currentLocation(Model.Location.create.source(source))
			Model.Location.currentLocation.is.continent("NA")
			Model.Location.currentLocation.is.country("United States")
		}
		Text("")
	}
 	
	def initCreateOccurrenceMediaFile: NodeSeq = {
		var redirect = true
		val source: Model.Source = Model.Source.find(By(Model.Source.uniqueID, "HDZ_AMPHIBIAN_PRIVATE")) openOr null
		if (null != source) {
			val occurrence = Model.Occurrence.find(S.param("OccurrenceID") openOr -1) openOr null
			if (null != occurrence && occurrence.source.obj.open_! == source) {
				redirect = false
				Model.Occurrence.currentOccurrence(occurrence)
			}
		}
		if (redirect) {
			S.redirectTo("/HDZ/amphibian")
		}
		Text("")
	}
 	
	def renderCreateOccurrence(xhtml: NodeSeq): NodeSeq = {
	  	var airTemperature: String = ""
	  	var chytridSwabNumber: String = ""
	  	var commonName: String = ""
	  	var habitat: String = ""
	  	var school: String = ""
	  	var teacherName: String = ""
	  	var teacherEmail: String = ""
	  	var waterTemperature: String = ""
	  	var waterPH: String = ""
	  	var waterNitrates: String = ""
	  	var waterDissolvedOxygen: String = ""
	  	var waterTurbidity: String = ""
	  	var waterFishPresent: Boolean = false
	  	var waterVegetationPresent: Boolean = false
	  	
		def save(): JsCmd = {
			if ("" == school) {
				Alert("Enter the school you attend.")
			} else if ("" == teacherName) {
				Alert("Enter your teacher's name.")
			} else if ("" == teacherEmail) {
				Alert("Enter your teacher's email.")
			} else if (null == Model.Location.currentLocation.is.stateProvince.is || "" == Model.Location.currentLocation.is.stateProvince.is) {
				Alert("Enter the state where the event occurred.")
			} else if (null == Model.Location.currentLocation.is.county.is || "" == Model.Location.currentLocation.is.county.is) {
				Alert("Enter the county where the event occurred.")
			} else if (null == Model.Location.currentLocation.is.latitude.is || 0.0 == Model.Location.currentLocation.is.latitude.is) {
				Alert("Latitude cannot be blank.  Drag the marker to the location where the event occurred.")
			} else if (null == Model.Location.currentLocation.is.longitude.is || 0.0 == Model.Location.currentLocation.is.longitude.is) {
				Alert("Longitude cannot be blank.  Drag the marker to the location where the event occurred.")
			} else if (null == Model.Event.currentEvent.is.date.is) {
				Alert("Enter a date.")
			} else if ("" == habitat) {
				Alert("Describe the habitat of the location.")
			} else {
				Model.Location.currentLocation.is.save
				
				Model.Event.currentEvent.is.location(Model.Location.currentLocation.is)
				Model.Event.currentEvent.is.habitat(habitat)
				var additionalPropertyBundle: Model.AdditionalPropertyBundle = Model.AdditionalPropertyBundle.create
				additionalPropertyBundle.save
				Model.AdditionalProperty.create.additionalPropertyBundle(additionalPropertyBundle).name("Air Temperature").value(airTemperature).save
				Model.AdditionalProperty.create.additionalPropertyBundle(additionalPropertyBundle).name("Water Temperature").value(waterTemperature).save
				Model.AdditionalProperty.create.additionalPropertyBundle(additionalPropertyBundle).name("Water PH").value(waterPH).save
				Model.AdditionalProperty.create.additionalPropertyBundle(additionalPropertyBundle).name("Water Nitrates").value(waterNitrates).save
				Model.AdditionalProperty.create.additionalPropertyBundle(additionalPropertyBundle).name("Water Dissolved Oxygen").value(waterDissolvedOxygen).save
				Model.AdditionalProperty.create.additionalPropertyBundle(additionalPropertyBundle).name("Water Turbidity").value(waterTurbidity).save
				Model.AdditionalProperty.create.additionalPropertyBundle(additionalPropertyBundle).name("Water Fish Present").valueType(Model.AdditionalPropertyType.Boolean).value(Model.AdditionalPropertyType.booleanAsValue(waterFishPresent)).save
				Model.AdditionalProperty.create.additionalPropertyBundle(additionalPropertyBundle).name("Water Vegetation Present").valueType(Model.AdditionalPropertyType.Boolean).value(Model.AdditionalPropertyType.booleanAsValue(waterVegetationPresent)).save
				Model.Event.currentEvent.is.additionalPropertyBundle(additionalPropertyBundle)
				Model.Event.currentEvent.is.save
				
				Model.Occurrence.currentOccurrence.is.event(Model.Event.currentEvent.is)
				additionalPropertyBundle = Model.AdditionalPropertyBundle.create
				additionalPropertyBundle.save
				Model.AdditionalProperty.create.additionalPropertyBundle(additionalPropertyBundle).name("Chytrid Swab #").value(chytridSwabNumber).save
				Model.AdditionalProperty.create.additionalPropertyBundle(additionalPropertyBundle).name("Common Name").value(commonName).save
				Model.AdditionalProperty.create.additionalPropertyBundle(additionalPropertyBundle).name("School Name").value(school).save
				Model.AdditionalProperty.create.additionalPropertyBundle(additionalPropertyBundle).name("Teacher Name").value(teacherName).save
				Model.AdditionalProperty.create.additionalPropertyBundle(additionalPropertyBundle).name("Teacher Email").value(teacherEmail).save
				val person = Model.User.currentUser.person.obj.open_!
				Model.AdditionalProperty.create.additionalPropertyBundle(additionalPropertyBundle).name("User Name").value(person.fullName).save
				Model.AdditionalProperty.create.additionalPropertyBundle(additionalPropertyBundle).name("User Email").value(Model.User.currentUser.email).save
				Model.Occurrence.currentOccurrence.is.additionalPropertyBundle(additionalPropertyBundle)
				Model.Occurrence.currentOccurrence.is.save
				
				JsCmds.RedirectTo("/HDZ/amphibian/success?OccurrenceID=" + Model.Occurrence.currentOccurrence.is.entityID)
			}
		}
		
		SHtml.ajaxForm(
			bind(
				"amphibian",
				xhtml,
				"AirTemperature" -> SHtml.ajaxText(
					"",
					value => {
						val showAlert = (value != "" && value != airTemperature)
						airTemperature = value
						try {
							val temperature = value.toDouble
							JsCmds.Noop
						} catch {
							case e:NumberFormatException => {
								if (showAlert) {
									Alert("Air temperature must be a number.")
								} else {
									JsCmds.Noop
								}
							}
						}
					}
				),
				"ChytridSwabNumber" -> SHtml.ajaxText("", value => {chytridSwabNumber = value; JsCmds.Noop}),
				"CommonName" -> SHtml.ajaxText("", value => {commonName = value; JsCmds.Noop}),
				"Habitat" -> SHtml.ajaxText("", value => {habitat = value; JsCmds.Noop}),
				"School" -> SHtml.ajaxText("", value => {school = value; JsCmds.Noop}),
				"TeacherName" -> SHtml.ajaxText("", value => {teacherName = value; JsCmds.Noop}),
				"TeacherEmail" -> SHtml.ajaxText("", value => {teacherEmail = value; JsCmds.Noop}),
				"WaterTemperature" -> SHtml.ajaxText(
					"",
					value => {
						val showAlert = (value != "" && value != waterTemperature)
						waterTemperature = value
						try {
							val temperature = value.toDouble
							JsCmds.Noop
						} catch {
							case e:NumberFormatException => {
								if (showAlert) {
									Alert("Water temperature must be a number.")
								} else {
									JsCmds.Noop
								}
							}
						}
					}
				),
				"WaterPH" -> SHtml.ajaxText("", value => {waterPH = value; JsCmds.Noop}),
				"WaterNitrates" -> SHtml.ajaxText("", value => {waterNitrates = value; JsCmds.Noop}),
				"WaterDissolvedOxygen" -> SHtml.ajaxText("", value => {waterDissolvedOxygen = value; JsCmds.Noop}),
				"WaterTurbidity" -> SHtml.ajaxText("", value => {waterTurbidity = value; JsCmds.Noop}),
				"WaterFishPresent" -> SHtml.ajaxCheckbox(false, value => {waterFishPresent = value; JsCmds.Noop}),
				"WaterVegetationPresent" -> SHtml.ajaxCheckbox(false, value => {waterVegetationPresent = value; JsCmds.Noop}),
				"Save" -> SHtml.ajaxButton("Save", () => save)
			)
		)
	}
}