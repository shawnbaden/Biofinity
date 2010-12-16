package edu.unl.biofinity.site.snippet

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

class Map {
	def renderMap: NodeSeq = {
		Script(renderMapJavaScriptCommand)
	}
	
	def renderMapJavaScriptCommand: JsCmd = {
		val latitude = S.attr("latitude") openOr Map.mapLatitude.get
		val longitude = S.attr("longitude") openOr Map.mapLongitude.get
		val zoom = S.attr("zoom") openOr Map.mapZoom.get
		Call(
			"displayMap",
			JsVar(latitude.toString),
			JsVar(longitude.toString),
			JsVar(zoom.toString)
		)
	}
	
	def setBounds: NodeSeq = {
		Script(
			Call(
				"setBounds",
				JsVar(Map.maximumLatitude.toString),
				JsVar(Map.maximumLongitude.toString),
				JsVar(Map.minimumLatitude.toString),
				JsVar(Map.minimumLongitude.toString)
			)
		)
	}
	
	def setZoom: NodeSeq = {
		val zoom = S.attr("zoom") openOr "-1"
		if (!zoom.equals("-1")) {
			Script(
				Call(
					"setZoom",
					JsVar(zoom)
				)
			)
		} else {
			Text("")
		}
	}
	
	def renderMarker: NodeSeq = {
		Script(renderMarkerJavaScriptCommand)
	}
	
	def renderMarkerJavaScriptCommand: JsCmd = {
		val latitude = S.attr("latitude") openOr Map.markerLatitude.get
		val longitude = S.attr("longitude") openOr Map.markerLongitude.get
		val title = S.attr("title") openOr Map.markerTitle.get
		val targetURL = S.attr("targetURL") openOr Map.markerURL.get
		val centerMap: Boolean = {
			if (S.attr("centerMap").isEmpty) {
				Map.markerCenterMap
			} else {
				true
			}
		}
		val draggable: Boolean = {
			if (S.attr("draggable").isEmpty) {
				Map.markerDraggable
			} else {
				true
			}
		}
		
		Call(
			"addMarker",
			JsVar(latitude.toString),
			JsVar(longitude.toString),
			Str(title.toString),
			Str(targetURL.toString),
			JsFalse,
			if (draggable) {
				JsTrue
			} else {
				JsFalse
			},
			if (centerMap) {
				JsTrue
			} else {
				JsFalse
			}
		)
	}
}

object Map {
	object mapLatitude extends RequestVar[Double](0)
	object mapLongitude extends RequestVar[Double](0)
	object mapZoom extends RequestVar[Integer](3)
	
	object maximumLatitude extends RequestVar[Double](-10000)
	object maximumLongitude extends RequestVar[Double](-10000)
	object minimumLatitude extends RequestVar[Double](10000)
	object minimumLongitude extends RequestVar[Double](10000)
	
	def adjustLimits(latitude: Double, longitude: Double) = {
		if (latitude > maximumLatitude.get) {
			maximumLatitude(latitude)
		}
		if (longitude > maximumLongitude.get) {
			maximumLongitude(longitude)
		}
		if (latitude < minimumLatitude.get) {
			minimumLatitude(latitude)
		}
		if (longitude < minimumLongitude.get) {
			minimumLongitude(longitude)
		}
	}
	
	object markerLatitude extends RequestVar[Double](0)
	object markerLongitude extends RequestVar[Double](0)
	object markerTitle extends RequestVar[String]("")
	object markerURL extends RequestVar[String]("")
	object markerCenterMap extends RequestVar[Boolean](true)
	object markerDraggable extends RequestVar[Boolean](false)
	
	def convertDDtoDMSDegrees(decimalDegrees: Double): Int = {
		decimalDegrees.abs.toInt
	}
	
	def convertDDtoDMSMinutes(decimalDegrees: Double): Int = {
		((decimalDegrees.abs - decimalDegrees.abs.toInt) * 60).toInt
	}
	
	def convertDDtoDMSSeconds(decimalDegrees: Double): Double = {
		(((decimalDegrees.abs - decimalDegrees.abs.toInt) * 60) - ((decimalDegrees.abs - decimalDegrees.abs.toInt) * 60).toInt) * 60
	}
	
	def convertDMStoDD(degrees: Int, minutes: Int, seconds: Double): Double = {
		degrees + (((minutes * 60) + seconds) / 3600)
	}
}