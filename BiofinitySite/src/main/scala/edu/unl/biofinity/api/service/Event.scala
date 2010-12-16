package edu.unl.biofinity.api.service

import edu.unl.biofinity.api.{model => Model}
import edu.unl.biofinity.site.{snippet => Snippet}

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

object Event {
	def read(r: Req): Box[LiftResponse] = {
		val event: Model.Event = Model.Event.find(S.param("ID") openOr -1) openOr null

		if (null == event) {
			Full(NotFoundResponse(""))
		} else {
			if (event.canRead_?) {
				var dateValue = ""
				if (null != event.date.is) {
					dateValue = Snippet.Global.ISO_8601_2004_E.format(event.date.is)
				}
				Full(
					XmlResponse(
<dwr:DarwinRecordSet
	xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
	xsi:schemaLocation="http://rs.tdwg.org/dwc/dwcrecord/ http://rs.tdwg.org/dwc/xsd/tdwg_dwc_classes.xsd"
	xmlns:dcterms="http://purl.org/dc/terms/"
	xmlns:dwc="http://rs.tdwg.org/dwc/terms/"
	xmlns:dwr="http://rs.tdwg.org/dwc/dwcrecord/">
	<dwc:Event>
		<dwc:eventDate>{dateValue}</dwc:eventDate>
		<dwc:eventID>{"urn:lsid:biofinity.unl.edu:Event:" + event.entityID.is.toString}</dwc:eventID>
		<dwc:habitat>{event.habitat.is}</dwc:habitat>
		<dwc:samplingEffort>{event.samplingEffort.is}</dwc:samplingEffort>
		<dwc:samplingProtocol>{event.samplingProtocol}</dwc:samplingProtocol>
		<dwc:locationID>{"urn:lsid:biofinity.unl.edu:Location:" + event.location.obj.openOr(Model.Location).entityID.is.toString}</dwc:locationID>
		<dwc:verbatimEventDate>{event.verbatimDate.is}</dwc:verbatimEventDate>
	</dwc:Event>
</dwr:DarwinRecordSet>
					)
				)
			} else {
				Full(UnauthorizedResponse(""))
			}
		}
	}
}