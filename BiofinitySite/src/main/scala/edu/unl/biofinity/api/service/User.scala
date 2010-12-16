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

object User {
	def read(r: Req): Box[LiftResponse] = {
		val event: Model.Event = Model.Event.find(S.param("ID") openOr -1) openOr null

		if (Model.User.signedIn_?) {
			Full(
				XmlResponse(
<User>
	<ID>{Model.User.currentUser.is.entityID.is.toString}</ID>
	<Group>{Model.User.currentGroup.is.entityID.is.toString}</Group>
</User>
				)
			)
		} else {
			Full(
				PlainTextResponse(
					"No user is currently authenticated.",
					List(),
					401
				)
			)
		}
	}
}