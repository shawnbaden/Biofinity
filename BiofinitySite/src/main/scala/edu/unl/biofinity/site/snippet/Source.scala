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

import scala.xml._

class Source {
	def renderSource(xhtml: NodeSeq): NodeSeq = {
		if (null == Model.Source.currentSource.is) {
			return xhtml
		} else {
			if (Model.Source.currentSource.sourceType != Model.SourceType.Public) {
				var group = Model.Group.find(By(Model.Group.publicSource, Model.Source.currentSource.entityID)) openOr null
				if (null == group) {
					group = Model.Group.find(By(Model.Group.privateSource, Model.Source.currentSource.entityID)) openOr null
				}
				if (!Model.User.currentUser.groups.exists(_ == group)) {
					return xhtml
				}
			}
			
			bind(
				"source",
				xhtml,
				"Stats" -> {(nodeSeq: NodeSeq) => {
					bind(
						"stats",
						nodeSeq,
						"Classifications" -> Global.NUMBER_FORMAT.format(Model.Classification.count(BySql("SOURCE_ID = ?", null, Model.Source.currentSource.entityID))),
						"Events" ->          Global.NUMBER_FORMAT.format(Model.Event.count(         BySql("SOURCE_ID = ?", null, Model.Source.currentSource.entityID))),
						"Locations" ->       Global.NUMBER_FORMAT.format(Model.Location.count(      BySql("SOURCE_ID = ?", null, Model.Source.currentSource.entityID))),
						"MediaFiles" ->      Global.NUMBER_FORMAT.format(Model.MediaFile.count(     BySql("SOURCE_ID = ?", null, Model.Source.currentSource.entityID))),
						"Occurrences" ->     Global.NUMBER_FORMAT.format(Model.Occurrence.count(    BySql("SOURCE_ID = ?", null, Model.Source.currentSource.entityID)))
					)
				}},
				"SourceType" -> Model.SourceType.typeMapInverted(Model.SourceType(Model.Source.currentSource.is.sourceType.toInt))
			)
		}
	}
}

object Source {
	def sourceFilter = (source: Model.Source) => {
		if (null == source) {
			false
		} else {
			if (S.attr("group").isEmpty) {
				source.public_?
			} else {
				(source == Model.User.currentGroup.privateSource.obj.openOr(null) || source == Model.User.currentGroup.publicSource.obj.openOr(null))
			}
		}
	}
}