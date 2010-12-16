package edu.unl.biofinity.site.snippet

import edu.unl.biofinity.api.{model => Model}
import edu.unl.biofinity.api.{service => Service}
import edu.unl.biofinity.site.TwitterBioBlitzImporterScheduler

import java.sql.PreparedStatement
import java.sql.ResultSet
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

class Administration {
	def renderScripts: NodeSeq = {
		def clearBioBlitz() = {
			val group = Service.BioBlitz.getGroup
			if (null != group) {
				val publicSource = group.publicSource.obj openOr null
				val privateSource = group.privateSource.obj openOr null
				
				val mediaFiles = Model.MediaFile.findAllByPreparedStatement({database =>
					val preparedStatement: PreparedStatement = database.connection.prepareStatement("select entity_id from media_files where (source_id = ? or source_id = ?)")
					preparedStatement.setLong(1, publicSource.entityID)
					preparedStatement.setLong(2, privateSource.entityID)
					preparedStatement
				}).removeDuplicates
				mediaFiles.foreach(_.deleteFull_!)
				
				val occurrences = Model.Occurrence.findAllByPreparedStatement({database =>
					val preparedStatement: PreparedStatement = database.connection.prepareStatement("select entity_id from occurrences where (source_id = ? or source_id = ?)")
					preparedStatement.setLong(1, publicSource.entityID)
					preparedStatement.setLong(2, privateSource.entityID)
					preparedStatement
				}).removeDuplicates
				occurrences.foreach(_.deleteFull_!)
				
				val events = Model.Event.findAllByPreparedStatement({database =>
					val preparedStatement: PreparedStatement = database.connection.prepareStatement("select entity_id from events where (source_id = ? or source_id = ?)")
					preparedStatement.setLong(1, publicSource.entityID)
					preparedStatement.setLong(2, privateSource.entityID)
					preparedStatement
				}).removeDuplicates
				events.foreach(_.deleteFull_!)
				
				val locations = Model.Location.findAllByPreparedStatement({database =>
					val preparedStatement: PreparedStatement = database.connection.prepareStatement("select entity_id from locations where (source_id = ? or source_id = ?)")
					preparedStatement.setLong(1, publicSource.entityID)
					preparedStatement.setLong(2, privateSource.entityID)
					preparedStatement
				}).removeDuplicates
				locations.foreach(_.deleteFull_!)
				
				val classifications = Model.Classification.findAllByPreparedStatement({database =>
					val preparedStatement: PreparedStatement = database.connection.prepareStatement("select entity_id from classifications where (source_id = ? or source_id = ?)")
					preparedStatement.setLong(1, publicSource.entityID)
					preparedStatement.setLong(2, privateSource.entityID)
					preparedStatement
				}).removeDuplicates
				classifications.foreach(_.deleteFull_!)
			}
			
			Alert("All BioBlitz data has been cleared.") & JsCmds.Run("location.reload(true)")
		}
		
		Script(
			Function(
				"showClearBioBlitzDialog",
				Nil,
				JsRaw(
					"if (confirm(\"Are you sure you want to clear all BioBlitz data?  This action cannot be undone.\")) {" +
						"clearBioBlitz();" +
					"}"
				)
			)
		) ++
		Script(
			Function(
				"clearBioBlitz",
				Nil,
				SHtml.ajaxInvoke(clearBioBlitz)._2
			)
		)
	}
 	
	def renderPost(xhtml: scala.xml.Group): NodeSeq = {
		var title = ""
		var description = ""
		
		def create() = {
			val post: Model.Post = Model.Post.create
			post.title(title)
			post.description(description)
			post.published(new Date())
			post.save
		}
		
		bind("p", xhtml,
			"title" -> SHtml.text("", title = _),
			"description" -> SHtml.textarea("", description = _),
			"submit" -> SHtml.submit("Add Post", create))
	}
	
	def renderGroupRequests(xhtml: NodeSeq): NodeSeq = { 
		Model.GroupRequest.findAll().flatMap(groupRequest => { 
		val requested_by: Model.User = groupRequest.user.obj.open_!
		
		def approve(groupRequest: Model.GroupRequest) = {
			val group: Model.Group = Model.Group.create
			group.name(groupRequest.name)
			group.description(groupRequest.description)
			group.institution(groupRequest.institution)
			group.department(groupRequest.department)
			
			val uid: String = { 
				val id: String = groupRequest.user.obj.open_!.person.obj.open_!.lastName
				val matched = Model.Source.findAll(Like(Model.Source.uniqueID, id+"%")).length
				
				if (matched != 0) id+matched
				else id
			}
			
			val publicSource: Model.Source = Model.Source.create.name(group.name).uniqueID(uid).sourceType(Model.SourceType.Public)
			val privateSource: Model.Source = Model.Source.create.name(group.name+" (private)").uniqueID(uid+"_PRIVATE").sourceType(Model.SourceType.Private)
			
			publicSource.save
			privateSource.save
			
			group.publicSource(publicSource)
			group.privateSource(privateSource)
			
			group.save
			
			val groupUser: Model.GroupUser = new Model.GroupUser
			groupUser.user(groupRequest.user.obj.open_!)
			groupUser.group(group)
			groupUser.approved(true)
			groupUser.userType(Model.GroupUserType.Administrator)
			
			groupUser.save
			
			groupRequest.delete_!
			Alert(groupRequest.name+" request approved") & JsCmds.Run("location.reload(true)")
		}
		
		def deny(groupRequest: Model.GroupRequest) = { 
			groupRequest.delete_!
			Alert(groupRequest.name+" request denied") & JsCmds.Run("location.reload(true)")
		}
		
		bind("g", xhtml, "name" -> groupRequest.name, 
			"instutition" -> groupRequest.institution, 
			"department" -> groupRequest.department, 
			"description" -> groupRequest.description, 
			"requestor" -> Text(requested_by.person.obj.open_!.fullName), 
			"approve" -> SHtml.a(() => approve(groupRequest), Text("Approve Request") ), 
			"deny" -> SHtml.a(() => deny(groupRequest), Text("Deny Request")) ) 
		})
	}
	
	def renderGroup(xhtml: NodeSeq): NodeSeq = {
		val groups: List[Model.Group] = Model.Group.findAll() 
		groups.flatMap(
			group => bind(
				"g",
				xhtml,
				"ID" -> group.entityID,
				"Name" -> <a href={"/data/group?SourceID=" + group.publicSource.obj.open_!.entityID}>{group.name}</a>,
				"Description" -> group.description
			)
		)
	}
	
	def renderFixer(xhtml: NodeSeq): NodeSeq = {
		def fixClassificationNames(): JsCmd = {
			val classifications: List[Model.Classification] = Model.Classification.findAll()
			classifications.foreach(classification => {
				val name =
					if (null != classification.name.is && !classification.name.is.trim().equals("")) {
						classification.name.is.trim().replaceAll(" +", " ")
					} else {
						val taxons: List[Model.Taxon] = classification.taxons
						if (1 < taxons.length) {
							val topTaxonName = try {
								taxons(taxons.length - 2).name.is.trim
							} catch { case e:Exception => "" }
							val bottomTaxonName = try {
								taxons(taxons.length - 1).name.is.trim
							} catch { case e:Exception => "" }
							topTaxonName + " " + bottomTaxonName
						} else if (0 < taxons.length) {
							try {
								taxons(taxons.length - 1).name.is.trim
							} catch { case e:Exception => "" }
						} else {
							""
						}
					}.trim
				if (!classification.name.is.equals(name)) {
					classification.name(name)
					classification.save
				}
			})
			
			JsCmds.Run("location.reload(true)")
		}
		
		def fixTaxonNames(): JsCmd = {
			val taxons: List[Model.Taxon] = Model.Taxon.findAll()
			taxons.foreach(taxon => {
				if (taxon.name.contains("-")) {
					taxon.name(taxon.name.replaceAll("-", ""))
				}
				if (taxon.name.length > 1) {
					taxon.name(taxon.name.substring(0, 1).toUpperCase() + taxon.name.substring(1).toLowerCase())
				} else {
					taxon.name(taxon.name.toUpperCase())
				}
				if (taxon.rank.contains("-")) {
					taxon.rank(taxon.rank.replaceAll("-", ""))
				}
				if (taxon.rank.length > 1) {
					taxon.rank(taxon.rank.substring(0, 1).toUpperCase() + taxon.rank.substring(1).toLowerCase())
				} else {
					taxon.rank(taxon.rank.toUpperCase())
				}
				taxon.save
			})
			
			JsCmds.Run("location.reload(true)")
		}
		
		bind(
			"fixer",
			xhtml,
			"ClassificationNames" -> {(nodeSeq: NodeSeq) => SHtml.ajaxButton(nodeSeq, () => fixClassificationNames)},
			"TaxonNames" -> {(nodeSeq: NodeSeq) => SHtml.ajaxButton(nodeSeq, () => fixTaxonNames)}
		)
	}
	
	def renderTwitterBioBlitzImporter(xhtml: NodeSeq): NodeSeq = {
		def start(): JsCmd = {
			TwitterBioBlitzImporterScheduler.start()
			JsCmds.Run("location.reload(true)")
		}
		
		def stop(): JsCmd = {
			TwitterBioBlitzImporterScheduler.stop()
			JsCmds.Run("location.reload(true)")
		}
		
		bind(
			"twitter-bioblitz-importer",
			xhtml,
			"StartLink" -> {(nodeSeq: NodeSeq) => SHtml.ajaxButton(nodeSeq, () => start)},
			"Status" -> {if (TwitterBioBlitzImporterScheduler.running_?) "RUNNING" else "STOPPED"},
			"StopLink" -> {(nodeSeq: NodeSeq) => SHtml.ajaxButton(nodeSeq, () => stop)}
		)
	}

	def renderBioBlitz(xhtml: NodeSeq): NodeSeq = {
		bind(
			"bioblitz",
			xhtml,
			"ClearLink" -> {(nodeSeq: NodeSeq) => <a href="javascript: showClearBioBlitzDialog();">{nodeSeq}</a>}
		)
	}
}