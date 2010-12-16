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

class Group {
	def init: NodeSeq = {
		val group = {
			val group = Model.Group.find(S.param("ID") openOr -1) openOr null
			if (null == group) {
				val source = Model.Source.find(S.param("SourceID") openOr -1) openOr null
				if (null != source) {
					source.group
				} else {
					null
				}
			} else {
				group
			}
		}
		if (null != group) {
			if (!S.attr("group").isEmpty) {
				if (Model.User.currentUser.groups.exists(_ == group)) {
					Model.Group.currentGroup(group)
				}
			} else {
				Model.Group.currentGroup(group)
			}
		}
		Text("")
	}
	
	def renderGroup(xhtml: NodeSeq): NodeSeq = {
		if (null == Model.Group.currentGroup.is) {
			Text("")
		} else {
			def initPrivateSource() = {
				Model.Source.currentSource(Model.Group.currentGroup.is.privateSource.obj openOr null)
			}
			
			def initPublicSource() = {
				Model.Source.currentSource(Model.Group.currentGroup.is.publicSource.obj openOr null)
			}
			
			if (!S.attr("group").isEmpty && !S.attr("update").isEmpty && Model.Group.currentGroup.canUpdate_?) {
				def save(): JsCmd = {
					val name = Model.Group.currentGroup.is.name.is.trim.replaceAll(" +", " ")
					if (name.equals("")) {
						Alert("Name cannot be empty.")
					} else {
						if (null != Model.Group.currentGroup.is.publicSource.obj) {
							val publicSource = Model.Group.currentGroup.is.publicSource.obj.open_!
							publicSource.name(Model.Group.currentGroup.is.name.is)
							publicSource.save
						}
						if (null != Model.Group.currentGroup.is.privateSource.obj) {
							val privateSource = Model.Group.currentGroup.is.privateSource.obj.open_!
							privateSource.name(Model.Group.currentGroup.is.name.is + " (private)")
							privateSource.save
						}
						if (!Model.Group.currentGroup.is.name.is.equals(name)) {
							Model.Group.currentGroup.is.name(name)
						}
						Model.Group.currentGroup.is.save

						SetHtml("group-details", <lift:embed what="/lab/group-details"/>)
					}
				}
				
				bind(
					"group",
					xhtml,
					"CancelLink" -> {(nodeSeq: NodeSeq) => <a href="javascript: cancelGroupDetailsUpdate();">{nodeSeq}</a>},
					"Department" -> SHtml.ajaxText(Model.Group.currentGroup.is.department, value => {Model.Group.currentGroup.is.department(value);JsCmds.Noop}),
					"Description" -> SHtml.ajaxTextarea(Model.Group.currentGroup.is.description, value => {Model.Group.currentGroup.is.description(value);JsCmds.Noop}),
					"EditLink" -> Text(""),
					"EventAdditionalPropertyBundle" -> {(nodeSeq: NodeSeq) => {
						var additionalPropertyBundle = Model.Group.currentGroup.is.eventAdditionalPropertyBundle.obj openOr null
						if (null == additionalPropertyBundle) {
							additionalPropertyBundle = Model.AdditionalPropertyBundle.create
							additionalPropertyBundle.save
							Model.Group.currentGroup.is.eventAdditionalPropertyBundle(additionalPropertyBundle)
							Model.Group.currentGroup.is.save
							//Model.Group.currentGroup(Model.Group.find(Model.Group.currentGroup.is.entityID) openOr null)
						}
						Model.AdditionalPropertyBundle.currentAdditionalPropertyBundle(additionalPropertyBundle)
						new AdditionalPropertyBundle().renderAdditionalPropertyBundle(nodeSeq)
					}},
					"GroupUserRequests" -> Text(""),
					"GroupUsers" -> Text(""),
					"Institution" -> SHtml.ajaxText(Model.Group.currentGroup.is.institution, value => {Model.Group.currentGroup.is.institution(value);JsCmds.Noop}),
					"Name" -> SHtml.ajaxText(Model.Group.currentGroup.is.name, value => {Model.Group.currentGroup.is.name(value);JsCmds.Noop}),
					"OccurrenceAdditionalPropertyBundle" -> {(nodeSeq: NodeSeq) => {
						var additionalPropertyBundle = Model.Group.currentGroup.is.occurrenceAdditionalPropertyBundle.obj openOr null
						if (null == additionalPropertyBundle) {
							additionalPropertyBundle = Model.AdditionalPropertyBundle.create
							additionalPropertyBundle.save
							Model.Group.currentGroup.is.occurrenceAdditionalPropertyBundle(additionalPropertyBundle)
							Model.Group.currentGroup.is.save
							//Model.Group.currentGroup(Model.Group.find(Model.Group.currentGroup.is.entityID) openOr null)
						}
						Model.AdditionalPropertyBundle.currentAdditionalPropertyBundle(additionalPropertyBundle)
						new AdditionalPropertyBundle().renderAdditionalPropertyBundle(nodeSeq)
					}},
					"PrivateSource" -> {(nodeSeq: NodeSeq) => {
						initPrivateSource
						new Source().renderSource(nodeSeq)
					}},
					"PublicSource" -> {(nodeSeq: NodeSeq) => {
						initPublicSource
						new Source().renderSource(nodeSeq)
					}},
					"RecentOccurrences" -> Text(""),
					"SaveLink" -> {(nodeSeq: NodeSeq) => SHtml.ajaxButton(nodeSeq, () => save)},
					"ViewLink" -> Text("")
				)
			} else {
				bind(
					"group",
					xhtml,
					"CancelLink" -> Text(""),
					"Department" -> Model.Group.currentGroup.is.department,
					"Description" -> {(nodeSeq: NodeSeq) => {
						if (null != Model.Group.currentGroup.is.description.is && !Model.Group.currentGroup.is.description.is.trim().equals("")) {
							Text(Model.Group.currentGroup.is.description)
						} else {
							nodeSeq
						}
					}},
					"EditLink" -> {(nodeSeq: NodeSeq) => {
						if (!S.attr("group").isEmpty && Model.Group.currentGroup.canUpdate_?) {
							<a href="javascript: showGroupDetailsUpdate();">{nodeSeq}</a>
						} else {
							Text("")
						}
					}},
					"EventAdditionalPropertyBundle" -> {(nodeSeq: NodeSeq) => {
						Model.AdditionalPropertyBundle.currentAdditionalPropertyBundle(Model.Group.currentGroup.is.eventAdditionalPropertyBundle.obj openOr null)
						new AdditionalPropertyBundle().renderAdditionalPropertyBundle(nodeSeq)
					}},
					"GroupUserRequests" -> {(nodeSeq: NodeSeq) => {
						if (!S.attr("group").isEmpty && Model.Group.currentGroup.canUpdate_?) {
							val groupUserRequests = Model.Group.currentGroup.is.groupUserRequests
							if (groupUserRequests.length < 1) {
								Text("")
							} else {
								bind(
									"group-user-requests",
									nodeSeq,
									"Count" -> groupUserRequests.length,
									"List" -> {(nodeSeq: NodeSeq) => {
										val groupUserRequestsNodeSeq: NodeSeq = groupUserRequests.flatMap(groupUserRequest => {
											def approve(asAdministrator: Boolean) = {
												val groupUser: Model.GroupUser = Model.GroupUser.create
												groupUser.group(groupUserRequest.group)
												groupUser.user(groupUserRequest.user)
												if (asAdministrator) {
													groupUser.userType(Model.GroupUserType.Administrator)
												}
												groupUser.approved(true)
												groupUser.save
												groupUserRequest.delete_!
												JsCmds.Run("location.reload(true)")
											}
											
											def delete() = {
												groupUserRequest.delete_!
												JsCmds.Run("location.reload(true)")
											}
											
											bind(
												"group-user-request",
												nodeSeq,
												"FirstName" -> ((groupUserRequest.user.obj openOr Model.User).person.obj openOr Model.Person).firstName,
												"LastName" -> ((groupUserRequest.user.obj openOr Model.User).person.obj openOr Model.Person).lastName,
												"Email" -> groupUserRequest.user.obj.open_!.email,
												"ApproveLink" -> {(nodeSeq: NodeSeq) => {SHtml.a(() => approve(false), nodeSeq)}},
												"ApproveAsAdministratorLink" -> {(nodeSeq: NodeSeq) => {SHtml.a(() => approve(true), nodeSeq)}},
												"DeleteLink" -> {(nodeSeq: NodeSeq) => {SHtml.a(() => delete, nodeSeq)}}
											)
										})
										groupUserRequestsNodeSeq
									}},
									"ViewLink" -> {(nodeSeq: NodeSeq) => {
										<a href={"group?ID=" + Model.Group.currentGroup.is.entityID}>{nodeSeq}</a>
									}}
								)
							}
						} else {
							Text("")
						}
					}},
					"GroupUsers" -> {(nodeSeq: NodeSeq) => {
						if (!S.attr("group").isEmpty && Model.Group.currentGroup.canUpdate_?) {
							val groupUsers = Model.Group.currentGroup.is.groupUsers
							if (groupUsers.length < 1) {
								Text("")
							} else {
								bind(
									"group-users",
									nodeSeq,
									"List" ->{(nodeSeq: NodeSeq) => {
										val groupUsersNodeSeq: NodeSeq = groupUsers.flatMap(groupUser => {
											def delete() = {
												if (groupUser.userType == Model.GroupUserType.Administrator && groupUsers.filter(_.userType == Model.GroupUserType.Administrator).length < 2) {
													JsCmds.Alert("There must be at least one administrator.")
												} else {
													groupUser.delete_!
													JsCmds.Run("location.reload(true)")
												}
											}
											
											bind(
												"group-user",
												nodeSeq,
												"FirstName" -> ((groupUser.user.obj openOr Model.User).person.obj openOr Model.Person).firstName,
												"LastName" -> ((groupUser.user.obj openOr Model.User).person.obj openOr Model.Person).lastName,
												"Email" -> (groupUser.user.obj openOr Model.User).email,
												"UserType" -> Model.GroupUserType.typeMapInverted(Model.GroupUserType(groupUser.userType.toInt)),
												"DeleteLink" -> {(nodeSeq: NodeSeq) => {SHtml.a(() => delete, nodeSeq)}}
											)
										})
										groupUsersNodeSeq
									}}
								)
							}
						} else {
							Text("")
						}
					}},
					"Institution" -> Model.Group.currentGroup.is.institution,
					"Name" -> Model.Group.currentGroup.is.name,
					"OccurrenceAdditionalPropertyBundle" -> {(nodeSeq: NodeSeq) => {
						Model.AdditionalPropertyBundle.currentAdditionalPropertyBundle(Model.Group.currentGroup.is.occurrenceAdditionalPropertyBundle.obj openOr null)
						new AdditionalPropertyBundle().renderAdditionalPropertyBundle(nodeSeq)
					}},
					"PrivateSource" -> {(nodeSeq: NodeSeq) => {
						initPrivateSource
						new Source().renderSource(nodeSeq)
					}},
					"PublicSource" -> {(nodeSeq: NodeSeq) => {
						initPublicSource
						new Source().renderSource(nodeSeq)
					}},
					"RecentOccurrences" -> {(nodeSeq: NodeSeq) => {
						if (S.attr("group").isEmpty) {
							Text("")
						} else {
							val recentOccurrenceIDs = Controller.Occurrence.searchIDs(Model.Group.currentGroup.is.entityID, 20, "ID", true)
							if (0 < recentOccurrenceIDs.length) {
								bind(
									"recent-occurrences",
									nodeSeq,
									"Count" -> recentOccurrenceIDs.length,
									"SimpleOccurrences" -> {(nodeSeq: NodeSeq) => {
										Model.SimpleOccurrence.currentSimpleOccurrences(Controller.SimpleOccurrence.search(recentOccurrenceIDs))
										nodeSeq
									}},
									"ViewLink" -> {(nodeSeq: NodeSeq) => {
										SHtml.a(() => SetHtml("primary_content", <lift:embed what="lab/recent-occurrences"/>), nodeSeq)
									}}
								)
							} else {
								Text("")
							}
						}
					}},
					"SaveLink" -> Text(""),
					"UnclassifiedOccurrences" -> {(nodeSeq: NodeSeq) => {
						if (S.attr("group").isEmpty) {
							Text("")
						} else {
							val unclassifiedOccurrenceIDs = Model.Group.currentGroup.is.privateSource.obj.open_!.unclassifiedOccurrences.map(_.entityID.is)
							if (0 < unclassifiedOccurrenceIDs.length) {
								bind(
									"unclassified-occurrences",
									nodeSeq,
									"Count" -> unclassifiedOccurrenceIDs.length,
									"SimpleOccurrences" -> {(nodeSeq: NodeSeq) => {
										Model.SimpleOccurrence.currentSimpleOccurrences(Controller.SimpleOccurrence.search(unclassifiedOccurrenceIDs))
										nodeSeq
									}},
									"ViewLink" -> {(nodeSeq: NodeSeq) => {
										SHtml.a(() => SetHtml("primary_content", <lift:embed what="lab/unclassified-occurrences"/>), nodeSeq)
									}}
								)
							} else {
								Text("")
							}
						}
					}},
					"ViewLink" -> {(nodeSeq: NodeSeq) => {
						<a href={"group?ID=" + Model.Group.currentGroup.is.entityID}>{nodeSeq}</a>
					}}
				)
			}
		}
	}
	
	def renderScripts: NodeSeq = {
		def showGroupDetails() = {
			SetHtml("group-details", <lift:embed what="lab/group-details" />)
		}
		
		def showGroupDetailsUpdate() = {
			SetHtml("group-details", <lift:embed what="lab/group-details-update" />)
		}
		
		def cancelGroupDetailsUpdate() = {
			Model.Group.currentGroup(Model.Group.find(Model.Group.currentGroup.is.entityID) openOr null)
			
			SetHtml("group-details", <lift:embed what="lab/group-details" />)
		}
		
		def showEventAdditionalPropertyBundle() = {
			SetHtml("event-additional-property-bundle", <lift:embed what="lab/group-event-additional-property-bundle" />)
		}
		
		def showEventAdditionalPropertyBundleUpdate() = {
			SetHtml("occurrence-additional-property-bundle", <lift:embed what="lab/group-occurrence-additional-property-bundle" />) &
			SetHtml("event-additional-property-bundle", <lift:embed what="lab/group-event-additional-property-bundle-update" />)
		}
		
		def showOccurrenceAdditionalPropertyBundle() = {
			SetHtml("occurrence-additional-property-bundle", <lift:embed what="lab/group-occurrence-additional-property-bundle" />)
		}
		
		def showOccurrenceAdditionalPropertyBundleUpdate() = {
			SetHtml("event-additional-property-bundle", <lift:embed what="lab/group-event-additional-property-bundle" />) &
			SetHtml("occurrence-additional-property-bundle", <lift:embed what="lab/group-occurrence-additional-property-bundle-update" />)
		}
		
		Script(
			Function(
				"showGroupDetails",
				Nil,
				SHtml.ajaxInvoke(showGroupDetails)._2
			)
		) ++
		Script(
			Function(
				"showGroupDetailsUpdate",
				Nil,
				SHtml.ajaxInvoke(showGroupDetailsUpdate)._2
			)
		) ++
		Script(
			Function(
				"cancelGroupDetailsUpdate",
				Nil,
				SHtml.ajaxInvoke(cancelGroupDetailsUpdate)._2
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
		) ++
		Script(
			Function(
				"showOccurrenceAdditionalPropertyBundle",
				Nil,
				SHtml.ajaxInvoke(showOccurrenceAdditionalPropertyBundle)._2
			)
		) ++
		Script(
			Function(
				"showOccurrenceAdditionalPropertyBundleUpdate",
				Nil,
				SHtml.ajaxInvoke(showOccurrenceAdditionalPropertyBundleUpdate)._2
			)
		)
	}
}