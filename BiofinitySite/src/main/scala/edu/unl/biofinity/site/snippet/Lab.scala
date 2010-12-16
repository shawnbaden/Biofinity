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

class Lab {
	def initGroup: NodeSeq = {
		if (null != Model.User.currentGroup.is) {
			Model.Group.currentGroup(Model.User.currentGroup)
		}
		Text("")
	}
	
	def renderGroupRequests(xhtml: NodeSeq): NodeSeq = {
		val groupRequests = Model.User.currentUser.is.groupRequests
		if (groupRequests.length < 1) {
			Text("")
		} else {
			bind(
				"group-requests",
				xhtml,
				"List" -> {(nodeSeq: NodeSeq) => {
					val groupRequestsNodeSeq: NodeSeq = groupRequests.flatMap(groupRequest => {
						def delete() = {
							groupRequest.delete_!
							JsCmds.Run("location.reload(true)")
						}

						bind(
							"group-request",
							nodeSeq,
							"Name" -> groupRequest.name,
							"Description" -> groupRequest.description,
							"DeleteLink" -> {(nodeSeq: NodeSeq) => {SHtml.a(() => delete, nodeSeq)}}
						)
					})
					groupRequestsNodeSeq
				}}
			)
		}
	}

	def renderGroupsAdministered(xhtml: NodeSeq): NodeSeq = {
		val groups: List[Model.Group] = Model.User.currentUser.groupUsers.filter(_.userType == Model.GroupUserType.Administrator).map(_.group.obj.open_!)
		if (groups.length < 1) {
			Text("")
		} else {
			bind(
				"groups",
				xhtml,
				"List" -> {(nodeSeq: NodeSeq) => {
					val groupsNodeSeq: NodeSeq = groups.flatMap(group => {
						bind(
							"group",
							nodeSeq,
							"NameLink" -> SHtml.link("/lab", () => Model.User.currentGroup(group), Text(group.name)),
							"Description" -> group.description,
							"ViewLink" -> {(nodeSeq: NodeSeq) => {
								SHtml.link("/lab/group?ID=" + group.entityID, () => Model.User.currentGroup(group), nodeSeq)
							}}
						)
					})
					groupsNodeSeq
				}}
			)
		}
	}

	def renderGroupsNotAdministered(xhtml: NodeSeq): NodeSeq = {
		val groups: List[Model.Group] = Model.User.currentUser.groupUsers.filter(_.userType != Model.GroupUserType.Administrator).map(_.group.obj.open_!)
		if (groups.length < 1) {
			Text("")
		} else {
			bind(
				"groups",
				xhtml,
				"List" -> {(nodeSeq: NodeSeq) => {
					val groupsNodeSeq: NodeSeq = groups.flatMap(group => {
						def leave() = {
							Model.User.currentUser.groupUsers.filter(_.group == group).foreach(_.delete_!)
							if (Model.User.groupMember_?) {
								Model.User.currentGroup(Model.User.currentUser.groups.first)
							} else {
								Model.User.currentGroup(null)
							}
							JsCmds.Run("location.reload(true)")
						}

						bind(
							"group",
							nodeSeq,
							"NameLink" -> SHtml.link("/lab", () => Model.User.currentGroup(group), Text(group.name)),
							"Description" -> group.description,
							"LeaveLink" -> {(nodeSeq: NodeSeq) => {
								val groupUser = Model.GroupUser.find(By(Model.GroupUser.user, Model.User.currentUser.is), By(Model.GroupUser.group, Model.User.currentGroup.is)) openOr null
								if (null != groupUser && groupUser.userType != Model.GroupUserType.Restricted) {
									SHtml.a(() => leave, nodeSeq)
								} else {
									Text("")
								}
							}},
							"ViewLink" -> {(nodeSeq: NodeSeq) => {
								SHtml.link("/lab/group?ID=" + group.entityID, () => Model.User.currentGroup(group), nodeSeq)
							}}
						)
					})
					groupsNodeSeq
				}}
			)
		}
	}

	def renderGroups(xhtml: NodeSeq): NodeSeq = {
		val groups: List[Model.Group] = Model.User.currentUser.groups.sort((a,b) => b.name.compareTo(a.name.is.toUpperCase) > 0)
		if (groups.length < 1) {
			Text("")
		} else {
			bind(
				"groups",
				xhtml,
				"List" -> {(nodeSeq: NodeSeq) => {
					val groupsNodeSeq: NodeSeq = groups.flatMap(group => {
						bind(
							"group",
							nodeSeq,
							"NameLink" -> SHtml.link("/lab", () => Model.User.currentGroup(group), Text(group.name)),
							"ViewLink" -> {(nodeSeq: NodeSeq) => {
								SHtml.link("/lab/group?ID=" + group.entityID, () => Model.User.currentGroup(group), nodeSeq)
							}}
						)
					})
					groupsNodeSeq
				}}
			)
		}
	}

	def renderGroupsToJoin(xhtml: NodeSeq): NodeSeq = {
		val groupsToJoin: List[Model.Group] =
			Model.Group.findAll()
			.filter(group => !Model.User.currentUser.groups.exists(_ == group))
			.filter(group => !Model.User.currentUser.groupUserRequests.exists(_.group == group))
		if (groupsToJoin.length < 1) {
			Script(Alert("There are no labs to join.") & JsCmds.RedirectTo("/lab/settings"))
		} else {
			bind(
				"groups",
				xhtml,
				"List" -> {(nodeSeq: NodeSeq) => {
					val groupsToJoinNodeSeq: NodeSeq = groupsToJoin.flatMap(group => {
						def join() = {
							val groupUserRequest: Model.GroupUserRequest = Model.GroupUserRequest.create
							groupUserRequest.group(group)
							groupUserRequest.user(Model.User.currentUser.get)
							groupUserRequest.save

							JsCmds.RedirectTo("/lab/settings")
						}

						bind(
							"group",
							nodeSeq,
							"Name" -> group.name,
							"Description" -> group.description,
							"JoinLink" -> {(nodeSeq: NodeSeq) => {SHtml.a(() => join, nodeSeq)}}
						)
					})
					groupsToJoinNodeSeq
				}}
			)
		}
	}

	def renderGroupUserRequests(xhtml: NodeSeq): NodeSeq = {
		val groupUserRequests = Model.User.currentUser.is.groupUserRequests
		if (groupUserRequests.length < 1) {
			Text("")
		} else {
			bind(
				"group-user-requests",
				xhtml,
				"List" -> {(nodeSeq: NodeSeq) => {
					val groupUserRequestsNodeSeq: NodeSeq =	groupUserRequests.flatMap(groupUserRequest => {
						def delete() = {
							groupUserRequest.delete_!
							JsCmds.Run("location.reload(true)")
						}

						bind(
							"group-user-request",
							nodeSeq,
							"Name" -> groupUserRequest.group.obj.open_!.name,
							"Description" -> groupUserRequest.group.obj.open_!.description,
							"DeleteLink" -> {(nodeSeq: NodeSeq) => {
								SHtml.a(() => delete, nodeSeq)
							}}
						)
					})
					groupUserRequestsNodeSeq
				}}
			)
		}
	}
	
	def renderCreateGroupRequest(xhtml: NodeSeq): NodeSeq = {
		val groupRequest: Model.GroupRequest = Model.GroupRequest.create
		
		def save(): JsCmd = {
			val name = groupRequest.name.is.trim.replaceAll(" +", " ")
			val person = Model.User.currentUser.is.person.obj.open_!
			val firstName = person.firstName.is.trim.replaceAll(" +", " ")
			val lastName = person.lastName.is.trim.replaceAll(" +", " ")
			if (name.equals("")) {
				Alert("Name cannot be empty.")
			} else if (firstName.equals("")) {
				Alert("First name cannot be empty.")
			} else if (lastName.equals("")) {
				Alert("Last name cannot be empty.")
			} else {
				if (!person.firstName.is.equals(firstName)) {
					person.firstName(firstName)
				}
				if (!person.lastName.is.equals(lastName)) {
					person.lastName(lastName)
				}
				person.save
				Model.User.currentUser.save
				groupRequest.user(Model.User.currentUser.is)
				if (!groupRequest.name.is.equals(name)) {
					groupRequest.name(name)
				}
				groupRequest.save

				RedirectTo("/lab/settings")
			}
		}
		
		SHtml.ajaxForm(
			bind(
				"group-request",
				xhtml,
				"Name" -> SHtml.ajaxText("", value => {groupRequest.name(value); JsCmds.Noop}),
				"Institution" -> SHtml.ajaxText("", value => {groupRequest.institution(value); JsCmds.Noop}),
				"Department" -> SHtml.ajaxText("", value => {groupRequest.department(value); JsCmds.Noop}),
				"Description" -> SHtml.ajaxText("", value => {groupRequest.description(value); JsCmds.Noop}), 
				"SaveLink" -> {(nodeSeq: NodeSeq) => SHtml.ajaxButton(nodeSeq, () => save)}
			)
		)
	}
}