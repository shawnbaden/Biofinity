package edu.unl.biofinity.site.snippet

import edu.unl.biofinity.api.{model => Model}
import edu.unl.biofinity.site.user.service._

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

class User {
	def renderIfSignedIn(xhtml: NodeSeq): NodeSeq = {
		if (Model.User.signedIn_?) {
			xhtml
		} else {
			Text("")
		}
	}
	
	def renderIfNotSignedIn(xhtml: NodeSeq): NodeSeq = {
		if (!Model.User.signedIn_?) {
			xhtml
		} else {
			Text("")
		}
	}

	def renderIfNotRestricted(xhtml: NodeSeq): NodeSeq = {
		if (Model.User.signedIn_? && Model.User.currentUser.is.userType != Model.UserType.Restricted) {
			xhtml
		} else {
			Text("")
		}
	}
	
	private val menuPages: List[(String, String)] =
		("/user/", "Overview") ::
		("/user/info", "Information") ::
		("/user/services", "Services") ::
		//("/user/preferences", "Preferences") ::
		Nil
	
 	def renderMenu(xhtml: NodeSeq): NodeSeq = {
		val uri = S.uri
		
		menuPages.flatMap { s =>
			bind(
				"menu",
				xhtml,
				"MenuItem" -> {
					<li>
						<a class={if (uri.equals(s._1)) "active" else "" } href={s._1}>{s._2}</a>
					</li>
				}
			)
		}
	}
	
	def renderUser(xhtml: NodeSeq) = {
		if (null == Model.User.currentUser.is) {
			Text("")
		} else {
			if (!S.attr("update").isEmpty) {
				val person = Model.User.currentUser.is.person.obj.openOr(Model.Person.create)
				def save(): JsCmd = {
					val firstName = person.firstName.is.trim.replaceAll(" +", " ")
					val lastName = person.lastName.is.trim.replaceAll(" +", " ")
					if (firstName.equals("")) {
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
						Model.User.currentUser.person(person)
						Model.User.currentUser.save

						SetHtml("user-details", <lift:embed what="/user/user-details"/>)
					}
				}
				
				SHtml.ajaxForm(
					bind(
						"user",
						xhtml,
						"Address" -> SHtml.ajaxText(person.address, value => {person.address(value); JsCmds.Noop}),
						"CancelLink" -> {(nodeSeq: NodeSeq) => <a href="javascript: cancelUserDetailsUpdate();">{nodeSeq}</a>},
						"EditLink" -> Text(""),
						"Email" -> SHtml.ajaxText(Model.User.currentUser.is.email, value => {Model.User.currentUser.is.email(value); JsCmds.Noop}),
						"FirstName" -> SHtml.ajaxText(person.firstName, value => {person.firstName(value); JsCmds.Noop}),
						"LastName" -> SHtml.ajaxText(person.lastName, value => {person.lastName(value); JsCmds.Noop}),
						"PhoneNumber" -> SHtml.ajaxText(person.phoneNumber, value => {person.phoneNumber(value); JsCmds.Noop}),
						"SaveLink" -> {(nodeSeq: NodeSeq) => SHtml.ajaxButton(nodeSeq.toString, () => save)}
					)
				)
			} else {
				val person = Model.User.currentUser.is.person.obj.openOr(Model.Person)
				
				bind(
					"user",
					xhtml,
					"Address" -> person.address,
					"CancelLink" -> Text(""),
					"EditLink" -> {(nodeSeq: NodeSeq) => {
						if (Model.User.currentUser.is.userType == Model.UserType.Restricted) {
							Text("")
						} else {
							<a href="javascript: showUserDetailsUpdate();">{nodeSeq}</a>
						}
					}},
					"Email" -> Model.User.currentUser.get.email,
					"FirstName" -> person.firstName,
					"LastName" -> person.lastName,
					"PhoneNumber" -> person.phoneNumber,
					"SaveLink" -> Text("")
				)
			}

		}
	}
	
	def renderScripts: NodeSeq = {
		def showUserDetails() = {
			SetHtml("user-details", <lift:embed what="user/user-details" />)
		}
		
		def showUserDetailsUpdate() = {
			SetHtml("user-details", <lift:embed what="user/user-details-update" />)
		}
		
		def cancelUserDetailsUpdate() = {
			Model.User.currentUser(Model.User.find(Model.User.currentUser.is.entityID) openOr null)
			
			SetHtml("user-details", <lift:embed what="user/user-details" />)
		}
		
		Script(
			Function(
				"showUserDetails",
				Nil,
				SHtml.ajaxInvoke(showUserDetails)._2
			)
		) ++
		Script(
			Function(
				"showUserDetailsUpdate",
				Nil,
				SHtml.ajaxInvoke(showUserDetailsUpdate)._2
			)
		) ++
		Script(
			Function(
				"cancelUserDetailsUpdate",
				Nil,
				SHtml.ajaxInvoke(cancelUserDetailsUpdate)._2
			)
		)
	}
	
	private val services: List[UserService] =
		new LabUserService ::
		//new MobileDeviceUserService ::
		Nil
	
	def renderServices(xhtml: NodeSeq): NodeSeq = {
		services.flatMap { service =>
			val icon =
				if (service.enabled) {
					service.activeIcon
				} else {
					service.inactiveIcon
				}
			bind(
				"service",
				xhtml,
				"Name" -> service.name,
				"Description" -> service.description,
				"Icon" -> <img src={ "/resources/images/profile/"+ icon } align="left"/>,
				"Action" -> {
					if (Model.User.currentUser.is.userType == Model.UserType.Restricted) {
						Text("")
					} else {
						if (service.enabled) {
							if (null == service.manageLogic) {
								Text("Service Active")
							} else {
								SHtml.a(() => service.manageLogic, Text("Manage Service"))
							}
						} else {
							SHtml.a(() =>{ service.enableLogic }, Text("Enable Service"))
						}
					}
				}
			)
		}
	}
	
	def renderService(xhtml: NodeSeq): NodeSeq = {
		bind(
			"service",
			xhtml,
			"MobileDevices" -> {(nodeSeq: NodeSeq) => {
				bind(
					"mobile-devices",
					nodeSeq,
					"Create" -> {(nodeSeq: NodeSeq) => {
						var uniqueID = ""
						var description = ""
						
						def save(): JsCmd = {
							if (uniqueID.length > 40) {
								Alert("The mobile device ID must not exceed 40 characters.")
							} else if (uniqueID.length < 1) {
								Alert("The mobile device ID cannot be empty.")
							} else {
								val mobileDevice: Model.MobileDevice = Model.MobileDevice.create
								mobileDevice.uniqueID(uniqueID)
								mobileDevice.description(description)
								mobileDevice.user(Model.User.currentUser.is)
								mobileDevice.save
								
								SetHtml("user-services-content", <lift:embed what="user/mobile-devices"/>)
							}
						}
						
						bind(
							"mobile-device",
							nodeSeq,
							"Description" -> SHtml.ajaxText(description, value => {description = value; Noop},"size" -> "40", "maxlength" -> "128"),
							"SaveLink" -> {(nodeSeq: NodeSeq) => SHtml.ajaxButton(nodeSeq, () => save)},
							"UniqueID" -> SHtml.ajaxText(uniqueID, value => {uniqueID = value; Noop}, "size" -> "40", "maxlength" -> "40" )
						)
					}},
					"EmptyText" -> {(nodeSeq: NodeSeq) => {
						if (Model.User.currentUser.is.mobileDevices.length > 0) {
							Text("")
						} else {
							nodeSeq
						}
					}},
					"List" -> {(nodeSeq: NodeSeq) => {
						val mobileDevicesNodeSeq: NodeSeq = Model.User.currentUser.is.mobileDevices.flatMap(mobileDevice => {
							def delete() = {
								mobileDevice.delete_!
								
								SetHtml("user-services-content", <lift:embed what="user/mobile-devices"/>)
							}
							
							bind(
								"mobile-device",
								nodeSeq,
								"DeleteLink" -> {(nodeSeq: NodeSeq) => SHtml.ajaxButton(nodeSeq, () => delete)},
								"Description" -> mobileDevice.description,
								"UniqueID" -> mobileDevice.uniqueID
							)
						})
						mobileDevicesNodeSeq
					}}
				)
			}}
		)
	}
}
