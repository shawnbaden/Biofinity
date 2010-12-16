package edu.unl.biofinity.api.service

import edu.unl.biofinity.api.{model => Model}

import java.sql.PreparedStatement
import java.sql.ResultSet

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

object BioBlitz {
	def redirectToGroup(r: Req): Box[LiftResponse] = {
		val group = getGroup
		if (null == group) {
			Full(RedirectResponse("/"))
		} else {
			Model.User.currentUser(getUser)
			Model.User.currentGroup(getGroup)
			
			Full(RedirectResponse("/lab"))
		}
	}
	
	def getGroup: Model.Group = {
		val privateSource = Model.Source.find(By(Model.Source.uniqueID, "BIOBLITZ_PRIVATE")) openOr null
		val group =
			if (null == privateSource) {
				null
			} else {
				privateSource.group
			}
		group
	}
	
	def getUser: Model.User = {
		val group = getGroup
		if (null == group) {
			return null
		} else {
			var user: Model.User = Model.User.find(By(Model.User.openID, "BIOBLITZ")) openOr null
			if (null == user) {
				val person = Model.Person.create
				person.firstName("BioBlitz")
				person.lastName("User")
				person.save
				user = Model.User.create
				user.person(person)
				user.openID("BIOBLITZ")
				user.userType(Model.UserType.Restricted)
				user.save
			} else {
				if (Model.UserType.Restricted != user.userType.is) {
					user.userType(Model.UserType.Restricted)
					user.save
				}
			}
			if (user.groupUsers.length > 1) {
				user.groupUsers.map(_.delete_!)
			}
			var groupUser: Model.GroupUser = Model.GroupUser.find(By(Model.GroupUser.user, user), By(Model.GroupUser.group, group)) openOr null
			if (null == groupUser) {
				groupUser = Model.GroupUser.create
				groupUser.approved(true)
				groupUser.group(group)
				groupUser.user(user)
				groupUser.userType(Model.GroupUserType.Restricted)
				groupUser.save
			} else {
				if (Model.GroupUserType != groupUser.userType.is) {
					groupUser.userType(Model.GroupUserType.Restricted)
					groupUser.save
				}
			}
			user
		}
	}
}