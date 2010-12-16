package edu.unl.biofinity.site.user.service

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

class LabUserService extends UserService { 
	def name: String = { "My Lab" }
	
	def description: String = {
		"My Lab is an innovative service offered by The Biofinity Project that is designed "+
		"to replace your existing database solution.  With My Lab you can upload, create, "+
		"browse and publish all of your biological related data from anywhere in the world "+
		"from a sinlge web interface."
	}
	
	def activeIcon: String = { "lab_icon.png" }
	def inactiveIcon: String = { "lab_icon_disabled.png" }
	def largeIcon: String = { "lab_icon_large.png" }
	def actions: String = { "" }
	
	def enabled: Boolean = {
		Model.User.groupMember_? || Model.User.pendingRequests_?
	}
	
	def enableLogic: JsCmd = {
		JsRaw("window.document.location.href = \"" + S.hostAndPath + "/lab/new\"")
	}
	
	def manageLogic: JsCmd = {
		JsRaw("window.document.location.href = \"" + S.hostAndPath + "/lab/settings\"")
	}
}