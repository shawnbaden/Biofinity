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

class MobileDeviceUserService extends UserService {
	def name: String = "Mobile Devices"
	
	def description: String = {
		"Run iOS and Android applications by The Biofinity Project. " +
		"You can browse data and record geo-coded occurrence data directly from the field " +
		"anywhere you have a cell phone signal or wireless connection."
	}
	
	def activeIcon: String = { "mobile_icon.png" }
	def inactiveIcon: String = { "mobile_icon_disabled.png" }
	def largeIcon: String = { "mobile_icon_large.png" }
	def actions: String = { "" }
	
	def enabled: Boolean = {
		Model.User.currentUser.mobileDevices.length > 0
	}
	
	def enableLogic:JsCmd = {
		SetHtml("user-services-content", <lift:embed what="user/mobile-devices"/>)
	}
	
	def manageLogic: JsCmd = {
		SetHtml("user-services-content", <lift:embed what="user/mobile-devices"/>)
	}
}