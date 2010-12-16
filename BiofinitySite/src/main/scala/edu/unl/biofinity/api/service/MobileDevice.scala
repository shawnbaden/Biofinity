package edu.unl.biofinity.api.service

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

object MobileDevice {
	def read(r: Req): Box[LiftResponse] = { 
		val mobileDevice: Model.MobileDevice = try {
			Model.MobileDevice.findAll(By(Model.MobileDevice.uniqueID, r.params("ID").first)).first
		} catch {
			case e: Exception => null
		}
		
		if ( mobileDevice == null ) { 
			Full(XmlResponse(<MobileDevice />))
		} else {
			val returnXML =
			<MobileDevice ID={mobileDevice.uniqueID.toString}>
				<Description>{mobileDevice.description}</Description>
				<User ID={mobileDevice.user.obj.open_!.entityID.toString}>{mobileDevice.user.obj.open_!.person.obj.open_!.fullName}</User>
				<Groups>
					{mobileDevice.user.obj.open_!.groups.flatMap(group => {
					<Group ID={group.entityID.toString}>{group.name}</Group>
					})}
				</Groups>
			</MobileDevice>
			
			Full(XmlResponse(returnXML))
		}
	}
}