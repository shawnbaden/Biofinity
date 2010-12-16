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

class DataProcessor {
	var fileParamHolder: Box[FileParamHolder] = Empty
	
	def processXML(xhtml: scala.xml.Group): NodeSeq = {
		def parseXML() = {
			fileParamHolder match {
				case Full(FileParamHolder(_, _, _, data)) => {
					new edu.unl.biofinity.site.DataParser().parseXML(data)
				}
				case _ => {
					S.error("Invalid file.")
				}
			}
		}
		
		bind(
			"x",
			xhtml,
			"file" -> SHtml.fileUpload(filePH => fileParamHolder = Full(filePH)),
			"submit" -> SHtml.submit("Process XML", parseXML)
		)
	}
}