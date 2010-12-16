package edu.unl.biofinity.api.service

import edu.unl.biofinity.api.{model => Model}

import java.io.ByteArrayInputStream

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

object MediaFile {
	def read(r: Req): Box[LiftResponse] = {
		val mediaFile: Model.MediaFile = try {
			Model.MediaFile.find(r.params("ID").first.toInt) openOr null
		} catch { case e: Exception => null }
		if (null == mediaFile) {
			Full(NotFoundResponse())
		} else {
			val rawData: Array[Byte] = {
				if (mediaFile.isImage) {
					val width: Int = try {
						r.param("Width").open_!.toInt
					} catch { case e: Exception => -1 }
					if (1 > width) {
						mediaFile.rawData
					} else {
						mediaFile.transformedMediaFile(width).rawData
					}
				} else {
					mediaFile.rawData
				}
			}

			val rawDataInputStream = new ByteArrayInputStream(rawData)
			Full(
				StreamingResponse(
					rawDataInputStream,
					() => { rawDataInputStream.close },
					rawData.length,
					List(("Content-Type", mediaFile.fileType.is)),
					List(),
					200
				)
			)
		}
    }
}