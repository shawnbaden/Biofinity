package edu.unl.biofinity.site

import edu.unl.biofinity.api.{model => Model}
import edu.unl.biofinity.api.{service => Service}
import edu.unl.biofinity.site.snippet.Global

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

import scala.actors.Actor
import scala.xml._

import java.io.BufferedInputStream
import java.io.ByteArrayOutputStream
import java.io.InputStream

import java.net.MalformedURLException
import java.net.URL

import java.util.Calendar
import java.util.Date
import java.util.concurrent.Executors
import java.util.concurrent.ScheduledExecutorService
import java.util.concurrent.ScheduledFuture
import java.util.concurrent.TimeUnit

import javax.xml.bind.DatatypeConverter

object TwitterBioBlitzImporter extends Actor {
	val logger = Logger("Twitter BioBlitz Importer")
	val searchURL = "http://search.twitter.com/search.atom?&q=%23BioBlitz&since_id="
	
	def act() = {
		logger.info("Running Twitter BioBlitz Importer...")
		val privateSource = Model.Source.find(By(Model.Source.uniqueID, "BIOBLITZ_PRIVATE")) openOr null
		if (null != privateSource) {
			val group = privateSource.group
			if (null != group) {
				val publicSource = group.publicSource.obj openOr null
				var additionalPropertyBundle = group.additionalPropertyBundle.obj openOr null
				if (null == additionalPropertyBundle) {
					additionalPropertyBundle = Model.AdditionalPropertyBundle.create
					additionalPropertyBundle.save
					group.additionalPropertyBundle(additionalPropertyBundle)
					group.save
				}
				var additionalProperty = additionalPropertyBundle.additionalProperties.find(additionalProperty => additionalProperty.name.equals("Twitter BioBlitz Importer Last ID")) getOrElse null
				if (null == additionalProperty) {
					additionalProperty = Model.AdditionalProperty.create.additionalPropertyBundle(additionalPropertyBundle).name("Twitter BioBlitz Importer Last ID").value("-1")
					additionalProperty.save
				}
				val previousLastID: Long =
					try {
						additionalProperty.value.is.toLong
					} catch {
						case e: NumberFormatException => -1
					}
				val response: Elem = XML.load(searchURL + previousLastID)
				var currentLastID: Long = previousLastID
				val entriesNodeSeq = (response \\ "entry")
				entriesNodeSeq.foreach(entryNodeSeq => {
					val currentIDValue = (entryNodeSeq \ "id").text
					val currentIDParts = currentIDValue.split(":")
					val currentID: Long =
						try {
							currentIDParts(2).toLong
						} catch {
							case e: NumberFormatException => -1
						}
					if (3 == currentIDParts.length && currentID > -1) {
						if (currentID > currentLastID) {
							additionalProperty.value(currentID.toString)
							additionalProperty.save
							currentLastID = currentID
						}
						val pointValue = (entryNodeSeq \ "geo" /*twitter:geo*/ \ "point" /*georss:point*/).text
						val pointParts = pointValue.split(" ")
						val latitude: Double =
							try {
								pointParts(0).toDouble
							} catch {
								case e: ArrayIndexOutOfBoundsException => 0.0
								case e: NumberFormatException => 0.0
							}
						val longitude: Double =
							try {
								pointParts(1).toDouble
							} catch {
								case e: ArrayIndexOutOfBoundsException => 0.0
								case e: NumberFormatException => 0.0
							}
						if (2 == pointParts.length && latitude != 0.0 && longitude != 0.0) {
							val statusURL = ((entryNodeSeq \ "link")(0) \ "@href").text
							val title = (entryNodeSeq \ "title").text.replaceAll("\\s+", " ")
							val titleParts = title.split(" ")
							if (4 == titleParts.length) {
								var foundTag = false
								var foundURL = false
								var foundGenus = false
								var foundSpecies = false
								var genus = ""
								var species = ""
								var imageURL = "" 
								titleParts.foreach(titlePart => {
									if (!foundTag && titlePart.toUpperCase.equals("#BIOBLITZ")) {
										foundTag = true
									} else if (!foundURL && titlePart.toUpperCase.startsWith("HTTP://")) {
										foundURL = true
										imageURL = titlePart
									} else if (!foundGenus) {
										foundGenus = true
										genus = titlePart
									} else if (!foundSpecies) {
										foundSpecies = true
										species = titlePart
									}
								})
								if (foundTag && foundURL && foundGenus && foundSpecies) {
									val existingLocation = Model.Location.find(By(Model.Location.source, privateSource), By(Model.Location.latitude, latitude), By(Model.Location.longitude, longitude)) openOr null
									val location =
										if (null == existingLocation) {
											val location = Model.Location.create.source(privateSource).latitude(latitude).longitude(longitude)
											location.save
											location
										} else {
											existingLocation
										}
									val event = Model.Event.create.source(privateSource)
									event.location(location)
									val dateValue = (entryNodeSeq \ "published").text
									event.verbatimDate(dateValue)
									val calendar: Calendar = DatatypeConverter.parseDateTime(dateValue);
									event.date(calendar.getTime())
									event.save
									val occurrence = Model.Occurrence.create.source(privateSource)
									occurrence.event(event)
									occurrence.occurrenceType(Model.OccurrenceType.HumanObservation)
									occurrence.remarks("Imported from " + statusURL + ": " + title)
									var fullImageURL = ""
									if (imageURL.toUpperCase.startsWith("HTTP://TWITPIC.COM")) {
										val imageURLSource: scala.io.Source = scala.io.Source.fromURL(imageURL + "/full")
										val imagePageSource = imageURLSource.mkString
										val startIndex = imagePageSource.indexOf("<img src=\"http://") + 10
										val finishIndex = imagePageSource.indexOf("\"", startIndex)
										fullImageURL = imagePageSource.substring(startIndex, finishIndex)
									} else if (imageURL.toUpperCase.startsWith("HTTP://YFROG")) {
										fullImageURL = imageURL + ":iphone"
									}
									if (fullImageURL != "") {
										try {
											val imageURLObject = new URL(fullImageURL)
											val inputStream: InputStream = new BufferedInputStream(imageURLObject.openStream())
											val byteArrayOutputStream  = new ByteArrayOutputStream()
											val bytes: Array[Byte] = new Array[Byte](1024)
											var n: Int = inputStream.read(bytes)
											while (n != -1)
											{
											   byteArrayOutputStream.write(bytes, 0, n)
											   n = inputStream.read(bytes)
											}
											byteArrayOutputStream.close()
											inputStream.close()
											val imageBytes: Array[Byte] = byteArrayOutputStream.toByteArray()
											val mediaFile: Model.MediaFile = Model.MediaFile.create
											mediaFile.source(privateSource)
											mediaFile.description(imageURLObject.getFile)
											mediaFile.fileType("image/*")
											mediaFile.encodedData(Model.MediaFile.encoder.encode(imageBytes))
											val mediaFileBundle = Model.MediaFileBundle.create
											mediaFileBundle.save
											occurrence.mediaFileBundle(mediaFileBundle)
											occurrence.save
											mediaFile.mediaFileBundle(mediaFileBundle)
											mediaFile.save
										} catch {
										  	case e: MalformedURLException => {}
										} 
									}
									val classification: Model.Classification = Service.Classification.findClassification(group, genus, species)
									if (null != classification) {
										occurrence.classification(classification)
										occurrence.save
									}
								}
							}
						}

					}
				})
			}
		}
	}
}

object TwitterBioBlitzImporterScheduler {
	val logger = Logger("Twitter BioBlitz Importer")
	private final val scheduledExecutorService: ScheduledExecutorService = Executors.newScheduledThreadPool(1)
	private final val runImporter: Boolean = {
		val publicSource = Model.Source.find(By(Model.Source.uniqueID, "BIOBLITZ")) openOr null
		val privateSource = Model.Source.find(By(Model.Source.uniqueID, "BIOBLITZ_PRIVATE")) openOr null
		val group =
			if (null == publicSource) {
				null
			} else {
				publicSource.group
			}
		null != publicSource && null != privateSource && null != group
	}
	private final val importer: Runnable = new Runnable() {
		def run() = {
			if (runImporter) {
				TwitterBioBlitzImporter.start
			}
		}
	}
	
	private var scheduledFuture: ScheduledFuture[AnyRef] = null
	
	def running_? = {
		null != scheduledFuture
	}
	
	def start() = {
		if (null == scheduledFuture) {
			logger.info("Starting Twitter BioBlitz Importer Scheduler...")
			scheduledFuture = scheduledExecutorService.scheduleAtFixedRate(importer, 5, 10, TimeUnit.MINUTES).asInstanceOf[ScheduledFuture[AnyRef]]
		}
	}
	
	def stop() = {
		if (null != scheduledFuture) {
			logger.info("Stopping Twitter BioBlitz Importer Scheduler...")
			scheduledFuture.cancel(true)
			scheduledFuture = null
		}
	}
}