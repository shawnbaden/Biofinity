package edu.unl.biofinity.api.service

import edu.unl.biofinity.api.{model => Model}

import java.io.BufferedWriter
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.OutputStreamWriter
import java.sql.ResultSet
import java.text.SimpleDateFormat
import java.util.Date

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
import collection.immutable.Queue
import xml.{Text, Node, NodeSeq}

object Occurrence {
	def read(r: Req): Box[LiftResponse] = {
		val occurrence: Model.Occurrence = Model.Occurrence.find(S.param("ID") openOr -1) openOr null

		if (null == occurrence) {
			Full(NotFoundResponse(""))
		} else {
			if (occurrence.canRead_?) {
				val basisOfRecord = Model.OccurrenceType.typeMapInverted(Model.OccurrenceType(occurrence.occurrenceType.toInt)).replaceAll(" ", "")
				
				Full(
					XmlResponse(
<dwr:DarwinRecordSet
	xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
	xsi:schemaLocation="http://rs.tdwg.org/dwc/dwcrecord/ http://rs.tdwg.org/dwc/xsd/tdwg_dwc_classes.xsd"
	xmlns:dcterms="http://purl.org/dc/terms/"
	xmlns:dwc="http://rs.tdwg.org/dwc/terms/"
	xmlns:dwr="http://rs.tdwg.org/dwc/dwcrecord/">
	<dwc:Occurrence>
		<dwc:basisOfRecord>{basisOfRecord}</dwc:basisOfRecord>
		<dwc:behavior>{occurrence.behavior.is}</dwc:behavior>
		<dwc:eventID>{"urn:lsid:biofinity.unl.edu:Event:" + occurrence.event.obj.openOr(Model.Event).entityID.is.toString}</dwc:eventID>
		<dwc:lifeStage>{occurrence.lifeStage.is}</dwc:lifeStage>
		<dwc:occurrenceID>{"urn:lsid:biofinity.unl.edu:Occurrence:" + occurrence.entityID.is.toString}</dwc:occurrenceID>
		<dwc:preparations>{occurrence.preparations.is}</dwc:preparations>
		<dwc:reproductiveCondition>{occurrence.reproductiveCondition.is}</dwc:reproductiveCondition>
		<dwc:sex>{occurrence.sex.is}</dwc:sex>
	</dwc:Occurrence>
</dwr:DarwinRecordSet>
					)
				)
			} else {
				Full(UnauthorizedResponse(""))
			}
		}
	}
	
	def create(req: Req): Box[LiftResponse] = {
		val user: Model.User = {
			val mobileDevice: Model.MobileDevice = {
				try {
					Model.MobileDevice.find(req.param("MobileDeviceID") openOr -1) openOr null
				} catch {
					case e: Exception => null
				}
			}
			if (null == mobileDevice) {
				BioBlitz.getGroup
				BioBlitz.getUser
			} else {
				mobileDevice.user.obj openOr null
			}
		}
		if (null == user) {
			return Full(PlainTextResponse("Invalid user request.", List(), 401))
		}
		
		val groupUsers: List[Model.GroupUser] = Model.GroupUser.findAll(By(Model.GroupUser.user, user))
		val group: Model.Group = {
			if (groupUsers.isEmpty) {
				null
			} else {
				try {
					groupUsers.first.group.obj.open_!
				} catch {
					case e: Exception => null
				}
			}
		}
		if (null == group) {
			return Full(PlainTextResponse("User is not associated with any lab.", List(), 403))
		}
	 	
		val source: Model.Source = group.privateSource.obj.open_!
		
		val location: Model.Location = Model.Location.create
		location.source(source)
		
		try {
			location.latitude(req.params("Latitude").first.toDouble)
			location.longitude(req.params("Longitude").first.toDouble)
		} catch {
			case e: Exception => {
				return Full(PlainTextResponse("A location (latitude and longitude) must be specified.", List(), 403))
			}
		}
		location.save
		
		val event: Model.Event = Model.Event.create
		event.source(source)
		event.date(new Date(System.currentTimeMillis))
		event.location(location)
		event.save
		
		val occurrence: Model.Occurrence = Model.Occurrence.create
		occurrence.source(source)
		occurrence.occurrenceType(Model.OccurrenceType.HumanObservation)
		try {
			val person = user.person.obj.open_!
			occurrence.recordedBy(person.fullName)
		} catch {
			case e: Exception => {}
		}
		occurrence.details(req.param("Description") openOr "")
		occurrence.sex(req.param("Sex") openOr "")
		occurrence.lifeStage(req.param("LifeStage") openOr "")
		occurrence.event(event)
		occurrence.save
		
		val file: FileParamHolder =
			try {
				req.uploadedFiles.first
			} catch {
				case e: Exception => null
			}
		if (null != file && null != file.file && file.file.length > 0) {
			var mediaFileBundle = occurrence.mediaFileBundle.obj openOr null
			if (null == mediaFileBundle) {
				mediaFileBundle = Model.MediaFileBundle.create
				mediaFileBundle.save
				occurrence.mediaFileBundle(mediaFileBundle)
				occurrence.save
			}
			val mediaFile: Model.MediaFile = Model.MediaFile.create
			mediaFile.source(source)
			mediaFile.description(file.fileName)
			mediaFile.fileType(file.mimeType)
			mediaFile.encodedData(Model.MediaFile.encoder.encode(file.file))
			mediaFile.mediaFileBundle(mediaFileBundle)
			mediaFile.save
		}
		
		val genus = req.param("Genus") openOr ""
		val species = req.param("Species") openOr ""
		if (!genus.equals("") && !species.equals("")) {
			val classification: Model.Classification = Classification.findClassification(group, genus, species)
			if (null == classification) {
				occurrence.remarks("Genus/Species: " + genus + " " + species)
				occurrence.save
			} else {
				occurrence.classification(classification)
				occurrence.save
			}
		}
		
		Full(XmlResponse(<Success/>))
	}

	def occurrencesKML(req: Req): Box[LiftResponse] = {
		if (null == Model.Occurrence.currentOccurrenceIDs.is) {
			return Full(PlainTextResponse("There are no occurrences.", List(), 412))
		}

		var folderNodes: List[NodeSeq] = List()
		DB.use(DefaultConnectionIdentifier) {connection =>
			val occurrenceIDsAsString: String = Model.Occurrence.currentOccurrenceIDs.is.map(_.toString).foldLeft[String]("-1")(_ + "," + _)
			var fullSQL = "select o.entity_id, o.details, l.latitude, l.longitude, c.name from occurrences o inner join events e on o.event_id = e.entity_id inner join locations l on e.location_id = l.entity_id left outer join classifications c on o.classification_id = c.entity_id where o.entity_id in (" + occurrenceIDsAsString + ") order by c.name"

			def randomColor: String = {
				var color: String = "ff"
				var rand: scala.util.Random = new scala.util.Random(System.currentTimeMillis())
				color += Integer.toHexString(rand.nextInt(255))
				color += Integer.toHexString(rand.nextInt(255))
				color += Integer.toHexString(rand.nextInt(255))

				color
			}

			DB.prepareStatement(fullSQL, connection) { preparedStatement =>
				val results: ResultSet = preparedStatement.executeQuery()

				var groupClassificationName: String = null
				var color: String = randomColor
				var placemarkNodes: List[NodeSeq] = List()

				while (results.next()) {
					val classificationName = {
						val name = results.getString("NAME")
						if (null == name) {
							""
						} else {
							name
						}
					}
					if (null == groupClassificationName) {
						groupClassificationName = classificationName
					}
					if (!classificationName.equals(groupClassificationName)) {	
						folderNodes = folderNodes :+ <Folder><name>{groupClassificationName}</name>{placemarkNodes.flatMap((nodeSeq: NodeSeq) => nodeSeq)}</Folder>
						groupClassificationName = classificationName
						placemarkNodes = List()
						color = randomColor
					}

					placemarkNodes = placemarkNodes :+ {
						<Placemark>
							<Style>
								<IconStyle><color>{color}</color></IconStyle>
							</Style>
							<description>
								{
								results.getString("DETAILS")+
								<div>
									<br/>
									<a href={"http://biofinity.unl.edu/lab/occurrence?ID=" + results.getString("ENTITY_ID")}>
									{
										Text("View in My Lab")
									}
									</a>
								</div>.toString()
								}
							</description>
							<Point>
								 <coordinates>{results.getString("LONGITUDE")},{results.getString("LATITUDE")}</coordinates>
							</Point>
						</Placemark>
					}
				}
				
				folderNodes = folderNodes :+ <Folder><name>{groupClassificationName}</name>{placemarkNodes.flatMap((nodeSeq: NodeSeq) => nodeSeq)}</Folder>
			}
		}

		Full(InMemoryResponse(
			<kml xmlns="http://www.opengis.net/kml/2.2">
				<Document name="Occurrences">
					{folderNodes.flatMap((nodeSeq: NodeSeq) => nodeSeq)}
				</Document>
			</kml>.toString().getBytes,
			List(("Content-Type", "application/vnd.google-earth.kml+xml"), ("content-disposition", "attachment;filename=occurrences.kml")),
			List(), 200)
		)
	}

	def occurrencesCSV(req: Req): Box[LiftResponse] = {
		if (null == Model.Occurrence.currentOccurrenceIDs.is) {
			return Full(PlainTextResponse("There are no occurrences.", List(), 412))
		}
		val byteArrayOutputStream = new ByteArrayOutputStream()
		val bufferedWriter = new BufferedWriter(new OutputStreamWriter(byteArrayOutputStream))
		bufferedWriter.write("Occurrence ID,Behavior,Classification,Continent,Country,County,Date,Details,Habitat,Island,Island Group,Latitude,Life Stage,Locality,Longitude,Municipality,Preparations,Recorded By,Reproductive Condition,Sampling Effor,Sampling Protocol,Sex,State/Province,Verbatim Date,Verbatim Elevation,Water Body")
		bufferedWriter.newLine
		val simpleDateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")
		DB.use(DefaultConnectionIdentifier) {connection =>
			val occurrenceIDsAsString: String = Model.Occurrence.currentOccurrenceIDs.is.map(_.toString).foldLeft[String]("-1")(_ + "," + _)
			var fullSQL = "select o.entity_id, o.behavior, c.name, l.continent, l.country, l.county, e.date, o.details, e.habitat, l.island, l.island_group, l.latitude, o.life_stage, l.locality, l.longitude, l.municipality, o.preparations, o.recorded_by, o.reproductive_condition, e.sampling_effort, e.sampling_protocol, o.sex, l.state_province, e.verbatim_date, l.verbatim_elevation, l.water_body from occurrences o inner join events e on o.event_id = e.entity_id inner join locations l on e.location_id = l.entity_id left outer join classifications c on o.classification_id = c.entity_id where o.entity_id in (" + occurrenceIDsAsString + ")"
			
			def escapeString(input: String): String = {
				if (null == input) {
					""
				} else {
					input.replaceAll("\"", "\"\"")
				}
			}
			
			DB.prepareStatement(fullSQL, connection) { preparedStatement =>
				val results: ResultSet = preparedStatement.executeQuery()
				var value: String = null
				while (results.next()) {
					val stringBuilder: StringBuilder = new StringBuilder
					stringBuilder.append("\"")
					stringBuilder.append(results.getLong("ENTITY_ID").toString)
					stringBuilder.append("\",\"")
					stringBuilder.append(escapeString(results.getString("BEHAVIOR")))
					stringBuilder.append("\",\"")
					stringBuilder.append(escapeString(results.getString("NAME")))
					stringBuilder.append("\",\"")
					stringBuilder.append(escapeString(results.getString("CONTINENT")))
					stringBuilder.append("\",\"")
					stringBuilder.append(escapeString(results.getString("COUNTRY")))
					stringBuilder.append("\",\"")
					stringBuilder.append(escapeString(results.getString("COUNTY")))
					stringBuilder.append("\",\"")
					val date = results.getDate("DATE")
					if (null != date) {
						stringBuilder.append(simpleDateFormat.format(date))
					}
					stringBuilder.append("\",\"")
					stringBuilder.append(escapeString(results.getString("DETAILS")))
					stringBuilder.append("\",\"")
					stringBuilder.append(escapeString(results.getString("HABITAT")))
					stringBuilder.append("\",\"")
					stringBuilder.append(escapeString(results.getString("ISLAND")))
					stringBuilder.append("\",\"")
					stringBuilder.append(escapeString(results.getString("ISLAND_GROUP")))
					stringBuilder.append("\",\"")
					stringBuilder.append(escapeString(results.getString("LATITUDE")))
					stringBuilder.append("\",\"")
					stringBuilder.append(escapeString(results.getString("LIFE_STAGE")))
					stringBuilder.append("\",\"")
					stringBuilder.append(escapeString(results.getString("LOCALITY")))
					stringBuilder.append("\",\"")
					stringBuilder.append(escapeString(results.getString("LONGITUDE")))
					stringBuilder.append("\",\"")
					stringBuilder.append(escapeString(results.getString("MUNICIPALITY")))
					stringBuilder.append("\",\"")
					stringBuilder.append(escapeString(results.getString("PREPARATIONS")))
					stringBuilder.append("\",\"")
					stringBuilder.append(escapeString(results.getString("RECORDED_BY")))
					stringBuilder.append("\",\"")
					stringBuilder.append(escapeString(results.getString("REPRODUCTIVE_CONDITION")))
					stringBuilder.append("\",\"")
					stringBuilder.append(escapeString(results.getString("SAMPLING_EFFORT")))
					stringBuilder.append("\",\"")
					stringBuilder.append(escapeString(results.getString("SAMPLING_PROTOCOL")))
					stringBuilder.append("\",\"")
					stringBuilder.append(escapeString(results.getString("SEX")))
					stringBuilder.append("\",\"")
					stringBuilder.append(escapeString(results.getString("STATE_PROVINCE")))
					stringBuilder.append("\",\"")
					/*
					if (null != classification) {
						val taxa = classification.taxonomyNodes
							.map(_.taxon.obj openOr Model.Taxon)
							.foldLeft("")((a, b) => a + " " + b.name + " (" + b.rank + ")")
						stringBuilder.append(taxa)
					}
					stringBuilder.append("\",\"")
					*/
					stringBuilder.append(escapeString(results.getString("VERBATIM_DATE")))
					stringBuilder.append("\",\"")
					stringBuilder.append(escapeString(results.getString("VERBATIM_ELEVATION")))
					stringBuilder.append("\",\"")
					stringBuilder.append(escapeString(results.getString("WATER_BODY")))
					stringBuilder.append("\"")
					bufferedWriter.write(stringBuilder.toString)
					bufferedWriter.newLine
				}
			}
		}
		bufferedWriter.close()
		val bytes = byteArrayOutputStream.toByteArray()
		//val data = MediaFile.decoder.decodeBuffer(encodedData.is)
		Full(
			StreamingResponse(
				new ByteArrayInputStream(bytes),
				() => {},
				bytes.length,
				List(("Content-Type", "text/csv"), ("content-disposition", "attachment;filename=occurrences.csv")),
				List(),
				200
			)
		)
	}
}