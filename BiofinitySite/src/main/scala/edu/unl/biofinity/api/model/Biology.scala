package edu.unl.biofinity.api.model

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

class Occurrence extends LongKeyedMapper[Occurrence] { 
	def getSingleton = Occurrence
	
	/* CORE ENTITY TRAITS */
	object entityID extends MappedLongIndex(this) { override def dbColumnName = "entity_id" }
	override def primaryKeyField = entityID
	
	object additionalPropertyBundle extends MappedLongForeignKey(this, AdditionalPropertyBundle) { override def dbColumnName = "additional_property_bundle_id"}
	object description extends MappedText(this) { override def dbColumnName = "description" }
	object mediaFileBundle extends MappedLongForeignKey(this, MediaFileBundle) { override def dbColumnName = "media_file_bundle_id"}
	object source extends MappedLongForeignKey(this, Source) { override def dbColumnName = "source_id" }
	object sourceItemID extends MappedString(this, 512) { override def dbColumnName = "source_item_id" }
	object wikiPageID extends MappedLong(this) { override def dbColumnName = "wiki_page_id" }

	def canRead_? = {
		(source.obj openOr Source).public_? || canUpdate_?
	}
	def canUpdate_? = {
		User.groupSignedIn_? && User.currentGroup.is == (source.obj openOr Source).group
	}
	def deleteFull_! = {
		val additionalPropertyB = additionalPropertyBundle.obj openOr null
		if (null != additionalPropertyB) {
			additionalPropertyB.additionalProperties.foreach(_.delete_!)
			additionalPropertyB.delete_!
		}
		val mediaFileB = mediaFileBundle.obj openOr null
		if (null != mediaFileB) {
			mediaFileB.mediaFiles.foreach(_.delete_!)
			mediaFileB.delete_!
		}
		delete_!
	}
	override def save = {
		if (!saved_?) {
			val group = (source.obj openOr Source).group
			if (null != group) {
				val requiredAdditionalPropertyBundle = group.occurrenceAdditionalPropertyBundle.obj openOr null
				if (null != requiredAdditionalPropertyBundle && requiredAdditionalPropertyBundle.additionalProperties.length > 0) {
					var additionalPropertyBundleLocal = additionalPropertyBundle.obj openOr null
					if (null == additionalPropertyBundleLocal) {
						additionalPropertyBundleLocal = AdditionalPropertyBundle.create
						additionalPropertyBundleLocal.save
						additionalPropertyBundle(additionalPropertyBundleLocal)
						AdditionalPropertyBundle.ensureContains(additionalPropertyBundleLocal, requiredAdditionalPropertyBundle)
					} else {
						AdditionalPropertyBundle.ensureContains(additionalPropertyBundleLocal, requiredAdditionalPropertyBundle)
					}
				}
			}
		}
		super.save
	}
	/* CORE ENTITY TRAITS */
	
	object behavior extends MappedString(this, 512) { override def dbColumnName = "behavior"}
	object catalogNumber extends MappedString(this, 128) {override def dbColumnName = "catalog_number"}
	object details extends MappedString(this, 512) {override def dbColumnName = "details"}
	object disposition extends MappedString(this, 128) {override def dbColumnName = "disposition"}
	object classification extends MappedLongForeignKey(this, Classification) { override def dbColumnName = "classification_id"}
	object establishmentMeans extends MappedString(this, 512) { override def dbColumnName = "establishment_means"}
	object event extends MappedLongForeignKey(this, Event) { override def dbColumnName = "event_id"}
	object individualCount extends MappedInt(this) {
		override def dbColumnName = "individual_count"
		override def defaultValue = 1
	}
	object individualID extends MappedString(this, 128) {override def dbColumnName = "individual_id"}
	object lifeStage extends MappedString(this, 512) { override def dbColumnName = "life_stage"}
	object occurrenceType extends MappedEnum(this, OccurrenceType) {
		override def dbColumnName = "occurrence_type"
		override def defaultValue = OccurrenceType.PreservedSpecimen
	}
	object otherCatalogNumbers extends MappedString(this, 512) {override def dbColumnName = "other_catalog_numbers"}
	object preparations extends MappedString(this, 512) { override def dbColumnName = "preparations"}
	object recordedBy extends MappedString(this, 512) {override def dbColumnName = "recorded_by"}
	object recordNumber extends MappedString(this, 128) {override def dbColumnName = "record_number"}
	object remarks extends MappedString(this, 512) {override def dbColumnName = "remarks"}
	object reproductiveCondition extends MappedString(this, 512) { override def dbColumnName = "reproductive_condition"}
	object sex extends MappedString(this, 512) { override def dbColumnName = "sex"}
	object status extends MappedString(this, 128) {override def dbColumnName = "status"}
	object taxon extends MappedLongForeignKey(this, ClassifiedTaxon) { override def dbColumnName = "taxon_id"}
	
	def associatedMedia = "" // TODO: return list of media files
	def associatedOccurrences = "" // TODO: record related occurrences
	def associatedReferences = "" // TODO: record references
	def associatedSequences = "" // TODO: record related sequences
	def associatedTaxa = "" // TODO: return list of classification's taxa
	def previousIdentifications = "" // TODO: record previous identifications
}

class OccurrenceType extends Enumeration {
	type OccurrenceType = Value
	val PreservedSpecimen, FossilSpecimen, LivingSpecimen, HumanObservation, MachineObservation = Value /*DO NOT CHANGE ENUM ORDER SINCE DATABASE STORES INT VALUE*/
	val typeMap = Map[String, Value](
		"Preserved Specimen" -> PreservedSpecimen,
		"Fossil Specimen" -> FossilSpecimen,
		"Living Specimen" -> LivingSpecimen,
		"Human Observation" -> HumanObservation,
		"Machine Observation" -> MachineObservation
	)
	def typeMapInverted: Map[Value, String] = {
		var inverted: Map[Value, String] = Map()
		typeMap.keySet.foreach(key => {
			inverted += typeMap(key) -> key
		})
		inverted
	}
}

class SimpleOccurrence {
	var country: String = ""
	var classificationName: String = ""
	var eventDate: Date = null
	var latitude: Double = 0.0
	var locality: String = ""
	var longitude: Double = 0.0
	var mediaFileID: Long = 0
	var mediaFileType: String = ""
	var occurrenceID: Long = 0
	var recordedBy: String = ""
	var stateProvince: String = ""
	var verbatimElevation: String = ""
	
	def verbatimElevationAsDouble: Double = {
		try {
			verbatimElevation.toDouble
		} catch {
			case e: Exception => 0.0
		}
	}
}

object Occurrence extends Occurrence with LongKeyedMetaMapper[Occurrence] {
	override def dbTableName = "OCCURRENCES"
	
	object currentOccurrence extends RequestVar[Occurrence](null)
	object currentOccurrences extends RequestVar[List[Occurrence]](List())
	
	object currentOccurrenceID extends RequestVar[Long](-1)
	object currentOccurrenceIDs extends RequestVar[List[Long]](List())
}
object OccurrenceType extends OccurrenceType {
}
object SimpleOccurrence {
	object currentSimpleOccurrence extends RequestVar[SimpleOccurrence](null)
	object currentSimpleOccurrences extends RequestVar[List[SimpleOccurrence]](List())
}