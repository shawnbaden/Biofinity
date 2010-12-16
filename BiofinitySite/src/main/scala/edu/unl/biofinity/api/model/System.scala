package edu.unl.biofinity.api.model

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.InputStream

import java.text.SimpleDateFormat

import java.awt.AlphaComposite
import java.awt.Graphics2D
import java.awt.RenderingHints
import java.awt.image.BufferedImage

import javax.imageio.ImageIO

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

import sun.misc.{BASE64Decoder, BASE64Encoder}

class AdditionalProperty extends LongKeyedMapper[AdditionalProperty] {
	def getSingleton = AdditionalProperty
	
	/* CORE ENTITY TRAITS */
	object entityID extends MappedLongIndex(this) { override def dbColumnName = "entity_id" }
	override def primaryKeyField = entityID
	/* CORE ENTITY TRAITS */
	
	object additionalPropertyBundle extends MappedLongForeignKey(this, AdditionalPropertyBundle) { override def dbColumnName = "additional_property_bundle_id"}
	object name extends MappedString(this, 128) { override def dbColumnName = "name" }
	object value extends MappedString(this, 512) { override def dbColumnName = "value" }
	object valueType extends MappedEnum(this, AdditionalPropertyType) {
		override def dbColumnName = "value_type"
		override def defaultValue = AdditionalPropertyType.String
	}
}

class AdditionalPropertyBundle extends LongKeyedMapper[AdditionalPropertyBundle] {
	def getSingleton = AdditionalPropertyBundle
	
	/* CORE ENTITY TRAITS */
	object entityID extends MappedLongIndex(this) { override def dbColumnName = "entity_id" }
	override def primaryKeyField = entityID
	/* CORE ENTITY TRAITS */
	
	def additionalProperties: List[AdditionalProperty] = AdditionalProperty.findAll(By(AdditionalProperty.additionalPropertyBundle, this.entityID))
}

class AdditionalPropertyType extends Enumeration {
	type AdditionalPropertyType = Value
	val String, Boolean = Value /*DO NOT CHANGE ENUM ORDER SINCE DATABASE STORES INT VALUE*/
	val typeMap = Map[String, Value](
		"Text" -> String,
		"Yes/No" -> Boolean
	)
	def typeMapInverted: Map[Value, String] = {
		var inverted: Map[Value, String] = Map()
		typeMap.keySet.foreach(key => {
			inverted += typeMap(key) -> key
		})
		inverted
	}
}

class Event extends LongKeyedMapper[Event] {
	def getSingleton = Event
	
	/* CORE ENTITY TRAITS */
	object entityID extends MappedLongIndex(this) { override def dbColumnName = "entity_id" }
	override def primaryKeyField = entityID
	
	object additionalPropertyBundle extends MappedLongForeignKey(this, AdditionalPropertyBundle) { override def dbColumnName = "additional_property_bundle_id"}
	object description extends MappedText(this) { override def dbColumnName = "description" }
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
		delete_!
	}
	def deleteIfNoChildren_! = {
		if (occurrences.length < 1/* && samples.length < 1*/) {
			deleteFull_!
		}
	}
	override def save = {
		if (!saved_?) {
			val group = (source.obj openOr Source).group
			if (null != group) {
				val requiredAdditionalPropertyBundle = group.eventAdditionalPropertyBundle.obj openOr null
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
	
	object date extends MappedDateTime(this) {
		override def dbColumnName = "date"
		override def toString = {
			if (null == is) {
				""
			} else {
				simpleDateFormat.format(is)
			}
		}
	}
	object endDate extends MappedDateTime(this) {
		override def dbColumnName = "end_date"
		override def toString = {
			if (null == is) {
				""
			} else {
				simpleDateFormat.format(is)
			}
		}
	}
	object fieldNotes extends MappedString(this, 512) {override def dbColumnName = "field_notes"}
	object fieldNumber extends MappedString(this, 128) {override def dbColumnName = "field_number"}
	object habitat extends MappedString(this, 512) { override def dbColumnName = "habitat"}
	object location extends MappedLongForeignKey(this, Location) { override def dbColumnName = "location_id"}
	object remarks extends MappedString(this, 512) {override def dbColumnName = "remarks"}
	object samplingEffort extends MappedString(this, 512) { override def dbColumnName = "sampling_effort"}
	object samplingProtocol extends MappedString(this, 512) { override def dbColumnName = "sampling_protocol"}
	object verbatimDate extends MappedString(this, 512) { override def dbColumnName = "verbatim_date"}
	
	def occurrences: List[Occurrence] = Occurrence.findAll(By(Occurrence.event, this))
	
	val simpleDateFormat = new SimpleDateFormat("MMMMM d, yyyy")
}

class Group extends LongKeyedMapper[Group] { 
	def getSingleton = Group
	
	/* CORE ENTITY TRAITS */
	object entityID extends MappedLongIndex(this) { override def dbColumnName = "entity_id" }
	override def primaryKeyField = entityID
	
	object additionalPropertyBundle extends MappedLongForeignKey(this, AdditionalPropertyBundle) { override def dbColumnName = "additional_property_bundle_id"}
	object description extends MappedText(this) { override def dbColumnName = "description" }
	/* CORE ENTITY TRAITS */
	
	object name extends MappedString(this, 128) { override def dbColumnName = "name" }
	object eventAdditionalPropertyBundle extends MappedLongForeignKey(this, AdditionalPropertyBundle) { override def dbColumnName = "event_additional_property_bundle_id"}
	object occurrenceAdditionalPropertyBundle extends MappedLongForeignKey(this, AdditionalPropertyBundle) { override def dbColumnName = "occurrence_additional_property_bundle_id"}
	object publicSource extends MappedLongForeignKey(this, Source) { override def dbColumnName = "public_source_id" }
	object privateSource extends MappedLongForeignKey(this, Source) { override def dbColumnName = "private_source_id" }
	def groupUsers: List[GroupUser] = GroupUser.findAll(By(GroupUser.group, this))
	def groupUserRequests: List[GroupUserRequest] = GroupUserRequest.findAll(By(GroupUserRequest.group, this))
	def users: List[User] = groupUsers.map(_.user.obj.open_!)
	object recordMultiplier extends MappedInt(this) {
		override def dbColumnName = "record_multiplier"
		override def defaultValue = 1
	}
	object institution extends MappedString(this, 128) { override def dbColumnName = "institution" }
	object department extends MappedString(this, 128) { override def dbColumnName = "department" }
	
	def posts: List[GroupPost] = GroupPost.findAll(By(GroupPost.group, this))
	
	def canUpdate_?(): Boolean = {
		if (User.signedIn_?) {
			val adminGroupUsers = User.currentUser.groupUsers.filter(_.userType == GroupUserType.Administrator)
			if (adminGroupUsers.length > 0) {
				if (adminGroupUsers.filter(_.group == this).length > 0) {
					return true
				}
			}
		}
		false
	}
}

class GroupRequest extends LongKeyedMapper[GroupRequest] {
	def getSingleton = GroupRequest
	
	/* CORE ENTITY TRAITS */
	object entityID extends MappedLongIndex(this) { override def dbColumnName = "entity_id" }
	override def primaryKeyField = entityID
	
	object description extends MappedText(this) { override def dbColumnName = "description" }
	/* CORE ENTITY TRAITS */
	
	object user extends MappedLongForeignKey(this, User) { override def dbColumnName = "user_id" }
	object name extends MappedString(this, 128) { override def dbColumnName = "name" }
	object institution extends MappedString(this, 128) { override def dbColumnName = "institution" }
	object department extends MappedString(this, 128) { override def dbColumnName = "department" }
}

class GroupUser extends LongKeyedMapper[GroupUser] { 
	def getSingleton = GroupUser
	
	/* CORE ENTITY TRAITS */
	object entityID extends MappedLongIndex(this) { override def dbColumnName = "entity_id" }
	override def primaryKeyField = entityID
	/* CORE ENTITY TRAITS */
	
	object user extends MappedLongForeignKey(this, User) { override def dbColumnName = "user_id"}
	object group extends MappedLongForeignKey(this, Group) { override def dbColumnName = "group_id"}
	object approved extends MappedBoolean(this) { override def dbColumnName = "approved"}
	object userType extends MappedEnum(this, GroupUserType) {
		override def dbColumnName = "user_type"
		override def defaultValue = GroupUserType.Default
	}
}

class GroupUserRequest extends LongKeyedMapper[GroupUserRequest] {
	def getSingleton = GroupUserRequest
	
	/* CORE ENTITY TRAITS */
	object entityID extends MappedLongIndex(this) { override def dbColumnName = "entity_id"}
	override def primaryKeyField = entityID
	/* CORE ENTITY TRAITS */
	
	object user extends MappedLongForeignKey(this, User) { override def dbColumnName = "user_id" }
	object group extends MappedLongForeignKey(this, Group) { override def dbColumnName = "group_id" }
}

class GroupUserType extends Enumeration {
	type GroupUserType = Value
	val Administrator, Default, Limited, Restricted = Value /*DO NOT CHANGE ENUM ORDER SINCE DATABASE STORES INT VALUE*/
	val typeMap = Map[String, Value](
		"Administrator" -> Administrator,
		"Default" -> Default,
		"Limited" -> Limited,
		"Restricted" -> Restricted
	)
	def typeMapInverted: Map[Value, String] = {
		var inverted: Map[Value, String] = Map()
		typeMap.keySet.foreach(key => {
			inverted += typeMap(key) -> key
		})
		inverted
	}
}

class Location extends LongKeyedMapper[Location] {
	def getSingleton = Location
	
	/* CORE ENTITY TRAITS */
	object entityID extends MappedLongIndex(this) { override def dbColumnName = "entity_id" }
	override def primaryKeyField = entityID
	
	object description extends MappedText(this) { override def dbColumnName = "description" }
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
		delete_!
	}
	def deleteIfNoChildren_! = {
		if (events.length < 1) {
			deleteFull_!
		}
	}
	/* CORE ENTITY TRAITS */
	
	object continent extends MappedString(this, 512) { override def dbColumnName = "continent"}
	object country extends MappedString(this, 512) { override def dbColumnName = "country"}
	object county extends MappedString(this, 512) { override def dbColumnName = "county"}
	object island extends MappedString(this, 512) { override def dbColumnName = "island"}
	object islandGroup extends MappedString(this, 512) { override def dbColumnName = "island_group"}
	object latitude extends MappedDouble(this) { override def dbColumnName = "latitude"}
	object locality extends MappedString(this, 512) { override def dbColumnName = "locality"}
	object longitude extends MappedDouble(this) { override def dbColumnName = "longitude"}
	object maximumDepthInMeters extends MappedDouble(this) { override def dbColumnName = "maximum_depth_in_meters"}
	object maximumDistanceAboveSurfaceInMeters extends MappedDouble(this) { override def dbColumnName = "maximum_distance_above_surface_in_meters"}
	object maximumElevationInMeters extends MappedDouble(this) { override def dbColumnName = "maximum_elevation_in_meters"}
	object municipality extends MappedString(this, 512) { override def dbColumnName = "municipality"}
	object minimumDepthInMeters extends MappedDouble(this) { override def dbColumnName = "minimum_depth_in_meters"}
	object minimumDistanceAboveSurfaceInMeters extends MappedDouble(this) { override def dbColumnName = "minimum_distance_above_surface_in_meters"}
	object minimumElevationInMeters extends MappedDouble(this) { override def dbColumnName = "minimum_elevation_in_meters"}
	object remarks extends MappedString(this, 512) { override def dbColumnName = "remarks"}
	object stateProvince extends MappedString(this, 512) { override def dbColumnName = "state_province"}
	object verbatimCoordinates extends MappedString(this, 512) { override def dbColumnName = "verbatim_coordinates"}
	object verbatimDepth extends MappedString(this, 512) { override def dbColumnName = "verbatim_depth"}
	object verbatimElevation extends MappedString(this, 512) { override def dbColumnName = "verbatim_elevation"}
	object waterBody extends MappedString(this, 512) { override def dbColumnName = "water_body"}
	
	def events: List[Event] = Event.findAll(By(Event.location, this))
}

class MediaFile extends LongKeyedMapper[MediaFile] {
	def getSingleton = MediaFile
	
	/* CORE ENTITY TRAITS */
	object entityID extends MappedLongIndex(this) { override def dbColumnName = "entity_id" }
	override def primaryKeyField = entityID
	
	object description extends MappedText(this) { override def dbColumnName = "description" }
	object source extends MappedLongForeignKey(this, Source) { override def dbColumnName = "source_id" }
	object sourceItemID extends MappedString(this, 512) { override def dbColumnName = "source_item_id" }

	def canRead_? = {
		(source.obj openOr Source).public_? || canUpdate_?
	}
	def canUpdate_? = {
		User.groupSignedIn_? && User.currentGroup.is == (source.obj openOr Source).group
	}
	def deleteFull_! = {
		transformedMediaFiles.foreach(_.delete_!)
		delete_!
	}
	/* CORE ENTITY TRAITS */
	
	object encodedData extends MappedText(this) { override def dbColumnName = "encoded_data"}
	object fileType extends MappedString(this, 128) { override def dbColumnName = "file_type"}
	object mediaFileBundle extends MappedLongForeignKey(this, MediaFileBundle) { override def dbColumnName = "media_file_bundle_id"}
	
	def isImage: Boolean = {
		(null != fileType.is && MediaFile.isImage(fileType.is))
	}
	
	def name: String = {
		if (null == description.is) {
			if (null == fileType.is) {
				""
			} else {
				fileType.is
			}
		} else {
			description.is
		}
	}

	def rawData: Array[Byte] = {
		MediaFile.decoder.decodeBuffer(encodedData.is)
	}

	def transformedMediaFile(width: Int): TransformedMediaFile = {
		if (isImage) {
			val transforms = transformedMediaFiles.filter(_.width.is == width)
			if (transforms.length > 0) {
				transforms.first
			} else {
				val bufferedImage = ImageIO.read(new ByteArrayInputStream(rawData))
				val height: Int = ((bufferedImage.getHeight.toDouble / bufferedImage.getWidth.toDouble) * width).toInt
				val imageType =
					if (0 == bufferedImage.getType) {
						BufferedImage.TYPE_INT_ARGB
					} else {
						bufferedImage.getType
					}
				val resizedBufferedImage = new BufferedImage(width, height, imageType)
				val g = resizedBufferedImage.createGraphics()
				g.setComposite(AlphaComposite.Src)
				g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)
				g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
				g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
				g.drawImage(bufferedImage, 0, 0, width, height, null)
				g.dispose()
				val byteArrayOutputStream: ByteArrayOutputStream = new ByteArrayOutputStream()
				val fileTypeString =
					if (null == fileType.is) {
						""
					} else {
						fileType.is
					}
				ImageIO.write(resizedBufferedImage, "PNG", byteArrayOutputStream)
				val transformedMediaFile: TransformedMediaFile = TransformedMediaFile.create
				transformedMediaFile.mediaFile(this)
				transformedMediaFile.encodedData(MediaFile.encoder.encode(byteArrayOutputStream.toByteArray()))
				transformedMediaFile.height(resizedBufferedImage.getHeight)
				transformedMediaFile.width(resizedBufferedImage.getWidth)
				transformedMediaFile.save
				transformedMediaFile
			}
		} else {
			null
		}
	}

	def transformedMediaFiles: List[TransformedMediaFile] = TransformedMediaFile.findAll(By(TransformedMediaFile.mediaFile, this.entityID))
}

class MediaFileBundle extends LongKeyedMapper[MediaFileBundle] {
	def getSingleton = MediaFileBundle
	
	/* CORE ENTITY TRAITS */
	object entityID extends MappedLongIndex(this) { override def dbColumnName = "entity_id" }
	override def primaryKeyField = entityID
	/* CORE ENTITY TRAITS */
	
	def mediaFiles: List[MediaFile] = MediaFile.findAll(By(MediaFile.mediaFileBundle, this.entityID))
}

class MobileDevice extends LongKeyedMapper[MobileDevice] {
	def getSingleton = MobileDevice 
	
	/* CORE ENTITY TRAITS */
	object entityID extends MappedLongIndex(this) { override def dbColumnName = "entity_id" }
	override def primaryKeyField = entityID
	/* CORE ENTITY TRAITS */
	
	object uniqueID extends MappedString(this, 40) { override def dbColumnName = "unique_id" }
	object description extends MappedString(this, 120) { override def dbColumnName = "description" }
	object user extends MappedLongForeignKey(this, User) { override def dbColumnName = "user_id" }
}

class Person extends LongKeyedMapper[Person] {
	def getSingleton = Person
	
	/* CORE ENTITY TRAITS */
	object entityID extends MappedLongIndex(this) { override def dbColumnName = "entity_id" }
	override def primaryKeyField = entityID
	
	object description extends MappedText(this) { override def dbColumnName = "description" }
	object source extends MappedLongForeignKey(this, Source) { override def dbColumnName = "source_id" }
	object sourceItemID extends MappedString(this, 512) { override def dbColumnName = "source_item_id" }
	/* CORE ENTITY TRAITS */
	
	object firstName extends MappedString(this, 128) { override def dbColumnName = "first_name"}
	object lastName extends MappedString(this, 128) { override def dbColumnName = "last_name"}
	object phoneNumber extends MappedString(this, 128) {
		override def dbColumnName = "phone_number"
		override def defaultValue = ""
	}
	object address extends MappedString(this, 512) { override def dbColumnName = "address"}
	
	def fullName: String = {
		(firstName + " " + lastName).trim
	}
}

class Source extends LongKeyedMapper[Source] {
	def getSingleton = Source
	
	/* CORE ENTITY TRAITS */
	object entityID extends MappedLongIndex(this) { override def dbColumnName = "entity_id" }
	override def primaryKeyField = entityID
	
	object description extends MappedText(this) { override def dbColumnName = "description" }
	/* CORE ENTITY TRAITS */
	
	object name extends MappedString(this, 128) { override def dbColumnName = "name"}
	object uniqueID extends MappedString(this, 128) { override def dbColumnName = "unique_id"}
	object sourceType extends MappedEnum(this, SourceType) {
		override def dbColumnName = "source_type"
		override def defaultValue = SourceType.Private
	}
	
	def public_? = {
		SourceType.Public == SourceType(sourceType.toInt)
	}
	def private_? = {
		SourceType.Private == SourceType(sourceType.toInt)
	}
	
	def group: Group = {
		Group.find(By(Group.publicSource, entityID)).openOr(Group.find(By(Group.privateSource, entityID)).openOr(null))
	}
	
	def classifications: List[Classification] = Classification.findAll(By(Classification.source, this.entityID))
	def events: List[Event] = Event.findAll(By(Event.source, this.entityID))
	def locations: List[Location] = Location.findAll(By(Location.source, this.entityID))
	def mediaFiles: List[MediaFile] = MediaFile.findAll(By(MediaFile.source, this.entityID))
	def persons: List[Person] = Person.findAll(By(Person.source, this.entityID))
	def occurrences: List[Occurrence] = Occurrence.findAll(By(Occurrence.source, this.entityID))
	
	def unclassifiedOccurrences: List[Occurrence] = Occurrence.findAll(By(Occurrence.source, this.entityID), NullRef(Occurrence.classification))
}

class SourceType extends Enumeration {
	type SourceType = Value
	val Public, Private = Value /*DO NOT CHANGE ENUM ORDER SINCE DATABASE STORES INT VALUE*/
	val typeMap = Map[String, Value](
		"Public" -> Public,
		"Private" -> Private
	)
	def typeMapInverted: Map[Value, String] = {
		var inverted: Map[Value, String] = Map()
		typeMap.keySet.foreach(key => {
			inverted += typeMap(key) -> key
		})
		inverted
	}
}

class TransformedMediaFile extends LongKeyedMapper[TransformedMediaFile] {
	def getSingleton = TransformedMediaFile

	/* CORE ENTITY TRAITS */
	object entityID extends MappedLongIndex(this) { override def dbColumnName = "entity_id" }
	override def primaryKeyField = entityID
	/* CORE ENTITY TRAITS */

	object encodedData extends MappedText(this) { override def dbColumnName = "encoded_data"}
	object height extends MappedLong(this) { override def dbColumnName = "height"}
	object mediaFile extends MappedLongForeignKey(this, MediaFile) { override def dbColumnName = "media_file_id"}
	object width extends MappedLong(this) { override def dbColumnName = "width"}

	def rawData: Array[Byte] = {
		MediaFile.decoder.decodeBuffer(encodedData.is)
	}
}

class User extends LongKeyedMapper[User] {
	def getSingleton = User
	
	/* CORE ENTITY TRAITS */
	object entityID extends MappedLongIndex(this) { override def dbColumnName = "entity_id" }
	override def primaryKeyField = entityID
	/* CORE ENTITY TRAITS */
	
	object email extends MappedString(this, 128) { override def dbColumnName = "email"}
	object openID extends MappedString(this, 128) { override def dbColumnName = "openid"}
	object person extends MappedLongForeignKey(this, Person) { override def dbColumnName = "person_id"}
	object userType extends MappedEnum(this, UserType) {
		override def dbColumnName = "user_type"
		override def defaultValue = UserType.Default
	}
	
	def groups: List[Group] = groupUsers.map(_.group.obj.open_!)
	def groupPosts: List[GroupPost] = GroupPost.findAll(By(GroupPost.user, this))
	def groupRequests: List[GroupRequest] = GroupRequest.findAll(By(GroupRequest.user, this))
	def groupUsers: List[GroupUser] = GroupUser.findAll(By(GroupUser.user, this))
	def groupUserRequests: List[GroupUserRequest] = GroupUserRequest.findAll(By(GroupUserRequest.user, this))
	def mobileDevices: List[MobileDevice] = { MobileDevice.findAll(By(MobileDevice.user, this))}
}

class UserType extends Enumeration {
	type UserType = Value
	val Administrator, Default, Limited, Restricted = Value /*DO NOT CHANGE ENUM ORDER SINCE DATABASE STORES INT VALUE*/
	val typeMap = Map[String, Value](
		"Administrator" -> Administrator,
		"Default" -> Default,
		"Limited" -> Limited,
		"Restricted" -> Restricted
	)
}

object AdditionalProperty extends AdditionalProperty with LongKeyedMetaMapper[AdditionalProperty] {
	override def dbTableName = "ADDITIONAL_PROPERTIES"
}
object AdditionalPropertyBundle extends AdditionalPropertyBundle with LongKeyedMetaMapper[AdditionalPropertyBundle] {
	override def dbTableName = "ADDITIONAL_PROPERTY_BUNDLES"
	
	object currentAdditionalPropertyBundle extends RequestVar[AdditionalPropertyBundle](null)
	
	def contains(additionPropertyBundle: AdditionalPropertyBundle, requiredAdditionPropertyBundle: AdditionalPropertyBundle): Boolean = {
		if (null != requiredAdditionPropertyBundle && null != additionPropertyBundle) {
			requiredAdditionPropertyBundle.additionalProperties.foreach(requiredAdditionalProperty => {
				val found = additionPropertyBundle.additionalProperties.exists(additionalProperty => {
					additionalProperty.name.is.equalsIgnoreCase(requiredAdditionalProperty.name.is) && additionalProperty.valueType.is == requiredAdditionalProperty.valueType.is
				})
				if (!found) {
					return false
				}
			})
			
			true
		} else {
			false
		}
	}
	
	def ensureContains(additionPropertyBundle: AdditionalPropertyBundle, requiredAdditionPropertyBundle: AdditionalPropertyBundle) = {
		if (null != requiredAdditionPropertyBundle && null != additionPropertyBundle) {
			requiredAdditionPropertyBundle.additionalProperties.foreach(requiredAdditionalProperty => {
				val found = additionPropertyBundle.additionalProperties.exists(additionalProperty => {
					additionalProperty.name.is.equalsIgnoreCase(requiredAdditionalProperty.name.is) && additionalProperty.valueType.is == requiredAdditionalProperty.valueType.is
				})
				if (!found) {
					val additionalProperty: AdditionalProperty = AdditionalProperty.create
					additionalProperty.name(requiredAdditionalProperty.name)
					additionalProperty.value(requiredAdditionalProperty.value)
					additionalProperty.valueType(requiredAdditionalProperty.valueType)
					additionalProperty.additionalPropertyBundle(additionPropertyBundle)
					additionalProperty.save
				}
			})
		}
	}
}
object AdditionalPropertyType extends AdditionalPropertyType {
	def booleanAsValue(value: Boolean): String = {
		if (value) {
			"Yes"
		} else {
			"No"
		}
	}
	def valueAsBoolean(value: String): Boolean = {
		val upperCaseValue = value.toUpperCase
		if (upperCaseValue.equals("YES") || upperCaseValue.equals("TRUE")) {
			true
		} else if (upperCaseValue.equals("NO") || upperCaseValue.equals("FALSE")) {
			try {
				val longValue = value.toLong
				if (0 < longValue) {
					true
				} else {
					false
				}
			} catch {
				case e: NumberFormatException => false
			}
		} else {
			false
		}
	}
}
object Event extends Event with LongKeyedMetaMapper[Event] {
	override def dbTableName = "EVENTS"
	
	object currentEvent extends RequestVar[Event](null)
}
object Group extends Group with LongKeyedMetaMapper[Group] { 
	override def dbTableName = "GROUPS"
	
	object currentGroup extends RequestVar[Group](null)
}
object GroupRequest extends GroupRequest with LongKeyedMetaMapper[GroupRequest] { 
	override def dbTableName = "GROUP_REQUESTS"
}
object GroupUser extends GroupUser with LongKeyedMetaMapper[GroupUser] {
	override def dbTableName = "GROUP_USERS"
	
	def join ( user: User, group: Group) = this.create.user(user).group(group).save
}
object GroupUserRequest extends GroupUserRequest with LongKeyedMetaMapper[GroupUserRequest] { 
	override def dbTableName = "GROUP_USER_REQUESTS"
}
object GroupUserType extends GroupUserType {
}
object Location extends Location with LongKeyedMetaMapper[Location] {
	override def dbTableName = "LOCATIONS"
	
	object currentLocation extends RequestVar[Location](null)
}
object MediaFile extends MediaFile with LongKeyedMetaMapper[MediaFile] {
	override def dbTableName = "MEDIA_FILES"
	
	def decoder: BASE64Decoder = {
		new BASE64Decoder
	}
	def encoder: BASE64Encoder = {
		new BASE64Encoder
	}
	def isImage(value: String): Boolean = {
		value.toUpperCase.startsWith("IMAGE/")
	}
}
object MediaFileBundle extends MediaFileBundle with LongKeyedMetaMapper[MediaFileBundle] {
	override def dbTableName = "MEDIA_FILE_BUNDLES"
}
object MobileDevice extends MobileDevice with LongKeyedMetaMapper[MobileDevice] { 
	override def dbTableName = "MOBILE_DEVICES"
}
object Person extends Person with LongKeyedMetaMapper[Person] {
	override def dbTableName = "PERSONS"
}
object Source extends Source with LongKeyedMetaMapper[Source] {
	override def dbTableName = "SOURCES"
	
	object currentSource extends RequestVar[Source](null)
}
object SourceType extends SourceType {
}
object TransformedMediaFile extends TransformedMediaFile with LongKeyedMetaMapper[TransformedMediaFile] {
	override def dbTableName = "TRANSFORMED_MEDIA_FILES"
}
object User extends User with LongKeyedMetaMapper[User] {
	override def dbTableName = "USERS"
	
	object currentUser extends SessionVar[User](null)
	
	object currentGroup extends SessionVar[Group](null)
	
	def signedIn_? = {
		!(currentUser.is == null)
	}
	
	def groupSignedIn_? = {
		(signedIn_? && null != currentGroup.is)
	}
	
	def groupMember_? = {
		if (!signedIn_?) {
			false
		} else {
			currentUser.groupUsers.length > 0
		}
	}
	
	def pendingRequests_? = {
		if (!signedIn_?) {
			false
		} else {
			(currentUser.groupRequests.length + currentUser.groupUserRequests.length) > 0
		}
	}
}
object UserType extends UserType {
}