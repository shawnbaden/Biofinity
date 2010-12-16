package edu.unl.biofinity.api.model

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

class GroupPost extends LongKeyedMapper[GroupPost] {
	def getSingleton = GroupPost
	
	/* CORE ENTITY TRAITS */
	object entityID extends MappedLongIndex(this) { override def dbColumnName = "entity_id" }
	override def primaryKeyField = entityID
	/* CORE ENTITY TRAITS */
	
	object description extends MappedText(this) { override def dbColumnName = "description"}
	object title extends MappedString(this, 128) { override def dbColumnName = "title"}
	object published extends MappedDateTime(this) { override def dbColumnName = "published"}
	object group extends MappedLongForeignKey(this, Group) { override def dbColumnName = "group_id"}
	object user extends MappedLongForeignKey(this, User) { override def dbColumnName = "user_id"}
	object typeCode extends MappedString(this, 128) { override def dbColumnName = "type_code" }
	object link extends MappedString(this, 4096) { override def dbColumnName = "link" }
}

class Post extends LongKeyedMapper[Post] {
	def getSingleton = Post
	
	/* CORE ENTITY TRAITS */
	object entityID extends MappedLongIndex(this) { override def dbColumnName = "entity_id" }
	override def primaryKeyField = entityID
	/* CORE ENTITY TRAITS */
	
	object user extends MappedLongForeignKey(this, User) { override def dbColumnName = "user_id"}
	object description extends MappedText(this) { override def dbColumnName = "description"}
	object title extends MappedString(this, 128) { override def dbColumnName = "title"}
	object published extends MappedDateTime(this) { override def dbColumnName = "published"}
}

object GroupPost extends GroupPost with LongKeyedMetaMapper[GroupPost] { 
	override def dbTableName = "GROUP_POSTS"
}
object Post extends Post with LongKeyedMetaMapper[Post] {
	override def dbTableName = "POSTS"
}