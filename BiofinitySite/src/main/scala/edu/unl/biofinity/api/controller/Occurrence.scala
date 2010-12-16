package edu.unl.biofinity.api.controller

import edu.unl.biofinity.api.{model => Model}

import java.sql.ResultSet

import net.liftweb.http._
import net.liftweb.mapper._
import net.liftweb.util._

object Occurrence {
	object occurrenceIDs extends SessionVar[List[Long]](List())
	
	def searchIDs(): List[Long] = {
		searchIDs(0)
	}
	
	def searchIDs(groupID: Long): List[Long] = {
		searchIDs(groupID, 0)
	}
	
	def searchIDs(groupID: Long, limit: Long): List[Long] = {
		searchIDs(groupID, limit, null)
	}
	
	def searchIDs(groupID: Long, limit: Long, sortBy: String): List[Long] = {
		searchIDs(groupID, limit, sortBy, false)
	}
	
	def searchIDs(groupID: Long, limit: Long, sortBy: String, sortByDescending: Boolean): List[Long] = {
		var occurrenceIDs: List[Long] = List()
		
		DB.use(DefaultConnectionIdentifier) {connection =>
			var fullSQL = "SELECT o.entity_id FROM occurrences o INNER JOIN sources s ON o.source_id = s.entity_ID"
			
			if (0 < groupID) {
				fullSQL += " WHERE (o.source_id = ? or o.source_id = ?)"
			} else {
				fullSQL += " WHERE s.source_type = 0"
			}
			
			if (null != sortBy && !sortBy.equals("")) {
				if (sortBy.equals("ID")) {
					fullSQL += " ORDER BY o.entity_id"
				} else {
					fullSQL += " ORDER BY o.entity_id"
				}
				
				if (sortByDescending) {
					fullSQL += " DESC"
				}
			}
			
			if (0 < limit) {
				fullSQL += " LIMIT " + limit.toString
			}
			
			DB.prepareStatement(fullSQL, connection) { preparedStatement =>
				if (0 < groupID) {
					val group = Model.Group.find(groupID) openOr Model.Group
					preparedStatement.setLong(1, group.privateSource.obj.open_!.entityID)
					preparedStatement.setLong(2, group.publicSource.obj.open_!.entityID)
				}
				val results: ResultSet = preparedStatement.executeQuery()
				while (results.next()) {
					occurrenceIDs = occurrenceIDs :+ results.getLong("ENTITY_ID")
				}
			}
		}
		
		occurrenceIDs
	}
}