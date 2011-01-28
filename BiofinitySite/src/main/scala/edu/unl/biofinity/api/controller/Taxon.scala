package edu.unl.biofinity.api.controller

import edu.unl.biofinity.api.{model => Model}

import java.sql.PreparedStatement

import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.mapper._
import net.liftweb.util._

object Taxon {
	def names(query: String): List[String] = {
		names(query, "")
	}
	
	def names(query: String, rank: String): List[String] = {
		if (null == query || query.equals("") || !Model.User.groupSignedIn_?) {
			List()
		} else {
			val baseSQL = "select t.entity_id, t.name from taxons t inner join taxonomy_nodes n on t.entity_id = n.taxon_id inner join classifications c on c.entity_id = n.classification_id inner join sources s on s.entity_id = c.source_id"
			val taxons = Model.Taxon.findAllByPreparedStatement({database =>
				if (rank.equals("")) {
					val preparedStatement: PreparedStatement = database.connection.prepareStatement(baseSQL + " where (s.source_type = 0 or c.source_id = ? or c.source_id = ?) and t.name like ?                group by t.name order by t.name")
					preparedStatement.setLong(1, (Model.User.currentGroup.is.publicSource.obj openOr Model.Source).entityID)
					preparedStatement.setLong(2, (Model.User.currentGroup.is.privateSource.obj openOr Model.Source).entityID)
					preparedStatement.setString(3, query + "%")
					preparedStatement
				} else {
					val preparedStatement: PreparedStatement = database.connection.prepareStatement(baseSQL + " where (s.source_type = 0 or c.source_id = ? or c.source_id = ?) and t.name like ? and t.rank = ? group by t.name order by t.name")
					preparedStatement.setLong(1, (Model.User.currentGroup.is.publicSource.obj openOr Model.Source).entityID)
					preparedStatement.setLong(2, (Model.User.currentGroup.is.privateSource.obj openOr Model.Source).entityID)
					preparedStatement.setString(3, query + "%")
					preparedStatement.setString(4, rank)
					preparedStatement
				}
			})
			if (1 > taxons.length) {
				List()
			} else {
				taxons.map(_.name.is).distinct
			}
		}
	}

	def ranks(query: String): List[String] = {
		if (null == query || query.equals("") || !Model.User.groupSignedIn_?) {
			List()
		} else {
			val taxons = Model.Taxon.findAllByPreparedStatement({database =>
				val preparedStatement: PreparedStatement = database.connection.prepareStatement("select t.entity_id, t.rank from taxons t inner join taxonomy_nodes n on t.entity_id = n.taxon_id inner join classifications c on c.entity_id = n.classification_id inner join sources s on s.entity_id = c.source_id where (s.source_type = 0 or c.source_id = ? or c.source_id = ?) and t.rank like ? group by t.rank order by t.rank")
				preparedStatement.setLong(1, (Model.User.currentGroup.is.publicSource.obj openOr Model.Source).entityID)
				preparedStatement.setLong(2, (Model.User.currentGroup.is.privateSource.obj openOr Model.Source).entityID)
				preparedStatement.setString(3, query + "%")
				preparedStatement
			})
			if (1 > taxons.length) {
				List()
			} else {
				taxons.map(_.rank.is).distinct
			}
		}
	}
}