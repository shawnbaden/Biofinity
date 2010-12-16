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

class Classification extends LongKeyedMapper[Classification] {
	def getSingleton = Classification
	
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
		taxonomyNodes.foreach(_.delete_!)
		delete_!
	}
	/* CORE ENTITY TRAITS */
	
	object name extends MappedString(this, 512) {
		override def dbColumnName = "name"
	}
	
	def occurrences: List[Occurrence] = Occurrence.findAll(By(Occurrence.classification, this.entityID))
	def taxonomyNodes: List[TaxonomyNode] = TaxonomyNode.findAll(By(TaxonomyNode.classification, this.entityID))
	
	def taxons: List[Taxon] = {
		val topTaxonomyNodes = taxonomyNodes.filter(_.root)
		if (topTaxonomyNodes.length > 0) {
			topTaxonomyNodes.head.flatten(List())
		} else {
			List()
		}
	}
	
	def taxonsFrom(rank: String): List[Taxon] = {
		taxonomyNodes.filter(_.root).head.flatten(rank, List())
	}
	
	def classificationName = { 
		val leaf = taxonomyNodes.filter(_.leaf).head
		leaf.parent.obj.open_!.taxon.obj.open_!.name+" "+leaf.taxon.obj.open_!.name
	}
}

class Taxon extends LongKeyedMapper[Taxon] {
	def getSingleton = Taxon
	
	/* CORE ENTITY TRAITS */
	object entityID extends MappedLongIndex(this) { override def dbColumnName = "entity_id" }
	override def primaryKeyField = entityID
	
	object description extends MappedText(this) { override def dbColumnName = "description" }
	/* CORE ENTITY TRAITS */
	
	def taxonomyNodes: List[TaxonomyNode] = TaxonomyNode.findAll(By(TaxonomyNode.taxon, this.entityID))
	def occurrences: List[Occurrence] = Occurrence.findAllByPreparedStatement({ db => {
		 db.connection.prepareStatement("select s.* from occurrences s inner join classifications c on s.classification_id = c.entity_id inner join taxonomy_nodes n on n.classification_id = c.entity_id	inner join taxons t on n.taxon_id = t.entity_id where n.leaf = 1 and t.entity_id = "+entityID) 
	}})
	
	object name extends MappedString(this, 512) { override def dbColumnName = "name"}
	object rank extends MappedString(this, 128) { override def dbColumnName = "rank"}
}

class TaxonomyNode extends LongKeyedMapper[TaxonomyNode] {
	def getSingleton = TaxonomyNode
	
	/* CORE ENTITY TRAITS */
	object entityID extends MappedLongIndex(this) { override def dbColumnName = "entity_id" }
	override def primaryKeyField = entityID
	/* CORE ENTITY TRAITS */
	
	object root extends MappedBoolean(this) { override def dbColumnName = "root"}
	object leaf extends MappedBoolean(this) { override def dbColumnName = "leaf"}
	object taxon extends MappedLongForeignKey(this, Taxon) { override def dbColumnName = "taxon_id"}
	object parent extends MappedLongForeignKey(this, TaxonomyNode) { override def dbColumnName = "parent_id"}
	object child extends MappedLongForeignKey(this, TaxonomyNode) { override def dbColumnName = "child_id"}
	object classification extends MappedLongForeignKey(this, Classification) { override def dbColumnName = "classification_id"}
	
	def getTaxon:Taxon = { Taxon.find(taxon).get }
		
	/* recursive function to flatten the taxonomy structure */
	def flatten(col: List[Taxon]): List[Taxon] = {
		if ( leaf ) col ::: (taxon.obj.open_! :: Nil)
		else child.obj.open_!.flatten(col ::: (taxon.obj.open_! :: Nil))
	}
	
	/* recursive function to flatten the taxonomy structure */
	def flatten(rank: String, col: List[Taxon]): List[Taxon] = {
		if ( leaf ) col ::: (taxon.obj.open_! :: Nil)
		else if ( taxon.obj.open_!.rank.equals(rank) ) col ::: (taxon.obj.open_! :: Nil)
		else child.obj.open_!.flatten(rank, col ::: (taxon.obj.open_! :: Nil))
	}
}

object Classification extends Classification with LongKeyedMetaMapper[Classification] {
	override def dbTableName = "CLASSIFICATIONS"
	
	object currentClassification extends RequestVar[Classification](null)
	
	def exists(taxons: List[Taxon]): Boolean = {
		null != find(taxons)
	}
	
	def find(taxons: List[Taxon]): Classification = {
		var currentTaxonomyNodes: List[TaxonomyNode] = TaxonomyNode.findAll(By(TaxonomyNode.parent, Empty))
		
		var index = 0
		var continue = true
		var classification: Classification = null
		while (index < taxons.length && continue) {
			var currentTaxon = taxons(index)
			val matchingTaxonomyNodes: List[TaxonomyNode] = currentTaxonomyNodes.filter(taxonomyNode => taxonomyNode.taxon == currentTaxon)
			if (1 > matchingTaxonomyNodes.length) {
				continue = false
			} else {
				if (taxons.length - 1 == index) {
					if (matchingTaxonomyNodes.exists(taxonomyNode => taxonomyNode.leaf/*why doesn't 'null == taxonomyNode.child' work*/)) {
						classification = Classification.find(matchingTaxonomyNodes.first.classification).get
					}
				} else {
					currentTaxonomyNodes = matchingTaxonomyNodes.filter(taxonomyNode => !taxonomyNode.leaf).map(taxonomyNode => taxonomyNode.child.obj.open_!)
				}
			}
			index = index + 1
		}
		classification
	}
}
object Taxon extends Taxon with LongKeyedMetaMapper[Taxon] {
	override def dbTableName = "TAXONS"
	
	object currentTaxon extends RequestVar[Taxon](null)
}
object TaxonomyNode extends TaxonomyNode with LongKeyedMetaMapper[TaxonomyNode] {
	override def dbTableName = "TAXONOMY_NODES"
}