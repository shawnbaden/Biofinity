package edu.unl.biofinity.api.service

import edu.unl.biofinity.api.{model => Model}

import java.sql.PreparedStatement
import java.sql.ResultSet

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

import sun.misc.BASE64Decoder

object Classification {
	def read(r: Req): Box[LiftResponse] = {
		val classification: Model.Classification = Model.Classification.find(S.param("ID") openOr -1) openOr null

		if (null == classification) {
			Full(NotFoundResponse(""))
		} else {
			if (classification.canRead_?) {
				Full(
					XmlResponse(
<Classification>
	<ID>{classification.entityID.is.toString}</ID>
	<Name>{classification.name.is}</Name>
	<Taxa>
		{classification.taxons.flatMap(taxon => {
		<Taxon>
			<Name>{taxon.name.is}</Name>
			<Rank>{taxon.rank.is}</Rank>
		</Taxon>
		})}
	</Taxa>
</Classification>
					)
				)
			} else {
				Full(UnauthorizedResponse(""))
			}
		}
	}
	
	def findClassification(group: Model.Group, genus: String, species: String): Model.Classification = {
		if (null == group || null == genus || null == species) {
			return null
		}
		val publicSource = group.publicSource.obj.open_!
		val privateSource = group.privateSource.obj.open_!
		var classifications =
			Model.Classification.findAll(Like(Model.Classification.name, "%" + genus + " " + species + "%"))
				.filter(classification => {
					val source = classification.source.obj openOr null
					(source == publicSource || source == privateSource)
				})
		var classification: Model.Classification = null
		if (classifications.length > 0) {
			classification = classifications.first
		} else {
			val taxons = Model.Taxon.findAll(By(Model.Taxon.name, species), By (Model.Taxon.rank, "Species"))
			classifications = taxons.flatMap(taxon => {
				val taxonomyNodes = taxon.taxonomyNodes
					.filter(taxonomyNode => {taxonomyNode.leaf})
					.filter(taxonomyNode => {
						val parentTaxonomyNode = taxonomyNode.parent.obj openOr null
						if (null != parentTaxonomyNode) {
							val taxon = parentTaxonomyNode.taxon.obj openOr null
							(null != taxon && taxon.name.toUpperCase.equals(genus.toUpperCase) && taxon.rank.toUpperCase.equals("GENUS"))
						} else {
							false
						}
					})
				taxonomyNodes
					.map(taxonomyNode => taxonomyNode.classification.obj.open_!)
					.removeDuplicates
					.filter(classification => {
						val source = classification.source.obj openOr null
						(source == publicSource || source == privateSource)
					})
			})
			if (classifications.length > 0) {
				classification = classifications.first
			} else {
				val nameBankSearchURL: String           = "http://www.ubio.org/webservices/service.php?keyCode=272fd4d90d89c16b1dd32ae153d005af7a268018&function=namebank_search&sci=1&vern=0&searchName="
				val classificationBankSearchURL: String = "http://www.ubio.org/webservices/service.php?keyCode=272fd4d90d89c16b1dd32ae153d005af7a268018&function=classificationbank_search&namebankID="
				val classificationBankURL: String       = "http://www.ubio.org/webservices/service.php?keyCode=272fd4d90d89c16b1dd32ae153d005af7a268018&function=classificationbank_object&ancestryFlag=1&childrenFlag=1&hierarchiesID="
				
				def parseClassification(classificationBankID: String) {
					val searchURL: String = classificationBankURL + classificationBankID
					val response: Elem = XML.load(searchURL)
					
					if (0 < (response \\ "children").length) {
						val nodeSeq: NodeSeq = (response \\ "children" \ "value" \ "classificationBankID")
						if (nodeSeq.length > 0) {
							parseClassification(nodeSeq.first.text)
						}
					} else {
						var taxonValues: List[List[String]] = List()
						(response \\ "ancestry" \ "value").foreach(parent => {
							val taxonRank = (parent \ "rankName").text
							val taxonName = new String(new BASE64Decoder().decodeBuffer((parent \ "nameString").text), "UTF-8")
							taxonValues = List.concat(List(List(taxonName, taxonRank)), taxonValues)
						})
						val taxonRank = (response \ "rankName").text
						val taxonNameFull = new String(new BASE64Decoder().decodeBuffer((response \ "recordedName" \ "nameString").text), "UTF-8")
						val taxonNameParts = taxonNameFull.split(" ")
						val taxonName = 
							if (1 < taxonNameParts.length) {
								taxonNameParts(1)
							} else {
								taxonNameParts(0)
							}
						taxonValues = List.concat(taxonValues, List(List(taxonName, taxonRank)))
						classification = Model.Classification.create.source(privateSource)
						classification.name(taxonNameFull)
						classification.save
						var parent: Model.TaxonomyNode = null
						taxonValues.foreach(taxonValue => {
							val taxonName =
								if (taxonValue(0).length > 1) {
									taxonValue(0).substring(0, 1).toUpperCase() + taxonValue(0).substring(1).toLowerCase()
								} else if (taxonValue(0).length > 0) {
									taxonValue(0).substring(0, 1).toUpperCase()
								} else {
									"Unnamed"
								}
							val taxonRank =
								if (taxonValue(1).toUpperCase().equals("NO RANK")) {
									"Unranked"
								} else if (taxonValue(1).length > 1) {
									taxonValue(1).substring(0, 1).toUpperCase() + taxonValue(1).substring(1).toLowerCase()
								} else if (taxonValue(1).length > 0) {
									taxonValue(1).substring(0, 1).toUpperCase()
								} else {
									"Unranked"
								}
							var taxon: Model.Taxon = Model.Taxon.find(By(Model.Taxon.name, taxonName), By(Model.Taxon.rank, taxonRank)) openOr null
							if (null == taxon) {
								taxon = Model.Taxon.create
								taxon.name(taxonName)
								taxon.rank(taxonRank)
								taxon.save
							}
							val taxonomyNode: Model.TaxonomyNode = Model.TaxonomyNode.create
							taxonomyNode.taxon(taxon)
							taxonomyNode.classification(classification)
							if (null == parent) {
								taxonomyNode.root(true)
								taxonomyNode.save
							} else {
								taxonomyNode.root(false)
								taxonomyNode.leaf(false)
								taxonomyNode.parent(parent)
								taxonomyNode.save
								parent.child(taxonomyNode)
								parent.save
							}
							parent = taxonomyNode
						})
						if (null != parent) {
							parent.leaf(true)
							parent.save
						}
					}
				}
				
				var response: Elem = XML.load(nameBankSearchURL + genus + " " + species)
				var nodeSeq: NodeSeq = (response \\ "scientificNames" \ "value" \ "namebankID")
				if (nodeSeq.length > 0) {
					val nameBankID: String = nodeSeq.first.text
					response = XML.load(classificationBankSearchURL + nameBankID)
					nodeSeq = (response \\ "seniorNames" \ "value" \ "classificationBankID")
					if (nodeSeq.length > 0) {
						val classificationBankID: String = nodeSeq.first.text
						parseClassification(classificationBankID)
					}
				}
			}
		}
		classification
	}
}