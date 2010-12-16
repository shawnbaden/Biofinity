package edu.unl.biofinity.toolbox.data

import java.io.FileOutputStream
import java.io.OutputStreamWriter

import scala.collection.mutable.Map
import scala.xml._

import sun.misc.BASE64Decoder

object uBioDataImport {
	val outputFilePath: String = "C:\\Java\\eclipse_Workspaces\\Biofinity\\BiofinitySite\\src\\main\\webapp\\uBio.xml"
	var outputStreamWriter: OutputStreamWriter = null
	
	var taxons: Map[String, String] = Map()
	
	val classificationURL: String = "http://www.ubio.org/webservices/service.php?keyCode=272fd4d90d89c16b1dd32ae153d005af7a268018&function=classificationbank_object&childrenFlag=1&hierarchiesID="
	val taxonURL: String = "http://www.ubio.org/webservices/service.php?keyCode=272fd4d90d89c16b1dd32ae153d005af7a268018&function=namebank_object&namebankID="
	val classificationRootNodeMap_GBIF: List[String] = List(
		"11522135" /*Animalia*/,
		"11518862" /*Archaea*/,
		"11518861" /*Bacteria*/,
		"11524578" /*Chromista*/,
		"11520399" /*Fungi*/,
		"11519161" /*Plantae*/,
		"11521205" /*Protozoa*/)
	
	def main(args:Array[String]) {
		val fileOutputStream: FileOutputStream = new FileOutputStream(outputFilePath)
		outputStreamWriter = new OutputStreamWriter(fileOutputStream, "UTF8")
		
		outputStreamWriter.write("<?xml version=\"1.0\" encoding=\"utf-8\"?>")
		outputStreamWriter.write("<biofinity:Data xmlns:biofinity=\"edu.unl.biofinity\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"edu.unl.biofinity BiofinityDataSchema.xsd\">")
		writeNode(<Source ID="1"><Name>uBio</Name><UniqueID>UBIO</UniqueID></Source>)
		classificationRootNodeMap_GBIF.foreach(classificationNodeID => parseClassificationNode(List(), classificationNodeID))
		taxons.keySet.foreach(taxonID => {
			writeTaxon(taxonID)
		})
		outputStreamWriter.write("</biofinity:Data>")
		
		outputStreamWriter.flush()
		outputStreamWriter.close()
	}
	
	def writeNode(node: Node) {
		XML.write(outputStreamWriter, node, "UTF8", false, null)
	}
	
	def parseClassificationNode(parentNodeIDs: List[String], classificationNodeID: String) {
		val searchURL: String = classificationURL + classificationNodeID
		val response: Elem = XML.load(searchURL)
		val taxonID: String = (response \ "recordedName" \ "namebankID").text
		val taxonName: String = new String(new BASE64Decoder().decodeBuffer((response \ "recordedName" \ "nameString").text), "UTF-8")
		if (!taxons.contains(taxonID)) {
			taxons += taxonID -> taxonName
		}
		if (0 < (response \\ "children").length) {
			(response \\ "children" \ "value" \ "classificationBankID").foreach(node => {
				parseClassificationNode(List.concat(parentNodeIDs, List(taxonID)), node.text)
			})
		} else {
			writeNode(
				<Classification ID={classificationNodeID}><SourceID>1</SourceID><SourceItemID>{classificationNodeID}</SourceItemID><Taxonomy>
					{XML.loadString(parentNodeIDs.foldLeft("")(_ + "<TaxonomyNode TaxonID=\"" + _ + "\">") + "<TaxonomyNode TaxonID=\"" + taxonID + "\"></TaxonomyNode>" + parentNodeIDs.flatMap(parentNodeID => "</TaxonomyNode>").mkString)}
				</Taxonomy></Classification>
			)
		}
	}
	
	def writeTaxon(taxonID: String) {
		println(taxonID)
		val searchURL: String = taxonURL + taxonID
		val response: Elem = XML.load(searchURL)
		val name: String = taxons.get(taxonID).get
		val rank: String = (response \ "rankName").text
		
		writeNode(<Taxon ID={taxonID} Extinct="false"><SourceID>1</SourceID><SourceItemID>{taxonID}</SourceItemID><Name>{name}</Name><Rank>{rank}</Rank></Taxon>)
	}
}