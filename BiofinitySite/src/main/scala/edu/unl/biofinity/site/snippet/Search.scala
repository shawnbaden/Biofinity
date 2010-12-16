package edu.unl.biofinity.site.snippet

import edu.unl.biofinity.api.{controller => Controller}
import edu.unl.biofinity.api.{model => Model}
import edu.unl.biofinity.api.{service => Service}

import java.sql.PreparedStatement

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

class Search {
	object searchQuery extends RequestVar[String]("")
	
	def init: NodeSeq = {
		searchQuery(S.param("search-query") openOr "")
		Text("")
	}
	
	def renderAdvancedSearch(xhtml: NodeSeq): NodeSeq = {
		
		val advancedSearchTypes = "" :: "Occurrence" :: "Event" :: Nil
		val advancedSearchOccurrenceProperties = "" :: "Additional Property" ::  Nil
		val advancedSearchEventProperties = "" :: "Additional Property" :: Nil
		
		var advancedSearchType = ""
		var advancedSearchProperty = ""
		var advancedSearchPropertyQuery = searchQuery.is
		var advancedSearchAdditionalProperty = "Name"
		
		def setAdvancedSearchType(value: String): JsCmd = {
			advancedSearchType = value;
			advancedSearchProperty = ""
			
			val advancedSearchProperties = 
				if (advancedSearchType.equals("Occurrence")) {
					advancedSearchOccurrenceProperties.map(i => (i, i))
				} else if (advancedSearchType.equals("Event")) {
					advancedSearchEventProperties.map(i => (i, i))
				} else {
					List("" -> "")
				}
			
			def setAdvancedSearchProperty(value: String) = {
				advancedSearchProperty = value;
				
				if (advancedSearchProperty.equals("")) {
					SetHtml("advanced-search-additional-property", Text("")) &
					SetHtml("advanced-search-additional-property-custom", Text("")) &
					SetHtml("advanced-search-property-query", Text(""))
				} else if (advancedSearchProperty.equals("Additional Property")) {
					val separator = "------------"
					val customLabel = "Specify:"
					
					val additionalPropertyBundle =
						if (advancedSearchType.equals("Occurrence")) {
							Model.User.currentGroup.is.occurrenceAdditionalPropertyBundle.obj openOr null
						} else if (advancedSearchType.equals("Event")) {
							Model.User.currentGroup.is.eventAdditionalPropertyBundle.obj openOr null
						} else {
							null
						}
					
					def setAdvancedSearchAdditionalProperty(value: String): JsCmd = {
						if (value.equals("") || value.equals(separator)) {
							SetHtml("advanced-search-additional-property-custom", Text("")) &
							SetHtml("advanced-search-property-query", Text(""))
						} else if (value.equals(customLabel)) {
							SetHtml(
								"advanced-search-additional-property-custom",
								SHtml.ajaxText(advancedSearchAdditionalProperty, value => {advancedSearchAdditionalProperty = value; JsCmds.Noop})
							) &
							SetHtml(
								"advanced-search-property-query",
								SHtml.ajaxText(advancedSearchPropertyQuery, value => {advancedSearchPropertyQuery = value; JsCmds.Noop})
							)
						} else {
							advancedSearchAdditionalProperty = value
							val additionalProperty = additionalPropertyBundle.additionalProperties.find(_.name.is.equals(value)) getOrElse Model.AdditionalProperty
							if (Model.AdditionalPropertyType.String == additionalProperty.valueType.is) {
								SetHtml("advanced-search-additional-property-custom", Text("")) &
								SetHtml(
									"advanced-search-property-query",
									SHtml.ajaxText(advancedSearchPropertyQuery, value => {advancedSearchPropertyQuery = value; JsCmds.Noop})
								)
							} else if (Model.AdditionalPropertyType.Boolean == additionalProperty.valueType.is) {
								advancedSearchPropertyQuery = Model.AdditionalPropertyType.booleanAsValue(false)
								SetHtml("advanced-search-additional-property-custom", Text("")) &
								SetHtml(
									"advanced-search-property-query",
									SHtml.ajaxCheckbox(
										false,
										value => {advancedSearchPropertyQuery = Model.AdditionalPropertyType.booleanAsValue(value); JsCmds.Noop}
									)
								)
							} else {
								SetHtml("advanced-search-additional-property-custom", Text("")) &
								SetHtml(
									"advanced-search-property-query",
									Text("")
								)
							}
						}
					}
					
					if (null != additionalPropertyBundle && 0 < additionalPropertyBundle.additionalProperties.length) {
						SetHtml(
							"advanced-search-additional-property",
							SHtml.ajaxSelect(
								List("" -> "") ++ additionalPropertyBundle.additionalProperties.map(i => (i.name.is, i.name.is)) ++ List(separator -> separator, customLabel -> customLabel),
								Full(""),
								value => {setAdvancedSearchAdditionalProperty(value)}
							)
						) &
						SetHtml("advanced-search-additional-property-custom", Text("")) &
						SetHtml("advanced-search-property-query", Text(""))
					} else {
						SetHtml(
							"advanced-search-additional-property",
							SHtml.ajaxSelect(
								List(customLabel -> customLabel),
								Full(""),
								value => JsCmds.Noop
							)
						) &
						SetHtml(
							"advanced-search-additional-property-custom",
							SHtml.ajaxText(advancedSearchAdditionalProperty, value => {advancedSearchAdditionalProperty = value; JsCmds.Noop})
						) &
						SetHtml(
							"advanced-search-property-query",
							SHtml.ajaxText(advancedSearchPropertyQuery, value => {advancedSearchPropertyQuery = value; JsCmds.Noop})
						)
					}
				} else {
					SetHtml("advanced-search-additional-property", Text("")) &
					SetHtml("advanced-search-additional-property-custom", Text("")) &
					SetHtml(
						"advanced-search-property-query",
						SHtml.ajaxText(advancedSearchPropertyQuery, value => {advancedSearchPropertyQuery = value; JsCmds.Noop})
					)
				}
			}
			
			SetHtml(
				"advanced-search-property",
				if (advancedSearchType.equals("")) {
					Text("")
				} else {
					SHtml.ajaxSelect(advancedSearchProperties, Full(""), value => {setAdvancedSearchProperty(value)})
				}
			) &
			SetHtml("advanced-search-additional-property", Text("")) &
			SetHtml("advanced-search-additional-property-custom", Text("")) &
			SetHtml("advanced-search-property-query", Text(""))

		}
		
		def execute(): JsCmd = {
			if (advancedSearchProperty.equals("Additional Property")) {
				val baseSQL =
					if (advancedSearchType.equals("Occurrence")) {
						"SELECT o.entity_id FROM occurrences o INNER JOIN events e ON o.event_id = e.entity_id INNER JOIN additional_properties ap ON o.additional_property_bundle_id = ap.additional_property_bundle_id INNER JOIN sources s ON o.source_id = s.entity_id"
					} else {
						"SELECT o.entity_id FROM occurrences o INNER JOIN events e ON o.event_id = e.entity_id INNER JOIN additional_properties ap ON e.additional_property_bundle_id = ap.additional_property_bundle_id INNER JOIN sources s ON o.source_id = s.entity_id"
					}
				val occurrences = Model.Occurrence.findAllByPreparedStatement({ db =>
					if (!Model.User.groupSignedIn_?) {
						val preparedStatement: PreparedStatement = db.connection.prepareStatement(baseSQL + " where ap.name = ? and ap.value = ? and s.source_type = 0")
						preparedStatement.setString(1, advancedSearchAdditionalProperty)
						preparedStatement.setString(2, advancedSearchPropertyQuery)
						preparedStatement
					} else {
						val preparedStatement: PreparedStatement = db.connection.prepareStatement(baseSQL + " where ap.name = ? and ap.value = ? and (o.source_id = ? or o.source_id = ?)")
						preparedStatement.setString(1, advancedSearchAdditionalProperty)
						preparedStatement.setString(2, advancedSearchPropertyQuery)
						preparedStatement.setLong(3, (Model.User.currentGroup.is.publicSource.obj openOr Model.Source).entityID)
						preparedStatement.setLong(4, (Model.User.currentGroup.is.privateSource.obj openOr Model.Source).entityID)
						
						preparedStatement
					}
				}).removeDuplicates
				Model.SimpleOccurrence.currentSimpleOccurrences(Controller.SimpleOccurrence.search(occurrences.map(_.entityID.is)))
			}
			
			SetHtml("results", <lift:embed what="/search-results"/>)
		}
		
		bind(
			"advanced-search",
			xhtml,
			"AdvancedSearchType" -> SHtml.ajaxSelect(advancedSearchTypes.map(i => (i, i)), Full(""), value => {setAdvancedSearchType(value)}),
			"ExecuteLink" -> {(nodeSeq: NodeSeq) => SHtml.ajaxButton(nodeSeq, () => execute)}
		)
	}
	
	def renderSearch(xhtml: NodeSeq): NodeSeq = {
		var classifications: List[Model.Classification] = null
		var taxons: List[Model.Taxon] = null
		
		def initClassifications() = {
			if (null == classifications) {
				initTaxons
				classifications = (
					Model.Classification.findAll(Like(Model.Classification.name, "%" + searchQuery.is + "%")) :::
					taxons.flatMap(taxon => taxon.taxonomyNodes.filter(taxonomyNode => taxonomyNode.leaf).map(taxonomyNode => taxonomyNode.classification.obj.open_!))
				).removeDuplicates.filter(classification => Source.sourceFilter(classification.source.obj.openOr(null)))
			}
		}
		
		def initTaxons() = {
			if (null == taxons) {
				taxons = Model.Taxon.findAll(By(Model.Taxon.name, searchQuery))
			}
		}
		
		bind(
			"search",
			xhtml,
			"ExecuteLink" -> {(nodeSeq: NodeSeq) => SHtml.submit(nodeSeq.toString, () => (), ("class","button"))},
			"Query" -> SHtml.text(searchQuery, searchQuery(_), "name" -> "search-query"),
			"QueryText" -> searchQuery.is,
			"Results" -> {(nodeSeq: NodeSeq) => {
				if (null != searchQuery.is && searchQuery.is.length > 2) {
					bind(
						"results",
						nodeSeq,
						"AlertText" -> Text(""),
						"Classifications" -> {(nodeSeq: NodeSeq) => {
							initClassifications
							if (null != classifications && classifications.length > 0) {
								bind(
									"classifications",
									nodeSeq,
									"List" -> {(listNodeSeq: NodeSeq) => {
										val classificationsNodeSeq: NodeSeq = classifications.flatMap(classification => {
											Model.Classification.currentClassification(classification)
											new Classification().renderClassification(listNodeSeq)
										})
										Model.Classification.currentClassification(null)
										classificationsNodeSeq
									}}
								)
							} else {
								Text("")
							}
						}},
						"Occurrences" -> {(nodeSeq: NodeSeq) => {
							initClassifications
							val occurrences: List[Model.Occurrence] =
								(classifications
									.flatMap(classification => classification.occurrences.map(occurrence => occurrence))
									.filter(occurrence => Source.sourceFilter(occurrence.source.obj.openOr(null))) :::
								Model.Occurrence.findAll(Like(Model.Occurrence.details, "%" + searchQuery + "%")).filter(occurrence => Source.sourceFilter(occurrence.source.obj.openOr(null))))
								.removeDuplicates
							if (null != occurrences && occurrences.length > 0) {
								bind(
									"occurrences",
									nodeSeq,
									"DownloadCSVLink" -> {(nodeSeq: NodeSeq) => {
										SHtml.link("/service/occurrence/occurrencesCSV", () => Model.Occurrence.currentOccurrenceIDs(occurrences.map(_.entityID.is)), nodeSeq)
									}},
									"DownloadKMLLink" -> {(nodeSeq: NodeSeq) => {
										SHtml.link("/service/occurrence/occurrencesKML", () => Model.Occurrence.currentOccurrenceIDs(occurrences.map(_.entityID.is)), nodeSeq)
									}},
									"List" -> {(listNodeSeq: NodeSeq) => {
										val occurrencesNodeSeq: NodeSeq = occurrences.flatMap(occurrence => {
											Model.Occurrence.currentOccurrence(occurrence)
											new Occurrence().renderOccurrence(listNodeSeq)
										})
										Model.Occurrence.currentOccurrence(null)
										occurrencesNodeSeq
									}},
									"MapLink" -> {(nodeSeq: NodeSeq) => {
										SHtml.link("map", () => Controller.Occurrence.occurrenceIDs(occurrences.map(_.entityID.is)), nodeSeq)
									}}
								)
							} else {
								Text("")
							}
						}},
						"Taxons" -> {(nodeSeq: NodeSeq) => {
							initTaxons
							if (null != taxons && taxons.length > 0) {
								bind(
									"taxons",
									nodeSeq,
									"List" -> {(listNodeSeq: NodeSeq) => {
										val taxonsNodeSeq: NodeSeq = taxons.flatMap(taxon => {
											Model.Taxon.currentTaxon(taxon)
											new Taxon().renderTaxon(listNodeSeq)
										})
										Model.Taxon.currentTaxon(null)
										taxonsNodeSeq
									}}
								)
							} else {
								Text("")
							}
						}}
					)
				} else {
					bind(
						"results",
						nodeSeq,
						"AlertText" -> {(nodeSeq: NodeSeq) => nodeSeq},
						"Classifications" -> Text(""),
						"Occurrences" -> Text(""),
						"Taxons" -> Text("")
					)
				}
			}}
		)
	}
	
	def renderOccurrencesJSON: NodeSeq = {
		if (null != Controller.Occurrence.occurrenceIDs.is && 0 < Controller.Occurrence.occurrenceIDs.is.length) {
			Log.info(Controller.Occurrence.occurrenceIDs.is.length.toString)
			<script type="text/javascript" src={S.hostAndPath + "/service/search/occurrencesJSON"}></script>
		} else {
			Text("")
		}
	}
}