package edu.unl.biofinity.site

import edu.unl.biofinity.api.model._

import java.io.ByteArrayInputStream

import javax.xml.transform.stream.StreamSource
import javax.xml.validation.Schema
import javax.xml.validation.SchemaFactory
import javax.xml.validation.Validator

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

import org.joda.time.format.ISODateTimeFormat

import org.xml.sax.SAXException

import scala.collection.mutable.Map
import scala.xml.pull._

class DataParser {
	var eventReader: XMLEventReader = null
	
	def parseXML(sourceXML: Array[Byte]/*sourceXML: scala.io.Source*/) {
		try {
			val schemaFactory: SchemaFactory = SchemaFactory.newInstance("http://www.w3.org/2001/XMLSchema")
			val schema: Schema = schemaFactory.newSchema(new StreamSource(S.hostAndPath + "/BiofinityDataSchema.xsd"));
			val validator: Validator = schema.newValidator()
			validator.validate(new StreamSource(new ByteArrayInputStream(sourceXML)));
		} catch {
			case exception: SAXException => {
				S.error("XML is not valid: " + exception.getMessage())
				return
			}
		}
		
		eventReader = new XMLEventReader(scala.io.Source.fromBytes(sourceXML, "UTF-8"))
		var endOfFile: Boolean = false
		var event: XMLEvent = null
		var continue: Boolean = true
		
		var systemMap_Sources: Map[String, Source] = Map()
		var systemMap_Classifications: Map[String, Long] = Map()
		var systemMap_Taxons: Map[String, Long] = Map()
		var systemMap_Events: Map[String, Long] = Map()
		var systemMap_Locations: Map[String, Long] = Map()
		var systemMap_Occurrences: Map[String, Long] = Map()
		
		val systemMap_Classification_to_TaxonomyNodeIDs: Map[String, List[Long]] = Map()
		val systemMap_Taxon_to_TaxonomyNodeIDs: Map[String, List[Long]] = Map()
		val systemMap_Location_to_EventIDs: Map[String, List[Long]] = Map()
		val systemMap_Classification_to_OccurrenceIDs: Map[String, List[Long]] = Map()
		val systemMap_Event_to_OccurrenceIDs: Map[String, List[Long]] = Map()
		
		while (!endOfFile /*Scala BUG: XMLEventReader.hasNext always returns true*/) {
			event = eventReader.next
			event match {
				case EvElemStart(_, "Source", attributes, _) => {
					val sourceID: String = getIDAttribute(attributes)
					var sourceName: String = null
					var sourceUniqueID: String = null
					var sourceURL: String = null
					
					continue = true
					while (eventReader.hasNext && continue) {
						event = eventReader.next
						event match {
							case EvElemEnd (_, "Source") => {
								continue = false
							}
							case EvElemStart(_, "Name", attributes, _) => {
								sourceName = getElementValue(event)
							}
							case EvElemStart(_, "UniqueID", attributes, _) => {
								sourceUniqueID = getElementValue(event)
							}
							case EvElemStart(_, "URL", attributes, _) => {
								sourceURL = getElementValue(event)
							}
							case _ => {
							}
						}
					}
					
					val source = Source.find(By(Source.uniqueID, sourceUniqueID))
					source match {
						case Empty => {
							throw new IllegalArgumentException
						}
						case _ => {
							source.get.name(sourceName)
							source.get.save
							systemMap_Sources += sourceID -> source.get
						}
					}
				}
				case EvElemStart(_, "Classification", attributes, _) => {
					val classification: Classification = Classification.create
					classification.save
					val classificationID: String = getIDAttribute(attributes)
					systemMap_Classifications += classificationID -> classification.entityID.is
					
					continue = true
					var parent: TaxonomyNode = null
					var end = true
					while (eventReader.hasNext && continue) {
						event = eventReader.next
						event match {
							case EvElemEnd(_, "Classification") => {
								continue = false
							}
							case EvElemStart(_, "SourceID", _, _) => {
								val source: Source = systemMap_Sources.get(getElementValue(event)).get
								classification.source(source)
							}
							case EvElemStart(_, "SourceItemID", _, _) => {
								classification.sourceItemID(getElementValue(event))
							}
							case EvElemStart(_, "Name", _, _) => {
								classification.name(getElementValue(event))
							}
							case EvElemStart(_, "Taxonomy", _, _) => {
							}
							case EvElemStart(_, "TaxonomyNode", attributes, _) => {
								val taxonomyNode: TaxonomyNode = TaxonomyNode.create
								
								if (null == parent) { // this is the top and has no parent
									taxonomyNode.root(true)
									taxonomyNode.leaf(false)
									taxonomyNode.save
								} else {
									taxonomyNode.root(false)
									taxonomyNode.leaf(false)
									taxonomyNode.parent(parent)
									taxonomyNode.save
									parent.child(taxonomyNode)
									parent.save
								}
								
								if (!systemMap_Classification_to_TaxonomyNodeIDs.contains(classificationID)) {
									systemMap_Classification_to_TaxonomyNodeIDs.put(classificationID, List(taxonomyNode.entityID))
								} else {
									systemMap_Classification_to_TaxonomyNodeIDs.update(classificationID, systemMap_Classification_to_TaxonomyNodeIDs.get(classificationID).get :+ taxonomyNode.entityID.is)
								}
								
								val taxonID: String = getAttribute(attributes, "TaxonID")
								
								if (!systemMap_Taxon_to_TaxonomyNodeIDs.contains(taxonID)) {
									systemMap_Taxon_to_TaxonomyNodeIDs.put(taxonID, List(taxonomyNode.entityID))
								} else {
									systemMap_Taxon_to_TaxonomyNodeIDs.update(taxonID, systemMap_Taxon_to_TaxonomyNodeIDs.get(taxonID).get :+ taxonomyNode.entityID.is)
								}
								
								parent = taxonomyNode
							}
							case EvElemEnd(_, "TaxonomyNode") => {
								if (end) {
									parent.leaf(true)
									parent.save
									end = false
								}
							}
							case _ => {
							}
						}
					}
					classification.save
				}
				case EvElemStart(_, "Taxon", attributes, _) => {
					val taxonID: String = getIDAttribute(attributes)
					
					var name: String = null
					var rank: String = null
					
					continue = true
					while (eventReader.hasNext && continue) {
						event = eventReader.next
						event match {
							case EvElemEnd(_, "Taxon") => {
								continue = false
							}
							case EvElemStart(_, "Name", _, _) => {
								name = getElementValue(event)
							}
							case EvElemStart(_, "Rank", _, _) => {
								rank = getElementValue(event)
							}
							case _ => {
							}
						}
					}
					
					val taxon: Taxon = Taxon.find(By(Taxon.rank, rank), By(Taxon.name, name)) match {
						case Full(taxon: Taxon) => {
							taxon
						}
						case _ => {
							val t = Taxon.create
							t.rank(rank)
							t.name(name)
							t.save
							t
						}
					}
					systemMap_Taxons += taxonID -> taxon.entityID.is
					
					taxon.save
				}
				case EvElemStart(_, "Event", attributes, _) => {
					val eventX: Event = Event.create
					eventX.save
					val eventID: String = getIDAttribute(attributes)
					systemMap_Events += eventID -> eventX.entityID.is
					
					continue = true
					while (eventReader.hasNext && continue) {
						event = eventReader.next
						event match {
							case EvElemEnd(_, "Event") => {
								continue = false
							}
							case EvElemStart(_, "SourceID", _, _) => {
								val source: Source = systemMap_Sources.get(getElementValue(event)).get
								eventX.source(source)
							}
							case EvElemStart(_, "SourceItemID", _, _) => {
								eventX.sourceItemID(getElementValue(event))
							}
							case EvElemStart(_, "VerbatimDate", _, _) => {
								eventX.verbatimDate(getElementValue(event))
							}
							case EvElemStart(_, "LocationID", _, _) => {
								val locationID: String = getElementValue(event)
								if (!systemMap_Location_to_EventIDs.contains(locationID)) {
									systemMap_Location_to_EventIDs.put(locationID, List(eventX.entityID))
								} else {
									systemMap_Location_to_EventIDs.update(locationID, systemMap_Location_to_EventIDs.get(locationID).get :+ eventX.entityID.is)
								}
							}
							case EvElemStart(_, "TimeEvent", _, _) => {
								while (eventReader.hasNext && continue) {
									event = eventReader.next
									event match {
										case EvElemEnd(_, "TimeEvent") => {
											continue = false
										}
										case EvElemStart(_, "Start", _, _) => {
											eventX.date(ISODateTimeFormat.dateTimeNoMillis().parseDateTime(getElementValue(event)).toDate)
										}
										case EvElemStart(_, "End", _, _) => {
											eventX.endDate(ISODateTimeFormat.dateTimeNoMillis().parseDateTime(getElementValue(event)).toDate)
										}
									}
								}
							}
							case _ => {
							}
						}
					}
					eventX.save
				}
				case EvElemStart(_, "Location", attributes, _) => {
					val location: Location = Location.create
					location.save
					val locationID: String = getIDAttribute(attributes)
					systemMap_Locations += locationID -> location.entityID.is
					
					continue = true
					while (eventReader.hasNext && continue) {
						event = eventReader.next
						event match {
							case EvElemEnd(_, "Location") => {
								continue = false
							}
							case EvElemStart(_, "SourceID", _, _) => {
								val source: Source = systemMap_Sources.get(getElementValue(event)).get
								location.source(source)
							}
							case EvElemStart(_, "SourceItemID", _, _) => {
								location.sourceItemID(getElementValue(event))
							}
							case EvElemStart(_, "VerbatimElevation", _, _) => {
								location.verbatimElevation(getElementValue(event))
							}
							case EvElemStart(_, "Latitude", _, _) => {
								location.latitude(getElementValue(event).toDouble)
							}
							case EvElemStart(_, "Longitude", _, _) => {
								location.longitude(getElementValue(event).toDouble)
							}
							case _ => {
							}
						}
					}
					location.save
				}
				case EvElemStart(_, "Occurrence", attributes, _) => {
					val occurrence: Occurrence = Occurrence.create
					occurrence.save
					val occurrenceID: String = getIDAttribute(attributes)
					systemMap_Occurrences += occurrenceID -> occurrence.entityID.is
					
					continue = true
					while (eventReader.hasNext && continue) {
						event = eventReader.next
						event match {
							case EvElemEnd(_, "Occurrence") => {
								continue = false
							}
							case EvElemStart(_, "SourceID", _, _) => {
								val source: Source = systemMap_Sources.get(getElementValue(event)).get
								occurrence.source(source)
							}
							case EvElemStart(_, "SourceItemID", _, _) => {
								occurrence.sourceItemID(getElementValue(event))
							}
							case EvElemStart(_, "ClassificationID", _, _) => {
								val classificationID: String = getElementValue(event)
								if (!systemMap_Classification_to_OccurrenceIDs.contains(classificationID)) {
									systemMap_Classification_to_OccurrenceIDs.put(classificationID, List(occurrence.entityID))
								} else {
									systemMap_Classification_to_OccurrenceIDs.update(classificationID, systemMap_Classification_to_OccurrenceIDs.get(classificationID).get :+ occurrence.entityID.is)
								}
							}
							case EvElemStart(_, "EventID", _, _) => {
								val eventID: String = getElementValue(event)
								if (!systemMap_Event_to_OccurrenceIDs.contains(eventID)) {
									systemMap_Event_to_OccurrenceIDs.put(eventID, List(occurrence.entityID))
								} else {
									systemMap_Event_to_OccurrenceIDs.update(eventID, systemMap_Event_to_OccurrenceIDs.get(eventID).get :+ occurrence.entityID.is)
								}
							}
							case EvElemStart(_, "LifeStage", _, _) => {
								occurrence.lifeStage(getElementValue(event))
							}
							case EvElemStart(_, "Sex", _, _) => {
								occurrence.sex(getElementValue(event))
							}
							case EvElemStart(_, "Preparations", _, _) => {
								occurrence.preparations(getElementValue(event))
							}
							case _ => {
							}
						}
					}
					occurrence.save
				}
				case EvElemEnd(_, "Data") => {
					endOfFile = true
				}
				case _ => {
				}
			}
		}
		
		systemMap_Classification_to_TaxonomyNodeIDs.foreach{case (classificationID, taxonomyNodeIDs) => {
			if (systemMap_Classifications.contains(classificationID)) {
				val classification: Classification = Classification.find(By(Classification.entityID, systemMap_Classifications.get(classificationID).get)).get
				taxonomyNodeIDs.foreach{taxonomyNodeID => {
					val taxonomyNode: TaxonomyNode = TaxonomyNode.find(By(TaxonomyNode.entityID, taxonomyNodeID)).get
					taxonomyNode.classification(classification)
					taxonomyNode.save
				}}
			}
		}}
		
		systemMap_Taxon_to_TaxonomyNodeIDs.foreach{case (taxonID, taxonomyNodeIDs) => {
			if (systemMap_Taxons.contains(taxonID)) {
				val taxon: Taxon = Taxon.find(By(Taxon.entityID, systemMap_Taxons.get(taxonID).get)).get
				taxonomyNodeIDs.foreach{taxonomyNodeID => {
					val taxonomyNode: TaxonomyNode = TaxonomyNode.find(By(TaxonomyNode.entityID, taxonomyNodeID)).get
					taxonomyNode.taxon(taxon)
					taxonomyNode.save
				}}
			}
		}}
		
		systemMap_Location_to_EventIDs.foreach{case (locationID, eventIDs) => {
			if (systemMap_Locations.contains(locationID)) {
				val location: Location = Location.find(By(Location.entityID, systemMap_Locations.get(locationID).get)).get
				eventIDs.foreach{eventID => {
					val eventX: Event = Event.find(By(Event.entityID, eventID)).get
					eventX.location(location)
					eventX.save
				}}
			}
		}}
		
		systemMap_Classification_to_OccurrenceIDs.foreach{case (classificationID, occurrenceIDs) => {
			if (systemMap_Classifications.contains(classificationID)) {
				val classification: Classification = Classification.find(By(Classification.entityID, systemMap_Classifications.get(classificationID).get)).get
				occurrenceIDs.foreach{occurrenceID => {
					val occurrence: Occurrence = Occurrence.find(By(Occurrence.entityID, occurrenceID)).get
					occurrence.classification(classification)
					occurrence.save
				}}
			}
		}}
		
		systemMap_Event_to_OccurrenceIDs.foreach{case (eventID, occurrenceIDs) => {
			if (systemMap_Events.contains(eventID)) {
				val eventX: Event = Event.find(By(Event.entityID, systemMap_Events.get(eventID).get)).get
				occurrenceIDs.foreach{occurrenceID => {
					val occurrence: Occurrence = Occurrence.find(By(Occurrence.entityID, occurrenceID)).get
					occurrence.event(eventX)
					occurrence.save
				}}
			}
		}}
	}
	
	def getElementValue(event: XMLEvent): String = {
		if (!event.isInstanceOf[EvElemStart]) {
			throw new IllegalArgumentException
		}
		val startElement: EvElemStart = event.asInstanceOf[EvElemStart]
		var returnValue: String = ""
		var continue: Boolean = true
		var currentEvent: XMLEvent = null
		while (eventReader.hasNext && continue) {
			currentEvent = eventReader.next
			currentEvent match {
				case EvElemEnd(_, startElement.label) => {
					continue = false
				}
				case EvText(text) => {
					returnValue += text
				}
				case _ => {
				}
			}
		}
		return returnValue
	}
	
	def getAttribute(attributes: scala.xml.MetaData, name: String): String = {
		val option = attributes.get(name)
		option match {
			case None => {
				throw new IllegalArgumentException
			}
			case _ => {
				return option.get.text
			}
		}
	}
	
	def getIDAttribute(attributes: scala.xml.MetaData): String = {
		getAttribute(attributes, "ID")
	}
}