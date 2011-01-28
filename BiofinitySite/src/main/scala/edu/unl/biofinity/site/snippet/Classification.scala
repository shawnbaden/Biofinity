package edu.unl.biofinity.site.snippet

import edu.unl.biofinity.api.{controller => Controller}
import edu.unl.biofinity.api.{model => Model}

import java.sql.PreparedStatement
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

import scala.actors.Actor
import scala.xml._

import sun.misc.BASE64Decoder

class Classification {
	object taxonsMap extends RequestVar[scala.collection.mutable.Map[Long, List[Model.Taxon]]](scala.collection.mutable.Map())
	
	def init: NodeSeq = {
		val classification = Model.Classification.find(S.param("ID") openOr -1) openOr null
		if (null != classification) {
			val source: Model.Source = classification.source.obj openOr null
			if (!S.attr("group").isEmpty) {
				if (source == Model.User.currentGroup.privateSource.obj.openOr(null) || source == Model.User.currentGroup.publicSource.obj.openOr(null)) {
					Model.Classification.currentClassification(classification)
				}
			} else {
				if (source.public_?) {
					Model.Classification.currentClassification(classification)
				}
			}
		}
		Text("")
	}
 	
	def renderClassification(xhtml: NodeSeq) = {
		if (null == Model.Classification.currentClassification.is) {
			Text("")
		} else {
			def initSource() = {
				Model.Source.currentSource(Model.Classification.currentClassification.is.source.obj openOr null)
			}
			
			if (!S.attr("group").isEmpty && !S.attr("update").isEmpty && Model.Classification.currentClassification.is.canUpdate_?) {
				if (!taxonsMap.is.contains(Model.Classification.currentClassification.is.entityID)) {
					taxonsMap.is.put(Model.Classification.currentClassification.is.entityID, Model.Classification.currentClassification.is.taxons)
				}
				
				val taxons: List[Model.Taxon] = taxonsMap.is.get(Model.Classification.currentClassification.is.entityID).get
				
				def save(): JsCmd = {
					val name = Model.Classification.currentClassification.is.name.is.trim.replaceAll(" +", " ")
					if (name.equals("")) {
						Alert("The classification's name cannot be empty.")
					} else if (taxons.length < 1) {
						Alert("This classification contains no taxa.  A classification must contain at least one taxon.")
					} else {
						if (!Model.Classification.currentClassification.is.name.is.equals(name)) {
							Model.Classification.currentClassification.is.name(name)
						}
						saveClassification
						
						SetHtml("classification-details", <lift:embed what="/lab/classification-details"/>)
					}
				}
				
				SHtml.ajaxForm(
					bind(
						"classification",
						xhtml,
						"CancelLink" -> {(nodeSeq: NodeSeq) => <a href="javascript: cancelClassificationDetailsUpdate();">{nodeSeq}</a>},
						"DeleteLink" -> Text(""),
						"EditLink" -> Text(""),
						"ID" -> Model.Classification.currentClassification.is.entityID,
						"IDLink" -> Text(""),
						"Name" -> SHtml.ajaxText(Model.Classification.currentClassification.is.name, value => {Model.Classification.currentClassification.is.name(value); JsCmds.Noop}),
						"NameLink" -> Text(""),
						"SaveLink" -> {(nodeSeq: NodeSeq) => SHtml.ajaxButton(nodeSeq, () => save)},
						"SelectLink" -> Text(""),
						"Source" -> {(nodeSeq: NodeSeq) => {
							initSource
							new Source().renderSource(nodeSeq)
						}},
						"Taxons" -> {(nodeSeq: NodeSeq) => {
							val embedUpdateID = S.attr("embedUpdateID") openOr null
							val embedUpdateURL = S.attr("embedUpdateURL") openOr null
							
							bind(
								"taxons",
								nodeSeq,
								"Create" -> {(nodeSeq: NodeSeq) => {
									var taxonName = ""
									var taxonRank = ""

									def save(): JsCmd = {
										if (taxonName.length < 1 || taxonRank.length < 1) {
											Alert("A taxon's name and rank cannot be empty.")
										} else {
											var taxon: Model.Taxon = Model.Taxon.find(By(Model.Taxon.name, taxonName), By(Model.Taxon.rank, taxonRank)) openOr null
											if (null == taxon) {
												taxon = Model.Taxon.create
												taxon.name(taxonName)
												taxon.rank(taxonRank)
												taxon.save
											}
											taxonsMap.is.update(Model.Classification.currentClassification.is.entityID, taxons :+ taxon)
											
											if (null != embedUpdateID && null != embedUpdateURL) {
												SetHtml({embedUpdateID}, <lift:embed what={embedUpdateURL}/>) &
												Call("renderTaxonAutoComplete")
											} else {
												SetHtml("classification-details", <lift:embed what="/lab/classification-details-update"/>) &
												Call("renderTaxonAutoComplete")
											}
										}
									}
									
									bind(
										"taxon",
										nodeSeq,
										"Name" -> SHtml.ajaxText(
											"",
											value => {
												if (value.length > 1) {
													taxonName = value.substring(0, 1).toUpperCase() + value.substring(1).toLowerCase()
												} else if (value.length > 0) {
													taxonName = value.substring(0, 1).toUpperCase()
												} else {
													taxonName = value
												}
												Noop
											},
											"name" -> "classification-name"
										),
										"Rank" -> SHtml.ajaxText(
											"",
											value => {
												if (value.length > 1) {
													taxonRank = value.substring(0, 1).toUpperCase() + value.substring(1).toLowerCase()
												} else if (value.length > 0) {
													taxonRank = value.substring(0, 1).toUpperCase()
												} else {
													taxonRank = value
												}
												Noop
											},
											"name" -> "classification-rank"
										),
										"SaveLink" -> {(nodeSeq: NodeSeq) => SHtml.ajaxButton(nodeSeq, () => save)}
									)
								}},
								"EmptyText" -> {(nodeSeq: NodeSeq) => {
									if (taxons.length > 0) {
										Text("")
									} else {
										nodeSeq
									}
								}},
								"List" -> {(nodeSeq: NodeSeq) => {
									val taxonsNodeSeq: NodeSeq = taxons.flatMap(taxon => {
										def delete(): JsCmd = {
											taxonsMap.is.update(Model.Classification.currentClassification.is.entityID, taxons.remove(_.entityID == taxon.entityID))
											
											if (null != embedUpdateID && null != embedUpdateURL) {
												SetHtml({embedUpdateID}, <lift:embed what={embedUpdateURL}/>) &
												Call("renderTaxonAutoComplete")
											} else {
												SetHtml("classification-details", <lift:embed what="/lab/classification-details-update"/>) &
												Call("renderTaxonAutoComplete")
											}
										}
										
										bind(
											"taxon",
											nodeSeq,
											"DeleteLink" -> {(nodeSeq: NodeSeq) => SHtml.ajaxButton(nodeSeq, () => delete)},
											"Name" -> taxon.name,
											"Rank" -> taxon.rank
										)
									})
									taxonsNodeSeq
								}}
							)
						}},
						"ViewLink" -> Text(""),
						"WikiPageIDLink" -> Text("")
					) ++ Script(Call("renderTaxonAutoComplete"))
				)
			} else {
				bind(
					"classification",
					xhtml,
					"CancelLink" -> Text(""),
					"DeleteLink" -> {(nodeSeq: NodeSeq) => {
						if (!S.attr("group").isEmpty && Model.Classification.currentClassification.canUpdate_?) {
							<a href="javascript: showDeleteClassificationDialog();">{nodeSeq}</a>
						} else {
							Text("")
						}
					}},
					"EditLink" -> {(nodeSeq: NodeSeq) => {
						if (!S.attr("group").isEmpty && Model.Classification.currentClassification.canUpdate_?) {
							<a href="javascript: showClassificationDetailsUpdate();">{nodeSeq}</a>
						} else {
							Text("")
						}
					}},
					"ID" -> Model.Classification.currentClassification.get.entityID,
					"IDLink" -> <a href={"classification?ID=" + Model.Classification.currentClassification.is.entityID}>{Model.Classification.currentClassification.is.entityID}</a>,
					"Name" -> Model.Classification.currentClassification.is.name,
					"NameLink" -> <a href={"classification?ID=" + Model.Classification.currentClassification.is.entityID}>{Model.Classification.currentClassification.is.name}</a>,
					"SaveLink" -> Text(""),
					"SelectLink" -> {(nodeSeq: NodeSeq) => {
						<button onclick={"javascript: selectClassification(" + Model.Classification.currentClassification.is.entityID + ");"}>{nodeSeq}</button>
					}},
					"Source" -> {(nodeSeq: NodeSeq) => {
						initSource
						new Source().renderSource(nodeSeq)
					}},
					"Taxons" -> {(nodeSeq: NodeSeq) => {
						val taxons = Model.Classification.currentClassification.is.taxons
						bind(
							"taxons",
							nodeSeq,
							"Create" -> Text(""),
							"EmptyText" -> {(nodeSeq: NodeSeq) => {
								if (taxons.length > 0) {
									Text("")
								} else {
									nodeSeq
								}
							}},
							"List" -> {(nodeSeq: NodeSeq) => {
								val taxonsNodeSeq: NodeSeq = taxons.flatMap(taxon => {
									bind(
										"taxon",
										nodeSeq,
										"NameLink" -> <a href={"taxon?ID=" + taxon.entityID}>{taxon.name}</a>,
										"Rank" -> taxon.rank
									)
								})
								taxonsNodeSeq
							}}
						)
					}},
					"ViewLink" -> {(nodeSeq: NodeSeq) => {
						<a href={"classification?ID=" + Model.Classification.currentClassification.is.entityID}>{nodeSeq}</a>
					}},
					"WikiPageIDLink" -> {(nodeSeq: NodeSeq) => {
						val wikiPageID = Model.Classification.currentClassification.is.wikiPageID.is
						if (0 < wikiPageID) {
							<a href={"/wiki?WikiPageID=" + wikiPageID}>{wikiPageID}</a>
						} else {
							if (!S.attr("group").isEmpty) {
								def create() = {
									val newWikiPageID = Controller.WikiPage.create("classification", Model.Classification.currentClassification.is.entityID, Model.Classification.currentClassification.is.source.obj.open_!.entityID)
									Model.Classification.currentClassification.is.wikiPageID(newWikiPageID)
									Model.Classification.currentClassification.is.save
									JsCmds.Run("location.reload(true)")
								}
								
								bind(
									"wiki-page",
									nodeSeq,
									"CreateLink" -> {(nodeSeq: NodeSeq) => {SHtml.a(() => create, nodeSeq)}}
								)
							} else {
								Text("")
							}
						}
					}}
				)
			}
		}
	}
	
	def renderScripts: NodeSeq = {
		def showClassificationDetails() = {
			SetHtml("classification-details", <lift:embed what="lab/classification-details" />)
		}
		
		def showClassificationDetailsUpdate() = {
			SetHtml("classification-details", <lift:embed what="lab/classification-details-update" />) &
			Call("renderTaxonAutoComplete")
		}
		
		def cancelClassificationDetailsUpdate() = {
			Model.Classification.currentClassification(Model.Classification.find(Model.Classification.currentClassification.is.entityID) openOr null)
			taxonsMap.is.removeKey(Model.Classification.currentClassification.is.entityID)
			
			SetHtml("classification-details", <lift:embed what="lab/classification-details" />)
		}
		
		def deleteClassification() = {
			Model.Classification.currentClassification.is.deleteFull_!
			
			Alert("The classification has been deleted.") & JsCmds.RedirectTo("/lab")
		}
		
		Script(
			Function(
				"showClassificationDetails",
				Nil,
				SHtml.ajaxInvoke(showClassificationDetails)._2
			)
		) ++
		Script(
			Function(
				"showClassificationDetailsUpdate",
				Nil,
				SHtml.ajaxInvoke(showClassificationDetailsUpdate)._2
			)
		) ++
		Script(
			Function(
				"cancelClassificationDetailsUpdate",
				Nil,
				SHtml.ajaxInvoke(cancelClassificationDetailsUpdate)._2
			)
		) ++
		Script(
			Function(
				"showDeleteClassificationDialog",
				Nil,
				JsRaw(
					"if (confirm(\"Are you sure you want to delete this classification?  This action cannot be undone.\")) {" +
						"deleteClassification();" +
					"}"
				)
			)
		) ++
		Script(
			Function(
				"deleteClassification",
				Nil,
				SHtml.ajaxInvoke(deleteClassification)._2
			)
		) ++
		Script(
			Function(
				"renderTaxonAutoComplete",
				Nil,
				JsRaw(
					"$(\"input[name=classification-rank]\").autocomplete({" +
						"source: function(request, response) {" +
							"var data = [];" +
							"$.ajax({" +
								"dataType: \"xml\"," +
								"type: \"GET\"," +
								"url: \"" + S.hostAndPath + "/service/taxon/ranks?Query=\" + request.term," +
								"success: function(ranksResponse) {" +
									"$(ranksResponse).find(\"Rank\").each(function() {" +
										"data.push($(this).text());" +
									"});" +
									"response(data);" +
								"}" +
							"});" +
						"}" +
					"});" +
					"$(\"input[name=classification-name]\").autocomplete({" +
						"source: function(request, response) {" +
							"var data = [];" +
							"$.ajax({" +
								"dataType: \"xml\"," +
								"type: \"GET\"," +
								"url: \"" + S.hostAndPath + "/service/taxon/names?Query=\" + request.term + \"&Rank=\" + $(\"input[name=classification-rank]\").val()," +
								"success: function(namesResponse) {" +
									"$(namesResponse).find(\"Name\").each(function() {" +
										"data.push($(this).text());" +
									"});" +
									"response(data);" +
								"}" +
							"});" +
						"}" +
					"});"
				)
			)
		)
	}
	
	private def saveClassification() = {
		val taxons: List[Model.Taxon] = taxonsMap.is.get(Model.Classification.currentClassification.is.entityID).get
		Model.Classification.currentClassification.save
		Model.Classification.currentClassification.is.taxonomyNodes.foreach(_.delete_!)
		var parent: Model.TaxonomyNode = null
		taxons.foreach(taxon => {
			if (-1 == taxon.entityID.is) {
				taxon.save
			}
			val taxonomyNode: Model.TaxonomyNode = Model.TaxonomyNode.create
			taxonomyNode.taxon(taxon)
			taxonomyNode.classification(Model.Classification.currentClassification.is)
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
	
	def renderIfNull(xhtml: NodeSeq): NodeSeq = {
		if (null == Model.Classification.currentClassification.is) {
			xhtml
		} else {
			Text("")
		}
	}
	
	var createClassificationImportSearchClassifications: scala.collection.mutable.Map[String, List[List[String]]] = scala.collection.mutable.Map()
	var createClassificationImportSearchComplete: Boolean = false
	var createClassificationImportSearchQuery: String = ""
	
	def renderCreateClassification(xhtml: NodeSeq): NodeSeq = {
		if (null == Model.Classification.currentClassification.is) {
			Model.Classification.currentClassification(Model.Classification.create.source(Model.User.currentGroup.privateSource))
		}
		
		def next(): JsCmd = {
			JsCmds.Run("showCreateClassification()")
		}
		
		def previous(): JsCmd = {
			JsCmds.Run("showCreateClassificationStart()") 
		}
		
		val copySearchQuery = new Search().searchQuery
		
		def executeCopySearch(): JsCmd = {
			SetHtml("create-classification-copy-search-results", <lift:embed what="lab/create-classification-copy-search-results" />)
		}
		
		def executeImportSearch(): JsCmd = {
			createClassificationImportSearchClassifications = scala.collection.mutable.Map()
			createClassificationImportSearchComplete = false
			
			object ImportSearch extends Actor {
				def act() = {
					val nameBankSearchURL: String           = "http://www.ubio.org/webservices/service.php?keyCode=272fd4d90d89c16b1dd32ae153d005af7a268018&function=namebank_search&sci=1&vern=0&searchName="
					val classificationBankSearchURL: String = "http://www.ubio.org/webservices/service.php?keyCode=272fd4d90d89c16b1dd32ae153d005af7a268018&function=classificationbank_search&namebankID="
					val classificationBankURL: String       = "http://www.ubio.org/webservices/service.php?keyCode=272fd4d90d89c16b1dd32ae153d005af7a268018&function=classificationbank_object&ancestryFlag=1&childrenFlag=1&hierarchiesID="
					
					def parseClassification(classificationBankID: String) {
						val searchURL: String = classificationBankURL + classificationBankID
						val response: Elem = XML.load(searchURL)
						
						if (0 < (response \\ "children").length) {
							(response \\ "children" \ "value" \ "classificationBankID").foreach(classificationBankID => {
								parseClassification(classificationBankID.text)
							})
						} else {
							var taxons: List[List[String]] = List()
							(response \\ "ancestry" \ "value").foreach(parent => {
								val taxonRank = (parent \ "rankName").text
								val taxonName = new String(new BASE64Decoder().decodeBuffer((parent \ "nameString").text), "UTF-8")
								taxons = List.concat(List(List(taxonName, taxonRank)), taxons)
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
							taxons = List.concat(taxons, List(List(taxonName, taxonRank)))
							val classificationName = taxonNameFull
							createClassificationImportSearchClassifications += classificationName -> taxons
						}
					}
					
					if (createClassificationImportSearchQuery.length > 6/*EACH PART OF BINOMIAL NAME MUST BE 3 CHARS*/) {
						val response: Elem = XML.load(nameBankSearchURL + createClassificationImportSearchQuery)
						val nameBankIDs: List[String] = (response \\ "scientificNames" \ "value" \ "namebankID").map(nameBankID => nameBankID.text).toList
						var classificationBankIDs: List[String] = List()
						nameBankIDs.foreach(nameBankID => {
							val response: Elem = XML.load(classificationBankSearchURL + nameBankID)
							val currentClassificationBankID: List[String] = (response \\ "seniorNames" \ "value" \ "classificationBankID").map(classificationBankID => classificationBankID.text).toList
							classificationBankIDs = List.concat(classificationBankIDs, currentClassificationBankID)
						})
						classificationBankIDs.removeDuplicates
						classificationBankIDs.foreach(classificationBankID => parseClassification(classificationBankID))
					}
					
					createClassificationImportSearchComplete = true
				}
			}
			ImportSearch.start

			val queryParts = createClassificationImportSearchQuery.split(" ")
			if (createClassificationImportSearchQuery.equals("")) {
				Alert("The search query cannot be empty.")
			} else if (2 > queryParts.length) {
				Alert("The search query must be a binomial name at a minimum.")
			} else {
				SetHtml("create-classification-import-search-results", <lift:embed what="/lab/create-classification-import-search-results"/>) &
				JsCmds.Run("setTimeout('checkCreateClassificationImportSearch()', 1000)")
			}
		}
		
		def save(): JsCmd = {
			val taxons: List[Model.Taxon] = taxonsMap.is.get(Model.Classification.currentClassification.is.entityID).get
			val name = Model.Classification.currentClassification.is.name.is.trim.replaceAll(" +", " ")
			if (name.equals("")) {
				Alert("The classification's name cannot be empty.")
			} else if (taxons.length < 1) {
				Alert("This classification contains no taxa.  A classification must contain at least one taxon.")
			} else {
				saveClassification
				
				JsCmds.RedirectTo("/lab/classification?ID=" + Model.Classification.currentClassification.is.entityID)
			}
		}
		
		bind(
			"create-classification",
			xhtml,
			"CopySearchExecuteLink" -> {(nodeSeq: NodeSeq) => SHtml.ajaxButton(nodeSeq, () => executeCopySearch)},
			"CopySearchQuery" -> SHtml.ajaxText(copySearchQuery, value => {copySearchQuery(value); JsCmds.Noop}),
			"ImportSearchExecuteLink" -> {(nodeSeq: NodeSeq) => SHtml.ajaxButton(nodeSeq, () => executeImportSearch)},
			"ImportSearchQuery" -> SHtml.ajaxText(createClassificationImportSearchQuery, value => {createClassificationImportSearchQuery = value.trim.replaceAll(" +", " "); JsCmds.Noop}),
			"ImportSearchResults" -> {(nodeSeq: NodeSeq) => {
				bind(
					"import-search-results",
					nodeSeq,
					"EmptyText" -> {(nodeSeq: NodeSeq) => {
						if (createClassificationImportSearchComplete && createClassificationImportSearchClassifications.size < 1) {
							nodeSeq
						} else {
							Text("")
						}
					}},
					"List" -> {(nodeSeq: NodeSeq) => {
						val classificationsNodeSeq: NodeSeq = createClassificationImportSearchClassifications.toList.flatMap(classification => {
							def select() = {
								Model.Classification.currentClassification(Model.Classification.create.source(Model.User.currentGroup.privateSource))
								Model.Classification.currentClassification.is.name(classification._1)
								var taxons: List[Model.Taxon] = List()
								classification._2.foreach(classificationTaxon => {
									val taxonName =
										if (classificationTaxon(0).length > 1) {
											classificationTaxon(0).substring(0, 1).toUpperCase() + classificationTaxon(0).substring(1).toLowerCase()
										} else if (classificationTaxon(0).length > 0) {
											classificationTaxon(0).substring(0, 1).toUpperCase()
										} else {
											"Unnamed"
										}
									val taxonRank =
										if (classificationTaxon(1).toUpperCase().equals("NO RANK")) {
											"Unranked"
										} else if (classificationTaxon(1).length > 1) {
											classificationTaxon(1).substring(0, 1).toUpperCase() + classificationTaxon(1).substring(1).toLowerCase()
										} else if (classificationTaxon(1).length > 0) {
											classificationTaxon(1).substring(0, 1).toUpperCase()
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
									taxons = List.concat(taxons, List(taxon))
								})
								val taxonsMap = new edu.unl.biofinity.site.snippet.Classification().taxonsMap
								taxonsMap.is.update(Model.Classification.currentClassification.is.entityID, taxons)
								
								JsCmds.Run("showCreateClassification()")
							}
							
							bind(
								"classification",
								nodeSeq,
								"Name" -> classification._1,
								"Taxons" -> {(nodeSeq: NodeSeq) => {
									val taxons = classification._2
									bind(
										"taxons",
										nodeSeq,
										"List" -> {(nodeSeq: NodeSeq) => {
											val taxonsNodeSeq: NodeSeq = taxons.flatMap(taxon => {
												bind(
													"taxon",
													nodeSeq,
													"Name" -> taxon(0),
													"Rank" -> taxon(1)
												)
											})
											taxonsNodeSeq
										}}
									)
								}},	
								"SelectLink" -> {(nodeSeq: NodeSeq) => SHtml.ajaxButton(nodeSeq, () => select)}
							)
						})
						classificationsNodeSeq
					}},
					"WaitingText" -> {(nodeSeq: NodeSeq) => {
						if (!createClassificationImportSearchComplete) {
							nodeSeq
						} else {
							Text("")
						}
					}}
				)
			}},
			"NextLink" -> {
				(nodeSeq: NodeSeq) => SHtml.ajaxButton({nodeSeq}, () => next)
			},
			"PreviousLink" -> {
				(nodeSeq: NodeSeq) => SHtml.ajaxButton({nodeSeq}, () => previous)
			},
			"SaveLink" -> {
				(nodeSeq: NodeSeq) => SHtml.ajaxButton({nodeSeq}, () => save)
			}
		)
	}
	
	def renderCreateClassificationScripts: NodeSeq = {
		def showCreateClassification() = {
			SetHtml("create-classification", <lift:embed what="lab/create-classification" />) &
			Call("renderTaxonAutoComplete")
		}
		
		def showCreateClassificationCopy() = {
			SetHtml("create-classification", <lift:embed what="lab/create-classification-copy" />)
		}
		
		def showCreateClassificationImport() = {
			SetHtml("create-classification", <lift:embed what="lab/create-classification-import" />)
		}
		
		def showCreateClassificationStart() = {
			SetHtml("create-classification", <lift:embed what="lab/create-classification-start" />)
		}
		
		def selectClassification(entityID: String) = {
			val classification = Model.Classification.find(entityID) openOr null
			
			if (null != classification) {
				Model.Classification.currentClassification(Model.Classification.create.source(Model.User.currentGroup.privateSource))
				taxonsMap.is.update(Model.Classification.currentClassification.is.entityID, classification.taxons)
			}
			
			JsCmds.Run("showCreateClassification()")
		}
		
		def checkCreateClassificationImportSearch() = {
			if (createClassificationImportSearchComplete) {
				SetHtml("create-classification-import-search-results", <lift:embed what="/lab/create-classification-import-search-results"/>)
			} else {
				JsCmds.Run("setTimeout('checkCreateClassificationImportSearch()', 1000)")
			}
		}
		
		Script(
			Function(
				"showCreateClassification",
				Nil,
				SHtml.ajaxInvoke(showCreateClassification)._2
			)
		) ++
		Script(
			Function(
				"showCreateClassificationCopy",
				Nil,
				SHtml.ajaxInvoke(showCreateClassificationCopy)._2
			)
		) ++
		Script(
			Function(
				"showCreateClassificationImport",
				Nil,
				SHtml.ajaxInvoke(showCreateClassificationImport)._2
			)
		) ++
		Script(
			Function(
				"showCreateClassificationStart",
				Nil,
				SHtml.ajaxInvoke(showCreateClassificationStart)._2
			)
		) ++
		Script(
			Function(
				"selectClassification",
				"entityID" :: Nil,
				SHtml.ajaxCall(JsVar("entityID"), selectClassification _)._2
			)
		) ++
		Script(
			Function(
				"checkCreateClassificationImportSearch",
				Nil,
				SHtml.ajaxInvoke(checkCreateClassificationImportSearch)._2
			)
		)
	}
}