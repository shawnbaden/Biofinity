package edu.unl.biofinity.site.snippet

import edu.unl.biofinity.api.{controller => Controller}
import edu.unl.biofinity.api.{model => Model}

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

import scala.xml._

class Occurrence {
	def init: NodeSeq = {
		val occurrence = Model.Occurrence.find(S.param("ID") openOr -1) openOr null
		if (null != occurrence) {
			val source: Model.Source = occurrence.source.obj openOr null
			if (!S.attr("group").isEmpty) {
				if (source == Model.User.currentGroup.privateSource.obj.openOr(null) || source == Model.User.currentGroup.publicSource.obj.openOr(null)) {
					Model.Occurrence.currentOccurrence(occurrence)
				}
			} else {
				if (source.public_?) {
					Model.Occurrence.currentOccurrence(occurrence)
				}
			}
		}
		Text("")
	}
	
	def renderOccurrence(xhtml: NodeSeq): NodeSeq = {
		if (null == Model.Occurrence.currentOccurrence.is) {
			Text("")
		} else {
			var mediaFileBundle = Model.Occurrence.currentOccurrence.mediaFileBundle.obj openOr null
			
			def initClassification() = {
				Model.Classification.currentClassification(Model.Occurrence.currentOccurrence.is.classification.obj openOr null)
			}
			
			def initEvent() = {
				Model.Event.currentEvent(Model.Occurrence.currentOccurrence.is.event.obj openOr null)
			}
			
			def initSource() = {
				Model.Source.currentSource(Model.Occurrence.currentOccurrence.is.source.obj openOr null)
			}

			def initTaxon() = {
				Model.ClassifiedTaxon.currentClassifiedTaxon(Model.Occurrence.currentOccurrence.is.taxon.obj openOr null)
			}
			
			if ((!S.attr("group").isEmpty && !S.attr("update").isEmpty && Model.Occurrence.currentOccurrence.canUpdate_?) || !S.attr("updateExplicit").isEmpty) {				
				def save(): JsCmd = {
					Model.Occurrence.currentOccurrence.is.save
					SetHtml("occurrence-details", <lift:embed what="/lab/occurrence-details"/>)
				}
				
				SHtml.ajaxForm(
					bind(
						"occurrence",
						xhtml,
						"AdditionalPropertyBundle" -> {(nodeSeq: NodeSeq) => {
							var additionalPropertyBundle = Model.Occurrence.currentOccurrence.additionalPropertyBundle.obj openOr null
							if (null == additionalPropertyBundle) {
								additionalPropertyBundle = Model.AdditionalPropertyBundle.create
								additionalPropertyBundle.save
								Model.Occurrence.currentOccurrence.is.additionalPropertyBundle(additionalPropertyBundle)
								Model.Occurrence.currentOccurrence.is.save
								Model.Occurrence.currentOccurrence(Model.Occurrence.find(Model.Occurrence.currentOccurrence.is.entityID) openOr null)
							}
							Model.AdditionalPropertyBundle.currentAdditionalPropertyBundle(additionalPropertyBundle)
							new AdditionalPropertyBundle().renderAdditionalPropertyBundle(nodeSeq)
						}},
						"AdditionalPropertyBundleWarning" -> {(nodeSeq: NodeSeq) => {
							val additionalPropertyBundle = Model.Occurrence.currentOccurrence.additionalPropertyBundle.obj openOr null
							val groupAdditionalPropertyBundle = Model.User.currentGroup.is.occurrenceAdditionalPropertyBundle.obj openOr null
							
							if (Model.AdditionalPropertyBundle.contains(additionalPropertyBundle, groupAdditionalPropertyBundle)) {
								Text("")
							} else {
								def ensureContains(): JsCmd = {
									Model.AdditionalPropertyBundle.ensureContains(additionalPropertyBundle, groupAdditionalPropertyBundle)
									SetHtml("occurrence-additional-property-bundle", <lift:embed what="/lab/occurrence-additional-property-bundle-update"/>)
								}
								
								bind(
									"additional-property-bundle-warning",
									nodeSeq,
									"EnsureContainsLink" -> {(nodeSeq: NodeSeq) => SHtml.ajaxButton(nodeSeq, () => ensureContains)}
								)
							}
						}},
						"Behavior" -> SHtml.ajaxText(Model.Occurrence.currentOccurrence.is.behavior, value => {Model.Occurrence.currentOccurrence.is.behavior(value); JsCmds.Noop}),
						"CatalogNumber" -> SHtml.ajaxText(Model.Occurrence.currentOccurrence.is.catalogNumber, value => {Model.Occurrence.currentOccurrence.is.catalogNumber(value); JsCmds.Noop}),
						"CancelLink" -> {(nodeSeq: NodeSeq) => <a href="javascript: cancelOccurrenceDetailsUpdate();">{nodeSeq}</a>},
						"Classification" -> {(nodeSeq: NodeSeq) => {
							initClassification
							new Classification().renderClassification(nodeSeq)
						}},
						"ClassifyLink" -> Text(""),
						"DeleteLink" -> Text(""),
						"Details" -> SHtml.ajaxText(Model.Occurrence.currentOccurrence.is.details, value => {Model.Occurrence.currentOccurrence.is.details(value); JsCmds.Noop}),
						"Disposition" -> SHtml.ajaxText(Model.Occurrence.currentOccurrence.is.disposition, value => {Model.Occurrence.currentOccurrence.is.disposition(value); JsCmds.Noop}),
						"EditLink" -> Text(""),
						"EstablishmentMeans" -> SHtml.ajaxText(Model.Occurrence.currentOccurrence.is.establishmentMeans, value => {Model.Occurrence.currentOccurrence.is.establishmentMeans(value); JsCmds.Noop}),
						"Event" -> {(nodeSeq: NodeSeq) => {
							initEvent
							new Event().renderEvent(nodeSeq)
						}},
						"ID" -> Model.Occurrence.currentOccurrence.is.entityID,
						"IDLink" -> Text(""),
						"IndividualCount" -> SHtml.ajaxText(
							Model.Occurrence.currentOccurrence.is.individualCount.is.toString,
							value => {
								try {
									val count = value.toInt
									if (1 > count) {
										Alert("Count must be greater than 0.")
									} else {
										Model.Occurrence.currentOccurrence.is.individualCount(count)
										JsCmds.Noop
									}
								} catch {
									case e:NumberFormatException => {
										Alert("Count must be a number greater than 0.")
									}
								}
							}
						),
						"IndividualID" -> SHtml.ajaxText(Model.Occurrence.currentOccurrence.is.individualID, value => {Model.Occurrence.currentOccurrence.is.individualID(value); JsCmds.Noop}),
						"LifeStage" -> SHtml.ajaxText(Model.Occurrence.currentOccurrence.is.lifeStage, value => {Model.Occurrence.currentOccurrence.is.lifeStage(value); JsCmds.Noop}),
						"MediaFile" -> Text(""),
						"MediaFiles" -> {(nodeSeq: NodeSeq) => {
							bind(
								"media-files",
								nodeSeq,
								"CancelLink" -> {
									(nodeSeq: NodeSeq) => <a href="javascript: showOccurrenceMediaFiles();">{nodeSeq}</a>
								},
								"Create" -> {(nodeSeq: NodeSeq) => {
									val occurrence: Model.Occurrence = Model.Occurrence.find(Model.Occurrence.currentOccurrence.is.entityID).open_!
									var fileParamHolder: Box[FileParamHolder] = Empty
									val redirectURL = S.attr("redirectURL").get + occurrence.entityID.toString
									
									def save() = {
										fileParamHolder match {
											case Full(FileParamHolder(_, mimeType, fileName, data)) => {
												val mediaFile: Model.MediaFile = Model.MediaFile.create
												mediaFile.source(occurrence.source.obj openOr null)
												mediaFile.description(fileName)
												mediaFile.fileType(mimeType)
												mediaFile.encodedData(Model.MediaFile.encoder.encode(data))
												if (null == mediaFileBundle) {
													mediaFileBundle = Model.MediaFileBundle.create
													mediaFileBundle.save
													occurrence.mediaFileBundle(mediaFileBundle)
													occurrence.save
												}
												mediaFile.mediaFileBundle(mediaFileBundle)
												mediaFile.save
											}
											case _ => {
												S.error("Invalid file.")
											}
										}
										S.redirectTo(redirectURL)
									}
									
									bind(
										"media-file",
										nodeSeq,
										"File" -> SHtml.fileUpload(filePH => fileParamHolder = Full(filePH)),
										"SaveLink" -> {
											(nodeSeq: NodeSeq) => SHtml.submit(nodeSeq.toString, () => save)
										}
									)
								}},
								"EmptyText" -> {(nodeSeq: NodeSeq) => {
									if (null != mediaFileBundle && mediaFileBundle.mediaFiles.length > 0) {
										Text("")
									} else {
										nodeSeq
									}
								}},
								"List" -> {(nodeSeq: NodeSeq) => {
									if (null == mediaFileBundle) {
										Text("")
									} else {
										val mediaFilesNodeSeq: NodeSeq = mediaFileBundle.mediaFiles.flatMap(mediaFile => {
											def delete() = {
												mediaFile.deleteFull_!
												SetHtml("occurrence-media-files", <lift:embed what="/lab/occurrence-media-files-update"/>)
											}
											
											def save() = {
												mediaFile.save
												Noop
											}
											
											bind(
												"media-file",
												nodeSeq,
												"DeleteLink" -> {(nodeSeq: NodeSeq) => SHtml.ajaxButton(nodeSeq, () => delete)},
												"Description" -> SHtml.ajaxText(mediaFile.description, value => {mediaFile.description(value); Noop}),
												"SaveLink" -> {(nodeSeq: NodeSeq) => SHtml.ajaxButton(nodeSeq, () => save)},
												"ViewLink" -> {(nodeSeq: NodeSeq) => {
													if (mediaFile.isImage) {
														val widthParam = try {
															val width = BindHelpers.attr("width").first.open_!.toInt

															if (1 > width) {
																""
															} else {
																"&Width=" + width.toString
															}
														} catch { case e: Exception => "" }
														<img src={"/service/media-file/read?ID=" + mediaFile.entityID + widthParam} alt="Occurrence Image"/>
													} else {
														var name = mediaFile.name
														if (name.equals("")) {
															name = "Unnamed File"
														}
														<a href={"/service/media-file/read?ID=" + mediaFile.entityID}>{name}</a>
													}
												}}
											)
										})
										mediaFilesNodeSeq
									}
								}}
							)
						}},
						"OccurrenceType" -> SHtml.ajaxSelect(Model.OccurrenceType.typeMap.keySet.map(occurrenceType => (occurrenceType, occurrenceType)).toSeq, Full(Model.OccurrenceType.typeMapInverted(Model.OccurrenceType(Model.Occurrence.currentOccurrence.is.occurrenceType.toInt))), value => {Model.Occurrence.currentOccurrence.is.occurrenceType(Model.OccurrenceType.typeMap(value)); JsCmds.Noop}),
						"OtherCatalogNumbers" -> SHtml.ajaxText(Model.Occurrence.currentOccurrence.is.otherCatalogNumbers, value => {Model.Occurrence.currentOccurrence.is.otherCatalogNumbers(value); JsCmds.Noop}),
						"Preparations" -> SHtml.ajaxText(Model.Occurrence.currentOccurrence.is.preparations, value => {Model.Occurrence.currentOccurrence.is.preparations(value); JsCmds.Noop}),
						"PublishLink" -> Text(""),
						"RecordedBy" -> SHtml.ajaxText(Model.Occurrence.currentOccurrence.is.recordedBy, value => {Model.Occurrence.currentOccurrence.is.recordedBy(value); JsCmds.Noop}),
						"RecordNumber" -> SHtml.ajaxText(Model.Occurrence.currentOccurrence.is.recordNumber, value => {Model.Occurrence.currentOccurrence.is.recordNumber(value); JsCmds.Noop}),
						"Remarks" -> SHtml.ajaxText(Model.Occurrence.currentOccurrence.is.remarks, value => {Model.Occurrence.currentOccurrence.is.remarks(value); JsCmds.Noop}),
						"ReproductiveCondition" -> SHtml.ajaxText(Model.Occurrence.currentOccurrence.is.reproductiveCondition, value => {Model.Occurrence.currentOccurrence.is.reproductiveCondition(value); JsCmds.Noop}),
						"SaveLink" -> {(nodeSeq: NodeSeq) => SHtml.ajaxButton(nodeSeq, () => save)},
						"Sex" -> SHtml.ajaxText(Model.Occurrence.currentOccurrence.is.sex, value => {Model.Occurrence.currentOccurrence.is.sex(value); JsCmds.Noop}),
						"Source" -> {(nodeSeq: NodeSeq) => {
							initSource
							new Source().renderSource(nodeSeq)
						}},
						"Status" -> SHtml.ajaxText(Model.Occurrence.currentOccurrence.is.status, value => {Model.Occurrence.currentOccurrence.is.status(value); JsCmds.Noop}),
						"Taxon" -> {(nodeSeq: NodeSeq) => {
							initTaxon
							new Taxon().renderTaxon(nodeSeq)
						}},
						"UnpublishLink" -> Text(""),
						"ViewLink" -> Text(""),
						"WikiPageIDLink" -> Text("")
					)
				)
			} else {
				bind(
					"occurrence",
					xhtml,
					"AdditionalPropertyBundle" -> {(nodeSeq: NodeSeq) => {
						Model.AdditionalPropertyBundle.currentAdditionalPropertyBundle(Model.Occurrence.currentOccurrence.additionalPropertyBundle.obj openOr null)
						new AdditionalPropertyBundle().renderAdditionalPropertyBundle(nodeSeq)
					}},
					"AdditionalPropertyBundleWarning" -> Text(""),
					"Behavior" -> Model.Occurrence.currentOccurrence.get.behavior,
					"CatalogNumber" -> Model.Occurrence.currentOccurrence.get.catalogNumber,
					"CancelLink" -> Text(""),
					"Classification" -> {(nodeSeq: NodeSeq) => {
						initClassification
						new Classification().renderClassification(nodeSeq)
					}},
					"ClassifyLink" -> {(nodeSeq: NodeSeq) => {
						if (!S.attr("group").isEmpty && Model.Occurrence.currentOccurrence.canUpdate_?) {
							<a href={"/lab/classify-occurrence?ID=" + Model.Occurrence.currentOccurrence.entityID}>{nodeSeq}</a>
						} else {
							Text("")
						}
					}},
					"DeleteLink" -> {(nodeSeq: NodeSeq) => {
						if (!S.attr("group").isEmpty && Model.Occurrence.currentOccurrence.canUpdate_?) {
							<a href="javascript: showDeleteOccurrenceDialog();">{nodeSeq}</a>
						} else {
							Text("")
						}
					}},
					"Details" -> Model.Occurrence.currentOccurrence.get.details,
					"Disposition" -> Model.Occurrence.currentOccurrence.get.disposition,
					"EditLink" -> {(nodeSeq: NodeSeq) => {
						if (!S.attr("group").isEmpty && Model.Occurrence.currentOccurrence.canUpdate_?) {
							<a href="javascript: showOccurrenceDetailsUpdate();">{nodeSeq}</a>
						} else {
							Text("")
						}
					}},
					"EstablishmentMeans" -> Model.Occurrence.currentOccurrence.get.establishmentMeans,
					"Event" -> {(nodeSeq: NodeSeq) => {
						initEvent
						new Event().renderEvent(nodeSeq)
					}},
					"ID" -> Model.Occurrence.currentOccurrence.get.entityID,
					"IDLink" -> <a href={"occurrence?ID=" + Model.Occurrence.currentOccurrence.is.entityID}>{Model.Occurrence.currentOccurrence.is.entityID}</a>,
					"IndividualCount" -> Model.Occurrence.currentOccurrence.get.individualCount,
					"IndividualID" -> Model.Occurrence.currentOccurrence.get.individualID,
					"LifeStage" -> Model.Occurrence.currentOccurrence.get.lifeStage,
					"MediaFile" -> {(nodeSeq: NodeSeq) => {
						if (null != mediaFileBundle && mediaFileBundle.mediaFiles.length > 0) {
							val mediaFile = mediaFileBundle.mediaFiles.first
							bind(
								"media-file",
								nodeSeq,
								"ViewLink" -> {(nodeSeq: NodeSeq) => {
									if (mediaFile.isImage) {
										val widthParam = try {
											val width = BindHelpers.attr("width").first.open_!.toInt
											
											if (1 > width) {
												""
											} else {
												"&Width=" + width.toString
											}
										} catch { case e: Exception => "" }
										<a href={"occurrence?ID=" + Model.Occurrence.currentOccurrence.is.entityID}>
											<img src={"/service/media-file/read?ID=" + mediaFile.entityID + widthParam} alt="Occurrence Image"/>
										</a>
									} else {
										Text("")
									}
								}}
							)
						} else {
							Text("")
						}
					}},
					"MediaFiles" -> {(nodeSeq: NodeSeq) => {
						bind(
							"media-files",
							nodeSeq,
							"EmptyText" -> {(nodeSeq: NodeSeq) => {
								if (null != mediaFileBundle && mediaFileBundle.mediaFiles.length > 0) {
									Text("")
								} else {
									nodeSeq
								}
							}},
							"List" -> {(nodeSeq: NodeSeq) => {
								if (null == mediaFileBundle) {
									Text("")
								} else {
									val mediaFilesNodeSeq: NodeSeq = mediaFileBundle.mediaFiles.flatMap(mediaFile => {
										bind(
											"media-file",
											nodeSeq,
											"DeleteLink" -> Text(""),
											"Description" -> mediaFile.description,
											"SaveLink" -> Text(""),
											"ViewLink" -> {(nodeSeq: NodeSeq) => {
												if (mediaFile.isImage) {
													val widthParam = try {
														val width = BindHelpers.attr("width").first.open_!.toInt

														if (1 > width) {
															""
														} else {
															"&Width=" + width.toString
														}
													} catch { case e: Exception => "" }
													<a href={"/service/media-file/read.jpg?ID=" + mediaFile.entityID} rel="occurrence-media-files">
														<img src={"/service/media-file/read?ID=" + mediaFile.entityID + widthParam} alt="Occurrence Image"/>
													</a>
												} else {
													var name = mediaFile.name
													if (name.equals("")) {
														name = "Unnamed File"
													}
													<a href={"/service/media-file/read?ID=" + mediaFile.entityID}>{name}</a>
												}
											}}
										)
									})
									mediaFilesNodeSeq
								}
							}},
							"EditLink" -> {(nodeSeq: NodeSeq) => {
								if (!S.attr("group").isEmpty && Model.Occurrence.currentOccurrence.is.canUpdate_?) {
									<a href="javascript: showOccurrenceMediaFilesUpdate();">{nodeSeq}</a>
								} else {
									Text("")
								}
							}}
						)
					}},
					"OccurrenceType" -> Model.OccurrenceType.typeMapInverted(Model.OccurrenceType(Model.Occurrence.currentOccurrence.get.occurrenceType.toInt)),
					"OtherCatalogNumbers" -> Model.Occurrence.currentOccurrence.get.otherCatalogNumbers,
					"Preparations" -> Model.Occurrence.currentOccurrence.get.preparations,
					"PublishLink" -> {(nodeSeq: NodeSeq) => {
						val source = Model.Occurrence.currentOccurrence.is.source.obj openOr null
						val groupUser = Model.GroupUser.find(By(Model.GroupUser.user, Model.User.currentUser.is), By(Model.GroupUser.group, Model.User.currentGroup.is)) openOr null
						if (!S.attr("group").isEmpty && Model.Occurrence.currentOccurrence.canUpdate_? && null != source && source.private_? && null != groupUser && groupUser.userType != Model.GroupUserType.Restricted) {
							bind(
								"link",
								nodeSeq,
								"Name" -> {(nameNodeSeq: NodeSeq) => {
									<a href="javascript: showPublishOccurrenceDialog();">{nameNodeSeq}</a>
								}}
							)
						} else {
							Text("")
						}
					}},
					"RecordedBy" -> Model.Occurrence.currentOccurrence.get.recordedBy,
					"RecordNumber" -> Model.Occurrence.currentOccurrence.get.recordNumber,
					"Remarks" -> Model.Occurrence.currentOccurrence.get.remarks,
					"ReproductiveCondition" -> Model.Occurrence.currentOccurrence.get.reproductiveCondition,
					"SaveLink" -> Text(""),
					"Sex" -> Model.Occurrence.currentOccurrence.get.sex,
					"Source" -> {(nodeSeq: NodeSeq) => {
						initSource
						new Source().renderSource(nodeSeq)
					}},
					"Status" -> Model.Occurrence.currentOccurrence.get.status,
					"Taxon" -> {(nodeSeq: NodeSeq) => {
						initTaxon
						new Taxon().renderTaxon(nodeSeq)
					}},
					"UnpublishLink" -> {(nodeSeq: NodeSeq) => {
						val source = Model.Occurrence.currentOccurrence.is.source.obj openOr null
						val groupUser = Model.GroupUser.find(By(Model.GroupUser.user, Model.User.currentUser.is), By(Model.GroupUser.group, Model.User.currentGroup.is)) openOr null
						if (!S.attr("group").isEmpty && Model.Occurrence.currentOccurrence.canUpdate_? && null != source && source.public_? && null != groupUser && groupUser.userType != Model.GroupUserType.Restricted) {
							bind(
								"link",
								nodeSeq,
								"Name" -> {(nameNodeSeq: NodeSeq) => {
									<a href="javascript: showUnpublishOccurrenceDialog();">{nameNodeSeq}</a>
								}}
							)
						} else {
							Text("")
						}
					}},
					"ViewInDataLink" -> {(nodeSeq: NodeSeq) => {
						val source = Model.Occurrence.currentOccurrence.is.source.obj openOr null
						if (!S.attr("group").isEmpty && Model.Occurrence.currentOccurrence.canUpdate_? && null != source && source.public_?) {
							bind(
								"link",
								nodeSeq,
								"Name" -> {(nameNodeSeq: NodeSeq) => {
									<a href={"../data/occurrence?ID=" + Model.Occurrence.currentOccurrence.is.entityID}>{nameNodeSeq}</a>
								}}
							)
						} else {
							Text("")
						}
					}},
					"ViewInLabLink" -> {(nodeSeq: NodeSeq) => {
						val source = Model.Occurrence.currentOccurrence.is.source.obj openOr null
						if (S.attr("group").isEmpty && null != source && Model.User.signedIn_? && Model.User.currentUser.is.groups.contains(source.group)) {
							val occurrence = Model.Occurrence.currentOccurrence.is
							
							def view() = {
								Model.User.currentGroup(source.group)
								JsCmds.RedirectTo("/lab/occurrence?ID=" + occurrence.entityID)
							}
							
							bind(
								"link",
								nodeSeq,
								"Name" -> {(nameNodeSeq: NodeSeq) => {
									SHtml.a(() => view, nameNodeSeq)
								}}
							)
						} else {
							Text("")
						}
					}},
					"ViewLink" -> {(nodeSeq: NodeSeq) => {
						<a href={"occurrence?ID=" + Model.Occurrence.currentOccurrence.is.entityID}>{nodeSeq}</a>
					}},
					"WikiPageIDLink" -> {(nodeSeq: NodeSeq) => {
						val wikiPageID = Model.Occurrence.currentOccurrence.is.wikiPageID.is
						if (0 < wikiPageID) {
							<a href={"/wiki?WikiPageID=" + wikiPageID}>{wikiPageID}</a>
						} else {
							if (!S.attr("group").isEmpty) {
								def create() = {
									val newWikiPageID = Controller.WikiPage.create("occurrence", Model.Occurrence.currentOccurrence.is.entityID, Model.Occurrence.currentOccurrence.is.source.obj.open_!.entityID)
									Model.Occurrence.currentOccurrence.is.wikiPageID(newWikiPageID)
									Model.Occurrence.currentOccurrence.is.save
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
				) ++ Script(Run("if (window.initOccurrenceMediaFiles) {initOccurrenceMediaFiles();}"))
			}
		}
	}
	
	def renderScripts: NodeSeq = {
		def showOccurrenceDetails() = {
			SetHtml("occurrence-details", <lift:embed what="lab/occurrence-details" />)
		}
		
		def showOccurrenceDetailsUpdate() = {
			SetHtml("occurrence-details", <lift:embed what="lab/occurrence-details-update" />)
		}
		
		def cancelOccurrenceDetailsUpdate() = {
			Model.Occurrence.currentOccurrence(Model.Occurrence.find(Model.Occurrence.currentOccurrence.is.entityID) openOr null)
			
			SetHtml("occurrence-details", <lift:embed what="lab/occurrence-details" />)
		}
		
		def deleteOccurrence() = {
			val event = Model.Occurrence.currentOccurrence.is.event.obj openOr null
			val location = {
				if (null == event) {
					null
				} else {
					event.location.obj openOr null
				}
			}
			Model.Occurrence.currentOccurrence.is.deleteFull_!
			if (null != event) {
				event.deleteIfNoChildren_!
				if (null != location) {
					location.deleteIfNoChildren_!
				}
			}
			
			Alert("The occurrence has been deleted.") & JsCmds.RedirectTo("/lab")
		}
		
		def publishOccurrence() = {
			val event = Model.Occurrence.currentOccurrence.is.event.obj openOr null
			if (null != event) {
				val location = event.location.obj openOr null
				if (null != location) {
					location.source(location.source.obj.open_!.group.publicSource.obj.open_!)
					location.save
				}
				event.source(event.source.obj.open_!.group.publicSource.obj.open_!)
				event.save
			}
			
			val classification = Model.Occurrence.currentOccurrence.is.classification.obj openOr null
			if (null != classification) {
				classification.source(classification.source.obj.open_!.group.publicSource.obj.open_!)
				classification.save
			}
			
			val mediaFileBundle = Model.Occurrence.currentOccurrence.is.mediaFileBundle.obj openOr null
			if (null != mediaFileBundle) {
				mediaFileBundle.mediaFiles.foreach(mediaFile => {
					mediaFile.source(mediaFile.source.obj.open_!.group.publicSource.obj.open_!)
					mediaFile.save
				})
			}
			
			Model.Occurrence.currentOccurrence.is.source(Model.Occurrence.currentOccurrence.is.source.obj.open_!.group.publicSource.obj.open_!)
			Model.Occurrence.currentOccurrence.is.save
			
			Alert("The occurrence has been published.") & JsCmds.Run("location.reload(true)")
		}
		
		def unpublishOccurrence() = {
			Model.Occurrence.currentOccurrence.is.source(Model.Occurrence.currentOccurrence.is.source.obj.open_!.group.privateSource.obj.open_!)
			Model.Occurrence.currentOccurrence.is.save
			
			val mediaFileBundle = Model.Occurrence.currentOccurrence.is.mediaFileBundle.obj openOr null
			if (null != mediaFileBundle) {
				mediaFileBundle.mediaFiles.foreach(mediaFile => {
					mediaFile.source(mediaFile.source.obj.open_!.group.privateSource.obj.open_!)
					mediaFile.save
				})
			}
			
			val classification = Model.Occurrence.currentOccurrence.is.classification.obj openOr null
			if (null != classification && classification.occurrences.filter(_.source.obj.open_!.sourceType == Model.SourceType.Public).length < 1) {
				classification.source(classification.source.obj.open_!.group.privateSource.obj.open_!)
				classification.save
			}
			
			val event = Model.Occurrence.currentOccurrence.is.event.obj openOr null
			if (null != event && event.occurrences.filter(_.source.obj.open_!.sourceType == Model.SourceType.Public).length < 1) {
				event.source(event.source.obj.open_!.group.privateSource.obj.open_!)
				event.save
				val location = event.location.obj openOr null
				if (null != location && location.events.filter(_.source.obj.open_!.sourceType == Model.SourceType.Public).length < 1) {
					location.source(location.source.obj.open_!.group.privateSource.obj.open_!)
					location.save
				}
			}
			
			Alert("The occurrence has been unpublished.") & JsCmds.Run("location.reload(true)")
		}
		
		def showOccurrenceAdditionalPropertyBundle() = {
			SetHtml("occurrence-additional-property-bundle", <lift:embed what="lab/occurrence-additional-property-bundle" />)
		}
		
		def showOccurrenceAdditionalPropertyBundleUpdate() = {
			SetHtml("occurrence-additional-property-bundle", <lift:embed what="lab/occurrence-additional-property-bundle-update" />)
		}
		
		def showOccurrenceMediaFiles() = {
			SetHtml("occurrence-media-files", <lift:embed what="lab/occurrence-media-files" />) &
			Run("initOccurrenceMediaFiles()")
		}
		
		def showOccurrenceMediaFilesUpdate() = {
			SetHtml("occurrence-media-files", <lift:embed what="lab/occurrence-media-files-update" />)
		}
		
		Script(
			Function(
				"showOccurrenceDetails",
				Nil,
				SHtml.ajaxInvoke(showOccurrenceDetails)._2
			)
		) ++
		Script(
			Function(
				"showOccurrenceDetailsUpdate",
				Nil,
				SHtml.ajaxInvoke(showOccurrenceDetailsUpdate)._2
			)
		) ++
		Script(
			Function(
				"cancelOccurrenceDetailsUpdate",
				Nil,
				SHtml.ajaxInvoke(cancelOccurrenceDetailsUpdate)._2
			)
		) ++
		Script(
			Function(
				"showDeleteOccurrenceDialog",
				Nil,
				JsRaw(
					"if (confirm(\"Are you sure you want to delete this occurrence?  This action cannot be undone.\")) {" +
						"deleteOccurrence();" +
					"}"
				)
			)
		) ++
		Script(
			Function(
				"deleteOccurrence",
				Nil,
				SHtml.ajaxInvoke(deleteOccurrence)._2
			)
		) ++
		Script(
			Function(
				"showPublishOccurrenceDialog",
				Nil,
				{
					if (null == Model.Occurrence.currentOccurrence.is) {
						JsCmds.Noop
					} else {
						if (null == (Model.Occurrence.currentOccurrence.is.classification.obj openOr null)) {
							Alert("An occurrence must be classified before it is published.")
						} else {
							JsRaw(
								"if (confirm(\"Are you sure you want to publish this occurrence?\")) {" +
									"publishOccurrence();" +
								"}"
							)
						}
					}
				}
			)
		) ++
		Script(
			Function(
				"publishOccurrence",
				Nil,
				SHtml.ajaxInvoke(publishOccurrence)._2
			)
		) ++
		Script(
			Function(
				"showUnpublishOccurrenceDialog",
				Nil,
				{
					JsRaw(
						"if (confirm(\"Are you sure you want to unpublish this occurrence?\")) {" +
							"unpublishOccurrence();" +
						"}"
					)
				}
			)
		) ++
		Script(
			Function(
				"unpublishOccurrence",
				Nil,
				SHtml.ajaxInvoke(unpublishOccurrence)._2
			)
		) ++
		Script(
			Function(
				"showOccurrenceAdditionalPropertyBundle",
				Nil,
				SHtml.ajaxInvoke(showOccurrenceAdditionalPropertyBundle)._2
			)
		) ++
		Script(
			Function(
				"showOccurrenceAdditionalPropertyBundleUpdate",
				Nil,
				SHtml.ajaxInvoke(showOccurrenceAdditionalPropertyBundleUpdate)._2
			)
		) ++
		Script(
			Function(
				"showOccurrenceMediaFiles",
				Nil,
				SHtml.ajaxInvoke(showOccurrenceMediaFiles)._2
			)
		) ++
		Script(
			Function(
				"showOccurrenceMediaFilesUpdate",
				Nil,
				SHtml.ajaxInvoke(showOccurrenceMediaFilesUpdate)._2
			)
		) ++
		Script(
			Function(
				"initOccurrenceMediaFiles",
				Nil,
				JsRaw("$(\"a[rel='occurrence-media-files']\").colorbox({height:'100%', width:'100%'});")
			)
		)
	}
	
	def renderCreateOccurrenceLocation(xhtml: NodeSeq): NodeSeq = {
		if (null == Model.Location.currentLocation.is) {
			Model.Location.currentLocation(Model.Location.create.source(Model.User.currentGroup.privateSource))
		}
		
		def next(): JsCmd = {
			if (
				null != Model.Location.currentLocation.is &&
				Model.Location.currentLocation.is.entityID > 0 &&
				(null == Model.Event.currentEvent.is || (null != Model.Event.currentEvent.is && Model.Event.currentEvent.is.entityID > 0))
				){
				JsCmds.Run("showCreateOccurrenceEventExisting()") 
			} else {
				JsCmds.Run("showCreateOccurrenceEventNew()")
			}
		}
		
		bind(
			"create-occurrence-location",
			xhtml,
			"NextLink" -> {
				(nodeSeq: NodeSeq) => SHtml.ajaxButton({nodeSeq}, () => next)
			},
			"ToggleLink" -> {
				(nodeSeq: NodeSeq) => {
					bind(
						"link",
						nodeSeq,
						"Name" -> {(nameNodeSeq: NodeSeq) => {
							if (!S.attr("new").isEmpty) {
								<a href="javascript: showCreateOccurrenceLocationExisting();">{nameNodeSeq}</a>
							} else {
								<a href="javascript: showCreateOccurrenceLocationNew();">{nameNodeSeq}</a>
							}
						}}
					)
				}
			}
		)
	}
	
	def renderCreateOccurrenceEvent(xhtml: NodeSeq): NodeSeq = {
		if (null == Model.Event.currentEvent.is) {
			Model.Event.currentEvent(Model.Event.create.source(Model.User.currentGroup.privateSource))
		}
		
		def next(): JsCmd = {
			JsCmds.Run("showCreateOccurrence()")
  		}
		
		def previous(): JsCmd = {
			if (null != Model.Location.currentLocation.is && Model.Location.currentLocation.is.entityID > 0) {
				JsCmds.Run("showCreateOccurrenceLocationExisting()") 
			} else {
				JsCmds.Run("showCreateOccurrenceLocationNew()")
			}
		}
		
		bind(
			"create-occurrence-event",
			xhtml,
			"NextLink" -> {
				(nodeSeq: NodeSeq) => SHtml.ajaxButton({nodeSeq}, () => next)
			},
			"PreviousLink" -> {
				(nodeSeq: NodeSeq) => SHtml.ajaxButton({nodeSeq}, () => previous)
			},
			"ToggleLink" -> {
				(nodeSeq: NodeSeq) => {
					if (!S.attr("new").isEmpty && Model.Location.currentLocation.is.entityID < 1) {
						Text("")
					} else {
						bind(
							"link",
							nodeSeq,
							"Name" -> {(nameNodeSeq: NodeSeq) => {
								if (!S.attr("new").isEmpty) {
									<a href="javascript: showCreateOccurrenceEventExisting();">{nameNodeSeq}</a>
								} else {
									<a href="javascript: showCreateOccurrenceEventNew();">{nameNodeSeq}</a>
								}
							}}
						)
					}
				}
			}
		)
	}
	
	def renderCreateOccurrence(xhtml: NodeSeq): NodeSeq = {
		if (null == Model.Occurrence.currentOccurrence.is) {
			Model.Occurrence.currentOccurrence(Model.Occurrence.create.source(Model.User.currentGroup.privateSource).sex("").lifeStage(""))
		}
		
		def previous(): JsCmd = {
			if (null != Model.Event.currentEvent.is && Model.Event.currentEvent.is.entityID > 0) {
				JsCmds.Run("showCreateOccurrenceEventExisting()") 
			} else {
				JsCmds.Run("showCreateOccurrenceEventNew()")
			}
		}
		
		def save(): JsCmd = {
			Model.Location.currentLocation.is.save
			Model.Event.currentEvent.is.location(Model.Location.currentLocation.is)
			Model.Event.currentEvent.is.save
			Model.Occurrence.currentOccurrence.is.event(Model.Event.currentEvent.is)
			Model.Occurrence.currentOccurrence.is.save
			JsCmds.RedirectTo("/lab/occurrence?ID=" + Model.Occurrence.currentOccurrence.is.entityID)
		}
		
		bind(
			"create-occurrence",
			xhtml,
			"SaveLink" -> {
				(nodeSeq: NodeSeq) => SHtml.ajaxButton({nodeSeq}, () => save)
			},
			"PreviousLink" -> {
				(nodeSeq: NodeSeq) => SHtml.ajaxButton({nodeSeq}, () => previous)
			}
		)
	}
	
	def renderCreateOccurrenceScripts: NodeSeq = {
		def selectLocation(value: Any): JsCmd = {
			value match {
				case map: scala.collection.immutable.Map[String, Long] => {
					val entityID = map.get("entityID")
					val location = Model.Location.find(entityID) openOr null
					
					if (null != Model.Location.currentLocation.is) {
						Model.Location.currentLocation(location)
					}
					if (null != Model.Event.currentEvent.is && Model.Event.currentEvent.is.entityID > 0) {
						Model.Event.currentEvent(null)
					}
					
					SetHtml("location-details", <lift:embed what="/lab/create-occurrence-location-details"/>)
				}
				case _ => {
					Noop
				}
			}
		}
		
		def selectEvent(value: Any): JsCmd = {
			value match {
				case map: scala.collection.immutable.Map[String, Long] => {
					val entityID = map.get("entityID")
					val event = Model.Event.find(entityID) openOr null
					
					if (null != Model.Event.currentEvent.is) {
						Model.Event.currentEvent(event)
					}
					
					SetHtml("event-details", <lift:embed what="/lab/create-occurrence-event-details"/>)
				}
				case _ => {
					Noop
				}
			}
		}
		
		def showCreateOccurrenceLocationNew() = {
			if (null != Model.Location.currentLocation.is && Model.Location.currentLocation.is.entityID > 0) {
				Model.Location.currentLocation(null)
			}
			if (null != Model.Event.currentEvent.is && Model.Event.currentEvent.is.entityID > 0) {
				Model.Event.currentEvent(null)
			}
			Map.markerDraggable(true)
			
			SetHtml("create-occurrence", <lift:embed what="lab/create-occurrence-location-new" />) &
			new Map().renderMapJavaScriptCommand &
			new Map().renderMarkerJavaScriptCommand
		}
		
		def showCreateOccurrenceLocationExisting() = {
			val displayLocationJavaScriptCommand =
				if (null != Model.Location.currentLocation.is && Model.Location.currentLocation.is.entityID > 0) {
					SetHtml("location-details", <lift:embed what="/lab/create-occurrence-location-details"/>)
				} else {
					JsCmds.Noop
				}
			SetHtml("create-occurrence", <lift:embed what="lab/create-occurrence-location-existing" />) &
			new Map().renderMapJavaScriptCommand &
			new Location().renderLocationsOnMapJavaScriptCommand &
			displayLocationJavaScriptCommand
		}
		
		def showCreateOccurrenceEventNew() = {
			if (null != Model.Event.currentEvent.is && Model.Event.currentEvent.is.entityID > 0) {
				Model.Event.currentEvent(null)
			}
			
			SetHtml("create-occurrence", <lift:embed what="lab/create-occurrence-event-new" />) &
			new Event().renderInitDatePickerJavaScriptCommand
		}
		
		def showCreateOccurrenceEventExisting() = {
			val displayEventJavaScriptCommand =
				if (null != Model.Event.currentEvent.is && Model.Event.currentEvent.is.entityID > 0) {
					SetHtml("event-details", <lift:embed what="/lab/create-occurrence-event-details"/>)
				} else {
					JsCmds.Noop
				}
			SetHtml("create-occurrence", <lift:embed what="lab/create-occurrence-event-existing" />) &
			displayEventJavaScriptCommand
		}
		
		def showCreateOccurrence() = {
			SetHtml("create-occurrence", <lift:embed what="lab/create-occurrence" />)
		}
		
		Script(
			Function(
				"selectLocation",
				"entityID" :: Nil,
				SHtml.jsonCall(JsObj("entityID" -> JsVar("entityID")), selectLocation _)._2
			)
		) ++
		Script(
			Function(
				"selectEvent",
				"entityID" :: Nil,
				SHtml.jsonCall(JsObj("entityID" -> JsVar("entityID")), selectEvent _)._2
			)
		) ++
		Script(
			Function(
				"showCreateOccurrenceLocationNew",
				Nil,
				SHtml.ajaxInvoke(showCreateOccurrenceLocationNew)._2
			)
		) ++
		Script(
			Function(
				"showCreateOccurrenceLocationExisting",
				Nil,
				SHtml.ajaxInvoke(showCreateOccurrenceLocationExisting)._2
			)
		) ++
		Script(
			Function(
				"showCreateOccurrenceEventNew",
				Nil,
				SHtml.ajaxInvoke(showCreateOccurrenceEventNew)._2
			)
		) ++
		Script(
			Function(
				"showCreateOccurrenceEventExisting",
				Nil,
				SHtml.ajaxInvoke(showCreateOccurrenceEventExisting)._2
			)
		) ++
		Script(
			Function(
				"showCreateOccurrence",
				Nil,
				SHtml.ajaxInvoke(showCreateOccurrence)._2
			)
		)
	}
	
	def renderClassifyOccurrence(xhtml: NodeSeq): NodeSeq = {
		val searchQuery = new Search().searchQuery
		
		def executeSearch(): JsCmd = {
			SetHtml("classify-occurrence-search-results", <lift:embed what="lab/classify-occurrence-search-results" />)
		}
		
		bind(
			"classify-occurrence",
			xhtml,
			"SearchExecuteLink" -> {(nodeSeq: NodeSeq) => SHtml.ajaxButton(nodeSeq, () => executeSearch)},
			"SearchQuery" -> SHtml.ajaxText(searchQuery, value => {searchQuery(value); JsCmds.Noop})
		)
	}
	
	def renderClassifyOccurrenceScripts: NodeSeq = {
		def selectTaxon(entityID: String) = {
			val taxon = Model.ClassifiedTaxon.find(entityID) openOr null
			
			if (null != taxon) {
				val occurrenceSource = Model.Occurrence.currentOccurrence.is.source.obj.open_!
				val taxonSource = taxon.source.obj.open_!
				if (occurrenceSource.public_? && taxonSource.private_?) {
					taxon.source(taxon.source.obj.open_!.group.publicSource.obj.open_!)
					taxon.save
				}
				Model.Occurrence.currentOccurrence.is.taxon(taxon)
				Model.Occurrence.currentOccurrence.is.save
			}
			
			JsCmds.RedirectTo("occurrence?ID=" + Model.Occurrence.currentOccurrence.is.entityID)
		}
		
		Script(
			Function(
				"selectTaxon",
				"entityID" :: Nil,
				SHtml.ajaxCall(JsVar("entityID"), selectTaxon _)._2
			)
		)
	}
	
	def renderFacebookLink: NodeSeq = {
		if (null == Model.Occurrence.currentOccurrence.is) {
			Text("")
		} else {
			if (S.attr("group").isEmpty && (Model.Occurrence.currentOccurrence.is.source.obj openOr Model.Source).public_?) {
				<a href={"http://www.facebook.com/sharer.php?u=http://biofinity.unl.edu/biofinity/data/occurrence?ID=" + Model.Occurrence.currentOccurrence.entityID + "&t=" + Model.Occurrence.currentOccurrence.is.classification.obj.open_!.classificationName}>
					<img src="/resources/images/Facebook_icon.png" alt="Post to facebook.com" title="Post to facebook.com"/>
				</a>
			} else {
				Text("")
			}
		}
	}
	
	def renderTwitterLink: NodeSeq = {
		if (null == Model.Occurrence.currentOccurrence.is) {
			Text("")
		} else {
			if (S.attr("group").isEmpty && (Model.Occurrence.currentOccurrence.is.source.obj openOr Model.Source).public_?) {
				val sourceURL: String = "http://tinyurl.com/api-create.php?url=http://biofinity.unl.edu/biofinity/data/occurrence?ID=" + Model.Occurrence.currentOccurrence.entityID
				val source: scala.io.Source = scala.io.Source.fromURL(sourceURL)
				val link = source.mkString
				
				<a href={"http://twitter.com/home?status=" + Model.Occurrence.currentOccurrence.is.classification.obj.open_!.classificationName + " " + link}>
					<img src="/resources/images/Twitter_icon.png" alt="Post to twitter.com" title="Post to twitter.com"/>
				</a>
			} else {
				Text("")
			}
		}
	}
	
	def renderOccurrencesOnMap: NodeSeq = {
		Script(
			Call(
				"addMarkers",
				JsVar("occurrencesJSON"),
				JsTrue
			)
		)
	}
	
	def renderOccurrenceMapScripts: NodeSeq = {
		val group = !S.attr("group").isEmpty
		def setInfoWindowContent(occurrence: Any): JsCmd = {
			occurrence match {
				case map: scala.collection.immutable.Map[String, String] => {
					val occurrenceID = map.get("occurrenceID").get.toLong
					
					Model.Occurrence.currentOccurrence(Model.Occurrence.find(occurrenceID) openOr null)

					val infoWindowContent: String  = {
						if (group) {
							runTemplate("templates-hidden" :: "lab" :: "occurrence-info-window" :: Nil).openOr(Text("")).toString
						} else {
							runTemplate("templates-hidden" :: "occurrence-info-window" :: Nil).openOr(Text("")).toString
						}
					}

					JsCmds.JsCrVar("infoWindowContent", infoWindowContent) &
					Call("openMarkerInfoWindow")
				}
				case _ => {
					Noop
				}
			}
		}

		Script(
			Function(
				"setInfoWindowContent",
				"occurrenceID" :: Nil,
				SHtml.jsonCall(JsObj("occurrenceID" -> JsVar("occurrenceID")), setInfoWindowContent _)._2
			)
		)
	}
}