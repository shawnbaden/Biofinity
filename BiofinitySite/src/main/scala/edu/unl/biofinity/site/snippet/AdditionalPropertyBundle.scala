package edu.unl.biofinity.site.snippet

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

class AdditionalPropertyBundle {
	def renderAdditionalPropertyBundle(xhtml: NodeSeq): NodeSeq = {
		if (null == Model.AdditionalPropertyBundle.currentAdditionalPropertyBundle.is) {
			bind(
				"additional-property-bundle",
				xhtml,
				"Create" -> Text(""),
				"EmptyText" -> {(nodeSeq: NodeSeq) => {
					nodeSeq
				}},
				"List" -> Text("")
			)
		} else {
			if (!S.attr("group").isEmpty && !S.attr("update").isEmpty) {
				bind(
					"additional-property-bundle",
					xhtml,
					"Create" -> {(nodeSeq: NodeSeq) => {
						var additionalPropertyName = ""
						var additionalPropertyValue = ""
						var additionalPropertyType = Model.AdditionalPropertyType.String
						
						def save(): JsCmd = {
							if (additionalPropertyName.length < 1) {
								Alert("Name cannot be empty.")
							} else {
								val additionalProperty: Model.AdditionalProperty = Model.AdditionalProperty.create
								additionalProperty.name(additionalPropertyName)
								additionalProperty.value(additionalPropertyValue)
								additionalProperty.valueType(additionalPropertyType)
								additionalProperty.additionalPropertyBundle(Model.AdditionalPropertyBundle.currentAdditionalPropertyBundle.is)
								additionalProperty.save
								
								SetHtml("additional-property-bundle", <lift:embed what="/lab/additional-property-bundle-update"/>)
							}
						}
						
						def valueInput(): NodeSeq = {
							if (Model.AdditionalPropertyType.String == additionalPropertyType) {
								additionalPropertyValue = ""
								SHtml.ajaxText(additionalPropertyValue, value => {additionalPropertyValue = value; Noop})
							} else if (Model.AdditionalPropertyType.Boolean == additionalPropertyType) {
								additionalPropertyValue = Model.AdditionalPropertyType.booleanAsValue(false)
								SHtml.ajaxCheckbox(
									Model.AdditionalPropertyType.valueAsBoolean(additionalPropertyValue),
									value => {additionalPropertyValue = Model.AdditionalPropertyType.booleanAsValue(value); Noop}
								)
							} else {
								Text("")
							}
						}
						
						bind(
							"additional-property",
							nodeSeq,
							"Name" -> SHtml.ajaxText(additionalPropertyName, value => {additionalPropertyName = value; Noop}),
							"SaveLink" -> {(nodeSeq: NodeSeq) => SHtml.ajaxButton(nodeSeq, () => save)},
							"Value" -> valueInput,
							"ValueType" -> SHtml.ajaxSelect(
								Model.AdditionalPropertyType.typeMap.keySet.map(additionalPropertyType => (additionalPropertyType, additionalPropertyType)).toSeq,
								Full(Model.AdditionalPropertyType.typeMapInverted(additionalPropertyType)),
								value => {
									additionalPropertyType = Model.AdditionalPropertyType.typeMap(value)
									SetHtml("additional-property-value", valueInput)
								}
							)
						)
					}},
					"EmptyText" -> {(nodeSeq: NodeSeq) => {
						if (Model.AdditionalPropertyBundle.currentAdditionalPropertyBundle.is.additionalProperties.length > 0) {
							Text("")
						} else {
							nodeSeq
						}
					}},
					"List" -> {(nodeSeq: NodeSeq) => {
						val additionalPropertiesNodeSeq: NodeSeq = Model.AdditionalPropertyBundle.currentAdditionalPropertyBundle.is.additionalProperties.sort((a, b) => b.name.is.compareToIgnoreCase(a.name.is) > 0).flatMap(additionalProperty => {
							def delete() = {
								additionalProperty.delete_!
								SetHtml("additional-property-bundle", <lift:embed what="/lab/additional-property-bundle-update"/>)
							}
							
							bind(
								"additional-property",
								nodeSeq,
								"DeleteLink" -> {(nodeSeq: NodeSeq) => SHtml.ajaxButton(nodeSeq, () => delete)},
								"Name" -> SHtml.ajaxText(
									additionalProperty.name,
									value => {
										if (value.length < 1) {
											Alert("Name cannot be empty.")
										} else {
											additionalProperty.name(value)
											additionalProperty.save
											Noop
										}
									}
								),
								"Value" -> {
									if (Model.AdditionalPropertyType.String == additionalProperty.valueType.is) {
										SHtml.ajaxText(
											additionalProperty.value,
											value => {
												additionalProperty.value(value)
												additionalProperty.save
												Noop
											}
										)
									} else if (Model.AdditionalPropertyType.Boolean == additionalProperty.valueType.is) {
										SHtml.ajaxCheckbox(
											Model.AdditionalPropertyType.valueAsBoolean(additionalProperty.value),
											value => {
												additionalProperty.value(Model.AdditionalPropertyType.booleanAsValue(value))
												additionalProperty.save
												Noop
											}
										)
									} else {
										Text("")
									}
								}
							)
						})
						additionalPropertiesNodeSeq
					}}
				)
			} else {
				bind(
					"additional-property-bundle",
					xhtml,
					"Create" -> Text(""),
					"EmptyText" -> {(nodeSeq: NodeSeq) => {
						if (Model.AdditionalPropertyBundle.currentAdditionalPropertyBundle.is.additionalProperties.length > 0) {
							Text("")
						} else {
							nodeSeq
						}
					}},
					"List" -> {(nodeSeq: NodeSeq) => {
						val additionalPropertiesNodeSeq: NodeSeq = Model.AdditionalPropertyBundle.currentAdditionalPropertyBundle.is.additionalProperties.sort((a, b) => b.name.is.compareToIgnoreCase(a.name.is) > 0).flatMap(additionalProperty => {
							bind(
								"additional-property",
								nodeSeq,
								"Name" -> additionalProperty.name,
								"Value" -> additionalProperty.value
							)
						})
						additionalPropertiesNodeSeq
					}}
				)
			}
		}
	}
	
	def renderScripts: NodeSeq = {
		def showAdditionalPropertyBundle() = {
			SetHtml("additional-property-bundle", <lift:embed what="lab/additional-property-bundle" />)
		}
		
		def showAdditionalPropertyBundleUpdate() = {
			SetHtml("additional-property-bundle", <lift:embed what="lab/additional-property-bundle-update" />)
		}
		
		Script(
			Function(
				"showAdditionalPropertyBundle",
				Nil,
				SHtml.ajaxInvoke(showAdditionalPropertyBundle)._2
			)
		) ++
		Script(
			Function(
				"showAdditionalPropertyBundleUpdate",
				Nil,
				SHtml.ajaxInvoke(showAdditionalPropertyBundleUpdate)._2
			)
		)
	}
}