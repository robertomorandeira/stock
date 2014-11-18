package controllers


import models._
import play.api.data._
import play.api.data.Forms._
import play.api.libs.json.Json
import play.api.libs.json.Json._
import play.api.mvc._
import play.api.db.slick._

import play.api.data.format.Formats._


object ProductApi extends Controller {

  //JSON read/write
  implicit val productFormat = Json.format[Product]

  val productForm = Form(
    mapping(
      "id" -> optional(longNumber),
      "code" -> longNumber,
      "name" -> nonEmptyText,
      "price" -> of[Double]
    )(Product.apply)(Product.unapply)
  )

  def getProducts = DBAction { implicit rs =>
    Ok(toJson(Products.list()))
  }

  def getProduct(id: Long) = DBAction { implicit rs =>
    Products.findById(id).map { product =>
      Ok(toJson(product))
    }.getOrElse(NotFound)
  }

  def addProduct = DBAction(parse.json) { implicit rs =>
    productForm.bindFromRequest.fold(
      formWithErrors => BadRequest,
      product => {
        Products.insert(product)
        Created(toJson(product))
      }
    )
  }

  def updateProduct(id: Long) = DBAction(parse.json) { implicit rs =>
    productForm.bindFromRequest.fold(
      formWithErrors => BadRequest,
      product => {
        Products.update(id, product)
        Ok
      }
    )
  }

  def deleteProduct(id: Long) = DBAction(parse.empty) { implicit rs =>
    Products.delete(id)
    Ok
  }

 }
