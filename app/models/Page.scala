package models

import play.api.libs.json.Json


trait Page[A] {
  val items: Seq[A]
  val page: Int
  val offset: Long
  val total: Long

  lazy val prev = Option(page - 1).filter(_ >= 0)
  lazy val next = Option(page + 1).filter(_ => (offset + items.size) < total)
}


case class ProductPage(items: Seq[Product], page: Int, offset: Long, total: Long) extends Page[Product]
case class CompanyPage(items: Seq[Company], page: Int, offset: Long, total: Long) extends Page[Company]
case class CustomerPage(items: Seq[Customer], page: Int, offset: Long, total: Long) extends Page[Customer]
case class OrderPage(items: Seq[Order], page: Int, offset: Long, total: Long) extends Page[Order]
case class UserPage(items: Seq[User], page: Int, offset: Long, total: Long) extends Page[User]

object Page {
  implicit def productPageWriter = Json.writes[ProductPage]
  implicit def companyPageWriter = Json.writes[CompanyPage]
  implicit def customerPageWriter = Json.writes[CustomerPage]
  implicit def orderPageWriter = Json.writes[OrderPage]
  implicit def userPageWriter = Json.writes[UserPage]
}