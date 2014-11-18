package models

import java.sql.Date
import play.api.db.slick.Config.driver.simple._
import play.api.libs.json.Json
import scala.slick.lifted.Tag

case class Status(id: Option[Int] = None, name: String)

object Status {
  implicit val statusFrmt = Json.format[Status]
}

class Statuses(tag: Tag) extends Table[Status](tag, "Status") {

  def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
  def name = column[String]("name", O.NotNull)

  def * = (id.?, name) <> ((Status.apply _).tupled, Status.unapply _)
}

object Statuses {

  val statuses = TableQuery[Statuses]
}


case class Order(id: Option[Long] = None, customerdId: Long, productId: Long, statusId: Int, price: Double, created: Date, updated: Date)

object Order {
  implicit val orderFrmt = Json.format[Order]
}

class Orders(tag: Tag) extends Table[Order](tag, "Order") {

  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def customerId = column[Long]("customer_id", O.NotNull)
  def productId = column[Long]("product_id", O.NotNull)
  def statusId = column[Int]("status_id", O.NotNull)
  def price = column[Double]("price", O.NotNull)
  def created = column[Date]("created", O.NotNull)
  def updated = column[Date]("updated", O.NotNull)

  def customer = foreignKey("customer_fk", customerId, Customers.customers)(_.id)
  def product = foreignKey("product_fk", productId, Products.products)(_.id)
  def status = foreignKey("status_fk", statusId, Statuses.statuses)(_.id)

  def * = (id.?, customerId, productId, statusId, price, created, updated) <> ((Order.apply _).tupled, Order.unapply _)
}

object Orders {

  val orders = TableQuery[Orders]

  /**
   * Retrieve a order from the id
   * @param id
   */
  def findById(id: Long)(implicit s: Session): Option[Order] =
    orders.filter(_.id === id).firstOption

  /**
   * Count all orders
   */
  def count(implicit s: Session): Int =
    Query(orders.length).first

  /**
   * Return a page of Order
   * @param page
   * @param pageSize
   * @param orderBy
   */
  def list(page: Int = 0, pageSize: Int = 10, orderBy: Int = 1)(implicit s: Session): OrderPage = {

    val offset = pageSize * page
    val query =
      (for {
        order <- orders
      } yield order)
        .drop(offset)
        .take(pageSize)

    val totalRows = count
    val result = query.list

    OrderPage(result, page, offset, totalRows)
  }

  /**
   * Insert a new order
   * @param order
   */
  def insert(order: Order)(implicit s: Session) {
    orders.insert(order)
  }

  /**
   * Update a order
   * @param id
   * @param order
   */
  def update(id: Long, order: Order)(implicit s: Session) {
    val orderToUpdate: Order = order.copy(Some(id))
    orders.filter(_.id === id).update(orderToUpdate)
  }

  /**
   * Delete a order
   * @param id
   */
  def delete(id: Long)(implicit s: Session) {
    orders.filter(_.id === id).delete
  }
}
