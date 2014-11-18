package models

import play.api.db.slick.Config.driver.simple._
import play.api.libs.json.Json
import scala.slick.lifted.Tag


case class Customer(id: Option[Long] = None, name: String, code: Long)

object Customer {
  implicit val customerFrmt = Json.format[Customer]
}

class Customers(tag: Tag) extends Table[Customer](tag, "Customer") {

  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def name = column[String]("name", O.NotNull)
  def code = column[Long]("code", O.NotNull)

  def * = (id.?, name, code) <> ((Customer.apply _).tupled, Customer.unapply _)
}

object Customers {

  val customers = TableQuery[Customers]


  /**
   * Construct the Map[String,String] needed to fill a select options set
   */
  def options(implicit s: Session): Seq[(String, String)] = {
    val query = (for {
      customer <- customers
    } yield (customer.id, customer.name)).sortBy(_._2)
    query.list.map(row => (row._1.toString, row._2))
  }


  /**
   * Retrieve a customer from the id
   * @param id
   */
  def findById(id: Long)(implicit s: Session): Option[Customer] =
    customers.filter(_.id === id).firstOption

  /**
   * Count all customers
   */
  def count(implicit s: Session): Int =
    Query(customers.length).first

  /**
   * Count customers with a filter
   * @param filter
   */
  def count(filter: String)(implicit s: Session): Int =
    Query(customers.filter(_.name.toLowerCase like filter.toLowerCase).length).first

  /**
   * Return a page of Customer
   * @param page
   * @param pageSize
   * @param orderBy
   * @param filter
   */
  def list(page: Int = 0, pageSize: Int = 10, orderBy: Int = 1, filter: String = "%")(implicit s: Session): CustomerPage = {

    val offset = pageSize * page
    val query =
      (for {
        customer <- customers
        if customer.name.toLowerCase like filter.toLowerCase()
      } yield customer)
        .drop(offset)
        .take(pageSize)

    val totalRows = count(filter)
    val result = query.list

    CustomerPage(result, page, offset, totalRows)
  }

  /**
   * Insert a new customer
   * @param customer
   */
  def insert(customer: Customer)(implicit s: Session) {
    customers.insert(customer)
  }

  /**
   * Update a customer
   * @param id
   * @param customer
   */
  def update(id: Long, customer: Customer)(implicit s: Session) {
    val customerToUpdate: Customer = customer.copy(Some(id))
    customers.filter(_.id === id).update(customerToUpdate)
  }

  /**
   * Delete a customer
   * @param id
   */
  def delete(id: Long)(implicit s: Session) {
    customers.filter(_.id === id).delete
  }
}
