package models

import play.api.db.slick.Config.driver.simple._
import play.api.libs.json.Json
import scala.slick.lifted.Tag


case class CompanyCustomer(customerId: Long, companyId: Long, code: Long)

object CompanyCustomer {
  implicit val companyCustomerFrmt = Json.format[CompanyCustomer]
}

class CompanyCustomers(tag: Tag) extends Table[CompanyCustomer](tag, "CompanyCustomer") {

  def customerId = column[Long]("customer_id", O.NotNull)
  def companyId = column[Long]("company_id", O.NotNull)
  def code = column[Long]("code", O.NotNull)

  def * = (customerId, companyId, code) <> ((CompanyCustomer.apply _).tupled, CompanyCustomer.unapply _)
}

case class CompanyProduct(productId: Long, companyId: Long, code: Long)

object CompanyProduct {
  implicit val companyProductFrmt = Json.format[CompanyProduct]
}

class CompanyProducts(tag: Tag) extends Table[CompanyProduct](tag, "CompanyProduct") {

  def productId = column[Long]("product_id", O.NotNull)
  def companyId = column[Long]("company_id", O.NotNull)
  def code = column[Long]("code", O.NotNull)

  def * = (productId, companyId, code) <> ((CompanyProduct.apply _).tupled, CompanyProduct.unapply _)
}

case class Company(id: Option[Long] = None, name: String, email: String, phone: String)

object Company {
  implicit val companyFrmt = Json.format[Company]
}

class Companies(tag: Tag) extends Table[Company](tag, "Company") {

  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def name = column[String]("name", O.NotNull)
  def email = column[String]("email", O.NotNull)
  def phone = column[String]("phone", O.NotNull)

  def * = (id.?, name, email, phone) <> ((Company.apply _).tupled, Company.unapply _)
}

object Companies {

  val companies = TableQuery[Companies]


  /**
   * Construct the Map[String,String] needed to fill a select options set
   */
  def options(implicit s: Session): Seq[(String, String)] = {
    val query = (for {
      company <- companies
    } yield (company.id, company.name)).sortBy(_._2)
    query.list.map(row => (row._1.toString, row._2))
  }


  /**
   * Retrieve a company from the id
   * @param id
   */
  def findById(id: Long)(implicit s: Session): Option[Company] =
    companies.filter(_.id === id).firstOption

  /**
   * Count all companies
   */
  def count(implicit s: Session): Int =
    Query(companies.length).first

  /**
   * Count companies with a filter
   * @param filter
   */
  def count(filter: String)(implicit s: Session): Int =
    Query(companies.filter(_.name.toLowerCase like filter.toLowerCase).length).first

  /**
   * Return a page of Company
   * @param page
   * @param pageSize
   * @param orderBy
   * @param filter
   */
  def list(page: Int = 0, pageSize: Int = 10, orderBy: Int = 1, filter: String = "%")(implicit s: Session): CompanyPage = {

    val offset = pageSize * page
    val query =
      (for {
        company <- companies
        if company.name.toLowerCase like filter.toLowerCase()
      } yield company)
        .drop(offset)
        .take(pageSize)

    val totalRows = count(filter)
    val result = query.list

    CompanyPage(result, page, offset, totalRows)
  }

  /**
   * Return a list of Companies
   */
  def list()(implicit s:Session): List[Company] = {
    companies.sortBy(_.id).list
  }

  /**
   * Insert a new product
   * @param company
   */
  def insert(company: Company)(implicit s: Session) {
    companies.insert(company)
  }

  /**
   * Update a company
   * @param id
   * @param company
   */
  def update(id: Long, company: Company)(implicit s: Session) {
    val companyToUpdate: Company = company.copy(Some(id))
    companies.filter(_.id === id).update(companyToUpdate)
  }

  /**
   * Delete a company
   * @param id
   */
  def delete(id: Long)(implicit s: Session) {
    companies.filter(_.id === id).delete
  }
}
