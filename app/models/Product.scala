package models

import play.api.db.slick.Config.driver.simple._
import play.api.libs.json.Json
import scala.slick.lifted.Tag


case class Product(id: Option[Long] = None, code: Long, name: String, price: Double)

object Product {
  implicit val productFrmt = Json.format[Product]
}

class Products(tag: Tag) extends Table[Product](tag, "Product") {

  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def code = column[Long]("code", O.NotNull)
  def name = column[String]("name", O.NotNull)
  def price = column[Double]("price", O.NotNull)

  def * = (id.?, code, name, price) <> ((Product.apply _).tupled, Product.unapply _)
}

object Products {

  val products = TableQuery[Products]


  /**
   * Construct the Map[String,String] needed to fill a select options set
   */
  def options(implicit s: Session): Seq[(String, String)] = {
    val query = (for {
      product <- products
    } yield (product.id, product.name)).sortBy(_._2)
    query.list.map(row => (row._1.toString, row._2))
  }


  /**
   * Retrieve a product from the id
   * @param id
   */
  def findById(id: Long)(implicit s: Session): Option[Product] =
    products.filter(_.id === id).firstOption

  /**
   * Count all products
   */
  def count(implicit s: Session): Int =
    Query(products.length).first

  /**
   * Count products with a filter
   * @param filter
   */
  def count(filter: String)(implicit s: Session): Int =
    Query(products.filter(_.name.toLowerCase like filter.toLowerCase).length).first

  /**
   * Return a page of Product
   * @param page
   * @param pageSize
   * @param orderBy
   * @param filter
   */
  def list(page: Int = 0, pageSize: Int = 10, orderBy: Int = 1, filter: String = "%")(implicit s: Session): ProductPage = {

    val offset = pageSize * page
    val query =
      (for {
        product <- products
        if product.name.toLowerCase like filter.toLowerCase()
      } yield product)
        .drop(offset)
        .take(pageSize)

    val totalRows = count(filter)
    val result = query.list

    ProductPage(result, page, offset, totalRows)
  }

  /**
   * Insert a new product
   * @param product
   */
  def insert(product: Product)(implicit s: Session) {
    products.insert(product)
  }

  /**
   * Update a product
   * @param id
   * @param product
   */
  def update(id: Long, product: Product)(implicit s: Session) {
    val productToUpdate: Product = product.copy(Some(id))
    products.filter(_.id === id).update(productToUpdate)
  }

  /**
   * Delete a product
   * @param id
   */
  def delete(id: Long)(implicit s: Session) {
    products.filter(_.id === id).delete
  }
}
