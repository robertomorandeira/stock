package models

import play.api.db.slick.Config.driver.simple._
import play.api.libs.json.Json
import scala.slick.lifted.Tag


case class User(id: Option[Long] = None, email: String, password: String, name: String, companyId: Option[Long] = None)

object User {
  implicit val userFrmt = Json.format[User]
}

class Users(tag: Tag) extends Table[User](tag, "User") {

  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def email = column[String]("email", O.NotNull)
  def password = column[String]("password", O.NotNull)
  def name = column[String]("name", O.NotNull)
  def companyId = column[Long]("company_id", O.Nullable)

  def emailIdx = index("idx_email", (email), unique = true)

  def * = (id.?, email, password, name, companyId.?) <> ((User.apply _).tupled, User.unapply _)
}

object Users {

  val users = TableQuery[Users]

  /**
   * Retrieve a user from the id
   * @param id
   */
  def findById(id: Long)(implicit s: Session): Option[User] =
    users.filter(_.id === id).firstOption

  /**
   * Count all users
   */
  def count(implicit s: Session): Int =
    Query(users.length).first

  /**
   * Count users with a filter
   * @param filter
   */
  def count(filter: String)(implicit s: Session): Int =
    Query(users.filter(_.name.toLowerCase like filter.toLowerCase).length).first

  /**
   * Return a page of User
   * @param page
   * @param pageSize
   * @param orderBy
   * @param filter
   */
  def list(page: Int = 0, pageSize: Int = 10, orderBy: Int = 1, filter: String = "%")(implicit s: Session): UserPage = {

    val offset = pageSize * page
    val query =
      (for {
        user <- users
        if user.name.toLowerCase like filter.toLowerCase()
      } yield user)
        .drop(offset)
        .take(pageSize)

    val totalRows = count(filter)
    val result = query.list

    UserPage(result, page, offset, totalRows)
  }

  /**
   * Insert a new user
   * @param user
   */
  def insert(user: User)(implicit s: Session) {
    users.insert(user)
  }

  /**
   * Update a user
   * @param id
   * @param user
   */
  def update(id: Long, user: User)(implicit s: Session) {
    val userToUpdate: User = user.copy(Some(id))
    users.filter(_.id === id).update(userToUpdate)
  }

  /**
   * Delete a user
   * @param id
   */
  def delete(id: Long)(implicit s: Session) {
    users.filter(_.id === id).delete
  }
}
