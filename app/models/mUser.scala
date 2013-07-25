package models

import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._
import akka.util.Crypt

case class mUser(id: Int, regtime: String, email: String, name: String,
                 city: String, school: String, comments: String, pass: String)

object mUser {
  
  // -- Parsers
  
  /**
   * Parse a User from a ResultSet
   */
  val simple = {
    get[Int]("Users.id") ~
    get[String]("Users.regtime") ~
    get[String]("Users.email") ~
    get[String]("Users.name") ~
    get[String]("Users.city") ~
    get[String]("Users.school") ~
    get[String]("Users.comments") ~
    get[String]("Users.pass") map {
      case id~regtime~email~name~city~school~comments~pass => mUser(id, regtime, email, name, city, school, comments, pass)
    }
  }
  
  // -- Queries
  
  /**
   * Retrieve a User from email.
   */
  def findByEmail(email: String): mUser = {
    DB.withConnection { implicit connection =>
      SQL("select * from Users where email = {email}").on(
        'email -> email
      ).as(mUser.simple.single)
    }
  }
  
   def findByEmailOpt(email: String): Option[mUser] = {
    DB.withConnection { implicit connection =>
      SQL("select * from Users where email = {email}").on(
        'email -> email
      ).as(mUser.simple.singleOpt)
    }
  }
  
  /**
   * Retrieve a User from id.
   */
  def find(id: Int): mUser = {
    DB.withConnection { implicit connection =>
      SQL("select * from Users where id = {id}").on(
        'id -> id
      ).as(mUser.simple.single)
    }
  }
  
  
  /**
   * Retrieve all users.
   */
  def findAll: Seq[mUser] = {
    DB.withConnection { implicit connection =>
      SQL("select * from Users").as(mUser.simple *)
    }
  }
  
  /**
   * Authenticate a User.
   */
  def authenticate(email: String, pass: String): Option[mUser] = {
    DB.withConnection { implicit connection =>
      SQL(
        """
         select * from Users where 
         email = {email} and pass = {pass}
        """
      ).on(
        'email -> email,
        'pass -> hashPass(pass)
      ).as(mUser.simple.singleOpt)
    }
  }

  /**
   * Check, if user is registered
   */
  def checkUser(email: String): Boolean = {
    if(isEmpty) true
    else {
      try {
        DB.withConnection { implicit connection =>
          SQL("SELECT * from Users where email = {email}").on('email -> email).as(mUser.simple.single)
        }
        false
      } catch {
        case _ : Any => true
      }
    }
  }

  def isEmpty: Boolean = {
    if(findAll.isEmpty) true else false
  }

  /**
   * Create a User.
   */
  def create(email: String, name: String, pass: String) = {
    DB.withConnection { implicit connection =>
      val timestamp: Long = System.currentTimeMillis
      SQL(
        """
          insert into Users (regtime, email,name,pass,city,school,comments) values (
            {timestamp}, {email}, {name}, {pass}, {city}, {school}, {comments}
          )
        """
      ).on(
        'timestamp -> timestamp,
        'email -> email,
        'name -> name,
        'pass -> hashPass(pass),
        'city -> "",
        'school -> "",
        'comments -> ""
      ).executeUpdate()
    }
  }
  
  def edit(id: Int, email: String, name: String, pass: String, city: String, school: String, comments: String) = {
    DB.withConnection { implicit connection => 
     SQL(
     """
         update Users set email={email}, name={name}, city={city}, school={school}, comments={comments}, pass={pass} where id={id}
     """
     ).on(
         'email -> email,
         'name -> name,
         'city -> city,
         'school -> school,
         'comments -> comments,
         'id -> id,
         'pass -> hashPass(pass)
     ).executeUpdate()  
    }
  }

  def hashPass(password: String): String =
    Crypt.sha1(Crypt.md5(Crypt.sha1(password)))

}