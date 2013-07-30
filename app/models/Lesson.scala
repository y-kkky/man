package models

import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._

case class Lesson(id: Long, name: String)

case class Bilet(id: Long, lesson_id: Long, num: Int)

case class Question(id: Long, bilet_id: Long, text: String, answer: Long)

case class Variant(id: Long, question_id: Long, text: String)

object Lesson {
  val simple = {
    get[Int]("Lessons.id") ~
      get[String]("Lessons.name") map {
      case id ~ name => Lesson(id, name)
    }
  }

  def find(id: Long): Lesson = {
    DB.withConnection(implicit connection =>
      SQL("select * from Lessons where id={id}").on('id -> id).as(Lesson.simple.single)
    )
  }

  def findAll: Seq[Lesson] = {
    DB.withConnection { implicit connection =>
      SQL("select * from Lessons").as(Lesson.simple *)
    }
  }
}

object Bilet {
  val simple = {
    get[Int]("Bilets.id") ~
      get[Int]("Bilets.lesson_id") ~
      get[Int]("Bilets.num") map {
      case id ~ lesson_id ~ num => Bilet(id, lesson_id, num)
    }
  }

  def find(id: Long): Bilet = {
    DB.withConnection(implicit connection =>
      SQL("select * from Bilets where id={id}").on('id -> id).as(Bilet.simple.single)
    )
  }
}

object Question {
  val simple = {
    get[Int]("Questions.id") ~
      get[Int]("Questions.bilet_id") ~
      get[String]("Questions.text") ~
      get[Long]("Questions.answer") map {
      case id ~ bilet_id ~ text ~ answer => Question(id, bilet_id, text, answer)
    }
  }

  def find(id: Long): Question = {
    DB.withConnection(implicit connection =>
      SQL("select * from Questions where id={id}").on('id -> id).as(Question.simple.single)
    )
  }
}

object Variant {
  val simple = {
    get[Int]("Variant.id") ~
      get[Int]("Variant.question_id") ~
      get[String]("Variant.text") map {
      case id ~ question_id ~ text => Variant(id, question_id, text)
    }
  }

  def find(id: Long): Variant = {
    DB.withConnection(implicit connection =>
      SQL("select * from Variants where id={id}").on('id -> id).as(Variant.simple.single)
    )
  }
}
