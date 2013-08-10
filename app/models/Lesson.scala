package models

import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._

case class Lesson(id: Long, name: String)

case class Bilet(id: Long, lesson_id: Long, num: Int)

case class Question(id: Long, bilet_id: Long, typ: Int, text: String, answer: Long)

case class Variant(id: Long, question_id: Long, text: String)

case class Stat(user_id: Long, lesson_id: Long, bilet_id: Long, question_id: Long, answer: Long)

case class BiletStat(user_id: Long, bilet_id: Long, perc: Int)

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

  def findByName(name: String): Lesson = {
    DB.withConnection(implicit connection =>
      SQL("select * from Lessons where name={name}").on(
	'name -> name).as(Lesson.simple.single)
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

  def inLesson(lesson_id: Long): List[Bilet] = {
    DB.withConnection(implicit connection => 
      SQL("select * from Bilets where lesson_id={lesson_id}").on(
	'lesson_id -> lesson_id 
      ).as(Bilet.simple *)
    )
  }
}

object Question {
  val simple = {
    get[Int]("Questions.id") ~
      get[Int]("Questions.bilet_id") ~
      get[Int]("Questions.typ") ~ 
      get[String]("Questions.text") ~
      get[Long]("Questions.answer") map {
      case id ~ bilet_id ~ typ ~ text ~ answer => Question(id, bilet_id, typ, text, answer)
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

object Stat {
  val simple = {
    get[Long]("Stat.user_id") ~
    get[Long]("Stat.lesson_id") ~
    get[Long]("Stat.bilet_id") ~
    get[Long]("Stat.question_id") ~
    get[Long]("Stat.answer") map {
    case user_id ~ lesson_id ~ bilet_id ~ question_id ~ answer =>
      Stat(user_id, lesson_id, bilet_id, question_id, answer)
   }
 }
} 

object BiletStat {
  val simple = {
    get[Long]("BiletStat.user_id") ~ 
    get[Long]("BiletStat.bilet_id") ~ 
    get[Int]("BiletStat.perc") map {
      case user_id ~ bilet_id ~ perc => BiletStat(user_id , bilet_id , perc) 
    }
  }

  def find(user_id: Long, bilet_id: Long): BiletStat = {
   try{
     DB.withConnection(implicit connection =>
       SQL("select * from BiletStat where user_id={user_id} and bilet_id={bilet_id}").on(
	 'user_id -> user_id,
	 'bilet_id -> bilet_id
       ).as(BiletStat.simple.single)
    )} catch{
       case _: Exception => BiletStat(0, 0, 0)
     }
  }

  def solved(user_id: Long, bilet_id: Long):Boolean = {
    var bilStat = BiletStat.find(user_id, bilet_id)
    if(bilStat.perc > 70) true else false
  }
}
