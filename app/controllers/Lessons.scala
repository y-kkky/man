package controllers

import play.api._
import play.api.mvc._
import models.Lesson
import models.Bilet
import models.BiletStat
import controllers.User.withUser

object Lessons extends Controller{
  
  def allLessons = Action {implicit request =>
    Ok(views.html.lessons.all(Lesson.findAll))
  }
  
  def lesson(id: Long) = Action {implicit request =>
    val lesson = Lesson.find(id)
    
    Ok(views.html.lessons.lesson(lesson))
  }

  def prep(id: Long) = withUser {user => implicit request =>
    val lesson = Lesson.find(id)
    val bilets: List[Bilet] = Bilet.inLesson(lesson.id)
    var counter: Int = 0
    Ok(views.html.lessons.prep(user, lesson, bilets, counter))
  }

  def lessonStat(user_id: Long, lesson_id: Long):Int = {
    val bilets: List[Bilet] = Bilet.inLesson(lesson_id)
    var bilLength = 0
    if(bilets.length == 0) bilLength = 1 else bilLength = bilets.length 
    var counter: Int = 0
    for(bilet <- bilets)
      if(BiletStat.solved(user_id, bilet.id)) counter += 1
    val result = (counter*100)/bilLength
    // bilLength - 100 %
    // counter - x %
    result
  }
}
