package controllers

import play.api._
import play.api.mvc._
import models.Lesson

object Lessons extends Controller{

  def allLessons = Action {implicit request =>
    Ok(views.html.lessons.all(Lesson.findAll))
  }

}
