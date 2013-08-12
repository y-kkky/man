package controllers

import play.api._
import play.api.mvc._
import models.Lesson
import models.Bilet
import models.Stat
import models.BiletStat
import models.Question
import models.Variant
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

  def bilet(bilet_id: Long) = withUser{ user => implicit request =>
    // На случай, если глобал не будет работать
    /*if(Bilet.exists(bilet_id))
      Ok(views.html.lessons.bilet(Bilet.find(bilet_id)))
    else
      NotFound(views.html.errors.onHandlerNotFound(request))*/
    // val bilet = Bilet.find(bilet_id)
    val questions = Question.findByBilet(bilet_id)
    if(questions.isEmpty)
      NotFound(views.html.errors.onHandlerNotFound(request))
    else
      Ok(views.html.lessons.bilet(questions, bilet_id))
  }

  def showQuestion(question: Question, number: Int) = {
    var result = s"""<div id='question'>${number+1}. ${question.text}<br>
    <img src='${question.image}'/>"""
    val variants = Variant.findByQuestion(question.id)
    if(question.typ==1){
      var varstring = ""
      for(variant <- variants) varstring += s"<input type='radio' name='f${question.id}' value='${variant.text}'>${variant.text}</input><br>"
      result += s"""
      <br>$varstring
      """
    }else if(question.typ==2){
      var varstring = ""
      val lis = split(variants, variants.length/2)
      for(variant <- lis(0)){
	varstring += s"<tr><td align=right>${variant.text} ---- </td><td><select name='${variant.id}s${question.id}'><option disabled selected>Оберіть відповідь</option>"
	for(option <- lis(1))
	  varstring += "<option>" + option.text + "</option>"
	varstring += "</select></td><br>"
      }
      result += s"""
      <table>$varstring</table>
      """
    }else if(question.typ==3){
      result += s"""
      <br><input type='text' name='t${question.id}'/>
      """
    }
    result += "</div><div id='line'></div>"
    result
  }
  
  def parseQuestion = withUser {user => request =>
    var ra = 0
    var max = 0
    val forma = request.body.asFormUrlEncoded
    val bilet = Bilet.find((forma.get("bilet_id")(0)).toLong)
    //if(Stat.exists(user.id, bilet.id)) 
    Stat.deleteResolve(user.id, bilet.id)
    val questions = Question.findByBilet(bilet.id)
    for(question <- questions){
      var answer = ""
      var right = 0
      if(question.typ==1){
	answer = forma.get("f"+question.id)(0)
	if(question.answer == answer) {ra+=1; right=1}
	max += 1
      }else if(question.typ==2){
	val variants = Variant.findByQuestion(question.id)
	val lis = split(variants, variants.length/2)
	answer = ""
	for(variant <- lis(0))
	  answer += (forma.get(variant.id + "s" + question.id)(0)) + "~"
	val right_answer = (question.answer).split("~")
	val user_answer = answer.split("~")
	val length = right_answer.length
	for(i <- 0 to (length-1))
	  if(right_answer(i)==user_answer(i)) ra += 1
	max += length
	if(ra == max) right = 1
      }else if(question.typ==3){
	answer = forma.get("t"+question.id)(0)
	if(answer == question.answer) {ra += 2; right = 1}
	max += 2
      }
      Stat.newStat(user.id, bilet.id, question.id, right, answer)
    }
    BiletStat.newBiletStat(user.id, bilet.id, ra, max)
    Redirect(routes.Static.home)
  }

  def split[A](xs: List[A], n: Int): List[List[A]] = {
    if (xs.size <= n) xs :: Nil
    else (xs take n) :: split(xs drop n, n)
  }
}
