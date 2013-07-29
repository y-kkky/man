package controllers

import play.api._
import play.api.mvc._
import java.util.Random

object Static extends Controller {

  def home = Action {
    implicit request => Ok(views.html.static_pages.home())
  }

  def about = Action {
    implicit request => Ok(views.html.static_pages.rules())
  }

  def contact = Action {
    implicit request => Ok(views.html.static_pages.contact())
  }

  def zno = Action {
    implicit request => Ok(views.html.static_pages.zno())
  }

  def randProverb = {
    val proverbs = scala.xml.XML.loadFile("public/proverb.xml")
    // Число пословиц
    val result = proverbs \ ("p" + new Random().nextInt(15).toString)
    result.text
  }
}