package controllers

import play.api._
import play.api.mvc._

object Static extends Controller {

  def home = Action { implicit request => Ok(views.html.static_pages.home()) }

  def about = Action { implicit request => Ok(views.html.static_pages.about()) }

  def contact = Action { implicit request => Ok(views.html.static_pages.contact()) }
}