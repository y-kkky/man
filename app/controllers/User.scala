package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import models.mUser
import services.EmailService.sendEmail
import akka.util.Crypt.md5
import akka.util.Crypt.sha1

object User extends Controller with Secured {

  // -- Контроллер для пользователей

  // Формы (для входа, регистрации, редактирования профиля и восстановления пароля)
  val loginForm = Form(
    tuple(
      "email" -> nonEmptyText(5, 50),
      "password" -> nonEmptyText) verifying("Невірна пошта або пароль", result => result match {
      case (email, pass) => mUser.authenticate(email, pass).isDefined
    }))

  val registerForm = Form(
    tuple(
      "email" -> nonEmptyText,
      "name" -> nonEmptyText,
      "password" -> nonEmptyText,
      "confirm" -> nonEmptyText) verifying("Користувач з такою поштою вже зареєстрований", result => result match {
      case (email, name, password, confirm) => mUser.checkUser(email)
    })
  )

  val editForm = Form(
    tuple(
      "email" -> nonEmptyText(5, 50),
      "name" -> nonEmptyText,
      "password" -> text,
      "newpass" -> text,
      "confirm" -> text,
      "city" -> text,
      "school" -> text,
      "comments" -> text))

  val recoverForm = Form(
    "email" -> nonEmptyText.verifying ("Немає користувача з такою поштою", result => result match {
      case email => !mUser.checkUser(email)
    })
  )

  val lessonForm = Form(
    tuple(
      "ukr" -> boolean,
      "mat" -> boolean,
      "ang" -> boolean,
      "ino" -> boolean,
      "fiz" -> boolean,
      "him" -> boolean,
      "bio" -> boolean,
      "geo" -> boolean,
      //История Украины и всемирная история
      "isy" -> boolean,
      "isv" -> boolean,
      "lit" -> boolean,
      "rus" -> boolean
    )
  )

  /**
   * Регистрация
   */

  // Страница регистрации
  def register = Action {
    implicit request =>
      Ok(views.html.user.register(registerForm))
  }

  // Обработка данных, введенных в форму регистрации.
  def createUser = Action {
    implicit request =>
      registerForm.bindFromRequest.fold(
        formWithErrors => BadRequest(views.html.user.register(formWithErrors)),
        user => {
          if (user._3 != user._4) Redirect(routes.User.register).flashing("error" -> "Ви ввели різні паролі")
          else {
            mUser.create(user._1, user._2, user._3)
            sendEmail(user._1,
              s"""
              Вітаю, ${user._2}
              Ви були успішно зареєстровані!

              Ваша пошта: ${user._1}
              Ваш пароль: ${user._3}

              З повагою, Ярослав Круковський.
            """)
            Redirect(routes.Static.home).flashing(
              "success" -> "Ви були успішно зареєстровані")
          }
        })
  }

  /**
   * Аутентификация пользователя
   */

  // Отправляет клиенту страницу входа.
  def login = Action {
    implicit request =>
      Ok(views.html.user.login(loginForm))
  }

  // Обработка информации, веденной клиентом на странице входа.
  def authenticate = Action {
    implicit request =>
      loginForm.bindFromRequest.fold(
        formWithErrors => BadRequest(views.html.user.login(formWithErrors)),
        user => {
          val id = mUser.findByEmail(user._1).id
          Redirect(routes.User.profile(id)).withSession(Security.username -> user._1, "id" -> id.toString).flashing(
            "success" -> "Ви успішно ввійшли")
        })
  }

  // Выход из аккаунта и очистка сессии.
  def logout = Action {
    Redirect(routes.Static.home).withNewSession.flashing(
      "success" -> "Ви вийшли з аккаунту.")
  }

  /**
   * Все, что связано с пользователем (профиль, редактирование информации и т.д.)
   */

  //Страница редактирования профиля пользователя.
  def edit = withUser {
    user =>
      implicit request =>
        Ok(views.html.user.edit(gravatarFor(user.email), editForm.fill(user.email, user.name, "", "", "", user.city, user.school, user.comments), lessonForm))
  }

  // Обработка даных из формы редактирования профиля.
  def changeUser = withUser { uuser =>
    implicit request =>
      editForm.bindFromRequest.fold(
        formWithErrors => BadRequest(views.html.user.edit(gravatarFor(username(request).toString), formWithErrors, lessonForm)),
        user => {
          if(user._4 != "" && user._5 != ""){
            if(uuser.pass != mUser.hashPass(user._3))
              Redirect(routes.User.edit).flashing("error" -> "Ви неправильно ввели старий пароль")
            else if(user._4 != user._5)
              Redirect(routes.User.edit).flashing("error" -> "Ви неправильно ввели підтвердження пароля")
            else {
              mUser.edit(uuser.id, user._1, user._2, mUser.hashPass(user._4), user._6, user._7, user._8)
              Redirect(routes.User.profile(uuser.id)).flashing("success" -> "Пароль був успішно змінений").withSession(Security.username -> user._1)
            }
          }else{
            mUser.edit(uuser.id, user._1, user._2, uuser.pass, user._6, user._7, user._8)
            Redirect(routes.User.profile(uuser.id)).flashing("success" -> "Профіль був успішно змінений").withSession(Security.username -> user._1)
          }
        })
  }

  def changeLesson = withUser { user =>
    implicit request =>
      lessonForm.bindFromRequest.fold(
        formWithErrors => BadRequest(views.html.user.edit(gravatarFor(username(request).toString), editForm.fill(user.email, user.name, "", "", "", user.city, user.school, user.comments), lessonForm)),
        lessons => {
          val newlist = lessons.productIterator.toList
          val listComp = List(1, 2, 3, 4, 5, 6, 7, 8, 9, "a", "b", "c")
          var string: String = ""
          for(index <- 0 to 11)
            if(newlist(index)==true) string+=listComp(index)
          mUser.editLessons(user.id, string)
          Redirect(routes.User.profile(user.id)).flashing(
            "success" -> "Інформація про предмети була успішно змінена."
          )
        }
      )
  }

  // Отображение профиля пользователя.
  def profile(id: Int) = withUser {
    user =>
      implicit request =>
        val user = mUser.find(id)
        Ok(views.html.user.profile(user, gravatarFor(user.email)))
  }

  // Страница восстановления пароля.
  def forgotPassword = Action {
    implicit request =>
      Ok(views.html.user.recover(recoverForm))
  }

  // Письмо пользователю с ссылкой на страницу восстановления пароля.
  def recoverPassword = Action {
    implicit request =>
      recoverForm.bindFromRequest.fold(
        formWithErrors => BadRequest(views.html.user.recover(formWithErrors)),
        email => {
          val user = mUser.findByEmail(email)

          // Ссылка генерируется по принципу www.example.com/1/2 , где 1 - почта, а 2 - хешированый пароль
          val href = request.domain + routes.User.changePassword(email, sha1(user.pass))
          sendEmail(user.email,
            s"""
              Шановний ${
    user.name
    }!

              Ви отримали цей лист через те, що забули пароль (якщо це не так - видаліть лист).
              Для вас буде згенерований новий пароль. Перейдіть, будь ласка, за посиланням:
              $href

              З повагою, Ярослав Круковський.
            """.stripMargin)
          Redirect(routes.Static.home()).flashing(
            "success" -> "Перевірте почтову скриньку. Лист має надійти з хвилини на хвилину.")
        })
  }

  // Если пользователь правильно перешел по ссылке - генерация нового пароля и отправка его по почте
  def changePassword(email: String, hash: String) = Action {
    implicit request =>
    // Отсеивается случай, когда пользователь правильно набрал email, но неправильно - пароль
      try {
        val user = mUser.findByEmail(email)
        if (sha1(user.pass) == hash) {
          val newPass = randomPassword
          sendEmail(email,
            s"""
           Шановний ${
    user.name
    }!

           Ось нові дані для входу на сайт:

           e-mail: ${
    user.email
    }
           пароль: $newPass

           Ви можете прямо зараз перейти на сайт: ${
    request.domain + routes.Static.home
    }

           З повагою, Ярослав Круковський.
        """.stripMargin)
          mUser.edit(user.id, user.email, user.name, mUser.hashPass(newPass), user.city, user.school, user.comments)
          Redirect(routes.Static.home()).flashing(
            "success" -> "Новий пароль висланий на вашу поштову скриньку."
          )
        } else
        // Отсеивается случай, когда не email, ни хэш пароля введен неправильно
          NotFound(views.html.errors.onHandlerNotFound(request))
      } catch {
        case _: Throwable => NotFound(views.html.errors.onHandlerNotFound(request))
      }
  }

  // Подготовка к ЗНО
  // Главная страница подготовки
  def preparation = withUser {
    user =>
      implicit request =>
      val lessons = codeToLessonsArr(user.lessons)
      Ok(views.html.user.prep(user, lessons))
  }
  /**
   * Вспомагательные функции
   */
  //Генерация url для Gravatar с использыванием функции md5 библиотеки akka.util.Crypt
  def gravatarFor(email: String) = "http://www.gravatar.com/avatar/" + md5(email) toLowerCase

  def randomPassword: String = {
    randomString("abcdefghijklmnopqrstuvwxyz0123456789")(10)
  }

  // Generate a random string of length n from the given alphabet
  def randomString(alphabet: String)(n: Int): String = {
    val random = new scala.util.Random
    Stream.continually(random.nextInt(alphabet.size)).map(alphabet).take(n).mkString
  }

  def codeToLessons(code: String) = {
    var string: String = ""
    val lessons = Map('1' -> "Українська мова і література", '2' -> "Математика", '3' ->"Англійська мова",
      '4' -> "Іноземна мова", '5' -> "Фізика", '6' -> "Хімія", '7' -> "Біологія", '8' -> "Географія",
      '9' -> "Історія України",  'a' -> "Всесвітня історія", 'b' -> "Всесвітня література", 'c' -> "Російська мова")
    for(char <- code) string += (lessons(char)+". ")
    string
  }

  def codeToLessonsArr(code: String) = {
    val lessons = Map('1' -> "Українська мова і література", '2' -> "Математика", '3' ->"Англійська мова",
      '4' -> "Іноземна мова", '5' -> "Фізика", '6' -> "Хімія", '7' -> "Біологія", '8' -> "Географія",
      '9' -> "Історія України",  'a' -> "Всесвітня історія", 'b' -> "Всесвітня література", 'c' -> "Російська мова")
    val less = for(char <- code) yield (lessons(char))
    less
  }

  def rankToText(rank: Int) = {
    val ranks = loadXml("public/ranks.xml")
    val result = ranks \ ("r" + rank)
    result.text
  }

  def allRanks = {
    val ranks = loadXml("public/ranks.xml")
    (for (elem <- 1 to 10) yield ranks \ ("r" + elem)).toList
  }

  def loadXml(path: String) = scala.xml.XML.loadFile(path)

}


// Авторизация
trait Secured {

  // Получение email из сессии
  def username(request: RequestHeader) = request.session.get(Security.username)

  // Переадресация пользователя на страничку входа, при попытке зайти на запрещенные страницы.
  private def onUnauthorized(request: RequestHeader) = Results.Redirect(routes.User.login)

  // Проверка, есть ли email в сессии
  def withAuth(f: => String => Request[AnyContent] => Result) = {
    Security.Authenticated(username, onUnauthorized) {
      user =>
        Action(request => f(user)(request))
    }
  }

  // Предоставляет доступ к текущему пользователю в контроллерах
  def withUser(f: mUser => Request[AnyContent] => Result) = withAuth {
    username =>
      implicit request =>
        mUser.findByEmailOpt(username).map {
          user =>
            f(user)(request)
        }.getOrElse(onUnauthorized(request))
  }
}