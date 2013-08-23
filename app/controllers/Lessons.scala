package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import models._
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
    val time = (Bilet.find(bilet_id)).time
    if(questions.isEmpty)
      NotFound(views.html.errors.onHandlerNotFound(request))
    else {
      val shuffled = util.Random.shuffle(questions)
      Ok(views.html.lessons.bilet(shuffled, bilet_id, time))
    }
  }

  def biletStat(bilet_id: Long) = withUser{ user => implicit request =>
    val questions = Question.findByBilet(bilet_id)
    val stat = Stat.find(user.id, bilet_id)
    if(questions.isEmpty || stat.isEmpty)
      NotFound(views.html.errors.onHandlerNotFound(request))
    else{
      Ok(views.html.lessons.biletstat(questions, bilet_id, stat))
    }
  }

  def showQuestion(question: Question, number: Int) = {
    var result = s"""<div id='question'>${number+1}. ${question.text}<br><img src='${question.image}'/>"""
    val variants = Variant.findByQuestion(question.id)
    if(question.typ==1){
      var varstring = ""
      val shuffled = util.Random.shuffle(variants)
      for(variant <- shuffled) varstring += s"<input type='radio' name='f${question.id}' value='${variant.text}'>${variant.text}</input><br>"
      result += s"""
      <br>$varstring
      """
    }else if(question.typ==2){
      result = s"""<div id='question'>${number+1}. ${question.text}
<img src='${question.image}'/>"""
      var varstring = ""
      val lis = split(variants, variants.length/2)
      val shuffled = util.Random.shuffle(lis(1))
      for(variant <- lis(0)){
	varstring += s"<tr><td align=right>${variant.text} ---- </td><td><select name='${variant.id}s${question.id}'><option disabled selected>Оберіть відповідь</option>"
	for(option <- shuffled)
	  varstring += "<option>" + option.text + "</option>"
	varstring += "</select></td><br>"
      }
      result += s"<table>$varstring</table>"
    }else if(question.typ==3){
      result += s"""
      <br><input type='text' name='t${question.id}'/>
      """
    }
    result += "</div><div id='line'></div>"
    result
  }

  def showQuestionStat(question: Question, number: Int, stat: Stat) = {
    var result = ""
    if(question.image!="")
      result = s"""<div id='questionstat' class='well'>${number+1}. ${question.text}<br><img src='${question.image}'/>"""
    else 
      result = s"""<div id='questionstat' class='well'>${number+1}. ${question.text}"""
    val variants = Variant.findByQuestion(question.id)
    if(question.typ == 1 || question.typ == 3){
      if(stat.answer == "none" || stat.answer == ""){
	result += "<p>Правильна відповідь: " + question.answer + "</p>"
	result += "<p>Ви не відповідали на це питання.</p>"
      }
      else{
	result += "<p>Правильна відповідь: " + question.answer + "</p><p>Ваша відповідь: "
	if(stat.right==1)
	  result+="<span style='color: green;'>"
	else
	  result+="<span style='color: red;'>"
	result += stat.answer
	result += "</span></p>"
	}
    }else if(question.typ==2){
      result = s"""<div id='question' class='well'>${number+1}. ${question.text}
<img src='${question.image}'/>"""
      var varstring = ""
      val lis = split(variants, variants.length/2)
      val right_answer = (question.answer).split("~")
      val user_answer = (stat.answer).split("~")
      val length = user_answer.length
      var color = ""
      for(i <- 0 to (length-1)){
	if(user_answer(i)==right_answer(i))
	  color = "green"
	else
	  color = "red"
	varstring += s"<tr><td align=right><font color="+color+ s">${(lis(0))(i).text} ---- </font></td><td>"
	if(user_answer(i)=="none")
	  varstring += s"<font color=${color}>Ви не відповіли.</font></td><td>(Правильно - ${(lis(1))(i).text})</td></tr><br>"
	else
	  if(user_answer(i)==right_answer(i))
	    varstring += s"<font color=${color}>${user_answer(i)}</font></td></tr><br>"
	  else
	    varstring += s"<font color=${color}>${user_answer(i)}</font></td><td>(Правильно - ${(lis(1))(i).text})</td></tr><br>"
      }
      result += s"<table>$varstring</table>"
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
	try{
	  answer = forma.get("f"+question.id)(0)
	  if(question.answer == answer) {ra+=1; right=1}
	}catch {
	  case _: Throwable => answer = "none"
	}
	max += 1
      }else if(question.typ==2){
	val variants = Variant.findByQuestion(question.id)
	val lis = split(variants, variants.length/2)
	answer = ""
	for(variant <- lis(0)){
	  try{
	    answer += (forma.get(variant.id + "s" + question.id)(0)) + "~"
	  }catch {
	    case _: Throwable => answer += "none~"
	  }
	}
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
    val perc = (ra*100)/max
    if(perc>70)
      Redirect(routes.Lessons.biletStat(bilet.id)).flashing(
	"success" -> s"Зараховано! Ви набрали ${perc}%." 
      )
    else
      Redirect(routes.Lessons.biletStat(bilet.id)).flashing(
	"error" -> s"Не зараховано :(. Ви набрали ${perc}%."
      )
  }

  // Ежедневное соревнование
  def daily = withUser { user => implicit request =>
    // Шаг первый: проверяем, не проходил ли принимал ли участие пользователь сегодня в ежедневке
    val current_day = currentDate
    //Текущий день уже есть. Найдем предметы юзера
    val lessons = User.codeToLessonsList(user.lessons)
    //Предметы есть. Теперь создадим список id Доступных предметов
    var questionsAllowed: List[Question] = List()
    import scala.util.Random
    val rand = new Random()
    if(lessons.length < 3){
      Redirect(routes.User.edit).flashing(
	"error" -> "Для участі в щоденних змаганнях оберіть щонайменше 3 предмети."
      )
    }else{
      for(lesson <- lessons){
	for(bilet <- Bilet.inLesson(lesson.id)){
	  questionsAllowed = questionsAllowed ::: Question.findByBilet(bilet.id)
	}
      }
      val microStat = microDailyStat.getByTime(user.id, current_day)
      if(microStat.length != 0) {
	Redirect(routes.Static.home).flashing("error" -> "Ви вже брали участь у змаганнях сьогодні.")
      }else{
	// Начинаем по крупицам собирать нужные 18 вопросов
	var randQuestions: List[Question] = List()
	// Начнем с первого типа, и т.д.
	val firstType = questionsAllowed.filter(x => x.typ == 1)
	val secondType = questionsAllowed.filter(x => x.typ == 2)
	val thirdType = questionsAllowed.filter(x => x.typ == 3)
	if(firstType.length < 6 || secondType.length < 6 || thirdType.length < 6)
	  Redirect(routes.Static.home).flashing("error" -> "З обраних вами предметів недостатньо питань для щоденного змагання")
	else{
	  for(i <- 1 to 6){
	    var first = firstType(rand.nextInt(firstType.length))
	    var second = secondType(rand.nextInt(secondType.length))
	    var third = thirdType(rand.nextInt(thirdType.length))
	    if(randQuestions.contains(first)){
	      while(randQuestions.contains(first)){
		first = firstType(rand.nextInt(firstType.length))
	      }
	    }
	    if(randQuestions.contains(second)){
	      while(randQuestions.contains(second)){
		second = secondType(rand.nextInt(secondType.length))
	  }
	    }
	    if(randQuestions.contains(third)){
	      while(randQuestions.contains(third)){
		third = thirdType(rand.nextInt(thirdType.length))
	      }
	    }  
	    randQuestions = rand.shuffle(randQuestions :+ first :+ second :+ third)
	  }
	  var bilet_ids_last = ""
	  randQuestions.foreach(x => bilet_ids_last+=(x.id+"~"))
	  microDailyStat.create(user.id, current_day, 0, bilet_ids_last)
	  Ok(views.html.lessons.daily(randQuestions, "00:05:00"))
	}	  
      }      
    }
		    }

   // Вспомогательная функция, опредетяет, записано ли в масиве уже значение

  def dailyEngine = withUser { user => request =>
    // Находим текущую дату
    var ra: Double = 0
    var max: Double = 0			     
    val current_date = currentDate
    val microDaily = microDailyStat.getByTime(user.id, current_date)
    // Получаем вопросы, на которые отвечал пользователь
    val ids_list: List[String] = (microDaily(0).ids).split("~").toList
    var questions: List[Question] = List()
    for(id <- ids_list)
      questions = questions :+ Question.find(id.toLong)
//    ids_list.foreach(id => questions :+ Question.find(id.toLong))
    val forma = request.body.asFormUrlEncoded
    for(question <- questions){
      var answer = ""
      var right = 0
      if(question.typ==1){
	try{
	  answer = forma.get("f"+question.id)(0)
	  if(question.answer == answer) {ra+=1; right=1}
	}catch {
	  case _: Throwable => answer = "none"
	}
	max += 1
      }else if(question.typ==2){
	val variants = Variant.findByQuestion(question.id)
	val lis = split(variants, variants.length/2)
	answer = ""
	for(variant <- lis(0)){
	  try{
	    answer += (forma.get(variant.id + "s" + question.id)(0)) + "~"
	  }catch {
	    case _: Throwable => answer += "none~"
	  }
	}
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
      DailyStat.newDailyStat(user.id, question.id, current_date, right, answer)
    }
    val startTime = request.cookies.get("tiz").getOrElse(new play.api.mvc.Cookie("tiz", "0")).value
    val endTime = request.cookies.get("zit").getOrElse(new play.api.mvc.Cookie("vit", "0")).value
    val resultTime = endTime.toLong - startTime.toLong
    // Время есть, теперь находим процент
    val perc: Double = (ra * 100)/max
    microDailyStat.update(user.id, resultTime, current_date, ra)
    Redirect(routes.Lessons.dailyStatGet(current_date)).flashing("success" -> s"Ваш результат: ${perc} балів зі 100")
  }

  // Отображение страницы статистики
  def dailyStatGet(time: String) = withUser {user => implicit request =>
    val microDaily = microDailyStat.getByTime(user.id, time)
    if(microDaily.length == 0)
      NotFound(views.html.errors.onHandlerNotFound(request))
    else{
      // Получаем вопросы, на которые отвечал пользователь
      val ids_list: List[String] = (microDaily(0).ids).split("~").toList
      var questions: List[Question] = List()
      var tempQuest: Question = new Question(0, 0, 0, "", "", "")
      var dailyStatList: List[DailyStat] = List()
      for(id <- ids_list){
	tempQuest = Question.find(id.toLong)  
	questions = questions :+ tempQuest
	dailyStatList = dailyStatList :+ DailyStat.find(user.id, id.toLong, time)
      }					     
      Ok(views.html.lessons.dailystat(questions, dailyStatList))
    }
  }

  def dailyStatRedirect = withUser{ user => implicit request =>
    val forma = request.body.asFormUrlEncoded
    val dateArray = (forma.get("datepicker")(0)).split("/")
    if(dateArray.length != 3){
      Redirect(routes.Lessons.profDaily())
    }else{
      val resultDate = dateArray(1) + "/" + dateArray(0) + "/" + dateArray(2)	   
      Redirect(routes.Lessons.dailyStatGet(resultDate))
    }
				 }

  def dailyRates(typ: String) = withUser{ user => implicit request =>
    if(typ != "day" && typ != "month" && typ != "year" && typ != "all"){
      Redirect(routes.Lessons.profDaily())
    }else{
      var pattern: String = currentDate
      if(typ == "month"){
	pattern = pattern.drop(2)
      }else if(typ == "year"){
	pattern = pattern.drop(4)
      }else if(typ == "all"){
	pattern = ""
      }
      val statList = microDailyStat.rate("%" + pattern)
      Ok(views.html.lessons.rates(statList))
    }
					}
  // Показ вопросов по статистике ежедневных соревнований
  def showDailyQuestionStat(question: Question, number: Int, stat: DailyStat) = {
    var result = ""
    if(question.image!="")
      result = s"""<div id='questionstat' class='well'>${number+1}. ${question.text}<br><img src='${question.image}'/>"""
    else 
      result = s"""<div id='questionstat' class='well'>${number+1}. ${question.text}"""
    val variants = Variant.findByQuestion(question.id)
    if(question.typ == 1 || question.typ == 3){
      if(stat.answer == "none" || stat.answer == ""){
	result += "<p>Правильна відповідь: " + question.answer + "</p>"
	result += "<p>Ви не відповідали на це питання.</p>"
      }
      else{
	result += "<p>Правильна відповідь: " + question.answer + "</p><p>Ваша відповідь: "
	if(stat.right==1)
	  result+="<span style='color: green;'>"
	else
	  result+="<span style='color: red;'>"
	result += stat.answer
	result += "</span></p>"
	}
    }else if(question.typ==2){
      result = s"""<div id='question' class='well'>${number+1}. ${question.text}
<img src='${question.image}'/>"""
      var varstring = ""
      val lis = split(variants, variants.length/2)
      val right_answer = (question.answer).split("~")
      val user_answer = (stat.answer).split("~")
      val length = user_answer.length
      var color = ""
      for(i <- 0 to (length-1)){
	if(user_answer(i)==right_answer(i))
	  color = "green"
	else
	  color = "red"
	varstring += s"<tr><td align=right><font color="+color+ s">${(lis(0))(i).text} ---- </font></td><td>"
	if(user_answer(i)=="none")
	  varstring += s"<font color=${color}>Ви не відповіли.</font></td><td>(Правильно - ${(lis(1))(i).text})</td></tr><br>"
	else
	  if(user_answer(i)==right_answer(i))
	    varstring += s"<font color=${color}>${user_answer(i)}</font></td></tr><br>"
	  else
	    varstring += s"<font color=${color}>${user_answer(i)}</font></td><td>(Правильно - ${(lis(1))(i).text})</td></tr><br>"
      }
      result += s"<table>$varstring</table>"
    }
    result += "</div><div id='line'></div>"
    result
   }

  def profDaily = withUser {user => implicit request => 
    val statList: List[microDailyStat] = microDailyStat.getByUser(user.id)
    var stringList: List[String] = List()
    var hel = ""
    for(stat <- statList){
      var hel = (stat.time).split("/")
      stringList = stringList :+ (hel(1)+"/"+hel(0)+"/"+hel(2))
    }
    Ok(views.html.lessons.profdaily(stringList))
  }

  // Подгрузка билетов 
  //------------------------------------------------------------
  def getLoad = withUser { user => implicit request =>
    if(user.id != 1) Redirect(routes.Static.home)
    else Ok(views.html.lessons.load("Upload"))
  }

  def postLoad = Action(parse.multipartFormData) { request =>
    request.body.file("xml").map { xm =>
      import java.io.File
      val time = System.currentTimeMillis
      xm.ref.moveTo(new File("/tmp/xml/" + time + ".xml"))
      val xmll = scala.xml.XML.loadFile("/tmp/xml/"+ time + ".xml")
      parseBilet(xmll)
      Redirect(routes.Lessons.getLoad).flashing(
	"success" -> "File was uploaded"
      )
    }.getOrElse{
      Redirect(routes.Lessons.getLoad).flashing(
	"error" -> "Missing file"
      )
    }
  }
  
  // Функция парсит xml документ и создает билет
  def parseBilet(data: scala.xml.Elem){
    // Шаг первый. Получаем id предмета
    val lesson_id = ((data \ "@lesson_id").text).toInt
    val time = ((data \ "@time").text)
    // Создаем билет
    Bilet.create(lesson_id, time)
    val lastBilet = Bilet.getLast
    // Шаг третий. Проходимся по всем вопросам
    for (question <- (data \\ "question")){
      // Получаем тип вопроса
      val typ = ((question \ "@type").text).toInt
      // Не зависимо от вопроса получаем картинку и текст вопроса
      val text = (question \\ "text").text
      val image = (question \\ "image").text
      var right = ""
      val thisQuestId =((Question.getLast).id + 1)
      // В зависимости от типа по разному обрабатываем вопросы
      if(typ == 1 || typ == 2){
	// Получаем правильный ответ
	if(typ == 1)
	  right = (question \\ "variant")(((question \ "@right").text).toInt).text
	else if(typ == 2){
	  val massive = for(variant <- (question \\ "variant"); if((variant \ "@answer").text == "true")) yield variant.text
	  for(mas <- massive) right+= mas+"~"
	}
	// Получаем варианты
	val variants = for(variant <- (question \\ "variant")) yield variant.text
	for(variant <- variants)
	  Variant.create(thisQuestId, variant)
      }else if(typ == 3){
	right = (question \\ "right").text
      } 
      Question.create(lastBilet.id, typ, text, image, right)
    }
  }
  

  def split[A](xs: List[A], n: Int): List[List[A]] = {
    if (xs.size <= n) xs :: Nil
    else (xs take n) :: split(xs drop n, n)
  }

  def currentDate = {
    import java.util.Calendar
    import java.text.SimpleDateFormat
    val today = Calendar.getInstance().getTime()
    val formatter = new SimpleDateFormat("dd/MM/YYYY")
    val current_day = formatter.format(today)
    current_day
  }
}
