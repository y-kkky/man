package services

import com.typesafe.plugin._
import play.api.i18n.Messages
import play.api.Play.current


object EmailService {
  def sendEmail(email: String, body: String) = {
    val mail = use[MailerPlugin].email
    mail.setSubject(Messages("contact.request.subject", email))
    mail.addRecipient(email)
    mail.addFrom(Messages("contact.request.email.from"))
    mail.send(body)
  }
}