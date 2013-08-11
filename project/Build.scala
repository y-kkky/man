import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "zno"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    // Add your project dependencies here,
    jdbc,
    anorm,
    "postgresql" % "postgresql" % "9.1-901-1.jdbc4",
    "com.typesafe" %% "play-plugins-mailer" % "2.1.0"
  )


  val main = play.Project(appName, appVersion, appDependencies).settings(
    // Add your own project settings here    
      lessEntryPoints <<= baseDirectory(_ / "app" / "assets" / "stylesheets" ** "main.less")
  )

}
