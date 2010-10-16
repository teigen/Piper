import sbt._

class Piper(info: ProjectInfo) extends ParentProject(info) {
    val ivyLocal = "Local Ivy Repository" at "file://"+Path.userHome+"/.ivy2/local"
    val mavenLocal = "Local Maven Repository" at "file://"+Path.userHome+"/.m2/repository"
    val scalaToolsReleases = "Scala Tools Releases" at "http://scala-tools.org/repo-releases/"
    
    object libs {
        val servletApi = "javax.servlet" % "servlet-api" % "2.4"
        val jetty = "org.mortbay.jetty" % "jetty" % "6.1.22" % "test->default"
        val h2 = "com.h2database" % "h2" % "1.2.121"
        val scalaz = "com.googlecode.scalaz" %% "scalaz-core" % "5.0"
    }

    lazy val library = project("library", "Piper Library", new Library(_))
    lazy val examples = project("examples", "Piper Examples", new Examples(_))
    
    class Library(info: ProjectInfo) extends DefaultProject(info) {
        override def libraryDependencies = Set(libs.servletApi) ++ super.libraryDependencies
    }
    
    class Examples(info: ProjectInfo) extends ParentProject(info) {
        lazy val hello = project("hello", "Hello World example", new ExampleProject(_), library)

        class ExampleProject(info: ProjectInfo) extends DefaultWebProject(info) {
            override def libraryDependencies = Set(libs.jetty) ++ super.libraryDependencies
        }
    }
}
