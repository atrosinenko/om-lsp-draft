package io.github.atrosinenko.omslp

import java.io.{File, PrintWriter}
import java.net.URI
import java.nio.file.Paths

import fastparse.all._

import scala.collection.mutable.ArrayBuffer
import scala.meta.lsp
import scala.meta.lsp._

object OmcInterface {

  val commandName = "OMShell-terminal"
  val scr = "OpenModelica.Scripting"
  val PS1 = "\n>>> "

  private val process = new ProcessBuilder(commandName).start()
  private val stdin = new PrintWriter(process.getOutputStream)
  private val stdout = process.getInputStream
  private val stderr = process.getErrorStream

  private def nextOutput(): StringBuilder = {
    val res = new StringBuilder()
    val buffer = new Array[Byte](1024)
    while(!res.endsWith(PS1)) {
      val cnt = stdout.available()
      stdout.read(buffer, 0, cnt)
      res ++= new String(buffer, 0, cnt, "UTF-8")
      Thread.sleep(10)
    }
    res.dropRight(PS1.length)
  }

  nextOutput() // skip greeting

  private def executeScriptingCommand(cmd: String): String = {
    val res = new StringBuilder
    stdin.println(cmd)
    stdin.flush()
    nextOutput().dropWhile(_ != '\n').drop(1).mkString
  }

  def getVersion: String = executeScriptingCommand(s"getVersion()")

  def openRoot(uri: String): Either[String, Unit] = {
    executeScriptingCommand(s"""$scr.loadFile("${Paths.get(new URI(uri)).toFile.toString}")""")
    Right(())
  }

  val notSemicolon: Parser[String] = P ( CharsWhile(_ != ':').! )
  val integer: Parser[Int] = P ( ("-".? ~ CharIn("0123456789").rep(1)).!.map(_.toInt) )
  val diagnosticParser: Parser[(String, Diagnostic)] =
    P ( "[" ~ notSemicolon ~ ":" ~
      integer ~ ":" ~ integer ~ "-" ~ integer ~ ":" ~ integer ~
      CharsWhile(_ != ']') ~ "]" ~ notSemicolon ~ ":" ~ AnyChar.rep.! ~ End
    ).map {
      case (file, l1, c1, l2, c2, severityStr, msg) =>
        val uri = new File(file).toURI.toString
        val range = lsp.Range(Position(l1 - 1, c1 - 1), Position(l2 - 1, c2 - 1))
        val severity = if (severityStr.contains("Error")) DiagnosticSeverity.Error else DiagnosticSeverity.Warning
        val diagnostic = Diagnostic(
          range = range,
          severity = Some(severity),
          code = None,
          source = None,
          message = msg
        )
        (uri, diagnostic)
    }

  def parseDiagnostic(line: String): Either[String, (String, Diagnostic)] = {
    diagnosticParser.parse(line) match {
      case Parsed.Success(res, _) => Right(res)
      case _ => Left(line)
    }
  }

  def load(uri: String, name: String, reload: Boolean)(implicit notifier: Notifier): Seq[PublishDiagnostics] = {
    val msg = executeScriptingCommand(s"""$scr.reloadClass($name);print(getErrorString())""")
    val diagnostics = ArrayBuffer[(String, Diagnostic)]()
    val miscMessages = ArrayBuffer[String]()
    msg.split("\n").dropRight(1).map(_.trim).filter(_.nonEmpty).map(parseDiagnostic).foreach {
      case Right(d) => diagnostics += d
      case Left(m) => miscMessages += m
    }
    if (miscMessages.nonEmpty)
      notifier.info(miscMessages.mkString("\n"))
    diagnostics.groupBy(_._1).map {
      case (msgUri, xs) =>
        PublishDiagnostics(
          uri = msgUri,
          diagnostics = diagnostics.map(_._2)
        )
    }.toSeq
  }

  def getClassNames(className: String)(implicit notifier: Notifier): Seq[String] = {
    executeScriptingCommand(s"getClassNames($className)").split("\\{|\\}|,|\n").filter(_.nonEmpty)
  }

  def getNamesForPrefix(className: String, prefix: String)(implicit notifier: Notifier): Seq[String] = {
    val ind = prefix.lastIndexOf('.')
    getClassNames(if (ind == -1) "AllLoadedClasses" else prefix.substring(0, ind))
  }
}
