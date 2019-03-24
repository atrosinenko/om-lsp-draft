package io.github.atrosinenko.omslp

import java.net.URI
import java.nio.file.Paths

import scala.collection.mutable
import scala.meta.lsp.DidOpenTextDocumentParams
import scala.collection.JavaConverters._
import scala.util.control.NonFatal

class OpenDocuments {
  private val documents = mutable.Map[String, Document]()
  private val roots = mutable.Map[String, Unit]()

  private def findRootAndClass(uri: String): (String, String) = {
    val file = Paths.get(new URI(uri))
    var root = file
    while (root.getParent.resolve("package.mo").toFile.exists()) {
      root = root.getParent
    }
    (root.resolve("package.mo").toUri.toString, root.getParent.relativize(file).iterator().asScala.toSeq.mkString(".").dropRight(3))
  }

  private def addNewRootIfNeededReturningClass(uri: String, info: String)(implicit notifier: Notifier): String = {
    val (root, className) = findRootAndClass(uri)
    if (!roots.contains(root)) {
      notifier.info(s"Adding new root: $root $info")
      roots += root -> Unit
    }
    try {
      OmcInterface.openRoot(root).left.foreach(notifier.warn)
    } catch {
      case NonFatal(e) => notifier.error(e.toString)
    }
    className
  }

  def open(params: DidOpenTextDocumentParams, info: String)(implicit notifier: Notifier): Unit = {
    val uri = params.textDocument.uri
    if (documents.contains(uri)) {
      notifier.warn(s"Re-opening $uri")
    }
    val className = addNewRootIfNeededReturningClass(uri, info)
    OmcInterface.load(uri, className, reload = false)
    documents(uri) = new Document(params, className)
  }

  def get(uri: String)(implicit notifier: Notifier): Option[Document] = {
    val res = documents.get(uri)
    if (res.isEmpty)
      notifier.error(s"Document $uri is not currently open")
    res
  }

  def close(uri: String)(implicit notifier: Notifier): Unit = {
    val removed = documents.remove(uri)
    if (removed.isEmpty) {
      notifier.warn(s"Closing $uri that is not currently open")
    }
  }
}
