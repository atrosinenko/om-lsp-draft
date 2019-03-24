package io.github.atrosinenko.omslp

import scala.meta.lsp.{DidChangeTextDocumentParams, DidOpenTextDocumentParams, Position, TextDocumentContentChangeEvent}

object Document {
  val whitespaceCharacters = " \t\r\n"
  def isSpace(ch: Char): Boolean = whitespaceCharacters.contains(ch)
}

class Document private(initialContents: String, initialVersion: Long, val className: String) {
  import Document._

  def this(open: DidOpenTextDocumentParams, cls: String) = {
    this(open.textDocument.text, open.textDocument.version + 1, cls)
  }

  private var _contents: String = initialContents
  private var _version: Long = initialVersion

  def contents: String = _contents
  private def consumeChangeEvent(evt: TextDocumentContentChangeEvent)(implicit notifier: Notifier): Unit = {
    if (evt.range.nonEmpty || evt.rangeLength.nonEmpty) {
      notifier.error("Partial document updates are not supported")
    } else {
      _contents = evt.text
    }
  }
  def consumeChanges(changes: DidChangeTextDocumentParams)(implicit notifier: Notifier): Unit = {
    if (_version != changes.textDocument.version) {
      notifier.error(s"Incorrect document change version, please re-open: ${changes.textDocument.version}, expected: ${_version}")
    } else {
      changes.contentChanges.foreach(consumeChangeEvent)
    }
    _version += 1
  }

  def wordAtPos(position: Position)(implicit notifier: Notifier): Option[String] = {
    val line = position.line
    val col = position.character

    var ind = 0
    for (_ <- 0 until line) {
      ind = _contents.indexOf('\n', ind) + 1
      if (ind == 0) {
        notifier.error(s"No such line: $line")
        return None
      }
    }
    ind += col
    if (ind > _contents.length) {
      None
    } else {
      var startIndex = ind - 1
      while (startIndex >= 0 && !isSpace(_contents(startIndex))) {
        startIndex -= 1
      }
      Some(_contents.substring(startIndex + 1, ind))
    }
  }
}
