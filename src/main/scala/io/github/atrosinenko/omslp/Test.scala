package io.github.atrosinenko.omslp

import io.circe.Json
import scribe._

import scala.meta.jsonrpc._
import scala.meta.lsp
import scala.meta.lsp._

object Test extends App {

  val keywords = Seq(
    "function",
    "package",
    "if",
    "then",
    "else",
    "end",
    "when"
  )

  def openModelicaServices(logger: LoggerSupport, client: LanguageClient): Services = {
    val documents = new OpenDocuments

    implicit val notifier: Notifier = new Notifier {
      override def info(message: String): Unit = Window.showMessage.info(message)(client)
      override def warn(message: String): Unit = Window.showMessage.warn(message)(client)
      override def error(message: String): Unit = Window.showMessage.error(message)(client)
    }

    Services
      .empty(logger)
      .request(Lifecycle.initialize) { params =>
        logger.warn(params.toString)
        val capabilities = ServerCapabilities(
          hoverProvider = true,
          completionProvider = Some(CompletionOptions(resolveProvider = false, triggerCharacters = List(".", ">"))),
          textDocumentSync = Some(TextDocumentSyncOptions(openClose = Some(true), change = Some(TextDocumentSyncKind.Full), save = Some(SaveOptions(includeText = Some(false)))))
//          workspaceSymbolProvider = false
//          documentOnTypeFormattingProvider = Some(DocumentOnTypeFormattingOptions("}", Seq.empty))
        )
        InitializeResult(capabilities)
      }
      .notification(Lifecycle.initialized) { _ =>
        notifier.info(OmcInterface.getVersion)
      }
      .request(Lifecycle.shutdown) { params =>
        System.exit(0)
        Json.Null
      }
      .notification(TextDocument.didOpen) { params =>
        documents.open(params, s"[${OmcInterface.getVersion}]")
      }
      .notification(TextDocument.didClose) { params =>
        documents.close(params.textDocument.uri)
      }
      .notification(TextDocument.didChange) { params =>
        documents.get(params.textDocument.uri).foreach { doc =>
          doc.consumeChanges(params)
        }
      }
      .notification(TextDocument.didSave) { params =>
        val uri = params.textDocument.uri
        documents.get(uri).map { doc =>
          OmcInterface.load(uri, doc.className, reload = true)
        }.get.foreach(TextDocument.publishDiagnostics.notify(_)(client))
      }
      .request(TextDocument.completion) { params =>
        val items = documents.get(params.textDocument.uri).toSeq.flatMap { doc =>
          doc.wordAtPos(params.position).toSeq.flatMap { word =>
            (keywords.filter(_.startsWith(word)) ++ OmcInterface.getNamesForPrefix(doc.className, word)).map(str =>
              CompletionItem(str)
            )
          }
        }
        CompletionList(isIncomplete = false, items)
      }
      .request(TextDocument.hover) { params =>
        Hover(List(RawMarkedString("en", s"TEST: ${params.toString}")), range = Some(Range(lsp.Position(1, 1), lsp.Position(1, 2))))
      }
  }

  implicit val scheduler = monix.execution.Scheduler(java.util.concurrent.Executors.newSingleThreadExecutor())
  val io = new InputOutput(
    System.in,
    System.out
  )
  val logger = Logger.empty.orphan().withHandler(writer = scribe.writer.FileWriter.simple()).withMinimumLevel(Level.Trace)

  val connection =
    Connection(io, serverLogger = logger, clientLogger = logger) { client =>
      openModelicaServices(logger, client)
    }(scheduler)
  scala.concurrent.Await.result(connection.server, scala.concurrent.duration.Duration.Inf)
}
