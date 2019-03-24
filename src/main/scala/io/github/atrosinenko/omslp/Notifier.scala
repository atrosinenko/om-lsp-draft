package io.github.atrosinenko.omslp

trait Notifier {
  def info(message: String): Unit
  def warn(message: String): Unit
  def error(message: String): Unit
}
