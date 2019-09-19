package com.nat.fptalk.shape

object Shape {

  def drawRectangle(width: Int): String =
    (1 to width).map(c =>"*"*width).mkString("\n")

  def printAlignedCenteredText(text: String)(width: Int, skipped: Int): String =
    (" " * skipped) + (text * (width - skipped* 2))

  def drawDownTriangle(width: Int): String =
    (1 to width).map(index => printAlignedCenteredText("*")(width, index)).mkString("\n")

  def drawUpTriangle(width: Int): String =
    drawDownTriangle(width).split("\n").reverse.mkString("\n")
}
