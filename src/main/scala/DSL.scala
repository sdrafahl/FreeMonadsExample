package com.freemonads

import cats.free.Free
import cats.free.Free.liftF
import cats.arrow.FunctionK
import cats.{Id, ~>}
import scala.collection.mutable
import cats.effect.IO

sealed trait PrintCommand[A]

case class PrintLine(msg: String) extends PrintCommand[Unit]
case class PrintVerticalLines(msg: String) extends PrintCommand[Unit]

object DSL {
  type Print[A] = Free[PrintCommand, A]

  def printLine[A](msg: String) = liftF[PrintCommand, Unit](PrintLine(msg))
  def printVerticalLines(msg: String) = liftF[PrintCommand, Unit](PrintVerticalLines(msg))
  def printBoth[A](msg: String) = for {
    _ <- printVerticalLines(msg)
    _ <- printLine(msg)
    _ <- printVerticalLines(msg)
  } yield ()

  def program: Print[Unit] = for {
    _ <- printBoth("Test message")
  } yield ()

  def ioCompiler: PrintCommand ~> IO = new (PrintCommand ~> IO) {
    def apply[A](fa: PrintCommand[A]): IO[A] = fa match {
      case PrintLine(msg) => IO(println(s"------------------------------------------- ${msg} ------------------------------------------------------"))
      case PrintVerticalLines(msg) => IO(println(s"||||||||||||||||||||||||||||||||||||||||||||| ${msg} |||||||||||||||||||||||||||||||||||||||||||||"))
    }
  }

  def runTest = program.foldMap(ioCompiler).unsafeRunSync()

}



