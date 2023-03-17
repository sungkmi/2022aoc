package dev.sungkm.adventofcode2022.day11to20

import scala.collection.immutable.Queue

import cats.effect.{IO, IOApp, Resource, Sync}
import cats.effect.std.Console
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*

object Day11 extends IOApp.Simple:

  opaque type MonkeyId = Int
  extension (mid: MonkeyId) def toInt: Int = mid
  object MonkeyId:
    def apply(int: Int): MonkeyId = int

  case class Monkey(id: MonkeyId, operation: Int => Int, test: Int, throwTrue: MonkeyId, throwFalse: MonkeyId)

  object Monkey:
    def parse(s: String): (Monkey, Queue[Int]) =
      val lines = s.split("\n").toVector
      val id = MonkeyId(lines(0).drop("Monkey ".size).init.toInt)
      val items = Queue.empty ++ lines(1).drop("  Starting items: ".size).split(", ").map(_.toInt)
      val operation = if lines(2) === "  Operation: new = old * old" then (x: Int) => x * x else
        val Array(operator, operand) = lines(2).drop("  Operation: new = old ".size).split(" ")
        operator match
          case "*" => (x: Int) => x * operand.toInt
          case "+" => (x: Int) => x + operand.toInt
      val test = lines(3).drop("  Test: divisible by ".size).toInt
      val throwTrue = MonkeyId(lines(4).drop("    If true: throw to monkey ".size).toInt)
      val throwFalse = MonkeyId(lines(5).drop("    If false: throw to monkey ".size).toInt)
      (Monkey(id, operation, test, throwTrue, throwFalse), items)

  def part1(input: String): BigInt =
    val monkeys = input.split("\n\n").map(Monkey.parse).toList

    monkeys.foreach(println)
    
    10605
      
  def part2(input: String): BigInt = ???

  def inputResource[F[_]: Sync](path: String): Resource[F, String] =
    Resource
      .fromAutoCloseable(Sync[F].blocking(scala.io.Source.fromFile(path)))
      .map(_.mkString)

  def printAns[F[_]: Console: Sync](path: String) = inputResource[F](path).use {
    input =>
      for
        _ <- Console[F].println(part1(input))
        _ <- Console[F].println(part2(input))
      yield ()
  }

  val run = printAns[IO]("input/day11")
