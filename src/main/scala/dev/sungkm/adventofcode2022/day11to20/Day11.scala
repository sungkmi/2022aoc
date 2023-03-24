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

  case class Monkey(
      id: MonkeyId,
      operation: BigInt => BigInt,
      test: Int,
      throwTrue: MonkeyId,
      throwFalse: MonkeyId,
  )

  object Monkey:
    def parse(s: String): (Monkey, Queue[BigInt]) =
      val lines = s.split("\n").toVector
      val id    = MonkeyId(lines(0).drop("Monkey ".size).init.toInt)
      val items = Queue.empty ++ lines(1)
        .drop("  Starting items: ".size)
        .split(", ")
        .map(BigInt(_))
      val operation =
        if lines(2) === "  Operation: new = old * old" then (x: BigInt) => x * x
        else
          val Array(operator, operand) =
            lines(2).drop("  Operation: new = old ".size).split(" ")
          operator match
            case "*" => (x: BigInt) => x * BigInt(operand)
            case "+" => (x: BigInt) => x + BigInt(operand)
      val test = lines(3).drop("  Test: divisible by ".size).toInt
      val throwTrue = MonkeyId(
        lines(4).drop("    If true: throw to monkey ".size).toInt,
      )
      val throwFalse = MonkeyId(
        lines(5).drop("    If false: throw to monkey ".size).toInt,
      )
      (Monkey(id, operation, test, throwTrue, throwFalse), items)

  def nextRound(monkeys: IndexedSeq[Monkey])(
      items: IndexedSeq[Queue[BigInt]],
      acc: IndexedSeq[Int],
  ): (IndexedSeq[Queue[BigInt]], IndexedSeq[Int]) =
    def doTurn(monkeyIndex: Int)(
        items: IndexedSeq[Queue[BigInt]],
        acc: IndexedSeq[Int],
    ): (IndexedSeq[Queue[BigInt]], IndexedSeq[Int]) =
      if items(monkeyIndex).isEmpty then (items, acc)
      else
        val item      = items(monkeyIndex).head
        val remainder = items(monkeyIndex).tail

        val monkey     = monkeys(monkeyIndex)
        val worryLevel = monkey.operation(item) / 3

        val receiverMonkeyIndex =
          if worryLevel % monkey.test === 0 then monkey.throwTrue
          else monkey.throwFalse
        val items1 = items
          .updated(monkeyIndex, remainder)
          .updated(
            receiverMonkeyIndex,
            items(receiverMonkeyIndex) :+ worryLevel,
          )
        val acc1 = acc.updated(monkeyIndex, acc(monkeyIndex) + 1)
        doTurn(monkeyIndex)(items1, acc1)

    (0 until monkeys.size).foldLeft((items, acc)):
      case ((items, acc), index) =>
        doTurn(index)(items, acc)

  def nextRound2(monkeys: IndexedSeq[Monkey])(
      items: IndexedSeq[Queue[BigInt]],
      acc: IndexedSeq[Int],
  ): (IndexedSeq[Queue[BigInt]], IndexedSeq[Int]) =

    val lcm = monkeys.map(m => BigInt(m.test)).product

    def doTurn(monkeyIndex: Int)(
        items: IndexedSeq[Queue[BigInt]],
        acc: IndexedSeq[Int],
    ): (IndexedSeq[Queue[BigInt]], IndexedSeq[Int]) =
      if items(monkeyIndex).isEmpty then (items, acc)
      else
        val item      = items(monkeyIndex).head
        val remainder = items(monkeyIndex).tail

        val monkey     = monkeys(monkeyIndex)
        val worryLevel = monkey.operation(item) % lcm

        val receiverMonkeyIndex =
          if worryLevel % monkey.test === 0 then monkey.throwTrue
          else monkey.throwFalse
        val items1 = items
          .updated(monkeyIndex, remainder)
          .updated(
            receiverMonkeyIndex,
            items(receiverMonkeyIndex) :+ worryLevel,
          )
        val acc1 = acc.updated(monkeyIndex, acc(monkeyIndex) + 1)
        doTurn(monkeyIndex)(items1, acc1)

    (0 until monkeys.size).foldLeft((items, acc)):
      case ((items, acc), index) =>
        doTurn(index)(items, acc)

  def part1(input: String): BigInt =
    val (monkeys, items) =
      input.split("\n\n").map(Monkey.parse).toIndexedSeq.unzip

    val (items1, acc) =
      (1 to 20).foldLeft((items, IndexedSeq.fill(monkeys.size)(0))):
        case ((items, acc), _) =>
          nextRound(monkeys)(items, acc)

    val IndexedSeq(first, second) = acc.sortBy(-_).take(2)

    first * second

  def part2(input: String): BigInt =
    val (monkeys, items) =
      input.split("\n\n").map(Monkey.parse).toIndexedSeq.unzip

    val (items1, acc) =
      (1 to 10000).foldLeft((items, IndexedSeq.fill(monkeys.size)(0))):
        case ((items, acc), index) =>
          nextRound2(monkeys)(items, acc)

    val IndexedSeq(first, second) = acc.sortBy(-_).take(2)

    BigInt(first) * BigInt(second)

  def inputResource[F[_]: Sync](path: String): Resource[F, String] =
    Resource
      .fromAutoCloseable(Sync[F].blocking(scala.io.Source.fromFile(path)))
      .map(_.mkString)

  def printAns[F[_]: Console: Sync](path: String) = inputResource[F](path).use:
    input =>
      for
        _ <- Console[F].println(part1(input))
        _ <- Console[F].println(part2(input))
      yield ()

  val run = printAns[IO]("input/day11")
