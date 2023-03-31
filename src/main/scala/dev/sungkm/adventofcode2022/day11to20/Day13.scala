package dev.sungkm.adventofcode2022.day11to20

import cats.effect.{IO, IOApp, Resource, Sync}
import cats.effect.std.Console
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*

object Day13 extends IOApp.Simple:

  enum Packet extends Ordered[Packet]:
    case PInt(value: BigInt)
    case PList(value: List[Packet])

    infix def compare(that: Packet): Int = (this, that) match
      case (PInt(x), PInt(y))       => x compare y
      case (PList(Nil), PList(Nil)) => 0
      case (PList(Nil), PList(_))   => -1
      case (PList(_), PList(Nil))   => 1
      case (PList(x :: xs), PList(y :: ys)) =>
        val compareHead = x compare y
        if compareHead != 0 then compareHead
        else PList(xs) compare PList(ys)
      case (PInt(_), PList(_)) => this.toPList compare that
      case (PList(_), PInt(_)) => this compare that.toPList

    def toPList: Packet.PList = this match
      case PInt(n)   => PList(PInt(n) :: Nil)
      case pl: PList => pl

  object Packet:
    def parse(s: String): Packet =
      def parsePInt(index: Int): (PInt, Int) =
        val newIndex = s.indexWhere(!_.isDigit, index)
        val number   = BigInt(s.slice(index, newIndex))
        (PInt(number), newIndex)

      def parsePList(index: Int, acc: List[Packet] = Nil): (PList, Int) =
        if index >= s.length || s(index) == ']' then
          (PList(acc.reverse), index + 1)
        else
          s(index) match
            case '[' =>
              val (nestedPList, newIndex) = parsePList(index + 1)
              parsePList(newIndex, nestedPList :: acc)
            case c if c.isDigit =>
              val (pInt, newIndex) = parsePInt(index)
              parsePList(newIndex, pInt :: acc)
            case ',' => parsePList(index + 1, acc)
            case _   => throw new IllegalArgumentException("Invalid input")

      parsePList(0)._1

  def part1(input: String): BigInt = input
    .split("\n\n")
    .zipWithIndex
    .filter: (s, i) =>
      val Array(p1, p2) = s.split("\n").map(Packet.parse)
      p1 < p2
    .map(_._2 + 1)
    .sum

  def part2(input: String): BigInt =
    val packets =
      input.split("\n").filter(_.size > 0).map(Packet.parse).toVector
    val d1     = Packet.parse("[[2]]")
    val d2     = Packet.parse("[[6]]")
    val sorted = (packets :+ d1 :+ d2).sorted
    (sorted.indexOf(d1) + 1) * (sorted.indexOf(d2) + 1)

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

  val run = printAns[IO]("input/day13")
