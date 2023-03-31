package dev.sungkm.adventofcode2022.day11to20

import cats.effect.{IO, IOApp, Resource, Sync}
import cats.effect.std.Console
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*

object Day13 extends IOApp.Simple:

  enum Packet:
    case PInt(value: BigInt)
    case PList(value: List[Packet])

  object Packet:
    extension (p: Packet)
      def toPList: Packet.PList = p match
        case Packet.PInt(n)   => Packet.PList(Packet.PInt(n) :: Nil)
        case pl: Packet.PList => pl

    def compare(x: Packet, y: Packet): Int = (x, y) match
      case (Packet.PInt(x), Packet.PInt(y))       => x.compare(y)
      case (Packet.PList(Nil), Packet.PList(Nil)) => 0
      case (Packet.PList(Nil), Packet.PList(_))   => -1
      case (Packet.PList(_), Packet.PList(Nil))   => 1
      case (Packet.PList(x :: xs), Packet.PList(y :: ys)) =>
        val compareHead = compare(x, y)
        if compareHead =!= 0 then compareHead
        else compare(Packet.PList(xs), Packet.PList(ys))
      case (Packet.PInt(_), Packet.PList(_)) => compare(x.toPList, y)
      case (Packet.PList(_), Packet.PInt(_)) => compare(x, y.toPList)

    def parse(s: String): Packet =
      def parsePInt(str: String, index: Int): (BigInt, Int) =
        val newIndex = str.indexWhere(!_.isDigit, index)
        val number   = BigInt(str.slice(index, newIndex))
        (number, newIndex)

      def parsePList(
          str: String,
          index: Int,
          acc: List[Packet] = Nil,
      ): (List[Packet], Int) =
        if index >= str.length || str(index) == ']' then
          (acc.reverse, index + 1)
        else
          str(index) match
            case '[' =>
              val (nestedPackets, newIndex) = parsePList(str, index + 1)
              parsePList(
                str,
                newIndex,
                Packet.PList(nestedPackets) :: acc,
              )
            case c if c.isDigit =>
              val (number, newIndex) = parsePInt(str, index)
              parsePList(str, newIndex, Packet.PInt(number) :: acc)
            case ',' => parsePList(str, index + 1, acc)
            case _   => throw new IllegalArgumentException("Invalid input")

      val (packets, _) = parsePList(s, 0)
      Packet.PList(packets)

  def part1(input: String): BigInt = input
    .split("\n\n")
    .zipWithIndex
    .filter: (s, i) =>
      val Array(p1, p2) = s.split("\n").map(Packet.parse)
      Packet.compare(p1, p2) < 0
    .map(_._2 + 1)
    .sum

  def part2(input: String): BigInt =
    val packets =
      input.split("\n").filter(_.size > 0).map(Packet.parse).toVector
    val d1     = Packet.parse("[[2]]")
    val d2     = Packet.parse("[[6]]")
    val sorted = (packets :+ d1 :+ d2).sortWith(Packet.compare(_, _) < 0)
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
