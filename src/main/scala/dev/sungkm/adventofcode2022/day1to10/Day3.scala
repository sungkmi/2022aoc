package dev.sungkm.adventofcode2022.day1to10

import cats.effect.{IO, IOApp, Resource, Sync}
import cats.effect.std.Console
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*

object Day3 extends IOApp.Simple:

  opaque type Item = Char
  extension (item: Item)
    def priority: Int = if item.isLower then item - 'a' + 1 else item - 'A' + 27

  opaque type RuckSack = String
  extension (rs: RuckSack)
    def findCommon: Item =
      val (front, back) = rs.splitAt(rs.size / 2)
      (front.toSet intersect back.toSet).head

  def findCommon(group: Seq[RuckSack]): Item =
    group.map(_.toSet).reduce(_ intersect _).head

  def parse(input: String): Seq[RuckSack] = input.split("\n").toSeq

  def part1(input: String): BigInt =
    parse(input).map(_.findCommon).map(_.priority).sum

  def part2(input: String): BigInt =
    parse(input).grouped(3).map(findCommon).map(_.priority).sum

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

  val run = printAns[IO]("input/day3")
