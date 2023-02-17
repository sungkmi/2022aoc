package dev.sungkm.adventofcode2022.day1to10

import cats.effect.{IO, IOApp, Resource, Sync}
import cats.effect.std.Console
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*

object Day6 extends IOApp.Simple:

  def startOfPacketMarket(n: Int)(input: String): Int =
    input.sliding(n).zipWithIndex.find(_._1.toSet.size === n).get._2 + n

  def part1(input: String): Int = startOfPacketMarket(4)(input)

  def part2(input: String): Int = startOfPacketMarket(14)(input)

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

  val run = printAns[IO]("input/day6")
