package dev.sungkm.adventofcode2022
package day1to10

import cats.effect.{IO, IOApp, Resource, Sync}
import cats.effect.std.Console
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*

object Day2 extends IOApp.Simple:

  def parseLine(line: String): (Int, Int) =
    val Array(opponent, you) = line.split(" ").map(_.head)
    (opponent - 'A' + 1, you - 'X' + 1)

  def outcome(opponent: Int, you: Int): Int =
    (you - opponent + 2) % 3 match
      case 0 => 6
      case 1 => 0
      case 2 => 3

  def choose(opponent: Int, you: Int): Int =
    you match
      case 1 => (opponent + 1) % 3 + 1
      case 2 => (opponent + 2) % 3 + 1
      case 3 => (opponent + 0) % 3 + 1

  def strategy(opponent: Int, you: Int): Int =
    choose(opponent, you) + (you - 1) * 3

  def part1(input: String): BigInt = input
    .split("\n")
    .map(parseLine)
    .map { case (opponent, you) =>
      you + outcome(opponent, you)
    }
    .sum

  def part2(input: String) = input
    .split("\n")
    .map(parseLine)
    .map { case (opponent, you) =>
      BigInt(strategy(opponent, you))
    }
    .sum

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

  val run = printAns[IO]("input/day2")
