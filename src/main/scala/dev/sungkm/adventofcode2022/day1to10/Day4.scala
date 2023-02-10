package dev.sungkm.adventofcode2022.day1to10

import cats.effect.{IO, IOApp, Resource, Sync}
import cats.effect.std.Console
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*

object Day4 extends IOApp.Simple:

  def parseLine(line: String): Seq[Set[Int]] =
    line
      .split(",")
      .map { section =>
        val Array(start, end) = section.split("-")
        (start.toInt to end.toInt).toSet
      }
      .toSeq

  def countLine(input: String)(cond: String => Boolean): Int =
    input.split("\n").count(cond)

  def part1(input: String): BigInt =
    countLine(input) { line =>
      val Seq(smaller, larger) = parseLine(line).sortBy(_.size)
      smaller.forall(larger.contains)
    }

  def part2(input: String): BigInt =
    countLine(input) { line =>
      val Seq(first, second) = parseLine(line)
      (first intersect second).nonEmpty
    }

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

  val run = printAns[IO]("input/day4")
