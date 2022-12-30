package dev.sungkm.adventofcode2022
package day1to10

import cats.effect.{IO, IOApp, Resource, Sync}
import cats.effect.std.Console
import cats.syntax.flatMap.*
import cats.syntax.functor.*

object Day1 extends IOApp.Simple:

  def elves(input: String): Seq[BigInt] =
    input.split("\n\n").toSeq.map { elf =>
      elf.split("\n").map(BigInt(_)).sum
    }

  def part1(input: String) = elves(input).max

  def part2(input: String) = elves(input).sorted.reverse.take(3).sum

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

  val run = printAns[IO]("input/day1")
