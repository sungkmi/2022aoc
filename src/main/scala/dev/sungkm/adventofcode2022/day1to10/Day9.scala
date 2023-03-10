package dev.sungkm.adventofcode2022.day1to10

import cats.effect.{IO, IOApp, Resource, Sync}
import cats.effect.std.Console
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*

object Day9 extends IOApp.Simple:

  def moveHead(direction: String): ((Int, Int)) => (Int, Int) =
    (x, y) =>
      direction match
        case "R" => (x + 1, y)
        case "L" => (x - 1, y)
        case "U" => (x, y + 1)
        case "D" => (x, y - 1)
        case d   => throw new Exception(s"Unknown direction: $d")

  def moveTail(head: (Int, Int)): ((Int, Int)) => (Int, Int) =
    (tx, ty) =>
      head match
        case (hx, hy) =>
          if ((tx - hx).abs max (ty - hy).abs) <= 1 then (tx, ty)
          else if tx == hx then (tx, ty + (hy - ty).sign)
          else if ty == hy then (tx + (hx - tx).sign, ty)
          else (tx + (hx - tx).sign, ty + (hy - ty).sign)

  val ref = (0, 0)

  def tailLocusSizeOfRope(size: Int)(input: String): Int =
    input
      .split("\n")
      .foldLeft((Set(ref), List.fill(size)(ref))) { case ((locus, knots), move) =>
        val Array(direction, step) = move.split(" ")
        (1 to step.toInt).foldLeft((locus, knots)) { case ((locus, knots), _) =>
          val head1 = moveHead(direction)(knots.head)
          val knots1reversed = knots.tail
            .foldLeft((List(head1), head1)) { case ((acc, head), tail) =>
              val tail1 = moveTail(head)(tail)
              (tail1 :: acc, tail1)
            }
            ._1
          (locus + knots1reversed.head, knots1reversed.reverse)
        }
      }
      ._1
      .size

  def part1(input: String): BigInt = tailLocusSizeOfRope(2)(input)
      
  def part2(input: String): BigInt = tailLocusSizeOfRope(10)(input)

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

  val run = printAns[IO]("input/day9")
