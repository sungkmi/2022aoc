package dev.sungkm.adventofcode2022.day1to10

import cats.effect.{IO, IOApp, Resource, Sync}
import cats.effect.std.Console
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*

object Day8 extends IOApp.Simple:

  opaque type TreeHeightMap = Vector[Vector[Int]]
  object TreeHeightMap:
    def parse(s: String): TreeHeightMap =
      s.split("\n").toVector.map(_.toVector.map(_.toString.toInt))
  extension (m: TreeHeightMap)
    def intermediateTrees(row: Int, col: Int): List[Vector[Int]] =
      val left     = m(row).take(col).reverse
      val right    = m(row).drop(col + 1)
      val vertical = m.map(_(col))
      val up       = vertical.take(row).reverse
      val down     = vertical.drop(row + 1)
      List(left, right, up, down)

  def part1(input: String): BigInt =
    val map = TreeHeightMap.parse(input)

    val visibleTrees = for
      row <- 0 until map.size
      col <- 0 until map.head.size
      if map.intermediateTrees(row, col).exists(_.forall(_ < map(row)(col)))
    yield (row, col)

    visibleTrees.size

  def part2(input: String): BigInt =

    val map = TreeHeightMap.parse(input)
    val scenicScores = for
      row <- 1 until (map.size - 1)
      col <- 1 until (map.head.size - 1)
    yield
      map.intermediateTrees(row, col).map { trees =>
        val smallerTrees = trees.takeWhile(_ < map(row)(col))
        if trees == smallerTrees then trees.size else smallerTrees.size + 1
      }.product

    scenicScores.max

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

  val run = printAns[IO]("input/day8")
