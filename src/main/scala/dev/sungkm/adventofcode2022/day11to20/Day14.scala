package dev.sungkm.adventofcode2022.day11to20

import scala.collection.immutable.{SortedMap, SortedSet}

import cats.effect.{IO, IOApp, Resource, Sync}
import cats.effect.std.Console
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*

object Day14 extends IOApp.Simple:

  opaque type CaveMap = Map[Int, SortedSet[Int]]
  object CaveMap:
    def apply(points: Map[Int, SortedSet[Int]]): CaveMap = points
    def parse(s: String): CaveMap = CaveMap:
        val rocks = s
          .split("\n")
          .foldLeft(Set.empty[(Int, Int)]):
            case (set, line) =>
              val points = line
                .split(" -> ")
                .map(_.split(","))
                .map:
                  case Array(x, y) => (x.toInt, y.toInt)
              set ++ points
                .zip(points.tail)
                .flatMap:
                  case ((x1, y1), (x2, y2)) if x1 == x2 =>
                    ((y1 min y2) to (y1 max y2)).map(y => (x1, y)).toSet
                  case ((x1, y1), (x2, y2)) if y1 == y2 =>
                    ((x1 min x2) to (x1 max x2)).map(x => (x, y1)).toSet
                  case _ => throw new Exception("Invalid input")
        SortedMap.empty ++
          rocks.groupMapReduce(_._1)(p => SortedSet(p._2))(_ ++ _)

  extension (cm: CaveMap)
    def toMap: Map[Int, SortedSet[Int]] = cm
    def verticalFall(x: Int, y: Int): Option[Int] =
      cm.toMap.get(x).flatMap(_.dropWhile(_ < y).headOption).map(_ - 1)
    def fall(x: Int, y: Int): Option[(Int, Int)] =
      verticalFall(x, y).flatMap: y1 =>
        def isLeftOccupied: Boolean =
          cm.toMap.getOrElse(x - 1, SortedSet.empty[Int]).contains(y1 + 1)
        def isRightOccupied: Boolean =
          cm.toMap.getOrElse(x + 1, SortedSet.empty[Int]).contains(y1 + 1)

        if !isLeftOccupied then fall(x - 1, y1 + 1)
        else if !isRightOccupied then fall(x + 1, y1 + 1)
        else Some((x, y1))
    def add(x: Int, y: Int): CaveMap = CaveMap:
        cm.toMap.updated(x, cm.toMap.getOrElse(x, SortedSet.empty[Int]) + y)

    def fallToBottom(x: Int, y: Int, bottom: Int): (Int, Int) =
      val y1                = verticalFall(x, y).getOrElse(bottom - 1)
      def isBottom: Boolean = y1 == bottom - 1
      def isLeftOccupied: Boolean = isBottom || cm.toMap
        .getOrElse(x - 1, SortedSet.empty[Int])
        .contains(y1 + 1)
      def isRightOccupied: Boolean = isBottom || cm.toMap
        .getOrElse(x + 1, SortedSet.empty[Int])
        .contains(y1 + 1)

      if !isLeftOccupied then fallToBottom(x - 1, y1 + 1, bottom)
      else if !isRightOccupied then fallToBottom(x + 1, y1 + 1, bottom)
      else (x, y1)

  def part1(input: String): BigInt =
    val caveMap = CaveMap.parse(input)

    def loop(caveMap: CaveMap, count: Int): Int =
      caveMap.fall(500, 0) match
        case Some((x, y)) =>
          loop(caveMap.add(x, y), count + 1)
        case None =>
          count
    loop(caveMap, 0)

  def part2(input: String): BigInt =
    val caveMap = CaveMap.parse(input)

    val bottom = caveMap.toMap.values.flatten.max + 2

    def loop(caveMap: CaveMap, count: Int): Int =
      val (x, y) = caveMap.fallToBottom(500, 0, bottom)
      if x == 500 && y == 0 then count + 1
      else loop(caveMap.add(x, y), count + 1)

    loop(caveMap, 0)

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

  val run = printAns[IO]("input/day14")
