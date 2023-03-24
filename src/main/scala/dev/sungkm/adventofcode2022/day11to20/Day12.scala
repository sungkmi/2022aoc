package dev.sungkm.adventofcode2022.day11to20

import scala.collection.immutable.Queue

import cats.effect.{IO, IOApp, Resource, Sync}
import cats.effect.std.Console
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*

object Day12 extends IOApp.Simple:

  case class ElevationMap(
      elevation: Map[(Int, Int), Char],
      start: (Int, Int),
      end: (Int, Int),
  )
  
  object ElevationMap:
    def parse(s: String): ElevationMap =
      val elevations = s
        .split("\n")
        .toVector
        .zipWithIndex
        .flatMap: (line, rowIndex) =>
          line.toVector.zipWithIndex.map: (char, columnIndex) =>
            (rowIndex, columnIndex) -> char
      val start       = elevations.find(_._2 == 'S').get._1
      val end         = elevations.find(_._2 == 'E').get._1
      val elevations1 = elevations.toMap + (start -> 'a') + (end -> 'z')
      ElevationMap(elevations1, start, end)

  def neighbors(p: (Int, Int)): Set[(Int, Int)] = p match
    case (x, y) => Set((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1))

  def bfs(
      map: ElevationMap,
      step: Int,
      visiting: Set[(Int, Int)],
      visited: Set[(Int, Int)],
  ): Option[Int] =
    if visiting.isEmpty then None
    else if visiting contains map.end then Some(step)
    else
      val toVisit = for
        next      <- visiting
        neighbor  <- neighbors(next) if !visited.contains(neighbor)
        elevation <- map.elevation.get(neighbor).toList.toSet
        if elevation - map.elevation(next) <= 1
      yield neighbor
      bfs(map, step + 1, toVisit, visited ++ visiting)

  def bfs2(
      map: ElevationMap,
      step: Int,
      visiting: Set[(Int, Int)],
      visited: Set[(Int, Int)],
  ): Option[Int] =
    if visiting.isEmpty then None
    else if visiting.exists(p => map.elevation.get(p) === Some('a')) then
      Some(step)
    else
      val toVisit = for
        next      <- visiting
        neighbor  <- neighbors(next) if !visited.contains(neighbor)
        elevation <- map.elevation.get(neighbor).toList.toSet
        if map.elevation(next) - elevation <= 1
      yield neighbor
      bfs2(map, step + 1, toVisit, visited ++ visiting)

  def part1(input: String): BigInt =
    val map = ElevationMap.parse(input)
    bfs(map, 0, Set(map.start), Set.empty).get

  def part2(input: String): BigInt =
    val map    = ElevationMap.parse(input)
    bfs2(map, 0, Set(map.end), Set.empty).get

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

  val run = printAns[IO]("input/day12")
