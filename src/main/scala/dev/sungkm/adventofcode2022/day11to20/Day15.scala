package dev.sungkm.adventofcode2022.day11to20

import scala.collection.immutable.SortedSet

import cats.effect.{IO, IOApp, Resource, Sync}
import cats.effect.std.Console
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*

object Day15 extends IOApp.Simple:

  case class SensorReport(sx: BigInt, sy: BigInt, bx: BigInt, by: BigInt):
    def distance: BigInt = (sx - bx).abs + (sy - by).abs
    def yProjection(y: BigInt): Option[(BigInt, BigInt)] =
      val dy   = (y - sy).abs
      val size = distance - dy
      if size < 0 then None
      else Some((sx - size, sx + size))

  object SensorReport:
    def parse(line: String): SensorReport =
      val Array(s, b)   = line.split(": closest beacon is at x=")
      val Array(sx, sy) = s.drop("Sensor at x=".size).split(", y=")
      val Array(bx, by) = b.split(", y=")
      SensorReport(BigInt(sx), BigInt(sy), BigInt(bx), BigInt(by))

  opaque type RangeSet = SortedSet[(BigInt, BigInt)]
  object RangeSet:
    def empty: RangeSet = SortedSet.empty[(BigInt, BigInt)]
  extension (rs: RangeSet)
    def add(r: (BigInt, BigInt)): RangeSet = (rs + r)
    def merge: RangeSet =
      @annotation.tailrec
      def loop(
          rs: List[(BigInt, BigInt)],
          acc: List[(BigInt, BigInt)],
      ): List[(BigInt, BigInt)] =
        (rs, acc) match
          case (Nil, _)         => acc
          case (r :: tail, Nil) => loop(tail, r :: Nil)
          case (r :: tail, r1 :: accTail) =>
            if r._1 <= r1._2 + 1 then
              loop(tail, (r1._1, r1._2 max r._2) :: accTail)
            else loop(tail, r :: r1 :: accTail)
      SortedSet.empty ++ loop(rs.toList, Nil)
    def rangeSetSize: BigInt = rs.map(r => r._2 - r._1 + 1).sum

  def part1(input: String, y: BigInt): BigInt =
    val reports = input.split("\n").map(SensorReport.parse)
    val rangeSet = reports
      .flatMap(_.yProjection(y))
      .foldLeft(RangeSet.empty)(_ add _)
    val merged = rangeSet.merge
    val beaconCount =
      reports.map(r => (r.bx, r.by)).filter(_._2 == y).toSet.size
    merged.rangeSetSize - beaconCount

  def part2(input: String, searchSpace: BigInt): BigInt =
    val reports = input.split("\n").map(SensorReport.parse)
    @annotation.tailrec
    def loop(y: BigInt, yMax: BigInt): Option[(BigInt, BigInt)] =
      if y >= yMax then None
      else
        val rangeSet = reports
          .flatMap(_.yProjection(y))
          .foldLeft(RangeSet.empty)(_ add _)
        val merged = rangeSet.merge
//        println(s"(y=$y) $merged")
        val rangeOption = for
          range <- merged
          (x0, x1) = range if x0 <= 0 && x1 >= 0
        yield if x1 >= searchSpace then None else Some(x1 + 1)
        rangeOption.flatten.headOption match
          case Some(x) => Some((x, y))
          case None    => loop(y + 1, yMax)
    val Some((x, y)) = loop(0, searchSpace): @unchecked
    x * BigInt(4000000) + y

  def inputResource[F[_]: Sync](path: String): Resource[F, String] =
    Resource
      .fromAutoCloseable(Sync[F].blocking(scala.io.Source.fromFile(path)))
      .map(_.mkString)

  def printAns[F[_]: Console: Sync](path: String) = inputResource[F](path).use {
    input =>
      for
        _ <- Console[F].println(part1(input, BigInt(2000000)))
        _ <- Console[F].println(part2(input, BigInt(4000000)))
      yield ()
  }

  val run = printAns[IO]("input/day15")
