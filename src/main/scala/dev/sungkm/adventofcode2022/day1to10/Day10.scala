package dev.sungkm.adventofcode2022.day1to10

import scala.collection.immutable.{SortedMap, SortedSet}

import cats.effect.{IO, IOApp, Resource, Sync}
import cats.effect.std.Console
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*

object Day10 extends IOApp.Simple:

  enum Instruction:
    case Noop
    case Addx(v: Int)
  object Instruction:
    def parse(line: String): Instruction = if line == "noop" then Noop
    else
      val splitted = line.split(" ")
      assert(splitted(0) == "addx" && splitted.size == 2)
      Addx(splitted(1).toInt)
  export Instruction.*

  enum Event:
    case ReadSignal
    case ExecuteInstruction(i: Instruction)
  export Event.*

  case class CycleEvent(cycle: Int, event: Event)
  given Ordering[CycleEvent] = Ordering.by(e => (e.cycle, e.event.ordinal))

  def signalStrength(instructions: List[Instruction]): BigInt =

    def loop(
        cycle: Int,
        x: BigInt,
        acc: Map[Int, BigInt],
        readSignals: List[Int],
        instructions: List[Instruction],
    ): Map[Int, BigInt] = readSignals match
      case Nil => acc
      case nextRead :: tail =>
        instructions match
          case Nil => acc ++ readSignals.map(c => c -> x * c).toMap
          case ins :: remainder =>
            val (cycle1, x1) = ins match
              case Noop    => (cycle + 1, x)
              case Addx(v) => (cycle + 2, x + v)
            val (acc1, readSignals1) =
              if nextRead > cycle1 then (acc, readSignals)
              else (acc + (nextRead -> x * nextRead), tail)
            loop(cycle1, x1, acc1, readSignals1, remainder)
    loop(
      0,
      1,
      SortedMap.empty,
      (20 to 220 by 40).toList,
      instructions,
    ).view.values.sum

  def crtImage(instructions: List[Instruction]): List[String] =

    def pixels(x: BigInt)(from: Int, until: Int): List[String] =
      (from until until).toList.map { c =>
        if ((c % 40) - x).abs <= 1 then "#" else "."
      }

    def loop(
        cycle: Int,
        x: BigInt,
        acc: List[String],
        instructions: List[Instruction],
    ): List[String] =
      instructions match
        case Nil => acc.reverse ::: pixels(x)(cycle, 240)
        case ins :: remainder =>
          val (cycle1, x1) = ins match
            case Noop    => (cycle + 1, x)
            case Addx(v) => (cycle + 2, x + v)
          val acc1 = pixels(x)(cycle, cycle1).reverse ::: acc
          loop(cycle1, x1, acc1, remainder)
    loop(0, 1, Nil, instructions).sliding(40, 40).map(_.mkString).toList

  def part1(input: String): BigInt =
    signalStrength(input.split("\n").map(Instruction.parse).toList)

  def part2(input: String): List[String] =
    crtImage(input.split("\n").map(Instruction.parse).toList)

  def inputResource[F[_]: Sync](path: String): Resource[F, String] =
    Resource
      .fromAutoCloseable(Sync[F].blocking(scala.io.Source.fromFile(path)))
      .map(_.mkString)

  def printAns[F[_]: Console: Sync](path: String) = inputResource[F](path).use {
    input =>
      import cats.syntax.traverse.*
      for
        _ <- Console[F].println(part1(input))
        _ <- part2(input).traverse(Console[F].println)
      yield ()
  }

  val run = printAns[IO]("input/day10")
