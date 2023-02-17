package dev.sungkm.adventofcode2022.day1to10

import cats.effect.{IO, IOApp, Resource, Sync}
import cats.effect.std.Console
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import scala.collection.immutable.SortedMap

object Day5 extends IOApp.Simple:

  opaque type CrateStacks = Map[Int, List[String]]
  object CrateStacks:
    def apply(stacks: Map[Int, List[String]]): CrateStacks = stacks
    def parse(stackInput: String): CrateStacks =
      val lines = stackInput.split("\n").toList.reverse
      val indexes =
        lines.head.grouped(4).map(_.trim.toInt -> List.empty[String]).toMap
      lines.tail.foldLeft(indexes) { (acc, line) =>
        def parseCrate(s: String): Option[String] =
          Option.when(s.trim.size >= 2)(s.trim.tail.init)
        val cargos = line.grouped(4).map(parseCrate).zipWithIndex

        cargos.foldLeft(acc) { case (acc, (crateOption, index)) =>
          crateOption.fold(acc) { crate =>
            acc.updated(index + 1, crate :: acc(index + 1))
          }
        }
      }
  extension (stacks: CrateStacks)
    def toMap: Map[Int, List[String]] = stacks

    @annotation.tailrec
    def doProcedure(p: ProcedureStep): CrateStacks =
      if p.quantity <= 0 then stacks
      else
        val next = stacks(p.from) match
          case head :: tail =>
            stacks.updated(p.from, tail).updated(p.to, head :: stacks(p.to))
          case Nil => throw new Exception(s"Try to move from empty stack")
        next.doProcedure(p.copy(quantity = p.quantity - 1))

    def doProcedure1(p: ProcedureStep): CrateStacks =
      val (front, back) = stacks(p.from).splitAt(p.quantity)
      stacks.updated(p.from, back).updated(p.to, front ::: stacks(p.to))

  final case class ProcedureStep(
      quantity: Int,
      from: Int,
      to: Int,
  )
  object ProcedureStep:
    def parse(line: String): ProcedureStep =
      val Array(init, to)    = line.split(" to ")
      val Array(init1, from) = init.split(" from ")
      val quantity           = init1.drop("move ".size)
      ProcedureStep(quantity.toInt, from.toInt, to.toInt)

  def parseInput(input: String): (CrateStacks, Seq[ProcedureStep]) =
    val Array(stackInput, procedureInput) = input.split("\n\n")
    (
      CrateStacks.parse(stackInput),
      procedureInput.split("\n").toSeq.map { ProcedureStep.parse },
    )

  def run(
      doProcedure: (CrateStacks, ProcedureStep) => CrateStacks,
  )(input: String): String =
    val (stacks, steps) = parseInput(input)
    val stacks1         = steps.foldLeft(stacks)(doProcedure).toMap
    (1 to stacks1.keySet.size).map(stacks1(_).head).mkString

  def part1(input: String): String = run(_ doProcedure _)(input)

  def part2(input: String): String = run(_ doProcedure1 _)(input)

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

  val run = printAns[IO]("input/day5")
