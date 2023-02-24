package dev.sungkm.adventofcode2022.day1to10

import cats.effect.{IO, IOApp, Resource, Sync}
import cats.effect.std.Console
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*

object Day7 extends IOApp.Simple:

  final case class File(path: List[String], size: BigInt)

  sealed trait CommandResult
  final case class CD(dir: String)         extends CommandResult
  final case class LS(files: List[BigInt]) extends CommandResult

  def parse(input: String): List[CommandResult] =
    input
      .split("\n\\$ ")
      .tail
      .map { part =>
        part.split("\n").toList match
          case command :: result =>
            if command startsWith "cd" then CD(command.split(" ").last)
            else
              LS(result.flatMap { line =>
                val Array(first, second) = line.split(" ")
                if first === "dir" then Nil else List(BigInt(first))
              })
          case Nil => throw new Exception("Empty List")
      }
      .toList

  def getFiles(commandResults: List[CommandResult]): List[File] =
    commandResults
      .foldLeft((List.empty[String], List.empty[File])) {
        case ((path, acc), cr) =>
          cr match
            case CD(dir) =>
              val path1 = if dir == ".." then path.tail else dir :: path
              (path1, acc)
            case LS(files) =>
              (path, files.map(size => File(path, size)) ::: acc)
      }
      ._2

  def getDirSizes(input: String): Map[List[String], BigInt] =
    val pathFiles = for
      file <- getFiles(parse(input))
      path <- file.path.scanRight(List.empty[String])(_ :: _)
    yield (path, file.size)

    pathFiles.groupMapReduce(_._1)(_._2)(_ + _)

  def part1(input: String): BigInt =
    getDirSizes(input).values.filter(_ < 100000).sum

  def part2(input: String): BigInt =
    val dirSizes = getDirSizes(input)

    dirSizes.values.filter(_ >= dirSizes(Nil) - BigInt("40000000")).min

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

  val run = printAns[IO]("input/day7")
