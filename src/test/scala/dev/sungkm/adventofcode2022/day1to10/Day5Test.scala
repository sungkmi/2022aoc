package dev.sungkm.adventofcode2022
package day1to10

import hedgehog.munit.HedgehogSuite
import hedgehog.*

import Day5.*

class Day5Test extends HedgehogSuite:

  val input = """
    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"""


  test("parseInput") {
    withMunitAssertions { assertions =>

      val expectedStack = CrateStacks(Map(
        1 -> List("N", "Z"),
        2 -> List("D", "C" , "M"),
        3 -> List("P")
      ))

      val procedureStep = Seq(
        ProcedureStep(1, 2, 1),
        ProcedureStep(3, 1, 3),
        ProcedureStep(2, 2, 1),
        ProcedureStep(1, 1, 2),
      )

      assertions.assertEquals(parseInput(input), (expectedStack, procedureStep))
    }
  }

  test("part1") {
    withMunitAssertions { assertions =>

      assertions.assertEquals(Day5.part1(input), "CMZ")
    }
  }

  test("part2") {
    withMunitAssertions { assertions =>
      assertions.assertEquals(Day5.part2(input), "MCD")

    }
  }
