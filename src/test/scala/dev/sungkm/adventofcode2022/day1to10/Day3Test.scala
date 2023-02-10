package dev.sungkm.adventofcode2022
package day1to10

import hedgehog.munit.HedgehogSuite
import hedgehog.*

class Day3Test extends HedgehogSuite:

  val input = """vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"""


  test("part1") {
    withMunitAssertions { assertions =>

      assertions.assertEquals(Day3.part1(input), BigInt(157))
    }
  }

  test("part2") {
    withMunitAssertions { assertions =>

      assertions.assertEquals(Day3.part2(input), BigInt(70))
    }
  }
