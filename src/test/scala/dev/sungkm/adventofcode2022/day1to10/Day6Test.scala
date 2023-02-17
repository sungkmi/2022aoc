package dev.sungkm.adventofcode2022
package day1to10

import hedgehog.munit.HedgehogSuite
import hedgehog.*

class Day6Test extends HedgehogSuite:

  test("part1") {
    withMunitAssertions { a =>
      a.assertEquals(Day6.part1("mjqjpqmgbljsphdztnvjfqwrcgsmlb"), 7)
      a.assertEquals(Day6.part1("bvwbjplbgvbhsrlpgdmjqwftvncz"), 5)
      a.assertEquals(Day6.part1("nppdvjthqldpwncqszvftbrmjlhg"), 6)
      a.assertEquals(Day6.part1("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"), 10)
      a.assertEquals(Day6.part1("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"), 11)
    }
  }

  test("part2") {
    withMunitAssertions { a =>
      a.assertEquals(Day6.part2("mjqjpqmgbljsphdztnvjfqwrcgsmlb"), 19)
      a.assertEquals(Day6.part2("bvwbjplbgvbhsrlpgdmjqwftvncz"), 23)
      a.assertEquals(Day6.part2("nppdvjthqldpwncqszvftbrmjlhg"), 23)
      a.assertEquals(Day6.part2("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"), 29)
      a.assertEquals(Day6.part2("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"), 26)
    }
  }
