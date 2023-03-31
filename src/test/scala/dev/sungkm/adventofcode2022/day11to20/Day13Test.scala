package dev.sungkm.adventofcode2022
package day11to20

import hedgehog.munit.HedgehogSuite
import hedgehog.*

class Day13Test extends HedgehogSuite:

  val input = """[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]"""

  test("part1"):
    withMunitAssertions: a =>
      a.assertEquals(Day13.part1(input), BigInt(13))

  test("part2"):
    withMunitAssertions: a =>
      a.assertEquals(Day13.part2(input), BigInt(140))
