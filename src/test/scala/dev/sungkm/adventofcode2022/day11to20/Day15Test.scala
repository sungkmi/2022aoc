package dev.sungkm.adventofcode2022
package day11to20

import hedgehog.munit.HedgehogSuite
import hedgehog.*
import dev.sungkm.adventofcode2022.day11to20.Day15.SensorReport

class Day15Test extends HedgehogSuite:

  val input = """Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3"""

  test("yProjection"):
    withMunitAssertions: a =>
      val sr = SensorReport.parse("Sensor at x=8, y=7: closest beacon is at x=2, y=10")
      a.assertEquals(sr.yProjection(10), Some((BigInt(2), BigInt(14))))

  test("part1"):
    withMunitAssertions: a =>
//      a.assertEquals(Day15.part1(input, 9), BigInt(25))
      a.assertEquals(Day15.part1(input, 10), BigInt(26))
//      a.assertEquals(Day15.part1(input, 11), BigInt(25))

  test("part2"):
    withMunitAssertions: a =>
      a.assertEquals(Day15.part2(input, 20), BigInt(56000011))
