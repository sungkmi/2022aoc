package dev.sungkm.adventofcode2022
package day1to10

import hedgehog.munit.HedgehogSuite
import hedgehog.*

class Day7Test extends HedgehogSuite:

  val input = """$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"""

  test("part1") {
    withMunitAssertions { a =>
      a.assertEquals(Day7.part1(input), BigInt("95437"))
    }
  }

  test("part2") {
    withMunitAssertions { a =>
      a.assertEquals(Day7.part2(input), BigInt("24933642"))
    }
  }
