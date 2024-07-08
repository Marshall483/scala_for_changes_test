package scala.list.sorts

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SortSuite extends AnyFunSuite {
  test("empty list") {
    val sorters = FList(InsertSorter, SelectSorter, MergeSorter, QuickSorter)
    sorters.foreach { s =>
        val empty: FList[(String, Int)] = Nil
        val result = s.sorted(empty)
        assert(empty == result) 
    }
  }

  test("single element list") {
    val sorters = FList(InsertSorter, SelectSorter, MergeSorter, QuickSorter)
    sorters.foreach { s =>
        val input: FList[(String, Int)] = FList(("hello", 123))
        val output = s.sorted(input)
        assert(input == output)
    }
  }

  test("two elements with same key") {
    val sorters = FList(InsertSorter, SelectSorter, MergeSorter, QuickSorter)
    sorters.foreach { s =>
        val input: FList[(String, Int)] = FList(("hello", 123), ("hello", 111))
        val output = s.sorted(input)
        assert(input == output)
    }
  }

  test("several elements list") {
    val sorters = FList(InsertSorter, SelectSorter, MergeSorter, QuickSorter)
    sorters.foreach { s =>
        val input: FList[(String, Int)] = FList(("hello", 123), ("abc", 20), ("hello", 555), ("qwe", 12), ("hello", 0))
        val output = s.sorted(input)
        val expected = FList(("abc", 20), ("hello", 123), ("hello", 555), ("hello", 0), ("qwe", 12))
        assert(input == expected)
    }
  }

}
