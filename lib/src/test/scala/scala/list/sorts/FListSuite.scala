package scala.list.sorts

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FListSuite extends AnyFunSuite {
  test("empty list") {
    val empty: FList[String] = FList[String]()
    assert(empty.isInstanceOf[Nil.type]) 
  }

  test("single element list") {
    val single: FList[String] = FList[String]("hello")
    assert(single.isInstanceOf[Cons[_]])
  }

  test("stress") {
    val longList = (0 to 1000000).toList
    val data = FList(longList:_*)
    assert(data.isInstanceOf[Cons[_]])
  }

  test("equals empty-empty") {
    assert(FList() == FList())
    assert(Nil == Nil)
    assert(FList() == Nil)
    assert(Nil == FList())
  }

  test("equals empty-single") {
    assert(FList() != FList("123"))
    assert(Nil != Cons("123", Nil))
    assert(FList("123") != Nil)
    assert(Nil != FList("234"))
  }

  test("equals single-single") {
    assert(FList("123") == FList("123"))
    assert(Cons("123", Nil) == Cons("123", Nil))
    assert(FList("123") == Cons("123", Nil))
    assert(Cons("123", Nil) == FList("123"))
  }

  test("not equals single-single") {
    assert(FList("123") != FList(""))
    assert(Cons("123", Nil) != Cons("13", Nil))
    assert(FList("23") != Cons("123", Nil))
    assert(Cons("123", Nil) != FList("1234"))
  }

  test("equals stress") {
    val longList1 = (0 to 1000000).toList
    val longList2 = (0 to 1000000).toList
    val data1 = FList(longList1:_*)
    val data2 = FList(longList2:_*)

    assert(data1 == data2)
  }

  test("not equals stress") {
    val longList1 = (0 to 1000000).toList ++ List(123)
    val longList2 = (0 to 1000000).toList ++ List(234)
    val data1 = FList(longList1:_*)
    val data2 = FList(longList2:_*)

    assert(data1 != data2)
  }
}
