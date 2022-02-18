package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for
      x <- arbitrary[Int]
      h <- genHeap
    yield insert(x, h)
  )
  given Arbitrary[H] = Arbitrary(genHeap)

  def getList(h: H, acc: List[Int]): List[Int] =
      //println(s"h is $h,acc is $acc")
      if isEmpty(h) then acc
      else
        val min = findMin(h)
        val newAcc = min :: acc
        getList(deleteMin(h), newAcc)


  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("two min in empty") = forAll { (h: H) =>
    if isEmpty(h) then
      val lvl1 = insert(1, h)
      val lvl2 = insert(2, lvl1)
      findMin(lvl2) == 1
    else findMin(h) == findMin(h)
  }

  property("one del in empty should be empty") = forAll { (h: H) =>
    if isEmpty(h) then
      val inserted = insert(1, h)
      val deleted = deleteMin(inserted)
      deleted == empty
    else empty == empty

  }

  property("all elements are melded") = forAll{
    (h1:H, h2:H) => 
      val list1 = getList(h1,Nil)
      val list2 = getList(h2,Nil)
      val list12 = list1 ++ list2

      val h3 = meld(h1,h2)
      val list3 = getList(h3,Nil)

      list12.sorted == list3.sorted



  }

  property("minimum is sorted") = forAll { (h: H) =>
    //println("====================================================")

    val reversed = getList(h, Nil)
    reversed
      .sliding(2)
      .forall(p =>
        p match
          case Nil        => true
          case List(_)    => true
          case List(x, y) => x >= y
      )

  }

  property("melding should be ordered") = forAll { (h1raw: H, h2raw: H) =>
    println("=======================================")
    val h1 = if isEmpty(h1raw) then insert(Int.MinValue, empty) else h1raw
    val h2 = if isEmpty(h2raw) then insert(Int.MinValue, empty) else h2raw
    val h12 = meld(h1, h2)
    println(s"h1 is $h1, min h1 is ${findMin(h1)}")
    println(s"h2 is $h2, min h1 is ${findMin(h2)}")
    println(s"h12 is $h12, min h1 is ${findMin(h12)}")
    findMin(h12) == findMin(h1) || findMin(h12) == findMin(h2)
  }
