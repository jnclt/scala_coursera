package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    assert(contains(singletonSet(1), 1))
  }

  test("singletonSet(1) doesn't contain 2") {
    assert(!contains(singletonSet(1), 2))
  }

  test("union contains all elements of each set") {
      val s = union(singletonSet(1), singletonSet(2))
      assert(contains(s, 1), "contains 1")
      assert(contains(s, 2), "contains 2")
      assert(!contains(s, 3), "doesn't contain 3")
  }

  test("intersect contains common elements from each set") {
      val u1 = union(singletonSet(1), singletonSet(2))
      val u2 = union(singletonSet(2), singletonSet(3))
      val i = intersect(u1, u2)
      assert(contains(i, 2), "contains 2")
      assert(!contains(i, 1), "doesn't contain 1")
      assert(!contains(i, 3), "doesn't contain 3")
  }

  test("diff contains elements present only in the first set") {
      val u1 = union(singletonSet(1), singletonSet(2))
      val u2 = union(singletonSet(2), singletonSet(3))
      val d = diff(u1, u2)
      assert(contains(d, 1), "contains 1")
      assert(!contains(d, 2), "doesn't contain 2")
      assert(!contains(d, 3), "doesn't contain 3")
  }

  test("filtered set contains only elements from the original set satisfying predicate") {
    val s = union(singletonSet(2), union(singletonSet(3), singletonSet(4)))
    val f = filter(s, (x: Int) => x % 2 == 0)
    assert(contains(f, 2))
    assert(contains(f, 4))
    assert(!contains(f, 3))
    assert(!contains(f, 6))
  }

  test("forall checks condition for all elements in the set") {
    val s = (x:Int) => (x % 2 == 0) && (x > 100)
    assert(forall(s, (x:Int) => x > 50))
    assert(forall(s, (x:Int) => x > 100))
    assert(forall(s, (x:Int) => x % 2 == 0))
    assert(!forall(s, (x:Int) => x % 4 == 0))
    assert(!forall(s, (x:Int) => x == 101))
  }

  test("exists") {
    val s = (x:Int) => (x % 2 == 0) && (x > 100)
    assert(exists(s, (x:Int) => x == 102))
    assert(exists(s, (x:Int) => x > 100))
    assert(!exists(s, (x:Int) => x % 2 == 1))
    assert(!exists(s, (x:Int) => x == 101))
  }

  test("map") {
    val s = (x:Int) => (x % 2 == 0) && (x > 100)
    def inc(x:Int): Int = x+1
    val m = map(s, inc)
    assert(forall(m, (x:Int) => x % 2 == 1))
    assert(forall(m, (x:Int) => x > 102))
    assert(!exists(m, (x:Int) => x == 101))
  }
}
