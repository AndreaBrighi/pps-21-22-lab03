package u03

import org.junit.*
import org.junit.Assert.*
import Lists.*
import u02.Modules.Person
import u02.Optionals.Option.*

class ListTaskTest:

  import List.*

  private val lst = Cons(10, Cons(20, Cons(30, Nil())))
  private val l = Cons(3,Cons(7,Cons(1,Cons(5, Nil()))))

  @Test def testDrop(): Unit =
    assertEquals(Cons(20, Cons(30, Nil())), drop(lst, 1))
    assertEquals(Cons(30, Nil()), drop(lst, 2))
    assertEquals(Nil(), drop(lst, 3))

  @Test def testAppend(): Unit =
    val tail = Cons(40, Nil())
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Nil())))), append(lst, tail))

  @Test def testFlatMap(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(lst)(v => Cons(v + 1, Nil())))
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))), flatMap(lst)(v => Cons(v + 1, Cons(v + 2, Nil()))))

  @Test def testMax(): Unit =
    assertEquals(Some(25), max(Cons(10, Cons(25, Cons(20, Nil())))))
    assertEquals(None(), max(Nil()))

  @Test def testOnlyCourses(): Unit =
    val list = Cons(Person.Teacher("Viroli", "pps"), Cons(Person.Student("Brighi", 2018), Nil()))
    assertEquals(Cons("pps", Nil()), onlyCourses(list))

  @Test def testOnlyCourses2(): Unit =
    val list = Cons(Person.Teacher("Viroli", "pps"), Cons(Person.Student("Brighi", 2018), Nil()))
    assertEquals(Cons("pps", Nil()), onlyCourses2(list))

  @Test def testFoldLeft(): Unit =
    assertEquals(-16, foldLeft(l)(0)(_ - _))
    assertEquals(-15, foldLeft(l)(1)(_ - _))

  @Test def testFoldRight(): Unit =
    assertEquals(-8, foldRight(l)(0)(_ - _))
    assertEquals(-7, foldRight(l)(1)(_ - _))