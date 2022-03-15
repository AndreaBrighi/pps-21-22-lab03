package u03

import org.junit.Assert.assertEquals
import org.junit.Test
import u03.Lists.List.*
import u03.Streams.Stream

class StreamTest {

  @Test def testDropStream(): Unit =
    val s = Stream.take(Stream.iterate(0)(_ + 1))(10)
    assertEquals(Cons(6, Cons(7, Cons(8, Cons(9, Nil())))), Stream.toList(Stream.drop(s)(6)))

  @Test def testConstantStream(): Unit =
    val s = Stream.take(Stream.iterate(0)(_ + 1))(10)
    assertEquals(Cons("x", Cons("x", Cons("x", Cons("x", Cons("x", Nil()))))), Stream.toList(Stream.take(Stream.constant("x"))(5)))

  @Test def testFibonacciStream(): Unit =
    val fibs: Stream[Int] = Stream.fib()
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Cons(13, Nil())))))))), Stream.toList(Stream.take(fibs)(8)))
}
