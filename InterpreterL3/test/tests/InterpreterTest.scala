/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package tests

import org.junit._
import Assert._
import interpreterl3._

class InterpreterTest {

  @Before
  def setUp: Unit = {
  }

  @After
  def tearDown: Unit = {
  }
  
  def complexExpression(seed : Int, target : Int, incr : Int) : Expr = {
    /*
     * let x = fn x => x >= 42 in
     * let f = fn y => fn count => fn next =>
     *    if x y
     *    then count
     *    else next (y + 9) (count + 1) next
     * in f 6 0 f
     * ->
     * 4
     */
    val e =
      Let(X("x"), Fn(X("x"), Op(LargerOrEqual, X("x"), N(target))),
        Let(X("f"), Fn(X("y"), Fn(X("count"), Fn(X("next"),
          If(App(X("x"), X("y")),
             X("count"),
             App(App(App(X("next"), Op(Plus, X("y"), N(incr))), Op(Plus, X("count"), N(1))), X("next")))))),
        App(App(App(X("f"), N(seed)), N(0)), X("f"))))

    return e
  }
  
  @Test
  def testComplex = {
    val eval = (seed : Int, target : Int, incr : Int) => Interpreter eval complexExpression(seed, target, incr)
    
    assertEquals(Some(N(4)), eval(6, 42, 9))
    assertEquals(Some(N(18)), eval(23, 110, 5))
    assertEquals(Some(N(0)), eval(41, 41, 0))
    assertEquals(Some(N(100)), eval(0, 100, 1))
  }
}
