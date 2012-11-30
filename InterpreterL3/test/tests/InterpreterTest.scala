/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package tests

import org.junit._
import Assert._
import interpreterl3._
import ExprBuilder._

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
    val e : Expr =
      X("x") \<- (X("x") \=>: (X("x") \>= N(target))) _in {
        X("f") \<- (X("y") \=>: X("count") \=>: X("next") \=>: {
          (X("x") _app X("y")) _then X("count")  _else
             X("next") _app (X("y") \+ N(incr)) _app (X("count") \+ N(1)) _app X("next")
        }) _in
        (X("f") _app N(seed) _app N(0) _app X("f"))
      }
    
    //println(e)

    return e
  }
  
  //@Test
  def testComplex = {
    val eval = (seed : Int, target : Int, incr : Int) => Interpreter eval complexExpression(seed, target, incr)
    
    assertEquals(Some(N(4)), eval(6, 42, 9))
    assertEquals(Some(N(18)), eval(23, 110, 5))
    assertEquals(Some(N(0)), eval(41, 41, 0))
    assertEquals(Some(N(100)), eval(0, 100, 1))
  }
  
  @Test
  def testSuit = {
    assertEquals(Some(X("x") \=>: X("x")), Interpreter eval (X("x") \=>: X("x")))
    assertEquals(Some(N(1)), Interpreter eval ((X("x") \=>: X("x")) _app N(1)))
    assertEquals(Some(Raise), Interpreter eval (Raise))
    assertEquals(Some(Raise), Interpreter eval (Raise \+ N(1)))
    assertEquals(Some(Raise), Interpreter eval (Raise _then Raise _else (Raise \+ N(1))))
    assertEquals(None, Interpreter eval ((X("x") \=>: (X("x") \+ N(1))) _app B(true)))
  }
}
