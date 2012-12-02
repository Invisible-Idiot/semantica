/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package tests

import org.junit._
import Assert._
import interpreterl3._
import ExprBuilder._

class TypeInferenceTest {

  @Before
  def setUp: Unit = {
  }

  @After
  def tearDown: Unit = {
  }

  @Test
  def testTypeInference = {
    assertEquals(None, TypeInference typeInfer (X("x") \=>: X("x")))
    assertEquals(Some(Int_()), TypeInference typeInfer ((X("x") \=>: X("x")) _app N(1)))
    assertEquals(None, TypeInference typeInfer (Raise))
    assertEquals(Some(Int_()), TypeInference typeInfer (Raise \+ N(1)))
    assertEquals(Some(Int_()), TypeInference typeInfer (Raise _then Raise _else (Raise \+ N(1))))
    assertEquals(None, TypeInference typeInfer ((X("x") \=>: (X("x") \+ N(1))) _app B(true)))
  }

}