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
    assertEquals(Some(Bool_()), TypeInference typeInfer (N(3) \>= N(4)))
    assertEquals(Some(Bool_()), TypeInference typeInfer (N(7) \>= N(2)))
    assertEquals(None, TypeInference typeInfer ( N(1) \+ B(true) _tryWith N(3)))
    assertEquals(Some(Int_()), TypeInference typeInfer ( N(1) \+ Raise _tryWith N(3)))
    assertEquals(Some(Func(Int_(), Func(Bool_(), Func(Int_(), Int_())))), TypeInference typeInfer (X("x") \=>: X("y") \=>: X("z") \=>: (X("y") _then (X("x") \+ X("z")) _else Raise)))
    assertEquals(Some(Func(Func(Int_(), Bool_()), Func(Bool_(), Func(Int_(), Bool_())))), TypeInference typeInfer (X("x") \=>: X("y") \=>: X("z") \=>: (X("y") _then (X("x") _app X("z")) _else ((X("w") \=>: (X("w") \>= X("z"))) _app X("z")))))
  }

}