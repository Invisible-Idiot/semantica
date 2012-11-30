/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package interpreterl3

import ExprBuilder._

object Main {

  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]): Unit = {
    TypeInference typeInfer ((X("x") \=>: (X("x") \+ N(1))) _app B(true))
  }

}
