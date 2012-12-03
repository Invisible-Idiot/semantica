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
    val e : Expr = (X("x") \=>: ((N(42) \>= X("x")) _then X("x") _else (X("x") \+ N(-1)))) _app (Raise \+ N(1) _tryWith N(45))
    val inferedType = TypeInference typeInfer e
    val result = Interpreter eval e 
    
    println("Expressão fornecida:")
    println(e.toString)
    
    inferedType match {
      case None => println("A expressão não é bem-tipada.")
      case Some(typeOperand) => {
          println("Tipo inferido: " + typeOperand.toString)
          
          result match {
            case None => println("A expressão levou a um erro de execução.")
            case Some(typeOperand) => println("Resultado da avaliação: " + typeOperand.toString)
          }
      }
    }
  }
}