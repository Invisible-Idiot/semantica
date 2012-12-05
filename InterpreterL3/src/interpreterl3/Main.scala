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
    val e : Expr = X("x") \=>: X("Y")
    
    println("Expressão fornecida:")
    println(e.toString)
    
    try {
      val inferedType = TypeInference typeInfer e
      val result = Interpreter eval e

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
    catch {
      case _ => println("Identificador não defidio.")
    }
  }
}