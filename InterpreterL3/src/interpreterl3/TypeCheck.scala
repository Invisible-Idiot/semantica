/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package interpreterl3


abstract class TypeOperand
case class B(b : Boolean) extends TypeOperand
case class N(n : Int) extends TypeOperand
case class Fn (x : X, e : Expr) extends TypeOperand
case class Variable(v : String) extends TypeOperand

abstract class TypeEquation(op1 : TypeOperand, op2: TypeOperand);

object TypeCheck {
  def collect(e : Expr, t : Set[TypeEquation]) : Set[TypeEquation] =
    {
      e match{
        case If => t ++ TypeEquation(e.)
      } 
    }
}




