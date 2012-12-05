/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package interpreterl3

abstract class TypeOperand{
  override def toString() : String = this match {
    case Bool_() => "bool"
    case Int_() => "int"
    case Func(t1, t2) => "(" + t1.toString + " -> " + t2.toString + ")"
    case Variable(v) => "X" + v.toString
  }
}

case class Bool_() extends TypeOperand
case class Int_() extends TypeOperand
case class Func(T1 : TypeOperand, T2 : TypeOperand) extends TypeOperand
case class Variable(v : Int) extends TypeOperand

case class TypeEquation(op1 : TypeOperand, op2: TypeOperand) {
  override def toString() : String = op1.toString + " = " + op2.toString
}
