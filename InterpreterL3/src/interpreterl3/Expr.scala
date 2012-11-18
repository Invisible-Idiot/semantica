/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package interpreterl3

sealed abstract class Expr {
  override def toString() : String = this match {
    case Raise => "raise"
    case N(n) => n.toString
    case B(b) => b.toString
    case Fn(x, e) => "fn " + x.toString + " =>\n" + e.toString
    case Op(op, e1, e2) => "(" + e1.toString + " " + op.toString + " " + e2.toString + ")"
    case If(e1, e2, e3) => "if " + e1.toString + "\nthen " + e2.toString + "\nelse " + e3.toString
    case App(e1, e2) => "(" + e1.toString + " " + e2.toString + ")"
    case X(name) => name
    case Let(x, e1, e2) => "let " + x.toString + " = " + e1.toString + "\nin\n" + e2.toString + "\nend"
    case TryWith(e1, e2) => "try\n" + e1.toString + "\nwith\n" + e2.toString
  }
}

  sealed abstract class Final extends Expr // Representa expressões que chegaram ao final da avaliação
    case object Raise extends Final // Representa um resultado de erro da computação
    sealed abstract class Value extends Final // Representa resultados válidos da computação
      case class N(n : Int) extends Value // Representa valores inteiros
      case class B(b : Boolean) extends Value // Representa valores booleanos
      case class Fn(x : X, e : Expr) extends Value // Representa função
  case class Op(op : Operator, e1 : Expr, e2 : Expr) extends Expr// Representa uso de operadores
  case class If(e1 : Expr, e2 : Expr, e3 : Expr) extends Expr // Representa expressão if-then-else
  case class App(e1 : Expr, e2 : Expr) extends Expr // Representa aplicação de função
  case class X(name : String) extends Expr // Representa identificador dentro de função ou bloco de let
  case class Let(x : X, e1 : Expr, e2 : Expr) extends Expr // Representa bloco let..in
  case class TryWith(e1 : Expr, e2 : Expr) extends Expr // Representa bloco try..with

sealed abstract class Operator {
  override def toString() : String = this match {
    case Plus => "+"
    case LargerOrEqual => ">="
  }
}

case object Plus extends Operator // Operador de soma
case object LargerOrEqual extends Operator // Operador de "maior ou igual"