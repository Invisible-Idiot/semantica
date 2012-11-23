/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package interpreterl3


abstract class TypeOperand
case class B_() extends TypeOperand
case class N_() extends TypeOperand
case class Fn_ (T1 : TypeOperand, T2 : TypeOperand) extends TypeOperand
case class Variable(v : Int) extends TypeOperand

abstract class TypeEquation(op1 : TypeOperand, op2: TypeOperand);

object TypeCheck {
 
  var nextVar : Int = 0
  def newInt(a : Unit) : Int = 
    {
      nextVar = nextVar + 1
      nextVar
    }

  
  def typecheck(e : Expr) : (TypeOperand,Set[TypeEquation]) = {
    e match {
      case If(e1,e2,e3)=>
        {
          var T1 : Variable = Variable(newInt())
          var T2 : Variable = Variable(newInt())
           (typecheck(e2)._t1,
            TypeEquation(typecheck(e1)._1,B_()) ++ // tipo de e1 == bool
            TypeEquation(T1,T2) ++ // T1==T2
            TypeEquation(T1,typecheck(e2)._1) ++ // tipo e2 = T1
            TypeEquation(T2,typecheck(e3)._1) ++ // tipo e3 = T2
            typecheck(e1)._2 ++ // adiciona os subconjuntos
            typecheck(e2)._2 ++
            typecheck(e3)._2
           )
        }
    }
  }
    
}




