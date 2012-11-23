/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package interpreterl3


abstract class TypeOperand
case class B() extends TypeOperand
case class N() extends TypeOperand
case class Fn (x : TypeOperand, e : TypeOperand) extends TypeOperand
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
          var newVar1 : Variable = Variable(newInt())
          var newVar2 : Variable = Variable(newInt())
           (typecheck(e2)._t1,
            TypeEquation(typecheck(e1)._1,B()) ++ 
            TypeEquation(newVar1,newVar2) ++
            TypeEquation(newVar1,typecheck(e2)._1) ++
            TypeEquation(newVar2,typecheck(e3)._1) ++
            typecheck(e1)._2 ++
            typecheck(e2)._2 ++
            typecheck(e3)._2
           )
        }
    }
  }
    
}




