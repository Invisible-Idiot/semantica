/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package interpreterl3


abstract class TypeOperand
case class Bool_() extends TypeOperand
case class Int_() extends TypeOperand
case class Func(T1 : TypeOperand, T2 : TypeOperand) extends TypeOperand
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
            TypeEquation(typecheck(e1)._1,Bool_()) ++ // tipo de e1 == bool
            TypeEquation(T1,T2) ++ // T1==T2
            TypeEquation(T1,typecheck(e2)._1) ++ // tipo e2 = T1
            TypeEquation(T2,typecheck(e3)._1) ++ // tipo e3 = T2
            typecheck(e1)._2 ++ // adiciona os subconjuntos
            typecheck(e2)._2 ++
            typecheck(e3)._2
           )
        }
      case Op(op,e1,e2)=>
        {
          op match{
            case Plus =>
              {
                    var T1 : Variable = Variable(newInt())
                    var T2 : Variable = Variable(newInt())
                    (Int(),
                      TypeEquation(T1,Int_()) ++
                      TypeEquation(T2,Int_()) ++
                      typecheck(e1)._2 ++
                      typecheck(e2)._2
                    )
                    
              }
            case LargerOrEqual =>
              {
                     var T1 : Variable = Variable(newInt())
                      var T2 : Variable = Variable(newInt())
                    (Bool(),
                      TypeEquation(T1,Bool_()) ++
                      TypeEquation(T2,Bool_()) ++
                      typecheck(e1)._2 ++
                      typecheck(e2)._2
                     )
              }
          }
        }
      case App(e1,e2)=>
         {
            var T1 : Variable = Variable(newInt())
            var T2 : Variable = Variable(newInt())
            (T2,
             TypeEquation(Func(T1,T2),typecheck(e1)._1) ++
             TypeEquation(T1,typecheck(e2)._1) ++
             typecheck(e1)._2 ++
             typecheck(e2)._2
             
            )
            
         }
      case N => Int_()
      case B => Bool_()
      case Fn(x,e) => 
    }
  }
       
  def unify(c : Set[(TypeOperand, TypeOperand)]) : Option[Set[(TypeOperand, TypeOperand)]] = 
  {
    while(!c.isEmpty)
      {
        for(equation <- c)
          {
            equation match
            {
              case OBA
            }
          }
      }
    
  }   
}




