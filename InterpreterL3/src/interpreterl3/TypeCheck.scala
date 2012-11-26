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

case class TypeEquation(op1 : TypeOperand, op2: TypeOperand);

object TypeCheck {
 
  var nextVar : Int = 0
  def newInt(a : Unit) : Int = 
    {
      nextVar = nextVar + 1
      nextVar
    }

  
  def typecheck(e : Expr, gamma: Map[String,TypeOperand]) : (TypeOperand,Set[TypeEquation]) = {
    e match {
      case If(e1,e2,e3)=>
        {
          var T1 : Variable = Variable(newInt())
          var T2 : Variable = Variable(newInt())
           (typecheck(e2,gamma)._t1,
            TypeEquation(typecheck(e1,gamma)._1,Bool_()) ++ // tipo de e1 == bool
            TypeEquation(T1,T2) ++ // T1==T2
            TypeEquation(T1,typecheck(e2,gamma)._1) ++ // tipo e2 = T1
            TypeEquation(T2,typecheck(e3,gamma)._1) ++ // tipo e3 = T2
            typecheck(e1,gamma)._2 ++ // adiciona os subconjuntos
            typecheck(e2,gamma)._2 ++
            typecheck(e3,gamma)._2
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
                      typecheck(e1,gamma)._2 ++
                      typecheck(e2,gamma)._2
                    )
                    
              }
            case LargerOrEqual =>
              {
                     var T1 : Variable = Variable(newInt())
                      var T2 : Variable = Variable(newInt())
                    (Bool(),
                      TypeEquation(T1,Bool_()) ++
                      TypeEquation(T2,Bool_()) ++
                      typecheck(e1,gamma)._2 ++
                      typecheck(e2,gamma)._2
                     )
              }
          }
        }
      case App(e1,e2)=>
         {
            var T1 : Variable = Variable(newInt())
            var T2 : Variable = Variable(newInt())
            (T2,
             TypeEquation(Func(T1,T2),typecheck(e1,gamma)._1) ++
             TypeEquation(T1,typecheck(e2,gamma)._1) ++
             typecheck(e1)._2 ++
             typecheck(e2)._2
             
            )
            
         }
      case N => (Int_(),Set())
      case B => (Bool_(),Set())
      case X(name) => (gamma.get(name),Set())
      case Fn(x,e) => 
        {
          gamma+=(x -> Variable(newInt()))
          (typecheck(e,gamma)._1,
           typecheck(e._2,gamma))
        }
      case Raise => (Variable(newInt()),Set())
      case TryWith(e1,e2) => 
        {
          (typecheck(e1,gamma)._1,
           TypeEquation(typecheck(e1,gamma)._1,typecheck(e2,gamma)._1) ++
           typecheck(e1,gamma)._2 ++
           typecheck(e2,gamma)._2
          )
          
        }
  }
  }
    
       
  def unify(c : Set[TypeEquation]) : Set[TypeEquation] = 
  {   
    var aux = c;
    var result : Set[TypeEquation] = Set();
    var modified = false;
    while(!modified)
      {
        modified = false;
        for(equation <- aux)
          {
            equation match
            {
              case TypeEquation(Func(t1, t2), Func(t3,t4)) => 
                {
                    modified = true
                    result = result ++ Set(TypeEquation(t1,t3),TypeEquation(t2,t4))
                }
                
              case TypeEquation(t1 : Variable, Func(t2,t3)) =>
                 {
                    modified = true;
                    result = result ++ Set(TypeEquation(t1,Func(t2,t3)))
                 }
              case TypeEquation( Func(t1,t2), t3 : Variable) =>
                 {
                    modified = true;
                    result = result ++ Set(TypeEquation(Func(t1,t2), t3))
                 }
              case TypeEquation(t1, Func(t2,t3)) =>
                 {
                   return null
                 }
              case TypeEquation( Func(t1,t2), t3) =>
                 {
                    return null
                 }
              case TypeEquation(t1, t2 : Variable) =>
                {
                   result = result ++ Set(TypeEquation(t1, t2));
                }
              case TypeEquation(t1 : Variable, t2) =>
                {
                   result = result ++ Set(TypeEquation(t2, t1));                  
                }
              case TypeEquation(t1, t2) =>
                {
                  if(t1 equals t2)
                    {
                      //faznada
                    }
                  else
                    return null;
                }
              case _ => 
                {}
            }
          }
         aux=result;
      }
      
    return result;
    
  }
  
  def substitution(t : TypeOperand, c :  Map[Variable, TypeOperand]) : Option[TypeOperand] =
    {  
      t match
      {
        case Bool_() => Some(Bool_())
        case Int_() => Some(Int_())
        case x : Variable => c.get(x)
        case Func(t1, t2) => //Func(find(t1, v, c), find(t2, v, c))
          (substitution(t1,c),substitution(t2,c)) 
          match 
          { 
            case (Some( a),Some( b)) => Some(Func(a,b)) 
          case _ => None 
          }
      }
    }
    
  def find(t : TypeOperand, v : Variable, c : Map[Variable, TypeOperand]) : Option[TypeOperand] = 
    {
      t match
      {
        case Bool_() => Some(Bool_())
        case Int_() => Some(Int_())
        case Variable(int) => 
          if(t equals v) 
            c.get(v)
          else
            None;
        case Func(t1, t2) => //Func(find(t1, v, c), find(t2, v, c))
          (find(t1,v,c),find(t2,v,c)) match { case (Some( a),Some( b)) => Some(Func(a,b)) case _ => None }
      }
    }
  
  
    
}