/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package interpreterl3

object TypeInference {
 
  private var nextVar : Int = 0
  private def newInt(a : Unit) : Int = 
    {
      nextVar = nextVar + 1
      nextVar
    }

  
  private def typecheck(e : Expr, gamma: Map[String,TypeOperand]) : (TypeOperand,Set[TypeEquation]) =
    e match {
      case If(e1,e2,e3)=>
        {
          val (t1, c1) = typecheck(e1,gamma)
          val (t2, c2) = typecheck(e2,gamma)
          val (t3, c3) = typecheck(e3,gamma)
          
           (t2,
            Set(TypeEquation(t1,Bool_())) ++
            Set(TypeEquation(t2,t3)) ++ // T1==T2
            c1 ++ // adiciona os subconjuntos
            c2 ++
            c3
           )
        }
      case Op(op,e1,e2)=>
        {
          op match{
            case Plus =>
              {
                    val (t1, c1) = typecheck(e1,gamma)
                    val (t2, c2) = typecheck(e2,gamma)
                    (Int_(),
                      Set(TypeEquation(t1,Int_())) ++
                      Set(TypeEquation(t2,Int_())) ++
                      c1 ++
                      c2
                    ) 
              }
            case LargerOrEqual =>
              {
                     val (t1, c1) = typecheck(e1,gamma)
                    val (t2, c2) = typecheck(e2,gamma)
                    (Bool_(),
                      Set(TypeEquation(t1,Int_())) ++
                      Set(TypeEquation(t2,Int_())) ++
                      c1 ++
                      c2
                     )
              }
          }
        }
      case App(e1,e2)=>
         {
            val x : Variable = Variable(newInt())
            val (t1, c1) = typecheck(e1,gamma)
            val (t2, c2) = typecheck(e2,gamma)
            (x,
             Set(TypeEquation(t1, Func(t2,x))) ++
             c1 ++
             c2
            )
            
         }
      case N(e1) => (Int_(),Set())
      case B(e1) => (Bool_(),Set())
      case X(name) => (gamma.apply(name),Set())
      case Fn(X(name),e) => 
        {
          val x = Variable(newInt())
          val gamma2 = gamma + (name -> x)
          val (t, c) = typecheck(e,gamma2)
          (Func(x, t),
           c)
        }
      case Raise => (Variable(newInt()),Set())
      case TryWith(e1,e2) => 
        {
          val (t1, c1) = typecheck(e1,gamma)
          val (t2, c2) = typecheck(e2,gamma)
          (t1,
           Set(TypeEquation(t1,t2)) ++
           c1 ++
           c2
          )
          
        }
  }
  
  private def unify(c0 : Set[TypeEquation]) : Option[Map[Variable, TypeOperand]] = 
  {   
    var aux = c0;
    var c : Set[TypeEquation] = Set();
    var result : Map[Variable, TypeOperand] = Map();
    var modified = true;
    
    while(modified)
      {
        modified = false;
        c = Set();
        for(equation <- aux)
          {
            equation match
            {
              case TypeEquation(Func(t1, t2), Func(t3,t4)) => 
                {
                    modified = true
                    c = c ++ Set(TypeEquation(t1,t3),TypeEquation(t2,t4))
                }
              case TypeEquation(t1 : Variable, Func(x : Variable, t3)) =>
                {
                  result.get(x) match {
                    case None => c = c ++ Set(TypeEquation(t1, Func(x, t3)))
                    case Some(t) => {
                        modified = true
                        c = c ++ Set(TypeEquation(t1, Func(t, t3)))
                    }
                  }
                }
              case TypeEquation(t1 : Variable, Func(t2, x : Variable)) =>
                {
                  result.get(x) match {
                    case None => c = c ++ Set(TypeEquation(t1, Func(t2, x)))
                    case Some(t) => {
                        modified = true
                        c = c ++ Set(TypeEquation(t1, Func(t2, t)))
                    }
                  }
                }
              case TypeEquation(t1 : Variable, Func(t2,t3)) =>
                {
                  if(result.get(t1) == None || result.get(t1) == Some(Func(t2, t3)))
                  {
                    modified = true;
                    //c = c ++ Set(TypeEquation(t1,Func(t2,t3)))
                    result += (t1 -> Func(t2, t3))
                  }
                  else
                    return None
                }
              case TypeEquation(t1, Func(t2,t3)) =>
                {
                  return None
                }
              case TypeEquation(Func(x : Variable, t2), t3 : Variable) =>
                {
                  result.get(x) match {
                    case None => c = c ++ Set(TypeEquation(Func(x, t2), t3))
                    case Some(t) => {
                        modified = true
                        c = c ++ Set(TypeEquation(Func(t, t2), t3))
                    }
                  }
                }
              case TypeEquation(Func(t1, x : Variable), t3 : Variable) =>
                {
                  result.get(x) match {
                    case None => c = c ++ Set(TypeEquation(Func(t1, x), t3))
                    case Some(t) => {
                        modified = true
                        c = c ++ Set(TypeEquation(Func(t1, t), t3))
                    }
                  }
                }
              case TypeEquation( Func(t1,t2), t3) =>
                {
                  return None
                }
              case TypeEquation(t1 : Variable, t2 : Variable) =>
                (result.get(t1), result.get(t2)) match
                {
                  case (None, None) => {
                      c = c ++ Set(TypeEquation(t1, t2)) 
                  }
                  case (None, Some(type2)) => {
                      modified = true
                      result += (t1 -> type2)
                  }
                  case (Some(type1), None) => {
                      modified = true
                      result += (t2 -> type1)
                  }
                  case (Some(type1), Some(type2)) => {
                      if(!(type1 equals type2))
                        return None
                      else
                        modified = true
                  }
                }
              case TypeEquation(t1, t2 : Variable) =>
                {
                  if(result.get(t2) == None || result.get(t2) == Some(t1))
                  {
                    modified = true
                    result += (t2 -> t1) 
                    //c = c ++ Set(TypeEquation(t1, t2));
                  }
                  else
                    return None
                }
              case TypeEquation(t1 : Variable, t2) =>
                {
                  if(result.get(t1) == None || result.get(t1) == Some(t2))
                  {
                    modified = true
                    result += (t1 -> t2)
                    //c = c ++ Set(TypeEquation(t2, t1));  
                  }
                  else
                    return None
                }
              case TypeEquation(t1, t2) =>
                {
                  if(!(t1 equals t2))
                    return None;
                  else
                    modified = true
                }
            }
          }
         aux=c;
      }
      
    return Some(result);
    
  }
  
  private def substitution(t : TypeOperand, c :  Map[Variable, TypeOperand]) : Option[TypeOperand] =
    {  
      t match
      {
        case Bool_() => Some(Bool_())
        case Int_() => Some(Int_())
        case x : Variable => c.get(x)
        case Func(t1, t2) =>
          (substitution(t1,c),substitution(t2,c)) 
          match 
          { 
            case (Some( a),Some( b)) => Some(Func(a,b)) 
            case _ => None 
          }
      }
    }
    
  private def find(t : TypeOperand, v : Variable, c : Map[Variable, TypeOperand]) : Option[TypeOperand] = 
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
    
    def typeInfer(e : Expr) : Option[TypeOperand] = {
      val result = typecheck(e, Map())
      val t = result._1
      val c = result._2
      
      return for (typeMap <- unify(c); finalType <- substitution(t, typeMap)) yield finalType
    }
}