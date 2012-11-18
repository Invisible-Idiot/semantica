/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package interpreterl3

object Interpreter {
  
  def eval(e : Expr) : Option[Expr] = step(e) match {
      // step(e) retornou alguma coisa, continua a execução
      case Some(e_) => eval(e_)
      // step(e) retornou nada, fim da execução, duas possibilidades:
      case None => e match {
          // e é valor final, retorna esse valor
          case fnl : Final => Some(fnl)
          // e não é valor final, ocorreu erro de execução
          case _ => None
      }
  }
  
  def step(e : Expr) : Option[Expr] = e match {
    // Não pode dar mais passos; fim da avaliação
    case _ : Final => None
    // Regras do OP
    case Op(_, Raise, _) => Some(Raise)
    case Op(_, _, Raise) => Some(Raise)
    case Op(Plus, N(n1), N(n2)) => Some(N(n1 + n2))
    case Op(LargerOrEqual, N(n1), N(n2)) => Some(B(n1 >= n2))
    case Op(op, v : Value, e2) => step(e2) map { Op(op, v, _) }
    case Op(op, e1, e2) => step(e1) map { Op(op, _, e2) }
    // Regras do IF
    case If(Raise, e2, _) => Some(Raise)
    case If(B(true), e2, _) => Some(e2)
    case If(B(false), _, e3) => Some(e3)
    case If(e1, e2, e3) => step(e1) map { If(_, e2, e3) }
    // Regras do APP
    case App(Raise, _) => Some(Raise)
    case App(Fn(x, e), fnl : Final) => Some(replace(x, fnl, e))
    case App(v : Value, e2) => step(e2) map { App(v, _) }
    case App(e1, e2) => step(e1) map { App(_, e2) }
    // Regras do LET
    case Let(x, fnl : Final, e2) => Some(replace(x, fnl, e2))
    case Let(x, e1, e2) => step(e1) map { Let(x, _, e2) }
    // Regras do TRY
    case TryWith(Raise, e2) => Some(e2)
    case TryWith(v : Value, e2) => Some(v)
    case TryWith(e1, e2) => step(e1) map { TryWith(_, e2) }
    // Expressão inválida, erro de execução
    case _ => None
  }
  
  def replace(x0 : X, fnl : Final, e0 : Expr) : Expr = {
    val r : Expr => Expr = e => replace(x0, fnl, e)
    
    e0 match {
      case Fn(x, e) => if(x.name == x0.name) Fn(x, e) else Fn(x, r(e))
      case fnl : Final => fnl
      case Op(op, e1, e2) => Op(op, r(e1), r(e2))
      case If(e1, e2, e3) => If(r(e1), r(e2), r(e3))
      case App(e1, e2) => App(r(e1), r(e2))
      case x : X => if(x.name == x0.name) fnl else x
      case Let(x, e1, e2) => if(x0.name == x.name) Let(x, r(e1), e2) else Let(x, r(e1), r(e2))
      case TryWith(e1, e2) => TryWith(r(e1), r(e2))
    }
  }
}
