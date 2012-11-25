/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package tests

import interpreterl3._

object ExprBuilder {
  class BuildingExpr(e : Expr) {
    def \+ (e2 : Expr) = Op(Plus, e, e2)
    def \>= (e2 : Expr) = Op(LargerOrEqual, e, e2)
    def _if (e1 : Expr) = (e3 : Expr) => If(e1, e, e3)
    def \=>: (x : X) = Fn(x, e)
    def _app (e2 : Expr) = App(e, e2)
    def _tryWith (e2 : Expr) = TryWith(e, e2)
    
    def build() = e
  }
  
  implicit def buildingExpr(e : Expr) = new BuildingExpr(e);
  
  class BuildingVar(x : X) extends BuildingExpr(x) {
    def \<- (e : Expr) = (e2 : Expr) => Let(x, e, e2)
  }
  
  implicit def buildingVar(x : X) = new BuildingVar(x)
  
  abstract class PartialExpr(partial : Expr => Expr)
  
  class PartialIf(partial : Expr => If) extends PartialExpr(partial) {
    def _else (e3 : Expr) = partial(e3)
  }
  
  implicit def partialIf(partial : Expr => If) = new PartialIf(partial)
  
  class PartialLet(partial : Expr => Let) extends PartialExpr(partial) {
    def _in (e2 : Expr) = partial(e2)
  }
  
  implicit def partialLet(partial : Expr => Let) = new PartialLet(partial)
}
