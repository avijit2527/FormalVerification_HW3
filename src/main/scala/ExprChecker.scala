package twiddlebits

import com.microsoft.z3
import scala.collection.mutable.MutableList

object ExprChecker {

  
  /** Use the Z3 API to check whether the two provided
   *  expressions are equivalent.
   *
   *  Return true/false as appropriate.
   *  The Map should be empty if the expressions are equivalent and must
   *  contain a distinguishing input assignment if they are not.
   */

  var variableList : MutableList[BV_VAR] = MutableList.empty

  val noOfBits = 32

  def convertToZ3Eq(e : Expr, ctx : z3.Context) : z3.BitVecExpr = {
    e match{
    case BV_VAR(name) => {
      variableList += BV_VAR(name)
      ctx.mkBVConst(name,noOfBits)
    }
    case BV_NUM(value) => ctx.mkInt2BV(noOfBits, ctx.mkInt(value))
    case BV_NOT(expr) => ctx.mkBVNot(convertToZ3Eq(expr, ctx))
    case BV_RSHIFT(lexpr, rexpr) => ctx.mkBVLSHR(convertToZ3Eq(lexpr,ctx), convertToZ3Eq(rexpr, ctx))
    case BV_LSHIFT(lexpr, rexpr) => ctx.mkBVSHL(convertToZ3Eq(lexpr,ctx), convertToZ3Eq(rexpr, ctx))
    case BV_AND(lexpr, rexpr) => ctx.mkBVAND(convertToZ3Eq(lexpr,ctx), convertToZ3Eq(rexpr, ctx))
    case BV_OR(lexpr, rexpr) => ctx.mkBVOR(convertToZ3Eq(lexpr,ctx), convertToZ3Eq(rexpr, ctx))
    case BV_XOR(lexpr, rexpr) => ctx.mkBVXOR(convertToZ3Eq(lexpr,ctx), convertToZ3Eq(rexpr, ctx))
    case BV_ADD(lexpr, rexpr) => ctx.mkBVAdd(convertToZ3Eq(lexpr,ctx), convertToZ3Eq(rexpr, ctx))
    case BV_SUB(lexpr, rexpr) => ctx.mkBVSub(convertToZ3Eq(lexpr,ctx), convertToZ3Eq(rexpr, ctx))
    case BV_MUL(lexpr, rexpr) => ctx.mkBVMul(convertToZ3Eq(lexpr,ctx), convertToZ3Eq(rexpr, ctx))
    case BV_DIV(lexpr, rexpr) => ctx.mkBVUDiv(convertToZ3Eq(lexpr,ctx), convertToZ3Eq(rexpr, ctx))

    }
  }



  def areEquivalent(expr1 : Expr, expr2: Expr) : (Boolean, Map[BV_VAR, Int]) = {
    variableList = MutableList.empty
    val ctx = new z3.Context()
    val S = ctx.mkSolver()

    val expr1Converted = convertToZ3Eq(expr1,ctx)
    val expr2Converted = convertToZ3Eq(expr2,ctx)
    var resultMap :  Map[BV_VAR, Int]  = Map.empty

    val finalExpr = ctx.mkNot(ctx.mkEq(expr1Converted,expr2Converted))
    S.add(finalExpr)
    if(S.check().toString == "UNSATISFIABLE"){
      return (true,Map.empty[BV_VAR, Int])
    }else{
      val model = S.getModel()
      for(variable <- variableList){
        variable match{
          case BV_VAR(name) => resultMap += (variable -> (model.eval(ctx.mkBVConst(name,noOfBits),true).asInstanceOf[z3.BitVecNum].getLong().toInt))
        }
        
      }
      
     
      return (false,resultMap)
    }
    

    (true,Map.empty[BV_VAR, Int])
  }

}
