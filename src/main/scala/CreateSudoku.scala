package sumsudoku

import com.microsoft.z3
import scala.collection.mutable.MutableList

object PuzzleCreator {
  /** You must create numPuzzles number of different puzzles
   *  and return them.
   *
   *  You must throw an IllegalArgumentException if it is not
   *  possible to create a puzzle for the provided parameters.
   */
  def create(gridSize : Int, maxValue : Int, numPuzzles : Int) : List[Puzzle] = {
    var puzzleList : List[Puzzle] = List.empty
    
    val ctx = new z3.Context()
    val S = ctx.mkSolver()
    val maximimValue = ctx.mkInt(maxValue)

    var xyEquality : MutableList[z3.BoolExpr] = MutableList.empty
    var extraVariables : MutableList[z3.Expr] = MutableList.empty
    var extraVariableNames : MutableList[z3.Symbol] = MutableList.empty
    var finalExtraClauses : MutableList[z3.BoolExpr] = MutableList.empty

    //row constraints
    for(i <- 1 to gridSize){
      var tempRowConstraint : z3.ArithExpr = ctx.mkInt(0)
      var rowVars : MutableList[z3.IntExpr] = MutableList.empty

      var yTempRowConstraint : z3.ArithExpr = ctx.mkInt(0)
      var yRowVars : MutableList[z3.IntExpr] = MutableList.empty


      for(j <- 1 to gridSize){
        val xTempVar = ctx.mkIntConst("x" + i.toString() + j.toString())
        val yTempVar = ctx.mkIntConst("y" + i.toString() + j.toString())
        xyEquality += ctx.mkNot(ctx.mkEq(xTempVar, yTempVar))
        extraVariables += yTempVar
        extraVariableNames += ctx.mkSymbol("y" + i.toString() + j.toString())
        rowVars += xTempVar
        yRowVars += yTempVar
        S.add(ctx.mkLe(xTempVar, maximimValue))
        S.add(ctx.mkLe(ctx.mkInt(1), xTempVar))

        
        //finalExtraClauses += (ctx.mkLe(yTempVar, maximimValue))
        //finalExtraClauses += (ctx.mkLe(ctx.mkInt(1), yTempVar))
        //allVariables += xTempVar
        tempRowConstraint = ctx.mkAdd(tempRowConstraint, xTempVar)
        yTempRowConstraint = ctx.mkAdd(yTempRowConstraint, yTempVar)
      }




      //just for testing the code
      /*S.add(ctx.mkNot(ctx.mkEq(ctx.mkIntConst("row" + 1.toString), ctx.mkInt(6))))
      S.add(ctx.mkNot(ctx.mkEq(ctx.mkIntConst("row" + 2.toString), ctx.mkInt(9))))
      S.add(ctx.mkNot(ctx.mkEq(ctx.mkIntConst("row" + 3.toString), ctx.mkInt(12))))
      S.add(ctx.mkNot(ctx.mkEq(ctx.mkIntConst("col" + 1.toString), ctx.mkInt(6))))
      S.add(ctx.mkNot(ctx.mkEq(ctx.mkIntConst("col" + 2.toString), ctx.mkInt(9))))
      S.add(ctx.mkNot(ctx.mkEq(ctx.mkIntConst("col" + 3.toString), ctx.mkInt(12))))*/



      S.add(ctx.mkDistinct(rowVars:_*))
      //S.add(ctx.mkDistinct(yRowVars:_*))
      S.add(ctx.mkEq(tempRowConstraint, ctx.mkIntConst("row" + i.toString)))
      finalExtraClauses += (ctx.mkEq(yTempRowConstraint, ctx.mkIntConst("row" + i.toString)))
    }

    //column constraints
    for(j <- 1 to gridSize){
      var tempColumnConstraint : z3.ArithExpr = ctx.mkInt(0)
      var colVars : MutableList[z3.IntExpr] = MutableList.empty
      
      var yTempColumnConstraint : z3.ArithExpr = ctx.mkInt(0)
      var yColVars : MutableList[z3.IntExpr] = MutableList.empty
      for(i <- 1 to gridSize){
        val xTempVar = ctx.mkIntConst("x" + i.toString() + j.toString())
        colVars += xTempVar
        
        val yTempVar = ctx.mkIntConst("y" + i.toString() + j.toString())
        yColVars += yTempVar

        tempColumnConstraint = ctx.mkAdd(tempColumnConstraint, xTempVar)
        yTempColumnConstraint = ctx.mkAdd(yTempColumnConstraint, yTempVar)
      }

      
      
      S.add(ctx.mkDistinct(colVars:_*))
      S.add(ctx.mkEq(tempColumnConstraint, ctx.mkIntConst("col" + j.toString)))
      
      //S.add(ctx.mkDistinct(yColVars:_*))
      finalExtraClauses += (ctx.mkEq(yTempColumnConstraint, ctx.mkIntConst("col" + j.toString)))
    }
    S.add(ctx.mkOr(xyEquality:_*))
    println(ctx.mkOr(xyEquality:_*))
    //println(ctx.mkNot(ctx.mkAnd(finalExtraClauses:_*)))
    var extraVarSort : MutableList[z3.Sort] = MutableList.empty
    for(extraVariable <- extraVariables){
      extraVarSort += (extraVariable.getSort)
    }
    val pattern : Array[z3.Pattern] = Array.empty
    //S.add(ctx.mkForall(extraVarSort.toArray,extraVariableNames.toArray,ctx.mkNot(ctx.mkAnd(finalExtraClauses:_*)),1,pattern,null,null,null))
    println(ctx.mkForall(extraVarSort.toArray,extraVariableNames.toArray,ctx.mkNot(ctx.mkAnd(finalExtraClauses:_*)),1,pattern,null,null,null))
    println(S.check())
    println(S.getModel())


    puzzleList 
  }
}
