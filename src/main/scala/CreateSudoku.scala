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
    val maximimValue = ctx.mkBV(maxValue, 4)

    var allVariablesX : MutableList[String] = MutableList.empty
    var row : MutableList[String] = MutableList.empty
    var col : MutableList[String] = MutableList.empty
    var allVariablesY : MutableList[String] = MutableList.empty
    var xConstraints : MutableList[z3.BoolExpr] = MutableList.empty
    var yConstraints : MutableList[z3.BoolExpr] = MutableList.empty


    for(i <- 1 to gridSize){
      var tempRowX : MutableList[z3.BitVecExpr] = MutableList.empty 
      var tempRowY : MutableList[z3.BitVecExpr] = MutableList.empty 
      var tempColX : MutableList[z3.BitVecExpr] = MutableList.empty 
      var tempColY : MutableList[z3.BitVecExpr] = MutableList.empty 
      for(j <- 1 to gridSize){
        val xTempVar = "x" + i.toString() + j.toString()
        val yTempVar = "y" + i.toString() + j.toString()
        xConstraints += ctx.mkAnd(ctx.mkBVULE(ctx.mkBV(1,4), ctx.mkBVConst(xTempVar,4)),ctx.mkBVULE(ctx.mkBVConst(xTempVar,4),maximimValue))
        yConstraints += ctx.mkAnd(ctx.mkBVULE(ctx.mkBV(1,4), ctx.mkBVConst(yTempVar,4)),ctx.mkBVULE(ctx.mkBVConst(yTempVar,4),maximimValue))

        
        tempRowX += ctx.mkBVConst(xTempVar,4)
        tempRowY += ctx.mkBVConst(yTempVar,4)
        tempColX += ctx.mkBVConst("x" + j.toString() + i.toString(),4)
        tempColY += ctx.mkBVConst("y" + j.toString() + i.toString(),4)



        allVariablesX += xTempVar
        allVariablesY += yTempVar
      }
      xConstraints += ctx.mkDistinct(tempRowX:_*)
      yConstraints += ctx.mkDistinct(tempRowY:_*)

      xConstraints += ctx.mkDistinct(tempColX:_*)
      yConstraints += ctx.mkDistinct(tempColY:_*)



      row += "row" + i.toString
      xConstraints += ctx.mkEq(tempRowX.reduce((a,b) => ctx.mkBVAdd(a,b)),ctx.mkBVConst("row" + i.toString,4))
      yConstraints += ctx.mkEq(tempRowY.reduce((a,b) => ctx.mkBVAdd(a,b)),ctx.mkBVConst("row" + i.toString,4))
      col += "col" + i.toString
      xConstraints += ctx.mkEq(tempColX.reduce((a,b) => ctx.mkBVAdd(a,b)),ctx.mkBVConst("col" + i.toString,4))
      yConstraints += ctx.mkEq(tempColY.reduce((a,b) => ctx.mkBVAdd(a,b)),ctx.mkBVConst("col" + i.toString,4))
    }

    


    var xBV = allVariablesX.map(x => (x -> ctx.mkBVConst(x,4))).toMap
    var yBV = allVariablesY.map(x => (x -> ctx.mkBVConst(x,4))).toMap
    var rowBV = row.map(x => (x -> ctx.mkBVConst(x,4))).toMap
    var colBV = col.map(x => (x -> ctx.mkBVConst(x,4))).toMap


    for(numOfSol <- 0 until numPuzzles){
      var p_x = ctx.mkAnd(xConstraints:_*)
      var p_y = ctx.mkAnd(yConstraints:_*)
      var equalityConstraint :MutableList[z3.BoolExpr] = MutableList.empty 
      for((a,b) <- (allVariablesX zip allVariablesY)){
        equalityConstraint += ctx.mkEq(xBV(a),yBV(b))
      }
      var xyEquals = ctx.mkAnd(equalityConstraint:_*)

      var impliedExpr = ctx.mkImplies(p_y, xyEquals)

      var forAll = ctx.mkForall(yBV.values.toArray,impliedExpr,1, Array.empty[z3.Pattern], null, null, null)
      var exists = ctx.mkExists(xBV.values.toArray, ctx.mkAnd(forAll,p_x), 1, Array.empty[z3.Pattern], null, null, null)
      S.add(exists)
      if(S.check().toString == "UNSATISFIABLE"){
        throw new IllegalArgumentException
      }else{
        var model = (S.getModel)
        var rowSum : List[Int] = List.empty 
        var colSum : List[Int] = List.empty 
        var body = List(List(0,0,0),List(0,0,0),List(0,0,0))

        for(i <- row){
          rowSum = rowSum :+ model.eval(rowBV(i),true).asInstanceOf[z3.BitVecNum].getInt
        }
        for(i <- col){
          colSum = colSum :+ model.eval(colBV(i),true).asInstanceOf[z3.BitVecNum].getInt
        }
        puzzleList = puzzleList :+ Puzzle(gridSize,maxValue,rowSum,colSum,body)
      
      }

    }

    puzzleList 
  }
}
