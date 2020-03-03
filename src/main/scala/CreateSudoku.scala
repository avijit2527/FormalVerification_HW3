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


    //row constraints
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

      //xConstraints += tempRowX
      row += "row" + i.toString
      col += "col" + i.toString
    }

    


    var xBV = allVariablesX.map(x => (x -> ctx.mkBVConst(x,4))).toMap
    var yBV = allVariablesY.map(x => (x -> ctx.mkBVConst(x,4))).toMap
    var rowBV = row.map(x => (x -> ctx.mkBVConst(x,4))).toMap
    var colBV = col.map(x => (x -> ctx.mkBVConst(x,4))).toMap

    println(xConstraints)






    puzzleList 
  }
}
