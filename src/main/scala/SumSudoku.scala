package sumsudoku

import com.microsoft.z3
import scala.collection.mutable.MutableList

/** In the notation (n,m) sum-sudoku puzzle:
  * - n is the gridSize variable,
  * - m is the maxValue variable,
  * - the row and column sums are in the respective variables.
  * - If an entry in the grid is zero, it means that cell is not
  *   filled (or unknown) and the solver will need to find a
  *   value of this grid entry.
  */
case class Puzzle(gridSize: Int, maxValue: Int, 
                  rowSums: List[Int],
                  colSums: List[Int],
                  grid: List[List[Int]])
{
  def isValid() : Boolean = {
    for (row <- Range(0, gridSize)) {
      val rowValues = grid(row)
      val rowSum = rowValues.sum
      assert (rowValues.distinct.size == gridSize)
      assert (rowValues.forall(v => 1 <= v && v <= maxValue))
      if (rowSum != rowSums(row)) return false
    }
    for (col <- Range(0, gridSize)) {
      val colValues = grid.foldRight(List.empty[Int])((row, c) => row(col) :: c)
      val colSum = colValues.sum
      assert (colValues.distinct.size == gridSize)
      assert (colValues.forall(v => 1 <= v && v <= maxValue))
      if (colSum != colSums(col)) return false
    }
    return true
  }
}

/**
 *  Implement your solver here.
 *  You must use SMT and Z3.
 */
object Solver
{
  /** This must solve the puzzle and return a new Puzzle where all the
   *  0-valued entries in the grid have been replaced by an appropriate
   *  solution.
   *
   *  You must throw an IllegalArgumentException if the puzzle has no
   *  solution.
   */
  
  



  def solve(puzzle : Puzzle) : Puzzle = {
    val ctx = new z3.Context()
    val S = ctx.mkSolver()
    val maxValue = ctx.mkInt(puzzle.maxValue)





    //row constraints
    for(i <- 1 to puzzle.gridSize){
      var tempRowConstraint : z3.ArithExpr = ctx.mkInt(0)
      var rowVars : MutableList[z3.IntExpr] = MutableList.empty
      var tempColumnConstraint : z3.ArithExpr = ctx.mkInt(0)
      var colVars : MutableList[z3.IntExpr] = MutableList.empty
      for(j <- 1 to puzzle.gridSize){
        val tempVar1 = ctx.mkIntConst("x" + i.toString() + j.toString())
        rowVars += tempVar1
        S.add(ctx.mkLe(tempVar1, maxValue))
        S.add(ctx.mkLe(ctx.mkInt(1), tempVar1))
        tempRowConstraint = ctx.mkAdd(tempRowConstraint, tempVar1)

        
        val tempVar2 = ctx.mkIntConst("x" + j.toString() + i.toString())
        colVars += tempVar2
        tempColumnConstraint = ctx.mkAdd(tempColumnConstraint, tempVar2)
      }

      S.add(ctx.mkDistinct(rowVars:_*))
      S.add(ctx.mkEq(tempRowConstraint, ctx.mkInt(puzzle.rowSums(i-1))))

      S.add(ctx.mkDistinct(colVars:_*))
      S.add(ctx.mkEq(tempColumnConstraint, ctx.mkInt(puzzle.colSums(i-1))))
    }

    
    if(S.check().toString == "UNSATISFIABLE"){
      throw new IllegalArgumentException
    }
    val model = (S.getModel())


    var retPuzzle : List[List[Int]] = List.empty



    for(i <- 1 to puzzle.gridSize){
      var tempList : List[Int] = List.empty 
      for(j <- 1 to puzzle.gridSize){
        tempList = tempList :+ model.eval(ctx.mkIntConst("x" + i.toString() + j.toString()),true).asInstanceOf[z3.IntNum].getInt
      }
      retPuzzle = retPuzzle :+ tempList
    }

    Puzzle(
        puzzle.gridSize, puzzle.maxValue, // n and m
        puzzle.rowSums, // row sum
        puzzle.colSums,  // col sum
        retPuzzle)
  }
}
