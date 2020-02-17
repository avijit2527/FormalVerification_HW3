package sumsudoku

import com.microsoft.z3

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
    /* Hard-coding a solution just for testing. */
    Puzzle(
        3, 5, // n and m
        List(8, 10, 10), // row sum
        List(8, 8, 12),  // col sum
        List(
          List(1, 4, 3),
          List(5, 1, 4),
          List(2, 3, 5)))
  }
}
