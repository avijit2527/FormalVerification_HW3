
object Main
{
  def testEquivalence() = {
    val x = twiddlebits.BV_VAR("x")
    val num1 = twiddlebits.BV_NUM(0)
    val p = (x )

    val y = twiddlebits.BV_VAR("y")                
    val num2 = twiddlebits.BV_NUM(2)
    val q = (y)

    println(p)
    val r = twiddlebits.ExprChecker.areEquivalent(p, q)
    assert(!r._1)

    // Add more tests here.
    // Refer to this webpage for a whole bunch of interesting hacks
    // you can use for your tests:
    // https://graphics.stanford.edu/~seander/bithacks.html
  }

  def main(args: Array[String]) {
    //println("Question 1 test(s).")
    //testEquivalence()

    println("Question 2 test(s)")

    /*val puzzle = sumsudoku.Puzzle(
      3, 5, // n and m
      List(6, 9, 12), // row sum
      List(6, 9, 12),  // col sum
      List( // empty grid
        List(0, 0, 0),
        List(0, 0, 0),
        List(0, 0, 0)))
    val solved = sumsudoku.Solver.solve(puzzle)
    assert (solved.isValid())
    assert (solved.gridSize == puzzle.gridSize &&
      solved.maxValue == puzzle.maxValue &&
      solved.rowSums == puzzle.rowSums &&
      solved.colSums == puzzle.colSums)*/

    println("Question 2(b)")
    val p1 = sumsudoku.PuzzleCreator.create(3, 5, 3)
    //assert (p1.size == 1)
    //assert (p1.head.isValid)

    //println("Question 2(c)")
    //val p2 = sumsudoku.PuzzleCreatorV2.create(2,5,5)
    //assert (p2.size == 1)
    //assert (p2.head.isValid)
  }
}
