package sumsudoku

import com.microsoft.z3
import scala.collection.mutable.MutableList
import scala.util.control.Breaks._

/** This is the new and improved puzzle creator. */
object PuzzleCreatorV2 {
  /** You must create numPuzzles number of different puzzles
   *  and return them.
   *
   *  You must throw an IllegalArgumentException if it is not
   *  possible to create a puzzle for the provided parameters.
   */

  def checkForSAT(S : z3.Solver, ctx : z3.Context, unsatVariables : MutableList[z3.IntExpr], m: z3.Model) : Boolean = {
    var andArgs : MutableList[z3.BoolExpr] =  MutableList.empty
    for(unsatVar <- unsatVariables){
      andArgs += (ctx.mkEq(unsatVar,m.eval(unsatVar,true).asInstanceOf[z3.IntNum]))
    }
    S.add((ctx.mkAnd(andArgs:_*)))
    var isSAT = true
    if(S.check().toString == "UNSATISFIABLE"){
      isSAT = false
    }else{
      isSAT = true
    }
    isSAT
    
  }
 


  def createPuzzle(S : z3.Solver, ctx : z3.Context, allVariables : MutableList[z3.IntExpr], rowColVars : MutableList[z3.IntExpr], m: z3.Model) : MutableList[z3.IntExpr] ={
    var andArgs : MutableList[z3.BoolExpr] =  MutableList.empty
    var rowColAndArgs : MutableList[z3.BoolExpr] =  MutableList.empty
    for(unsatVar <- allVariables){
      andArgs += (ctx.mkEq(unsatVar,m.eval(unsatVar,true).asInstanceOf[z3.IntNum]))
    }
    for(unsatVar <- rowColVars){
      rowColAndArgs += (ctx.mkEq(unsatVar,m.eval(unsatVar,true).asInstanceOf[z3.IntNum]))
    }
    S.add(ctx.mkNot(ctx.mkAnd(andArgs:_*)))
    S.add((ctx.mkAnd(rowColAndArgs:_*)))
    var unsatVariables = allVariables.tails
    var isSAT = false
    var finalVariables : MutableList[z3.IntExpr] =  MutableList.empty
    breakable{
      for(unsatVariable <- unsatVariables){
        S.push()
        isSAT = checkForSAT(S, ctx, unsatVariable, m)
        S.pop()
        if(isSAT){
          break
        }
        finalVariables = unsatVariable
      }
    }
    
    finalVariables
  }
  



  def create(gridSize : Int, maxValue : Int, numPuzzles : Int) : List[Puzzle] = {
    var puzzleList : MutableList[Puzzle] = MutableList.empty
    val ctx = new z3.Context()
    val S = ctx.mkSolver()
    val maximimValue = ctx.mkInt(maxValue)
    val allVariables : MutableList[z3.IntExpr] =  MutableList.empty
    val rowColVars : MutableList[z3.IntExpr] =  MutableList.empty
    
    for(i <- 1 to gridSize){
      var tempRowConstraint : z3.ArithExpr = ctx.mkInt(0)
      var rowVars : MutableList[z3.IntExpr] = MutableList.empty
      for(j <- 1 to gridSize){
        val tempVar = ctx.mkIntConst("x" + i.toString() + j.toString())
        rowVars += tempVar
        S.add(ctx.mkLe(tempVar, maximimValue))
        S.add(ctx.mkLe(ctx.mkInt(1), tempVar))
        allVariables += tempVar
        tempRowConstraint = ctx.mkAdd(tempRowConstraint, tempVar)
      }

      S.add(ctx.mkDistinct(rowVars:_*))


      S.add(ctx.mkEq(tempRowConstraint, ctx.mkIntConst("row" + i.toString)))
      rowColVars += ctx.mkIntConst("row" + i.toString)
    }

    for(j <- 1 to gridSize){
      var tempColumnConstraint : z3.ArithExpr = ctx.mkInt(0)
      var colVars : MutableList[z3.IntExpr] = MutableList.empty
      for(i <- 1 to gridSize){
        val tempVar = ctx.mkIntConst("x" + i.toString() + j.toString())
        colVars += tempVar
        tempColumnConstraint = ctx.mkAdd(tempColumnConstraint, tempVar)
      }
      
      
      S.add(ctx.mkDistinct(colVars:_*))
      S.add(ctx.mkEq(tempColumnConstraint, ctx.mkIntConst("col" + j.toString)))
      rowColVars += ctx.mkIntConst("col" + j.toString)
    }

    var blockingClauses :MutableList[MutableList[z3.BoolExpr]] = MutableList.empty 
    
    var isSAT = S.check().toString
    var model = (S.getModel())

    for(idx <- 0 until numPuzzles){
      if(isSAT == "UNSATISFIABLE"){
        throw new IllegalArgumentException
      }
      var rowSums : MutableList[Int] = MutableList.empty 
      var colSums : MutableList[Int] = MutableList.empty 
      
      for(i <- 1 to gridSize){
        rowSums += model.eval(ctx.mkIntConst("row" + i.toString()),true).asInstanceOf[z3.IntNum].getInt
        colSums += model.eval(ctx.mkIntConst("col" + i.toString()),true).asInstanceOf[z3.IntNum].getInt

      }

      S.push()
      var puzzleVars = createPuzzle(S,ctx,allVariables,rowColVars,model)
      S.pop()



      var blockingClause : MutableList[z3.BoolExpr] = MutableList.empty  

      var retPuzzle : List[List[Int]] = List.empty
      for(i <- 1 to gridSize){
        var tempList : List[Int] = List.empty 
        for(j <- 1 to gridSize){
          if(puzzleVars.contains(ctx.mkIntConst("x" + i.toString() + j.toString()))){
            tempList = tempList :+ model.eval(ctx.mkIntConst("x" + i.toString() + j.toString()),true).asInstanceOf[z3.IntNum].getInt

          }else{
            tempList = tempList :+ 0
          }
        }
        retPuzzle = retPuzzle :+ tempList
      }
      
      var blockingClauseRowColVars : MutableList[z3.BoolExpr] = MutableList.empty 
      for(x <- rowColVars){
        blockingClause += ctx.mkEq(x,model.eval(x,true))
        blockingClauseRowColVars += ctx.mkEq(x,model.eval(x,true))
      }
      for(x <- puzzleVars){
        blockingClause += ctx.mkEq(x,model.eval(x,true))
      }

      blockingClauses += blockingClause
      



      puzzleList += Puzzle(
        gridSize, maxValue, // n and m
        rowSums.toList, // row sum
        colSums.toList,  // col sum
        retPuzzle)
      

    }
    
 
    puzzleList.toList
  }

}
