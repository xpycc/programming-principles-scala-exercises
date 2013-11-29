package streams

/**
 * A main object that can be used to execute the Bloxorz solver
 */
object Bloxorz extends App {

  /**
   * A level constructed using the `InfiniteTerrain` trait which defines
   * the terrain to be valid at every position.
   */
  object InfiniteLevel extends Solver with InfiniteTerrain {
    val startPos = Pos(0,0)
    val goal = Pos(300,300)
  }

  println("Running GC...")
  System.gc; Thread.sleep(500)
  println("Running InfiniteLevel case...")
  val st = System.nanoTime
  val infOut = "Solution:\n" + InfiniteLevel.solution
  val ed = System.nanoTime
  println(infOut)
  println("#Steps: " + InfiniteLevel.solution.size)
  println((ed - st) / 1e9 + " sec spent on InfiniteLevel.")

  println("Running GC...")
  System.gc; Thread.sleep(500)
  println("Running RectangleLevel case...")
  case class RectangleLevel(H: Int, W: Int) extends Solver with GameDef {
    val terrain:Terrain = { case Pos(x, y) => 0 <= x && x < H && 0 <= y && y < W }
//    val terrain:Terrain = p => { 0 <= p.x && p.x < H && 0 <= p.y && p.y < W }
    val startPos = Pos(1,1)
    val goal = Pos(-1, -1)
  }
  
  val start = System.nanoTime
  val rectOut = "Solution:\n" + RectangleLevel(700, 700).solution
  val end = System.nanoTime
  println(rectOut)
  println((end - start) / 1e9 + " sec spent on RectangleLevel.")
  // FIXME: to be quicker
  // currently about 8.5 sec
  System.gc

  /**
   * A simple level constructed using the StringParserTerrain 
   */
  abstract class Level extends Solver with StringParserTerrain
  
  object Level0 extends Level {
    val level =
      """------
        |--ST--
        |--oo--
        |--oo--
        |------""".stripMargin
  }

  println(Level0.solution)

  /**
   * Level 1 of the official Bloxorz game
   */
  object Level1 extends Level {
    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin
  }

  println(Level1.solution)
  
  object Level2 extends Level {
    val level =
      """--------
        |--S--T--
    	|--o--o--
        |--o--o--
        |--oooo--
        |--------""".stripMargin
  }

  println(Level2.solution)
  
  object NoSolution extends Level {
    val level =
      """------
        |--ST--
        |---o--
        |--oo--
        |------""".stripMargin
  }
  //println(nosolution.pathsFromStart.take(10).toList)
  println(NoSolution.solution)
}
