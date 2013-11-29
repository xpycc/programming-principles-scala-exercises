package streams

import common._
import scala.collection.immutable
import scala.collection.mutable

/**
 * This component implements the solver for the Bloxorz game
 */
trait Solver extends GameDef {

  /**
   * Returns `true` if the block `b` is at the final position
   */
  def done(b: Block): Boolean = b.b1 == b.b2 && goal == b.b1
 //   b.b1.x == b.b2.x && b.b1.y == b.b2.y &&
 //   b.b1.x == goal.x && b.b1.y == goal.y

  /**
   * This function takes two arguments: the current block `b` and
   * a list of moves `history` that was required to reach the
   * position of `b`.
   * 
   * The `head` element of the `history` list is the latest move
   * that was executed, i.e. the last move that was performed for
   * the block to end up at position `b`.
   * 
   * The function returns a stream of pairs: the first element of
   * the each pair is a neighboring block, and the second element
   * is the augmented history of moves required to reach this block.
   * 
   * It should only return valid neighbors, i.e. block positions
   * that are inside the terrain.
   */
  def neighborsWithHistory(b: Block, history: List[Move]): Stream[(Block, List[Move])] =
     b.legalNeighbors.toStream.map(x => (x._1, x._2 :: history))		// get stream first
//    b.legalNeighbors.map(x => (x._1, x._2 :: history)).toStream

  /**
   * This function returns the list of neighbors without the block
   * positions that have already been explored. We will use it to
   * make sure that we don't explore circular paths.
   */
  def newNeighborsOnly(neighbors: Stream[(Block, List[Move])],
                       explored: Set[Block]): Stream[(Block, List[Move])] =
    neighbors.filter( x => !explored.contains(x._1) )

  /**
   * The function `from` returns the stream of all possible paths
   * that can be followed, starting at the `head` of the `initial`
   * stream.
   * 
   * The blocks in the stream `initial` are sorted by ascending path
   * length: the block positions with the shortest paths (length of
   * move list) are at the head of the stream.
   * 
   * The parameter `explored` is a set of block positions that have
   * been visited before, on the path to any of the blocks in the
   * stream `initial`. When search reaches a block that has already
   * been explored before, that position should not be included a
   * second time to avoid cycles.
   * 
   * The resulting stream should be sorted by ascending path length,
   * i.e. the block positions that can be reached with the fewest
   * amount of moves should appear first in the stream.
   * 
   * Note: the solution should not look at or compare the lengths
   * of different paths - the implementation should naturally
   * construct the correctly sorted stream.
   */
  def from(initial: Stream[(Block, List[Move])],
           explored: Set[Block]): Stream[(Block, List[Move])] = 
    if (initial.isEmpty)
      Stream()
    else {
//      val nn = initial.flatMap(x => newNeighborsOnly(neighborsWithHistory(x._1, x._2), explored))
//      initial #::: from(nn, explored ++ nn.unzip._1)
        // FIXME: too slow

//      val (b, h) = initial.head
//      val t = initial.tail
//      val newPos = newNeighborsOnly(neighborsWithHistory(b, h), explored)
      		// pick head to eliminate replicas
//      newPos #::: from(t #::: newPos, explored ++ newPos.unzip._1)
      	// FIXME: t #::: newPos is slow!

//      val nn = initial.flatMap(x => newNeighborsOnly(neighborsWithHistory(x._1, x._2), explored))
      val nn = initial.flatMap(y => y._1.neighbors.filter(x => x._1.isLegal && !explored.contains(x._1)).map(x => (x._1, x._2 :: y._2)))
//    		  .groupBy(_._1).toStream.map(x => (x._1, x._2.head._2))
    		  .foldLeft(Map[Block, List[Move]]())((a, b) => if (!a.contains(b._1)) a + b else a)
//    		  .foldLeft(Map[Block, List[Move]]())((a, b) => a + b).toStream
        // foldLeft is a little quicker than groupBy
        // "if contains" is a little quicker than just "a + b"
        // still slow
      val ns = nn.toStream
      ns #::: from(ns, explored ++ nn.keys)
    }
  
  def from2(que: immutable.Queue[(Block, List[Move])],
           explored: Set[Block]): Stream[(Block, List[Move])] = 
    if (que.isEmpty)
      Stream()
    else {
      val ((b, h), q) = que.dequeue
//      val newPos = newNeighborsOnly(neighborsWithHistory(b, h), explored)
//      newPos #::: from2(q.enqueue(newPos), explored ++ newPos.unzip._1)
      	// q.enqueue(newPos) is quicker than tail #::: newPos
      	//   but sadly slower than initial #::: from(nn, explored ++ nn.unzip._1)
      val newList = b.legalNeighbors.map(x => (x._1, x._2 :: h)).filter(x => !explored.contains(x._1))
      newList.toStream #::: from2(q.enqueue(newList), explored ++ newList.unzip._1)
      // faster than previous one but still slower than initial #::: form(nn, explored ++ nn.unzip._1)
    }
  
  def from3(que: mutable.Queue[(Block, List[Move])],
           explored: Set[Block]): Stream[(Block, List[Move])] = 
    if (que.isEmpty)
      Stream()
    else {
      val (b, h) = que.dequeue
      val newList = b.legalNeighbors.map(x => (x._1, x._2 :: h)).filter(x => !explored.contains(x._1))
//      que.enqueue(newList:_*)
      newList foreach {que.enqueue(_)}
      // foreach is slightly faster than _* type cast
      newList.toStream #::: from3(que, explored ++ newList.unzip._1)
      // slightly faster than immutable.Queue
    }

  /**
   * The stream of all paths that begin at the starting block.
   */
  lazy val pathsFromStart: Stream[(Block, List[Move])] =
    from(Stream((startBlock, List())), Set(startBlock))
//    from2(immutable.Queue((startBlock, List())), Set(startBlock))
//    from3(mutable.Queue((startBlock, List())), Set(startBlock))

  /**
   * Returns a stream of all possible pairs of the goal block along
   * with the history how it was reached.
   */
  lazy val pathsToGoal: Stream[(Block, List[Move])] =
    pathsFromStart.filter(x => done(x._1))

  /**
   * The (or one of the) shortest sequence(s) of moves to reach the
   * goal. If the goal cannot be reached, the empty list is returned.
   *
   * Note: the `head` element of the returned list should represent
   * the first move that the player should perform from the starting
   * position.
   */
  lazy val solution: List[Move] = pathsToGoal match {
    case Stream() => List()
    case x        => x.head._2.reverse 
  }
}
