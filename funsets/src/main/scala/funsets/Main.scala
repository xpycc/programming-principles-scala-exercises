package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  
  println(forall(x => 0 < x && x < 5, x => x < 5))
  printSet(x => 0 < x && x < 5)
}
