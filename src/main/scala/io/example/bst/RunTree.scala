package io.example.bst

object RunTree extends App {

  val myTreeInt: Tree[Int] = Tree(3,7,2,9,1,6,4,10,-2,20,0,100)
  val myTreeString: Tree[String] = Tree("e","a","p","m","z","t","b","h","s","A","qqq")

  println("Search:")
  println(myTreeInt.search(9))
  println(myTreeInt.search(1))
  println(myTreeInt.search(100))
  println(myTreeInt.search(5))

  println("\nTraverse myTreeInt:")
  Tree.traverse(myTreeInt)

  val myTreeIntAdded = myTreeInt + 8

  println("\nTraverse myTreeInt (with 8 added):")
  Tree.traverse(myTreeIntAdded)

  println("\nReverse traverse myTreeInt:")
  Tree.reverseTraverse(myTreeInt)

  println("\nTraverse myTreeString:")
  Tree.traverse(myTreeString)

  println("\nReverse traverse myTreeString:")
  Tree.reverseTraverse(myTreeString)

  // Create traverse log
  val myTreeIntLogged = Tree.traverseLog(myTreeInt)

  // Print the traverse log
  Tree.prettyPrintTraverseLog(myTreeIntLogged)

  /*

    Branch(
      3,
      Branch(
        2,
        Leaf(1),
        Empty
      ),
      Branch(
        7,
        Leaf(6),
        Leaf(9)
      )
    )

   */

  /*





   */

}
