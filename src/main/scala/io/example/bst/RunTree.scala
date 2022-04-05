package io.example.bst

object RunTree extends App {

  val bstInt: BST[Int] = BST(3,7,2,9,1,6,4,10,-2,20,0,100)
  val bstString: BST[String] = BST("e","a","p","m","z","t","b","h","s","A","qqq")

  println("Search:")
  println(bstInt.search(9))
  println(bstInt.search(1))
  println(bstInt.search(100))
  println(bstInt.search(5))

  println("\nTraverse bstInt:")
  BST.traverse(bstInt)

  val bstIntAdded = bstInt + 8

  println("\nTraverse bstInt (with 8 added):")
  BST.traverse(bstIntAdded)

  println("\nReverse traverse bstInt:")
  BST.reverseTraverse(bstInt)

  println("\nTraverse bstString:")
  BST.traverse(bstString)

  println("\nReverse traverse bstString:")
  BST.reverseTraverse(bstString)

  val bstIntDFS: Seq[Int] = BST.depthFirstSearch(bstInt)
  print("bstIntDFS = ")
  bstIntDFS.foreach(x => print(s"$x "))
  println()

  val bstIntBFS: Seq[Int] = BST.breadthFirstSearch(bstInt)
  print("bstIntBFS = ")
  bstIntBFS.foreach(x => print(s"$x "))
  println()

  // Create traverse log
  val bstIntLogged = BST.traverseLog(bstInt)

  // Print the traverse log
  BST.prettyPrintTraverseLog(bstIntLogged)

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
