package io.example.bst

import scala.annotation.tailrec
import scala.collection.mutable
import scala.math.Ordered.orderingToOrdered

sealed trait BST[+A] {
  def +[B >: A : Ordering](elem: B): BST[B]
  def search[B >: A : Ordering](searchValue: B): Option[B]

  // toString not all the way fully working yet. It is a complicated algorithm to output the bst in a nice way
  def toString: String
}

case class Branch[+A](value: A, l: BST[A], r: BST[A]) extends BST[A] {

  /*

  implicit def orderingToOrdered[T](x: T)(implicit ord: Ordering[T]): Ordered[T] =
    new Ordered[T] { def compare(that: T): Int = ord.compare(x, that) }

  implicit def orderingToOrdered[Int](elem: Int)(implicit ord: Ordering[Int]): Ordered[Int] =
    new Ordered[Int] { def compare(value: Int): Int = Ordering[Int].compare(elem, value) }

  trait Ordered[A] extends Any with java.lang.Comparable[A] {
    def <  (that: A): Boolean = (this compare that) <  0

  Verbose definition:
  def +[B >: A](elem: B)(implicit ord: Ordering[B]): BST[B] = {
    if (orderingToOrdered[B](elem)(ord).<(value)) this
    else this
  }

 */

  def +[B >: A : Ordering](elem: B): BST[B] = {
    if (elem < value) {
      l match {
        case Empty => this.copy(l = Leaf(elem))
        case _ => this.copy(l = l + elem)
      }
    }
    else {
      r match {
        case Empty => this.copy(r = Leaf(elem))
        case _ => this.copy(r = r + elem)
      }
    }
  }

  def search[B >: A : Ordering](searchValue: B): Option[B] = {
    if (searchValue == value) Some(searchValue)
    else if (searchValue < value) l.search(searchValue)
    else r.search(searchValue)
  }

  override def toString: String = {
    val line1: String = s"  ${value.toString}  "
    val (line2, line3) = this match {
      case Branch(_, Empty, Empty) => ("", s"")
      case Branch(_, _, Empty) =>     (" /   ", s"${l.toString}    ")
      case Branch(_, Empty, r) =>     ("   \\ ", s"    ${r.toString}")
      case Branch(_, l, r) =>         (" / \\ ", s"${l.toString}   ${r.toString}")
    }

    s"$line1\n$line2\n$line3\n"
  }
}

case class Leaf[+A](value: A) extends BST[A] {
  def +[B >: A : Ordering](elem: B): BST[B] =
    if (elem < value) Branch(value, Leaf(elem), Empty)
    else Branch(value, Empty, Leaf(elem))

  def search[B >: A : Ordering](searchValue: B): Option[B] = {
    if (searchValue == value) Some(value)
    else None
  }

  override def toString: String = value.toString
}

case object Empty extends BST[Nothing] {
  def +[B >: Nothing : Ordering](elem: B): BST[B] = throw new NoSuchElementException("Empty.add")
  def search[B >: Nothing : Ordering](searchValue: B): Option[B] = None

  override def toString: String = ""
}

object BST {

  def apply[A: Ordering](elem: A, elems: A*): BST[A] = {
    @tailrec
    def recursiveBuild(elems: Seq[A], tree: BST[A]): BST[A] = {
      if (elems.isEmpty) tree
      else recursiveBuild(elems.tail, tree + elems.head)
    }

    recursiveBuild(elems, Branch(elem, Empty, Empty))
  }

  def depthFirstSearch[A](tree: BST[A], seq: Seq[A] = Seq.empty[A]): Seq[A] = {
    tree match {
      case Branch(value, left, right) =>
        val seqLeft: Seq[A] = depthFirstSearch(left, seq :+ value)
        depthFirstSearch(right, seqLeft)
      case Leaf(value) => seq :+ value
      case Empty => seq
    }
  }

  def breadthFirstSearch[A](bst: BST[A]): Seq[A] = {
    // Initialize buffer to be empty
    val buffer: collection.mutable.ArrayBuffer[A] = collection.mutable.ArrayBuffer[A]()

    // Initialize queue with the input bst
    val queue: mutable.Queue[BST[A]] = mutable.Queue(bst)

    while (queue.nonEmpty) {
      val node: BST[A] = queue.dequeue
      node match {
        case Branch(value, left, right) =>
          buffer += value
          queue.enqueue(left)
          queue.enqueue(right)
        case Leaf(value) =>
          buffer += value
        case Empty => ()
      }
    }

    buffer.toSeq
  }

  def findMaxRoot(bst: BST[Int]): Int = {
    bst match {
      case Branch(value: Int, left, right) =>
        value + math.max(findMaxRoot(left), findMaxRoot(right))
      case Leaf(value) =>
        value
      case Empty =>
        0
    }
  }

  def findMinVal(bst: BST[Int]): Int = {
    bst match {
      case Branch(value: Int, left, right) =>
        val nodesMinimum: Int = math.min(findMinVal(left), findMinVal(right))
        math.min(value, nodesMinimum)
      case Leaf(value) =>
        value
      case Empty =>
        // Essentially infinity
        Int.MaxValue
    }
  }

  def traverse[A](tree: BST[A]): Unit = {
    def traverseRec(tree: BST[A]): Unit = {
      tree match {
        case Branch(value, l: BST[A], r: BST[A]) =>
          traverseRec(l)
          printElement(value)
          traverseRec(r)
        case Leaf(value) => printElement(value)
        case Empty => ()
      }
    }

    traverseRec(tree)
    println()
  }

  def reverseTraverse[A](tree: BST[A]): Unit = {
    def reverseTraverseRec(tree: BST[A]): Unit = {
      tree match {
        case Branch(value, l: BST[A], r: BST[A]) =>
          reverseTraverseRec(r)
          printElement(value)
          reverseTraverseRec(l)
        case Leaf(value) => printElement(value)
        case Empty => ()
      }
    }

    reverseTraverseRec(tree)
    println()
  }

  def printElement[A](value: A): Unit = print(s"$value ")

  def constructPrettySeq[A]: Seq[Seq[Option[A]]] = {
    val numOfLevels = 10

    @tailrec
    def constructSeqTR(accSeq: Seq[Seq[Option[A]]], numOfLevels: Int, level: Int = 0): Seq[Seq[Option[A]]] = {
      if (numOfLevels <= 0) accSeq
      else {
        val numElInLevel = Math.pow(2,level).toInt
        val newSeq = accSeq.updated(level, Seq.fill[Option[A]](numElInLevel)(None))
        constructSeqTR(newSeq, numOfLevels - 1, level + 1)
      }
    }

    constructSeqTR(Seq.fill(numOfLevels)(Seq.empty[Option[A]]), numOfLevels)
  }

  def traverseLog[A](tree: BST[A]): Seq[Seq[Option[A]]] = {
    def traverseLogRec[A](tree: BST[A], seq: Seq[Seq[Option[A]]], x: Int = 0, y: Int = 0, leftMove: Boolean = true): Seq[Seq[Option[A]]] = {
      /*
        Setting curX:
                                    0
                           0                 1
                       0       1         2       3
                     0   1   2   3     4   5   6   7

       */
      val curX = {
        if (y == 0) 0
        else if (leftMove) x * 2
        else               x * 2 + 1
      }

      //
      val curY = y

      tree match {
        case Branch(value, l: BST[A], r: BST[A]) => {
          val seq0 = seq.updated(curY, seq(curY).updated(curX, Some(value)))
          val seq1 = traverseLogRec(l, seq0, curX, curY + 1, leftMove = true)
          val seq2 = traverseLogRec(r, seq1, curX, curY + 1, leftMove = false)
          seq2
        }
        case Leaf(value) => {
          val seq0 = seq.updated(curY, seq(curY).updated(curX, Some(value)))
          seq0
        }
        case Empty => seq

      }
    }

    val seq: Seq[Seq[Option[A]]] = constructPrettySeq[A]
    val loggedSeq = traverseLogRec(tree, seq)
    loggedSeq
  }

  def prettyPrintTraverseLog[A](seq: Seq[Seq[Option[A]]]): Unit = {
    val offset = 50
    val diffSeq: Seq[Int] = Seq(12, 6, 4, 2, 0, 0, 0, 0, 0, 0, 0)

    def printPrintRec(seq: Seq[Seq[Option[A]]], diffSeq: Seq[Int], currLine: Int, lastOffsets: Seq[Int]): Unit = {
      if (seq.isEmpty) ()
      else {
        val diff: Int = diffSeq.head
        val line: Seq[Option[A]] = seq.head

        val nextOffsets: Seq[Int] = lastOffsets.flatMap(o => Seq(o - diff, o + diff))

        val offsetDiffs: Seq[Int] = {
          val diffs: Seq[Int] =
            if (lastOffsets.length <= 1) Nil
            else
              lastOffsets.sliding(2).map {
                case Seq(x, y, _*) => y - x
              }.toSeq
          lastOffsets.head +: diffs
        }

        def printLine(offsets: Seq[Int], line: Seq[Option[A]]): Unit = {
          val seqOfSpaces: Seq[String] = offsets.map(x => List.fill(x)(" ").mkString)
          val finalVal: Seq[(String, Option[A])] = seqOfSpaces.zip(line)
          finalVal.foreach({
            case (spaces, maybeValue) =>
              print(spaces)
              maybeValue match {
                case Some(value) => print(value)
                case None => print(" ")
              }
          })
        }

        printLine(offsetDiffs, line)
        println()

        printPrintRec(seq.tail, diffSeq.tail, currLine + 1, nextOffsets)
      }
    }

    printPrintRec(seq, diffSeq, 0, Seq(offset))
  }

}
