import scala.util.control._
import scala.collection.mutable.ArrayBuffer

class Node (val l: Int, val h: Int) {
  var low: Int = l
  var high: Int = h
  var max: Int = h
  var left: Node = null
  var right: Node = null
}

object Main_Augm {
  def main (args: Array[String]) {
    var root: Node = null
    var temp: Node = null
    var i: Int = 0
    var tempA: Int = 0
    var tempB: Int = 0
    val loop = new Breaks
    var ols = ArrayBuffer[Node]()

    root = insert(root, 20, 25)
    root = insert(root, 19, 25)
    root = insert(root, 23, 30)
    root = insert(root, 10, 20)
    root = insert(root, 12, 21)
    root = insert(root, 8, 11)

    print2D(root, 0)

    loop.breakable {
      do {
        println("Select an option")
        println("1) Insert")
        println("2) Delete")
        println("3) Search for an interval")
        println("4) Interval intersection query")
        println("5) Print")
        println("6) Exit")
        print("Option: ")

        i = scala.io.StdIn.readInt()

        i match {
          case 1 =>
            println("Give the interval to be inserted: ")
            print("Low value: ")
            tempA = scala.io.StdIn.readInt()
            print("High value: ")
            tempB = scala.io.StdIn.readInt()

            while (tempB <= tempA) {
              print("High value must be greater than low value! Please try again: ")
              tempB = scala.io.StdIn.readInt()
            }

            root = insert(root, tempA, tempB)
            println("\nInsertion completed!\n")

          case 2 =>
            print("Give the low value of interval to be deleted: ")
            tempA = scala.io.StdIn.readInt()

            root = deleteNode (root, tempA)
            println("\nDelete process completed!\n")

          case 3 =>
            println("Give the interval to be searched: ")
            print("Low value: ")
            tempA = scala.io.StdIn.readInt()
            print("High value: ")
            tempB = scala.io.StdIn.readInt()

            while (tempB <= tempA){
              print("High value must be greater than low value! Please try again: ")
              tempB = scala.io.StdIn.readInt()
            }

            if (!search(root, tempA, tempB)) println("\nInterval doesn't exist!\n")

          case 4 =>
            println("Give the interval to be searched: ")
            print("Low value: ")
            tempA = scala.io.StdIn.readInt()
            print("High value: ")
            tempB = scala.io.StdIn.readInt()

            while (tempB <= tempA){
              print("High value must be greater than low value! Please try again: ")
              tempB = scala.io.StdIn.readInt()
            }

            ols.clear
            overlapSearch(root, tempA, tempB)

            if(ols.size != 0){
              print("\nOverlaps with:")
              for ( x <- ols ) {
                print(" [" + x.low + "," + x.high + "]")
              }
              println("\n")
            }
            else println("\nDoesn't overlap!\n")

          case 5 =>
            print2D(root, 0)
            println()

          case 6 => loop.break

          case _ => println("\nInvalid option, please try again\n")
        }
      } while (true)
    }



    /* A utility function to insert a new Interval Search Tree Node
    This is similar to BST Insert.  Here the low value of interval
    is used to maintain BST property */
    def insert (root: Node, ilow: Int, ihigh: Int): Node = {
      if (root == null) return new Node(ilow, ihigh)

      // Get low value of interval at root
      var l = root.low

      // If root's low value is smaller, then new interval goes to left subtree
      if (ilow < l)
        root.left = insert(root.left, ilow, ihigh)

      // Else, new Node goes to right subtree.
      else
        root.right = insert(root.right, ilow, ihigh)

      // Update the max value of this ancestor if needed
      if (root.max < ihigh)
        root.max = ihigh

      return root
    }

    /* Given an Interval Tree and a key, this function deletes the interval
    with that key and returns the new root */
    def deleteNode (root: Node, key: Int): Node = {
      // Tree is empty
      if (root == null) return root

      // If the key to be deleted is smaller than the root's low value, then it lies in right subtree
      if (key < root.low)
        root.left = deleteNode(root.left, key)

      // If the key to be deleted is greater than the root's low value, then it lies in right subtree
      else if (key > root.low)
        root.right = deleteNode(root.right, key)

      // if key is same as root's low value, then this is the node to be deleted
      else {
        // node with only one child or no child
        if (root.left == null) {
          var temp: Node = root.right
          return temp
        }
        else if (root.right == null) {
          var temp: Node = root.left
          return temp
        }

        // node with two children: Get the inorder successor (smallest in the right subtree)
        var temp: Node = minValueNode(root.right)

        // Copy the inorder successor's content to this node
        root.low = temp.low
        root.high = temp. high
        root.max = temp.max

        // Delete the inorder successor
        root.right = deleteNode(root.right, temp.low)
      }

      return root
    }

    // Searches for a certain interval
    def search (root: Node, ilow: Int, ihigh: Int): Boolean = {
      // Tree is empty
      if (root == null) return false

      // If the low value to be searched is smaller than the root's low value, then it lies in right subtree
      if (ilow < root.low)
        search (root.left, ilow, ihigh)

      // If the low value to be searched is greater than the root's low value, then it lies in right subtree
      else if (ilow > root.low)
        search (root.right, ilow, ihigh)

      // if low value is same as root's low value, then check high value
      else {
        if (ihigh == root.high) {
          println("\nInterval found!\n")
          return true
        }
        else return false
      }

    }

    //Checks if 2 intervals, do overlap
    def overlap (low1: Int, high1: Int, low2: Int, high2: Int): Boolean = {
      if (low1 <= high2 && low2 <= high1)
        return true
      return false
    }

    // Given an interval, the algorithm searches the tree for an intersection
    def overlapSearch (root: Node, ilow: Int, ihigh: Int): Node = {
      // Tree is empty
      if (root == null) return null

      // If given interval overlaps with root
      if (overlap(root.low, root.high, ilow, ihigh)) ols += root

      /* If left child of root is present and max of left child is
      greater than or equal to given interval, then i may
      overlap with an interval is left subtree */
      if (root.left != null && root.left.max >= ilow)
        overlapSearch(root.left, ilow, ihigh)

      // Else interval can only overlap with right subtree
      overlapSearch(root.right, ilow, ihigh)
    }

    /* Given a non-empty Interval Tree, return the node
    with minimum low value found in that tree. */
    def minValueNode (node: Node): Node = {
      var current: Node = node

      /* loop down to find the leftmost leaf */
      while (current.left != null)
        current = current.left

      return current
    }

    // Prints the tree
    def print2D(root: Node, space: Int): Unit = {
      var i = 0;
      var temp = 0

      // Tree is empty
      if (root == null) return

      // Increase distance between levels
      temp = space + 10

      // Process right child first
      print2D(root.right, temp)

      // Print current node after space
      println()
      for (i <- 10 to temp)
        print(" ")
      println("[" + root.low + "," + root.high + "] - max:" + root.max)

      // Process left child
      print2D(root.left, temp)
    }
  }
}