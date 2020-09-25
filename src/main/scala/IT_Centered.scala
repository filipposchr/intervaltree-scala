import scala.collection.mutable.TreeSet
//TreeSet produce elements in a given order
import util.control.Breaks._
import java.util.concurrent.ThreadLocalRandom
object Main_Centered {

  //Class to represent Interval
  class Interval(_st: Double,_en: Double) {
    var st = _st //start point
    var en = _en //end point
    var center = (_st + _en) / 2 //middle value
  }

  //Make comparing object for TreeSet[Interval]
  object IntervalStartOrdering extends Ordering[Interval] {
    //sort by start point increase and if start point is same then sort by end point increase
    def compare(element1:Interval, element2:Interval) = {
      if (element1.st.compareTo(element2.st) != 0)
        element1.st.compareTo(element2.st)
      else
        element1.en.compareTo(element2.en)
    }
  }

  object IntervalEndOrdering extends Ordering[Interval] {
    //sort by end point decrease and if end point is same then sort by start point decrease
    def compare(element1:Interval, element2:Interval) = {
      if (element2.en.compareTo(element1.en) != 0)
        element2.en.compareTo(element1.en)
      else
        element2.st.compareTo(element1.st)
    }
  }

  //Tree Node class
  class Node() extends Serializable {
    var center: Double = 0 //center value of node
    var left: Option[Node] =  None //left child
    var right: Option[Node] = None //right child
    var count: Double = 0 //whole interval count which included the tree which root is current node
    //sort all intervals which contain center(increasing order of start point)
    var stOrder: TreeSet[Interval] = TreeSet()(IntervalStartOrdering)
    //sort all intervals which contain center(decreasing order of end point)
    var enOrder: TreeSet[Interval] = TreeSet()(IntervalEndOrdering)
  }

  //create node with one interval
  def createNode(interval: Interval): Node = {
    val node = new Node();
    node.stOrder += interval
    node.enOrder += interval
    node.center = interval.center
    node.count = 1
    node
  }
  //count of intervals from child which relate the node
  def pushUp(node: Option[Node]): Unit = {
    if (node != None) {
      node.get.count = node.get.stOrder.size//set current node's interval count
      if (node.get.left != None) {//add left child's subtree interval count
        node.get.count += node.get.left.get.count //number of intervals in left subtree
      }
      if (node.get.right != None) {//add right child's subtree interval count
        node.get.count += node.get.right.get.count //number of intervals in right subtree
      }
    }
  }

  //build tree with interval set
  def build(range: => Array[Interval]): Option[Node] = {
    if (range.size == 0)
      None
    else if (range.size == 1)
      Some(createNode(range(0)))
    else {
      //add all values to array and sort to find center value
      var arr: Array[Double] = Array()

      for (ele <- range) {
        arr = arr :+ ele.st
        arr = arr :+ ele.en
      }
      val sort_arr: Array[Double] = arr.sorted

      //find center value and set to root's center
      val mid_pos = arr.length / 2
      val center: Double = sort_arr(mid_pos)

      val node = new Node()

      //find corresponded interval set to make left and right child
      var range_left: Array[Interval] = Array()//add all intervals which is in left side of center point
      var range_right: Array[Interval] = Array()//add all intervals which is in right side of center point
      for (ele <- range) {
        if (ele.st > center) //go to right child
          range_right = range_right :+ ele //add ele to right child
        else if (ele.en < center) //go to left child
          range_left = range_left :+ ele //add ele to left child
        else { //add intervals which overlap with the center point
          node.stOrder += ele
          node.enOrder += ele
        }
      }
      node.center = center
      //make left and right child using recursion with range_left and range_right
      node.left = build(range_left)
      node.right = build(range_right)
      //update count
      pushUp(Some(node))
      Some(node)
    }
  }

  //insert range to root
  def insert(root: Option[Node], range: Interval): Option[Node] = {
    //check root is None
    if (root == None) {
      Some(createNode(range))
    }
    else {
      //check this interval added this node or left child subtree or right child subtree
      val center = root.get.center
      if (range.st > center) //add to right sub tree
        root.get.right = insert(root.get.right, range)
      else if (range.en < center) //add to left sub tree
        root.get.left = insert(root.get.left, range)
      else { //add to current node
        //check already added this node or not
        if(root.get.stOrder.contains(range)) {
        }
        else { //overlaps with center
          root.get.stOrder += range
          root.get.enOrder += range
        }
      }
      //update count of intervals
      pushUp(root)
      root
    }
  }

  //delete the interval from node
  def delete(root: Option[Node], range: Interval): Option[Node] = {
    if (root == None)
      None
    else {
      //check that interval is in this node or left or right child subtree
      if (root.get.center > range.en) {
        root.get.left = delete(root.get.left, range)
      }
      else if (root.get.center < range.st) {
        root.get.right = delete(root.get.right, range)
      }
      else {
        //check this interval exist or not
        if (root.get.stOrder.contains(range)) {
          root.get.stOrder -= range
          root.get.enOrder -= range
        }
      }

      //update interval count
      pushUp(root)

      //check the tree whose root is current node is empty or not and if empty then return none
      if (root.get.count == 0)
        None
      else
        root
    }
  }

  //search all interval from root which overlap with range
  def search(root: Option[Node], range: Interval): Array[Interval] = {
    var answer: Array[Interval] = Array()
    if (root == None) {
      answer
    }
    else {
      //check left child subtree or right child subtree can overlap this range and find using recursion
      if (range.st < root.get.center) {//check range is not in right side of center point and add result of left subtree
      val answer_left: Array[Interval] = search(root.get.left, range)
        for (ele <- answer_left)
          answer = answer :+ ele
      }
      if (range.en > root.get.center) { //check range is not in left side of center point and add result of right subtree
        val answer_right: Array[Interval] = search(root.get.right, range)
        for (ele <- answer_right)
          answer = answer :+ ele
      }
      breakable {
        //find all interval which overlap with this range
        for (ele <- root.get.stOrder) {
          //check element start point already right side of range and break
          if (ele.st > range.en)
            break
          if (ele.en >= range.st) {//check overlap
            answer = answer :+ ele
          }
        }
      }
      answer
    }
  }

  //search all interval from root which contain point
  def search_point(root: Option[Node], point: Double): Array[Interval] = {
    var answer: Array[Interval] = Array()
    if (root == None) {
      answer
    }
    else {
      //check left and right child subtree can overlap this point

      if (point < root.get.center) {
        //get result from left subtree and add
        val answer_left: Array[Interval] = search_point(root.get.left, point)
        for (ele <- answer_left)
          answer = answer :+ ele

        breakable {
          //all elements of current node contain center point so it's en value is bigger than center,
          //so we compare only st value to check overlap
          for (ele <- root.get.stOrder) {
            if (ele.st > point)
              break

            answer = answer :+ ele
          }
        }
      }
      else if (point > root.get.center) {
        //get result from right subtree and add
        val answer_right: Array[Interval] = search_point(root.get.right, point)

        for (ele <- answer_right)
          answer = answer :+ ele

        breakable {
          //all elements of current node contain center point so it's st value is smaller than center,
          //so we compare en value to check overlap
          for (ele <- root.get.enOrder) {
            if (ele.en < point)
              break

            answer = answer :+ ele
          }
        }
      }
      else { //center is same with search point and all intervals in current node contain center.
        for (ele <- root.get.stOrder) {
          answer = answer :+ ele
        }
      }
      answer
    }
  }

  //update range
  def update(root: Option[Node], source: Interval, change: Interval): Option[Node] = {
    //remove and add
    val original_count = root.get.count
    var new_root = delete(root, source)
    val new_count = root.get.count
    //check deleted source interval using count
    if (original_count == new_count) {
      println("Interval to be updated does not exist.")
    }
    else {
      //insert change interval
      new_root = insert(new_root, change)
      println("Updated successfully.")
    }
    new_root
  }

  //print all node
  def printAllNode(root: Option[Node], level: Int): Unit = {
    for (i <- 1 to level)
      print("  ")
    if (root == None) {
      println()
    }
    else {
      print("LEVEL" + level + "---> Center = " + BigDecimal(root.get.center).setScale(4, BigDecimal.RoundingMode.HALF_UP).toDouble + ",")
      for (ele <- root.get.stOrder) {
        print("[" + ele.st + "," + ele.en + "] ")
      }
      println()
      printAllNode(root.get.left, level + 1)
      printAllNode(root.get.right, level + 1)
    }
  }

  def showIntervals(arr: Array[Interval]):Unit = {
    for ( a <- arr ) {
      print("[" + a.st + "," + a.en + "], ")
    }
    println();println()
  }

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    val t = (t1 - t0) / 1e6d
    println("Elapsed time: " + t + " ms")
    result
  }

  def printMenu(): Unit = {
    println("1: Add Interval")
    println("2: Update Interval")
    println("3: Delete Interval")
    println("4: Search Interval")
    println("5: Search Point")
    println("6: Show Built Interval Tree")
    println("7: Show Intervals")
    println("8: Exit")
  }

  def main(args: Array[String]) {
    val number_of_intervals = 200
    val start = -5
    val end   = 10
    val rnd = new scala.util.Random

    var arr: Array[Interval] = Array()
    for (i <- 1 to number_of_intervals) {

      //INTEGERS
      //var x = start + rnd.nextInt( (end - start) + 1
      // var y = start + rnd.nextInt( (end - start) + 1 )

      //DOUBLE
      var x = ThreadLocalRandom.current.nextDouble(start, end)
      var y = ThreadLocalRandom.current.nextDouble(start, end)
      x = BigDecimal(x).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
      y = BigDecimal(y).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble

      while (x == y) {
        //x = start + rnd.nextInt( (end - start) + 1 )
        x = ThreadLocalRandom.current.nextDouble(start, end)
        x = BigDecimal(x).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
      }

      if (y < x) {
        val t = x
        x = y
        y = t
      }
      arr = arr :+ (new Interval(x, y))
    }

    val t_1 = System.nanoTime
    var root: Option[Node] = build(arr)
    val t_build = (System.nanoTime - t_1) / 1e9d
    println("Time elapsed to build tree with " + arr.length + " intervals: " + t_build + " seconds." + "\n")
    var select_menu = 0
    while (select_menu != 8) {
      printMenu()
      select_menu = scala.io.StdIn.readInt()
      if (select_menu == 1) {
        val st = scala.io.StdIn.readDouble()
        val en = scala.io.StdIn.readDouble()
        if (en < st) {
          println("Set end value bigger than start value.")
        }
        else {
          val original_count = root.get.count
          root = time{insert(root, new Interval(st, en))}
          if (original_count == root.get.count) {
            println("Already Exists." + "\n")
          } else {
            println("Added Successfully." + "\n")
            val arr1: Array[Interval] = Array(new Interval(st,en))
            arr = arr ++ arr1
          }
        }
      } else if (select_menu == 2) {
        val st = scala.io.StdIn.readDouble()
        val en = scala.io.StdIn.readDouble()
        val st1 = scala.io.StdIn.readDouble()
        val en1 = scala.io.StdIn.readDouble()
        if (en < st || en1 < st1) {
          println("Set end value bigger than start value.")
        } else {
          root = time{update(root, new Interval(st, en), new Interval(st1, en1))}
          val cur_length = arr.length
          arr = arr.filter(p => {
            var isSame = false
            if(p.st == st) {
              if(p.en == en)
                isSame = true
            }
            !isSame
          })
          if(arr.length != cur_length) {
            arr = arr :+ new Interval(st1, en1)
          }
        }
      } else if (select_menu == 3) {
        val st = scala.io.StdIn.readDouble()
        val en = scala.io.StdIn.readDouble()
        val original_count = root.get.count
        root = time{delete(root, new Interval(st, en))}
        if (root.get.count == original_count) {
          println("Interval does not exist.")
        }
        else {
          println("Deleted successfully.")
          arr = arr.filter(p => {
            var isSame = false
            if(p.st == st) {
              if(p.en == en)
                isSame = true
            }
            !isSame
          })
        }
      } else if (select_menu == 4) {
        val st = scala.io.StdIn.readDouble()
        val en = scala.io.StdIn.readDouble()
        if (en < st) {
          println("Set end value bigger than small value.")
        } else {
          val array_result: Array[Interval] = time{search(root, new Interval(st, en))}
          println("Search Result: ")
          for (ele <- array_result) {
            print("[" + ele.st + "," + ele.en + "] ")
          }
          println();println();
        }
      } else if (select_menu == 5) {
        val st = scala.io.StdIn.readDouble()

        val array_result: Array[Interval] = time{search_point(root, st)}
        println("Search Result: ")
        for (ele <- array_result) {
          print("[" + ele.st + "," + ele.en + "] ")
        }
        println();println()
      } else if (select_menu == 6) {
        printAllNode(root,0)
      }
      else if (select_menu == 7) {
        showIntervals(arr)
      }
    }
  }
}