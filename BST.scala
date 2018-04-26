object BST {

  abstract class BT
  case class Tree(left: BT, value: Int, right: BT) extends BT
  object Nil extends BT

  def minBST(t: BT): Int = t match {
  	case Nil => throw new Exception ("Empty Set: NO minimum")
  	case Tree(Nil, value, right) => value
    case Tree(left,value,right) => minBST(left)
  }

  def maxBST(t: BT): Int = t match {
  	case Nil => throw new Exception ("Empty Set: NO maximum")
  	case Tree(left, value, Nil) => value
    case Tree(left,value,right) => maxBST(right)
  }

  def isBST(t: BT): Boolean = t match {
  	case Nil => true
  	case Tree(Nil, value, Nil) => true
  	case Tree(Nil, value, right) =>
  	  (value < minBST(right)) &&
  	  isBST(right)
  	case Tree(left, value, Nil) =>
  	  (value > maxBST(left)) &&
  	  isBST(left)
    case Tree(left,value,right) => 
       (value > maxBST(left)) && 
       (value < minBST(right)) &&
       isBST(left) && 
       isBST(right)
  	
  }

  def memberBST(x: Int, t: BT): Boolean = t match {
    case Nil => false
    case Tree(left,value,right) => 
      if (x == value) 
        true
      else if (x < value) 
        memberBST(x, left)
      else 
        memberBST(x, right)
  }

  def insertBST(x: Int, t: BT): BT = t match {
    case Nil => Tree(Nil, x, Nil)
    case Tree(left,value,right) => 
      if (x == value)
        t
      else if (x < value)
       Tree(insertBST(x, left), value, right)
      else
       Tree(left, value, insertBST(x, right))
  }

  def heightBST(t: BT): Int = t match {
    case Nil => 0
    case Tree(Nil, value, Nil) => 0
    case Tree(left,value,right) =>
      if (heightBST(left) > heightBST(right))
        heightBST(left) + 1
      else 
        heightBST(right) + 1
  }

  def deleteBST(x: Int, t: BT): BT = t match {
    case Nil => t
    case Tree(left,value,right) => {
      if (x == value) {
        if (left == Nil && right == Nil)
          Nil
        else if (left == Nil)
          right
        else if (right == Nil)
          left
        else {
          val min = minBST(right) 
          Tree(left, min, deleteBST(min, right))
        }
      }
      else if (memberBST(x, left))
        Tree(deleteBST(x, left), value, right)
      else if (memberBST(x, right))
        Tree(left, value, deleteBST(x, right))
      else t
    }
  }

  def sizeBST(t: BT): Int = t match {
    case Nil => 0
    case Tree(left,value,right) =>
      (sizeBST(left) + sizeBST(right)) + 1
  }

  def toString(t: BT): String = t match {
    case Nil => " "
    case Tree(left,value,right) => 
      toString(left)++value.toString++toString(right)
  }

  def toString2(t: BT): String = t match {
    case Nil => "nil"
    case Tree(left,value,right) => 
      "tree("++toString2(left)++","++value.toString++","++toString2(right)++")"
  }

  def printBST(s: BT) = {
    println(toString2(s))
  }

  def printBSTAsSet(s: BT) = {
    println("("+toString(s)+")")
  }

}
