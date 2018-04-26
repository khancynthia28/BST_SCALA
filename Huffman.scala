abstract class BT
case class tree(left: BT, symbols: Set[Char], weight: Int, right: BT) extends BT
case object nil extends BT

object Huffman {

  // Construct Huffman Tree from input, a list of single node binary trees
  def constructHuffmanTree(input: List[BT]): BT = input match {
    case List(nil) => nil 
    case List(x) => x 
    case x::y::zs => constructHuffmanTree(insertHT(combine(x,y), zs))
  }

  // combines two trees t1 and t2 by making them children of a new node
  // whose weight is the sum of the weights of t1 and t2 and the symbols
  // are the union of the symbols of t1 and t2; returns the combined tree
  def combine(t1: BT, t2: BT): BT = (t1,t2) match {
    case (tree(l1,s1,w1,r1),(tree(l2,s2,w2,r2))) => tree(t1, s1|s2, w1+w2, t2) 
    case (nil, tree(l, s, w, r)) => t2
    case (tree(l,s,w,r), nil) => t1
    case (_,_) => nil
  }

  // inserts tree t in list of trees ts keeping ts sorted in increasing order
  // of weights; returns new list of trees

  def insertHT(t: BT, ts: List[BT]): List[BT] = (t, ts) match {
    case (tree(_,_,w,_), tree(l,s,xw,r)::xs) =>
      if (w <= xw) t::ts
      else tree(l,s,xw,r) :: insertHT(t, xs)
    case (_,_) => List(t)
    }
  
  

  // Given a character c and the Huffman tree t, return code for c based on t
  def code(c: Char, t: BT): String = {
    def helper(C: Char, T: BT, str: String): String = T match {
    case tree(tree(l1,s1,w1,r1),s,w,tree(l2,s2,w2,r2)) => {
      if (s.contains(c)) {
        if (s1.contains(c)) {
          helper(c, tree(l1,s1,w1,r1), str.concat("0"))
        } else { 
          helper(c, tree(l2,s2,w2,r2), str.concat("1"))
        }
      }
      else  
        "No match"
    }
    case tree(nil1,_,_,nil2) => {
      str
    }  
   }
    helper(c, t, "")
  }

  // Given a string s and the Huffman tree, encode the entire string based on t
  // This should call code(c,t) for each character in s and concatenate the results
  def huffmanEncoding(s: String, t: BT): String = {
    val str = s.toUpperCase().toCharArray()
    str.map(c => code(c, t)).mkString("")
    
  }
 
  // Given a list of 0s and 1s and the Huffman tree, htree, decode the string
  def decode(s: List[Char], htree: BT): String =  {
    def helper (S:List[Char], T:BT, str:String):String = (S, T) match {
    case ((x::xs),(tree(tree(l1,z1,w1,r1),z,w,tree(l2,z2,w2,r2)))) => 
        if (x.equals('0')) 
          helper(xs, tree(l1,z1,w1,r1), str) 
        else 
          helper(xs, tree(l2,z2,w2,r2), str) 
    case (x::xs,tree(nil1,z,w,nil2)) => 
          helper(x::xs, htree, str.concat(z.mkString))
    case (List(),tree(nil1,z,w,nil2)) => 
      str.concat(z.mkString)
    case (List(_),_) => str
    }
    helper(s, htree, "")
  } 
  // Given a string s of 0s and 1s and a Huffman tree t, decode the string
  // corresponding to s
  def huffmanDecoding(s: String, t: BT): String = {
    decode(s.toList, t)
  }
  
  def toString(t: BT): String = t match {
    case `nil` => " "
    case tree(left,set,value,right) => 
      toString(left)++set.mkString(" ")++value.toString++toString(right)
  }

  def toString2(t: BT): String = t match {
    case `nil` => "nil"
    case tree(left,set,value,right) => 
      "tree("++toString2(left)++","++set.mkString(" ")++","++value.toString++","++toString2(right)++")"
  }

  def printBST(s: BT) = {
    println(toString2(s))
  }

  def printBSTAsSet(s: BT) = {
    println("("+toString(s)+")")
  }


}
