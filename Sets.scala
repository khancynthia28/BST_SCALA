object Sets {
  type Set = Int => Boolean
  val bound = 1000                                //> bound  : Int = 1000

  // returns true if elem is in s, false otherwise
  def contains(s: Set, elem: Int): Boolean = s(elem)
                                                  //> contains: (s: Sets.Set, elem: Int)Boolean
  // returns a set consisting of a single element, elem
  def singletonSet(elem: Int): Set = {x: Int => elem == x}
                                                  //> singletonSet: (elem: Int)Sets.Set
  // returns an empty set
  def emptySet: Set = Nil                         //> emptySet: => Sets.Set

  // inserts element x into set s
  def add(s: Set, x: Int): Set = {elem: Int => s(elem) || x == elem }
                                                  //> add: (s: Sets.Set, x: Int)Sets.Set
  // returns the union of s and t
  def union(s: Set, t: Set): Set = {x: Int => contains(s,x) || contains(t, x)}
                                                  //> union: (s: Sets.Set, t: Sets.Set)Sets.Set

  // returns the intersection of s and t
  def intersect(s: Set, t: Set): Set = {elem: Int => contains(s,elem) && contains(t, elem)}
                                                  //> intersect: (s: Sets.Set, t: Sets.Set)Sets.Set

  // returns the difference of s and t, s-t
  def difference(s: Set, t: Set): Set =  {elem: Int => contains(s,elem) && !contains(t, elem)}
                                                  //> difference: (s: Sets.Set, t: Sets.Set)Sets.Set

  // returns true is s is a subset of t, false otherwise
  def subset(s: Set, t: Set): Boolean = {s == intersect(s, t)}
                                                  //> subset: (s: Sets.Set, t: Sets.Set)Boolean

  // returns true if s is equal to t, false otherwise
  def equal(s: Set, t: Set): Boolean = {subset(s, t) == subset(t, s)}
                                                  //> equal: (s: Sets.Set, t: Sets.Set)Boolean

  // returns the number of elements in s
  def size(s: Set): Int = {
  		val count = 0
  	  def iter (a: Int): Int = {
  	  	if (a == -bound) count
  	  	else if (contains(s, bound)) count + 1
  	  	else iter(a - 1)
  	  }
  	  iter (bound)
  }                                               //> size: (s: Sets.Set)Int

  // returns a subset of s of elements of s that satisfy the predicate p
  def filter(s: Set, p: Int => Boolean): Set = {x: Int => contains(s,x) && contains(p,x)}
                                                  //> filter: (s: Sets.Set, p: Int => Boolean)Sets.Set

  // returns true if all elements of s satisfy predicate p
  def forall(s: Set, p: Int => Boolean): Boolean = {
      def iter(a: Int): Boolean = {
         if (a == -bound) true
         else if ((contains(s,a)) && (!contains(filter(s,p),a))) false
         else iter(a-1)
      }
      iter(bound)
  }                                               //> forall: (s: Sets.Set, p: Int => Boolean)Boolean
  // returns true if at least one element of s satisfies predicate p
  def exists(s: Set, p: Int => Boolean): Boolean = {
     def iter(a: Int): Boolean = {
       if (a == -bound) false
       else if ((contains(s,a)) && (contains(filter(s,p),a))) true
       else iter(a-1)
     }
    iter(bound)
  }                                               //> exists: (s: Sets.Set, p: Int => Boolean)Boolean

  // returns a new set obtained by applying f to each element of s
  // and forming a set of the results
  // e.g. if s = {1, 2, 3} and f = (x>x*x) the map(s,f) = {1, 4, 9}
  def map(s: Set, f: Int => Int): Set = { x:Int => exists(s, {y:Int => f(y) == x}) }
                                                  //> map: (s: Sets.Set, f: Int => Int)Sets.Set
  def sumElements(s: Set): Int = {
    def helper(i:Int, sum:Int): Int = {
      if (i > 1000)
        sum
      else if (contains(s, i))
        helper(i + 1, sum + i)
      else if (contains(s, -i))
        helper(i+ 1, sum - i)
      else
        helper(i + 1, sum)
  }
  helper(0,0)
}

  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }                                               //> toString: (s: Sets.Set)String

  def printSet(s: Set) {
    println(toString(s))
  }                                               //> printSet: (s: Sets.Set)Unit

}
