import scala.collection.mutable
import scala.collection.mutable.Stack


class Nfa[A](var alphabet: List[Char],var states: Set[A], var q0: A, var Final: Set[A], var funct:mutable.HashMap[(A,String),Set[A]]) {

  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  def map[B](f: A => B) : Nfa[B] = ??? // TODO implement map

  def next(state:A, c: Char): Set[A] = {
    funct.get(state,c.toString) match {
      case Some(next) => next
      case None => Set()
    }
  } // TODO implement next

  def epsilon_closure(state: A, str: String): Set[A] = {
    funct.get(state, str) match {
      case Some(next) => next
      case None => Set()
    }
  } // TODO implement next

  def accepts(str: String): Boolean = {
    var current_states = Set(q0)
    var q1 = mutable.Queue[A]()
    for(c <- str)
    {
      var current_states_aux : Set[A] = Set()

      for (x <- current_states) // epsilon closures
      {
        q1.enqueue(x)
        while(! q1.isEmpty ){
          var y = q1.dequeue()
          if ( epsilon_closure(y,"eps") != Set() ) {
            current_states_aux = current_states_aux union epsilon_closure(y,"eps")
            for (z <- epsilon_closure(y, "eps")) {
              if(z != y)
                q1.enqueue(z)
            }
          }
        }
      }

      current_states = current_states_aux union current_states
      current_states_aux = Set()

      for (x <- current_states)
      {
        current_states_aux = current_states_aux union next(x,c)
      }

      current_states = current_states_aux
    }
    var current_states_aux : Set[A] = Set()
    for (x <- current_states) // epsilon closures
    {
      q1.enqueue(x)
      while (!q1.isEmpty) {
        var y = q1.dequeue()
        if (epsilon_closure(y, "eps") != Set()) {
          current_states_aux = current_states_aux union epsilon_closure(y, "eps")
          for (z <- epsilon_closure(y, "eps")) {
            if (z != y)
              q1.enqueue(z)
          }
        }
      }
    }

    current_states = current_states_aux union current_states

    if(current_states == Set()) {
      false
    } else
    {
      for (x <- current_states) {
        if(Final.contains(x) == true)
          return true
      }
      false
    }
  } // TODO implement accepts

  def getStates : Set[A] = {
    states
  }

  def getAlphabet: List[Char] = {
    alphabet
  }

  def getInitialState: A = {
    q0
  }

  def getFinalStates: Set[A] = {
    Final
  }

  def getFunct:mutable.HashMap[(A, String), Set[A]] = {
    funct
  }

  def isFinal(state: A): Boolean = {
    if (Final.contains(state)  == true) true else false
  }
}

// This is a companion object to the Nfa class. This allows us to call the method fromPrenex without instantiating the Nfa class beforehand.
// You can think of the methods of this object like static methods of the Nfa class
object Nfa {

  var cnt = 0

  def Concat(nfa1: Nfa[Int], nfa2: Nfa[Int]): Nfa[Int] = {
    var hashmap1 = nfa1.getFunct
    var hashmap2 = nfa2.getFunct
    var result = hashmap1.++(hashmap2)
    result += ( (nfa1.getFinalStates.head, "eps") -> Set(nfa2.getInitialState))
    var alphabet = nfa1.getAlphabet.filterNot(nfa2.getAlphabet.contains) ::: nfa2.getAlphabet
    new Nfa[Int](alphabet,nfa1.getStates union nfa2.getStates ,  nfa1.getInitialState, nfa2.getFinalStates, result)
  }

  def Plus(nfa1: Nfa[Int], nfa2: Nfa[Int]): Nfa[Int] = ???

  def Star(nfa1: Nfa[Int]): Nfa[Int] = {
    var hashmap1 = nfa1.getFunct

    val totalNrStates = cnt
    var new_states = nfa1.getStates union Set(totalNrStates, totalNrStates + 1)

    var result = hashmap1

    result += ((totalNrStates, "eps") -> Set(nfa1.getInitialState, totalNrStates + 1))

    result += ((nfa1.getFinalStates.head, "eps") -> Set(totalNrStates + 1, nfa1.getInitialState))

    cnt+=2
    new Nfa[Int](nfa1.getAlphabet , new_states, totalNrStates, Set(totalNrStates + 1), result)
  }

  def Union(nfa1: Nfa[Int], nfa2: Nfa[Int]): Nfa[Int] = {
    var hashmap1 = nfa1.getFunct
    var hashmap2 = nfa2.getFunct

    val totalNrStates = cnt
    var new_states = nfa1.getStates union nfa2.getStates union Set(totalNrStates, totalNrStates + 1)

    var result = hashmap1.++(hashmap2)

    result += ((totalNrStates, "eps") -> Set(nfa1.getInitialState,nfa2.getInitialState))

    result += ((nfa1.getFinalStates.head, "eps") -> Set(totalNrStates+1))
    result += ((nfa2.getFinalStates.head, "eps") -> Set(totalNrStates+1))
    var alphabet = nfa1.getAlphabet.filterNot(nfa2.getAlphabet.contains) ::: nfa2.getAlphabet
    cnt+=2
    new Nfa[Int](alphabet, new_states, totalNrStates, Set(totalNrStates + 1), result)
  }

  def fromPrenex(str: String): Nfa[Int] = {
    var z = str.split(" ")
    var words_stack = new Stack[String]()
    var nfa_stack = new Stack[Nfa[Int]]()
    cnt = 0

    for(el <- z) {
        words_stack.push(el)
      }

    while(!words_stack.isEmpty) {
      var a = words_stack.pop()
      if(a.length() == 1)
      {
        var nfa = new Nfa[Int](List(a(0)), Set( cnt, cnt + 1),cnt, Set(cnt + 1), mutable.HashMap( (cnt,a) -> Set(cnt+1) ) )
        nfa_stack.push(nfa)
        cnt += 2
      }
      else if ( a == "eps")
      {
        var nfa = new Nfa[Int](List(), Set(cnt), cnt, Set(cnt), mutable.HashMap((cnt, a) -> Set(cnt)))
        nfa_stack.push(nfa)
        cnt += 1
      }
      else if ( a == "void")
      {
        var nfa = new Nfa[Int](List(), Set( cnt, cnt + 1), cnt, Set(cnt+1), mutable.HashMap())
        nfa_stack.push(nfa)
        cnt += 2
      }
      else if(a == "CONCAT")
      {
        var nfa1 = nfa_stack.pop()
        var nfa2 = nfa_stack.pop()
        nfa_stack.push(Concat(nfa1,nfa2))
      }
      else if (a == "PLUS") {
        var nfa1 = nfa_stack.pop()
        var nfa2 = nfa_stack.pop()
        nfa_stack.push(Plus(nfa1, nfa2))
      }
      else if (a == "STAR") {
        var nfa1 = nfa_stack.pop()
        nfa_stack.push(Star(nfa1))
      }
      else if (a == "UNION") {
        var nfa1 = nfa_stack.pop()
        var nfa2 = nfa_stack.pop()
        nfa_stack.push(Union(nfa1, nfa2))
      }
    }

    val debug = nfa_stack.top

    nfa_stack.pop()
  }

}