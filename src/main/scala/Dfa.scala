import scala.collection.mutable
import scala.collection.mutable.Stack

class Dfa[A] (var alphabet: List[Char],var states: Set[A], var q0: A, var Final: Set[A], var funct:mutable.HashMap[(A,String),A]){

  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  def map[B](f: A => B) : Dfa[B] = ??? // TODO implement map

  def next(state:A, c: Char): A = {
    funct.get(state, c.toString) match {
      case Some(next) => next
    }
  } // TODO implement next

  def accepts(str: String): Boolean = {
    var current_state = q0
    var out = true
    for (c <- str) {
      if(!alphabet.contains(c))
        return false
      if(next(current_state,c) != -3) {
        current_state = next(current_state,c)
      }
      else {
        return false
      }
    }
    if(Final.contains((current_state)))
      out = true
    else
      out = false
    out

  } // TODO implement accepts

  def getStates : Set[A] = {
    states
  } // TODO implement getStates

  def isFinal(state: A): Boolean = {
    if (Final.contains(state)  == true) true else false
  }  // TODO implement isFinal
}

// This is a companion object to the Dfa class. This allows us to call the method fromPrenex without instantiating the Dfa class beforehand.
// You can think of the methods of this object like static methods of the Dfa class
object Dfa {
  def fromPrenex(str: String): Dfa[Int] = {
    var nfa = Nfa.fromPrenex(str)

    var dfa_alphabet = nfa.getAlphabet
    var nfa_init_state = nfa.getInitialState
    var q1 = mutable.Queue[Int]()
    var initial_states : Set[Int] = Set()

    var states = nfa.getStates
    var current_states_aux : Set[Int] = Set()
    var epsilon_closure_map = new mutable.HashMap[Int,Set[Int]]
    var dfa_map = new mutable.HashMap[(Set[Int],Char),Set[Int]]

    for (x <- states) // creating the epsion closures map
    {
      q1.enqueue(x)
      while (!q1.isEmpty) {
        var y = q1.dequeue()
        if (nfa.epsilon_closure(y, "eps") != Set()) {
          current_states_aux = current_states_aux union nfa.epsilon_closure(y, "eps")
          for (z <- nfa.epsilon_closure(y, "eps")) {
            if (z != y)
              q1.enqueue(z)
          }
        }
      }
      epsilon_closure_map += (x -> (current_states_aux union Set(x)))
      current_states_aux = Set()
    }

    epsilon_closure_map.get(nfa_init_state) match {
      case Some(next) => initial_states = next
    }


    var next_states : Set[Int] = Set()
    var states_stack = new Stack[Set[Int]]()
    states_stack.push(initial_states)
    var current_states : Set[Int] = Set()
    var all_states : Set[Set[Int]] = Set()
    var dfa_states : Set[Int] = Set(-3,0)
    all_states += initial_states
    var all_states_id : Set[(Int,Set[Int])] = Set((0,initial_states)) union Set((-3,Set(-3)))
    var id = 1
    while(!states_stack.isEmpty)
    {
      current_states = states_stack.pop()
        for( i <- dfa_alphabet){
          next_states = Set()
          for( s <- current_states) {
            next_states = next_states union nfa.next(s,i)
          }
          for( s <- next_states) {
            epsilon_closure_map.get(s) match {
              case Some(next) => next_states = next_states union next
            }
          }
          if(!next_states.isEmpty && !dfa_map.contains(current_states,i)){
            if(!all_states.contains(next_states)){
              states_stack.push(next_states)
              all_states += next_states
              all_states_id = all_states_id union Set((id,next_states))
              dfa_states = dfa_states union Set(id)
            }
            id += 1
            dfa_map +=  (current_states , i) -> next_states
          }
          else  {
            dfa_map +=  (current_states , i) -> Set(-3)
          }
        }
    }

    var dfa_transitions = new mutable.HashMap[(Int,String),Int]
    for (((stari1,litera),stari2) <- dfa_map){
      var stari1_id = -1
      var stari2_id = -1
      for( (a,s) <- all_states_id){
        if (s==stari1)
          stari1_id = a
        if (s == stari2)
          stari2_id = a
      }
      dfa_transitions += (stari1_id,litera.toString) -> stari2_id
    }

    var final_states :Set[Int] = Set()

    for ( (id,stari) <- all_states_id) {
      if ( !(stari.intersect(nfa.getFinalStates)).isEmpty ){
        final_states = final_states union Set(id)
      }
    }
    var dfa = new Dfa[Int](dfa_alphabet,dfa_states,0,final_states,dfa_transitions)
    dfa
  }
}
