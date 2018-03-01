package kfulton.fsa

class FiniteStateAutomaton {

  def runAutomaton(input: List[Char], currentState: Option[State]): Boolean = (input, currentState) match {
    case (h::t, Some(State(Nil, true, _))) => false
    case (Nil, Some(State(_, true, _))) => true
    case (Nil, Some(State(_, false, _))) => false
    case (h::t, Some(State(transitionH::transitionT, _, _))) =>
      if (transitionH.f(h).isDefined) {
        runAutomaton(t,transitionH.f(h))
      } else {
        val updatedState = currentState.map(state => state.copy(transitions = state.transitions.tail))
        runAutomaton(h::t, updatedState)
      }
    case (_, _) => false
  }
}




