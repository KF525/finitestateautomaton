package kfulton.fsa.two

import scala.annotation.tailrec

class FiniteStateAutomaton {
  @tailrec final def runAutomaton(input: List[Char], maybeState: Option[State], states: List[State]): Boolean =
    (input, maybeState) match {
    case (Nil, None) => false
    case (Nil, Some(state)) if state.isEnd => true
    case (Nil, Some(state)) if !state.isEnd => false
    case (h::t, None) => false
    case (h::t, Some(state)) =>
      val indexOption = findNextTransition(h, maybeState)
      val nextStateOption = findNextState(indexOption, states)
      runAutomaton(t, nextStateOption, states)
  }

  private def findNextTransition(char: Char, maybeState: Option[State]): Option[Int] =
    maybeState match {
      case Some(state) => state.transitions.map.get(char)
      case None => None
    }

  private def findNextState(index: Option[Int], states: List[State]): Option[State] =
    index.map(states(_))
}