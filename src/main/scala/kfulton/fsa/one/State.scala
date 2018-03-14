package kfulton.fsa.one

case class State(transitions: List[Transition], isEnd: Boolean, isStart: Boolean)

