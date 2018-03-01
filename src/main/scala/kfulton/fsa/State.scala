package kfulton.fsa

case class State(transitions: List[Transition], isEnd: Boolean, isStart: Boolean)

