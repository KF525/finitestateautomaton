package kfulton.fsa.two

case class State(isEnd: Boolean, isStart: Boolean, index: Int, transitions: Transitions)
