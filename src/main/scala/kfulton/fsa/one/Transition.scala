package kfulton.fsa.one

case class Transition(f: Char => Option[State])
