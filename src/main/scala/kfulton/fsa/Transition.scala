package kfulton.fsa

case class Transition(f: Char => Option[State])
