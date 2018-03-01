package kfulton.fsa

import org.scalatest.{FlatSpec, Matchers}

class FiniteStateAutomatonTest extends FlatSpec with Matchers {

  val finiteStateAutomaton = new FiniteStateAutomaton

  "runAutomaton" should "return Option of next state if input is valid" in {
    val charList = List('a')

    val endState: State = State(Nil, true, false)
    val f = (c: Char) => { if (c === 'a') Some(endState) else None }
    val currentState = State(List(Transition(f)), false, true)

    finiteStateAutomaton.runAutomaton(charList, Some(currentState)) shouldBe true
  }
  it should "return None if input is not valid" in {
    val charList = List('a')

    val endState: State = State(Nil, true, false)
    val f = (c: Char) => { if (c === 'b') Some(endState) else None }
    val currentState = State(List(Transition(f)), false, true)

    finiteStateAutomaton.runAutomaton(charList, Some(currentState)) shouldBe false
  }
  it should "handle multiple characters" in {
    val charList = List('a', 'b', 'c')

    val endState: State = State(Nil, true, false)
    val f3 = (c: Char) => { if (c === 'c') Some(endState) else None }
    val state3 = State(List(Transition(f3)), false, false)
    val f2 = (c: Char) => { if (c === 'b') Some(state3) else None }
    val state2 = State(List(Transition(f2)), false, false)
    val f1 = (c: Char) => { if (c === 'a') Some(state2) else None }
    val currentState = State(List(Transition(f1)), false, true)

    finiteStateAutomaton.runAutomaton(charList, Some(currentState)) shouldBe true
  }
  it should "fail if input does not result in end state" in {
    val charList = List('a', 'b')

    val endState: State = State(Nil, true, false)
    val f3 = (c: Char) => { if (c === 'c') Some(endState) else None }
    val state3 = State(List(Transition(f3)), false, false)
    val f2 = (c: Char) => { if (c === 'b') Some(state3) else None }
    val state2 = State(List(Transition(f2)), false, false)
    val f1 = (c: Char) => { if (c === 'a') Some(state2) else None }
    val currentState = State(List(Transition(f1)), false, false)

    finiteStateAutomaton.runAutomaton(charList, Some(currentState)) shouldBe false
  }
  it should "fail if input does not match exactly" in {
    val charList = List('a', 'b', 'c')

    val endState: State = State(Nil, true, false)
    val f2 = (c: Char) => { if (c === 'b') Some(endState) else None }
    val state2 = State(List(Transition(f2)), false, false)
    val f1 = (c: Char) => { if (c === 'a') Some(state2) else None }
    val currentState = State(List(Transition(f1)), false, false)

    finiteStateAutomaton.runAutomaton(charList, Some(currentState)) shouldBe false
  }
  it should "handle states that have more than one transition" in {
    val charList1 = List('a')
    val charList2 = List('b')
    val charList3 = List('c')

    val endState: State = State(Nil, true, false)
    val f2 = (c: Char) => { if (c === 'b') Some(endState) else None }
    val f = (c: Char) => { if (c === 'a') Some(endState) else None }
    val currentState = State(List(Transition(f), Transition(f2)), false, true)

    finiteStateAutomaton.runAutomaton(charList1, Some(currentState)) shouldBe true
    finiteStateAutomaton.runAutomaton(charList2, Some(currentState)) shouldBe true
    finiteStateAutomaton.runAutomaton(charList3, Some(currentState)) shouldBe false
  }
  it should "handle endState that has transition" in {
    val charList1 = List('a', 'b')
    val charList2 = List('a', 'b', 'a')

    lazy val f1 = (c: Char) => { if (c === 'b') Some(startState) else None}
    lazy val endState1 = State(List(Transition(f1)), true, false)
    lazy val f = (c: Char) => { if (c === 'a') Some(endState1) else None}
    lazy val startState: State = State(List(Transition(f)), false, true)

    finiteStateAutomaton.runAutomaton(charList1, Some(startState)) shouldBe false
    finiteStateAutomaton.runAutomaton(charList2, Some(startState)) shouldBe true
  }
  it should "handle empty string" in {
    val charList = List(' ')

    val endState: State = State(Nil, true, false)
    val f = (c: Char) => { if (c === ' ') Some(endState) else None }
    val currentState = State(List(Transition(f)), false, true)

    finiteStateAutomaton.runAutomaton(charList, Some(currentState)) shouldBe true
  }
  it should "handle start/end being in the same state" in {
    val charList = List()
    val singleState: State = State(Nil, true, true)

    finiteStateAutomaton.runAutomaton(charList, Some(singleState)) shouldBe true
  }
}
