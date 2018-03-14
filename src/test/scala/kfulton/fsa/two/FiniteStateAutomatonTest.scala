package kfulton.fsa.two

import org.scalatest.{FlatSpec, Matchers}

class FiniteStateAutomatonTest extends FlatSpec with Matchers {

  val finiteStateAutomaton = new FiniteStateAutomaton

  "runAutomaton" should "return Option of next state if input is valid" in {
    val charList = List('a')

    val endState = State(true, false, 1, Transitions(Map()))
    val currentState = State(false, true, 0, Transitions(Map('a' -> 1)))
    val states = List(currentState, endState)

    finiteStateAutomaton.runAutomaton(charList, Some(currentState), states) shouldBe true
  }
  it should "return None if input is not valid" in {
    val charList = List('a')

    val endState: State = State(true, false, 1, Transitions(Map()))
    val currentState = State(false, true, 0, Transitions(Map('b' -> 1)))
    val states = List(currentState, endState)

    finiteStateAutomaton.runAutomaton(charList, Some(currentState), states) shouldBe false
  }
  it should "handle multiple characters" in {
    val charList = List('a', 'b', 'c')

    val endState: State = State(true, false, 3, Transitions(Map()))
    val state3 = State(false, false, 2, Transitions(Map('c' -> 3)))
    val state2 = State(false, false, 1, Transitions(Map('b' -> 2)))
    val currentState = State(false, true, 0, Transitions(Map('a' -> 1)))
    val states = List(currentState, state2, state3, endState)

    finiteStateAutomaton.runAutomaton(charList, Some(currentState), states) shouldBe true
  }
  it should "fail if input does not result in end state" in {
    val charList = List('a', 'b')

    val endState: State = State(true, false,3, Transitions(Map()))
    val state3 = State(false, false,2, Transitions(Map('c' -> 3)))
    val state2 = State(false, false,1, Transitions(Map('b' -> 2)))
    val currentState = State(false, false, 0, Transitions(Map('a' -> 1)))
    val states = List(currentState, state2, state3, endState)

    finiteStateAutomaton.runAutomaton(charList, Some(currentState), states) shouldBe false
  }
  it should "fail if input does not match exactly" in {
    val charList = List('a', 'b', 'c')

    val endState: State = State(true, false, 2, Transitions(Map()))
    val state2 = State(false, false, 1, Transitions(Map('b' -> 2)))
    val currentState = State(false, false, 0, Transitions(Map('a' -> 1)))
    val states = List(currentState, state2, endState)

    finiteStateAutomaton.runAutomaton(charList, Some(currentState), states) shouldBe false
  }
  it should "handle states that have more than one transition" in {
    val charList1 = List('a')
    val charList2 = List('b')
    val charList3 = List('c')

    val endState: State = State(true, false, 1, Transitions(Map()))
    val currentState = State(false, true, 0, Transitions(Map('a' -> 1, 'b' -> 1)))
    val states = List(currentState, endState)

    finiteStateAutomaton.runAutomaton(charList1, Some(currentState), states) shouldBe true
    finiteStateAutomaton.runAutomaton(charList2, Some(currentState), states) shouldBe true
    finiteStateAutomaton.runAutomaton(charList3, Some(currentState), states) shouldBe false
  }
  it should "handle endState that has transition" in {
    val charList1 = List('a', 'b')
    val charList2 = List('a', 'b', 'a')

    val endState = State(true, false, 1, Transitions(Map('b' -> 0)))
    val startState: State = State(false, true, 0, Transitions(Map('a' -> 1)))
    val states = List(startState, endState)

    finiteStateAutomaton.runAutomaton(charList1, Some(startState), states) shouldBe false
    finiteStateAutomaton.runAutomaton(charList2, Some(startState), states) shouldBe true
  }
  it should "handle empty string" in {
    val charList = List(' ')

    val endState: State = State(true, false, 1, Transitions(Map()))
    val currentState = State(false, true, 0, Transitions(Map(' ' -> 1)))
    val states = List(currentState, endState)

    finiteStateAutomaton.runAutomaton(charList, Some(currentState), states) shouldBe true
  }
  it should "handle start/end being in the same state" in {
    val charList = List()

    val singleState: State = State(true, true, 0, Transitions(Map()))
    val states = List(singleState)

    finiteStateAutomaton.runAutomaton(charList, Some(singleState), states) shouldBe true
  }
  it should "fail if input is not correctly ordered" in {
    val charList = List('a', 'b', 'c')

    val endState = State(true, false, 3, Transitions(Map()))
    val state1 = State(false, false, 2, Transitions(Map('b' -> 3)))
    val state0 = State(false, false, 1, Transitions(Map('c' -> 2)))
    val startState = State(false, true, 0, Transitions(Map('a' -> 1)))
    val states = List(startState, state0, state1, endState)

    finiteStateAutomaton.runAutomaton(charList, Some(startState), states) shouldBe false

  }
  it should "handle multiple of the same characters appropriately" in {
    val charList = List('a', 'b', 'c', 'a')

   val endState = State(true, false, 4, Transitions(Map()))
    val state2 = State(false, false, 3, Transitions(Map('a' -> 4)))
    val state1 = State(false, false, 2, Transitions(Map('c' -> 3)))
    val state0 = State(false, false, 1, Transitions(Map('b' -> 2)))
    val startState = State(false, true, 0, Transitions(Map('a' -> 1)))
    val states = List(startState, state0, state1, state2, endState)

    finiteStateAutomaton.runAutomaton(charList, Some(startState), states) shouldBe true
  }
}