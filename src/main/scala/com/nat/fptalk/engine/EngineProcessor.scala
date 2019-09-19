package com.nat.fptalk.engine

object EngineProcessor {

  /**
    *
    * @tparam A any subtypes of C
    * @tparam B any type that can be treated as events of type C
    * @tparam C any type that modeled as states
    */
  trait StateEventProcessor[A<:C, B, C] {
    def processEvent(state: A, event: B): Either[String, C]
  }

  /**
    * Generic processor functions accepts state and events and returns new states
    * IF the transition is not valid, returns reason as string
    * @param state
    * @param event
    * @param sep
    * @tparam A
    * @tparam B
    * @tparam C
    * @return
    */
  def processEvent[A<:C, B, C](state: A, event: B)(implicit sep: StateEventProcessor[A, B, C]): Either[String, C] =
    sep.processEvent(state, event)
}



