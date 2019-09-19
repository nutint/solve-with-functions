package com.nat.fptalk.engine

import com.nat.fptalk.engine.EngineProcessor.StateEventProcessor

object NoFuelEngineProcessorLogic {

  implicit val myEngineProcessor = new StateEventProcessor[EngineState, EngineEvent, EngineState] {
    override def processEvent(state: EngineState, event: EngineEvent): Either[String, EngineState] = state match {
      case es: EngineStopped => EngineStopProcessor.processEvent(es, event)
      case er: EngineRunning => EngineRunningProcessor.processEvent(er, event)
      case eb: EngineBlewUp => EngineBlewUpProcessor.processEvent(eb, event)
    }
  }

  object EngineStopProcessor extends StateEventProcessor[EngineStopped, EngineEvent, EngineState] {
    override def processEvent(state: EngineStopped, event: EngineEvent): Either[String, EngineState] = event match {
      case StartEngine => Right(EngineRunning(0))
      case _ => Left("No Fuel")
    }
  }

  object EngineRunningProcessor extends StateEventProcessor[EngineRunning, EngineEvent, EngineState] {
    override def processEvent(state: EngineRunning, event: EngineEvent): Either[String, EngineState] = event match {
      case DecelerateEngine | DecelerateEngine=> if(state.velocity > 0) Right(state.copy(velocity = state.velocity - 1)) else Left("Engine velocity already 0")
      case StopEngine => if(state.velocity > 0) Right(EngineBlewUp(state.velocity)) else Right(EngineStopped())
      case StartEngine => Left("Engine already started")
    }
  }

  object EngineBlewUpProcessor extends StateEventProcessor[EngineBlewUp, EngineEvent, EngineState] {
    override def processEvent(state: EngineBlewUp, event: EngineEvent): Either[String, EngineState] = Left("Engine already blew up")
  }

}