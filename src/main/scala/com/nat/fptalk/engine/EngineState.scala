package com.nat.fptalk.engine

trait EngineState
case class EngineStopped() extends EngineState
case class EngineRunning(velocity: Int) extends EngineState
case class EngineBlewUp(engineDamage: Int) extends EngineState