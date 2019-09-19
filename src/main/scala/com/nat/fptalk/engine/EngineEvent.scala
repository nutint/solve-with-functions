package com.nat.fptalk.engine

trait EngineEvent
case object StartEngine extends EngineEvent
case object StopEngine extends EngineEvent
case object AccelerateEngine extends EngineEvent
case object DecelerateEngine extends EngineEvent