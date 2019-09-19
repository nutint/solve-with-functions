package com.nat.fptalk

class MySimpleApp
  extends App
{
  def playWithShapes(): Unit = {
    import shape.Shape._
    println(drawRectangle(3))
    println(drawRectangle(7))
    println(drawUpTriangle(7))
    println(drawDownTriangle(7))
  }

  def playWithEngine(): Unit = {
    import com.nat.fptalk.engine._
    import EngineProcessor._

    // business Logics (Choose either this ones to apply business logic)
    import EngineProcessingLogic._
//    import NoFuelEngineProcessorLogic._

    def processEngineEvent(engineState: EngineState, engineEvent: EngineEvent): Either[String, EngineState] =
      processEvent(engineState, engineEvent)

    val engineStateResult = processEngineEvent(EngineStopped(), StartEngine)
      .flatMap(e => processEngineEvent(e, StopEngine))
      .flatMap(e => processEngineEvent(e, AccelerateEngine))

    println(s"Engine state result = $engineStateResult")
  }
  playWithEngine()

  def playWithMonoid(): Unit = {
    import com.nat.fptalk.monoid._
    import com.nat.fptalk.monoid.MyBoolean._

    val booleanList = List(MyFalse, MyTrue, MyFalse, MyTrue)
    val andResult = MyBooleanAndMonoid.combineAll(booleanList)
    val orResult = MyBooleanOrMonoid.combineAll(booleanList)

    println(s"andResult= $andResult, orResult = $orResult")
  }

  def playWithMonad(): Unit = {
    import com.nat.fptalk.monad._
    import com.nat.fptalk.monad.OptionA._
    import cats.implicits._

    val aa = for {
      a <- OptionA(1)
      b <- OptionA(a + 1)
    } yield b
    println(aa)
  }
}


object SimpleApp extends MySimpleApp