package week2

abstract class Simulation {

  type Action = () => Unit

  private var curtime = 0
  def currentTime: Int = curtime

  private var agenda: Agenda = List()
  case class Event(time: Int, action: Action)

  private type Agenda = List[Event]

  private def insert(agenda: Agenda, event: Event): Agenda = agenda match {
    case first :: rest if first.time <= event.time =>
      first :: insert(rest, event)
    case _ =>
      event :: agenda
  }

  def afterDelay(delay: Int)(block: => Unit): Unit = {
    val event = Event(currentTime + delay, () => block)
    agenda = insert(agenda, event)
  }

  private def loop(): Unit = agenda match {
    case first :: rest =>
      agenda = rest
      curtime = first.time
      first.action()
      loop()
    case Nil =>
  }

  def run(): Unit = {
    afterDelay(0) {
      println("*** simulation started, time = "+currentTime+" ***")
    }
    loop()
  }
}
