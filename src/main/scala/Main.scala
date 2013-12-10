object Main {
  sealed trait Emotion
  case object Love extends Emotion
  case object Hate extends Emotion

  type User = String

  class Conversation(
    affinity: Map[(User, User), Emotion],
    message: Map[(User, Emotion), String]
  ){
    def generate_simple(u1: User, u2: User): Option[String] = {
      for {
        emotion <- affinity.get((u1, u2))
        message <- message.get((u1, emotion))
      } yield template(u1, u2, message)
    }

    def generate_complex(u1: User, u2: User): Option[String] = {
      for {
        emotion1 <- affinity.get((u1, u2))
        emotion2 <- affinity.get((u2, u1))
        if emotion1 == emotion2
        msg <- message.get((u1, inverse(emotion1))).orElse(message.get((u1, emotion1)))
      } yield template(u1, u2, msg)
    }

    private def template(u1: User, u2: User, msg: String) = s"$u2「$msg？」\n$u1「$msg！」"

    private def inverse(e: Emotion) = e match {
      case Love => Hate
      case Hate => Love
    }
  }
}
