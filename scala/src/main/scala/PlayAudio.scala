object PlayAudio extends App {
  val sariaSong = "2x(A B 3x(F G 2x(A))) F B E"

  AudioPlayer.reproducir(sariaSong)
}

object PlayOtherOne extends App{
  AudioPlayer.reproducir("2x(A B 3x(F G 2x(A))) F B E")
}
