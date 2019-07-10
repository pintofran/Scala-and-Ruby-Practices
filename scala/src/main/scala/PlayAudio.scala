object PlayAudio extends App {
  val fc = "4AM1/8 5C1/8 5C#1/8 5C#1/8 5D#1/8 5C1/8 4A#1/8 4G#1/2 - 4A#1/8 4A#1/8 5C1/4 5C#1/8 4A#1/4 4G#1/2 5G#1/4 5G#1/4 5D#1/2"

  val lista = ParserCombinator.parserDeMelodia.parse(fc).getResult()
  AudioPlayer.reproducir(lista)
}
