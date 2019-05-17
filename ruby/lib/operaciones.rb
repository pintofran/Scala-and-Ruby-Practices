
class Operaciones

  #precondición de dividir
  pre { divisor != 0 }
  #postcondición de dividir
  post { |result| result * divisor == dividendo }
  def dividir(dividendo, divisor)
    dividendo / divisor
  end

  post { |result| result * (divisor+1) == dividendo }
  def dividirYFallar(dividendo, divisor)
    dividendo / divisor
  end

  # este método no se ve afectado por ninguna pre/post condición
  post { |result| result == 5 }
  def sumar(uno, dos)
    return (uno + dos)
  end


  # este método no se ve afectado por ninguna pre/post condición
  post { 2==4 }
  def restar(minuendo, sustraendo)
    minuendo - sustraendo
  end

end