require_relative  'prueba.rb'

class Guerrero

  before_and_after_each_call(
      # Bloque Before. Se ejecuta antes de cada mensaje
      proc{ puts vida },
      # Bloque After. Se ejecuta después de cada mensaje
      proc{ puts 'Dps' }
  )

  attr_accessor :vida, :fuerza

  invariant { vida > 51 }
  invariant { fuerza > 20 }

  def initialize(vida)
    @vida = vida
    @fuerza = 21
  end

  def atacar(otro)
    otro.vida -= fuerza
  end

  pre { vida > 50 }
  post { |result| result == 5 }
  def mensaje
     puts 'Mensaje1'
     return 5
  end


  post { |result| result == 5 }
  def mensaje3
    puts 'Mensaje3'
    return 5
  end

end

class GuerreroDos

  before_and_after_each_call(
      # Bloque Before. Se ejecuta antes de cada mensaje
      proc{ puts 'Otro guerrero' },
      # Bloque After. Se ejecuta después de cada mensaje
      proc{ puts 'Mi fuerza es: ' + fuerza.to_s }
  )

  attr_accessor :vida, :fuerza

  invariant { vida > 30 }
  invariant { fuerza > 20 }

  def initialize(vida)
    @vida = vida
    @fuerza = 21
  end


  def atacar(otro)

    otro.vida -= fuerza
  end

  pre { vida > 20 }
  post { vida > 70 }
  def mensaje
    vida= 5
    puts 'Mensaje'
    return 5
  end

  def romper
    @vida = 0
  end

  pre { vida == 51 }
  post { vida == 51 }
  def mensaje3
    puts 'Mensaje'
    return 5
  end

end