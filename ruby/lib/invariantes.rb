load 'prueba.rb'

class Guerrero

  before_and_after_each_call(
      # Bloque Before. Se ejecuta antes de cada mensaje
      proc{ puts vida },
      # Bloque After. Se ejecuta después de cada mensaje
      proc{ puts 'Dps' }
  )

  attr_accessor :vida, :fuerza

  invariant { vida > 20 }
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
     puts 'Mensaje'
     return 5
  end

  def mensaje2
    puts 'Mensaje'
    return 6
  end

  post { |result| result == 5 }
  def mensaje3
    puts 'Mensaje'
    return 5
  end

end

guer = Guerrero.new(51)
guer.mensaje2
guer.mensaje
guer.mensaje3