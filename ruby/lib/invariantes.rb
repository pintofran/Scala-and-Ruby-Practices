load 'prueba.rb'

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
     puts 'Mensaje'
     return 5
  end

  post { |result| result == 5 }
  def mensaje3
    puts 'Mensaje'
    return 5
  end

end

class GuerreroDos

  before_and_after_each_call(
      # Bloque Before. Se ejecuta antes de cada mensaje
      proc{ puts 'Otro guerrero' },
      # Bloque After. Se ejecuta después de cada mensaje
      proc{ puts 'Dps de otro guerrero' }
  )

  attr_accessor :vida, :fuerza

  invariant { vida > 70 }
  invariant { fuerza > 20 }

  def initialize(vida)
    @vida = vida
    @fuerza = 21
  end

  def atacar(otro)
    otro.vida -= fuerza
  end

  pre { vida > 30 }
  post { vida > 30 }
  def mensaje
    puts 'Mensaje'
    @vida= vida-20
    return 5
  end

  post { |result| result == 5 }
  def mensaje3
    puts 'Mensaje'
    return 5
  end

end

guer = Guerrero.new(52)
guer.mensaje
guer.mensaje3

guer2 = GuerreroDos.new(60)
guer2.mensaje
guer2.mensaje