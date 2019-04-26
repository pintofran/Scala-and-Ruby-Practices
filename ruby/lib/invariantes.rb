load 'prueba.rb'

class Guerrero < Contrato

  before_and_after_each_call(
      # Bloque Before. Se ejecuta antes de cada mensaje
      proc{ puts 'Primero' },
      # Bloque After. Se ejecuta después de cada mensaje
      proc{ puts 'Dps' }
  )

  attr_accessor :vida, :fuerza

  invariant { puts vida }


  def initialize(vida)
    @vida = vida
  end

  def atacar(otro)
    otro.vida -= fuerza
  end

  def loco
    puts 'loco'
  end

  def mensaje
     puts 'Mensaje'
  end

end

guer = Guerrero.new(50)
guer.mensaje