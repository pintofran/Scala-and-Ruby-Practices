class Class

  @initialized = false
  @new_method = true

  def initialize
    @antes = Array.new
    @despues = Array.new
  end

  def before_and_after_each_call(antes,dps)
    if !@initialized
      initialize
      @initialized = true
    end
    @antes << antes
    @despues << dps
    pp @antes
  end

  def method_added(name)
    if @new_method
      @new_method = false
      pp name
      pp self

      old_method= instance_method(name)


      @new_method = true
    end
  end

end

class MiClase

  before_and_after_each_call(
      # Bloque Before. Se ejecuta antes de cada mensaje
      proc{ puts 'Primero aca' },
      # Bloque After. Se ejecuta después de cada mensaje
      proc{ puts 'Dps aca' }
  )

  def mensaje_1
    puts 'Primer prueba'
    return 5
  end


  def self.mensaje_9
    puts 'pise el mensaje'
  end
end

class MiClase2

  before_and_after_each_call(
      # Bloque Before. Se ejecuta antes de cada mensaje
      proc{ puts 'Aca otra cosa' },
      # Bloque After. Se ejecuta después de cada mensaje
      proc{ puts 'Wiiii' }
  )

  before_and_after_each_call(
      # Bloque Before. Se ejecuta antes de cada mensaje
      proc{ puts 'Re loco' },
      # Bloque After. Se ejecuta después de cada mensaje
      proc{ puts 'Seee' }
  )

  before_and_after_each_call(
      # Bloque Before. Se ejecuta antes de cada mensaje
      proc{ puts 'Una vez masss' },
      # Bloque After. Se ejecuta después de cada mensaje
      proc{ puts 'Daleeeeeeee' }
  )

  def mensaje_1
    puts 'Segunda prueba'
    return 3
  end

  def mensaje_2
    puts 'Tercera prueba'
    return 10
  end

end

pp MiClase.new.mensaje_1

pp MiClase2.new.mensaje_1

pp MiClase2.new.mensaje_2

pp MiClase.new.class.mensaje_9

#MiClase.new.mensaje_2
# Retorna 3 e imprime:
# Entré a un mensaje
# mensaje_2
# Salí de un mensaje