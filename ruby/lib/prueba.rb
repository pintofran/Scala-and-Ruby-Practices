class Contrato
  attr_accessor :antes

  def self.before_and_after_each_call(param1,param2)
    self.antes= 'hola'
  end

end

class MiClase < Contrato

  before_and_after_each_call(
      # Bloque Before. Se ejecuta antes de cada mensaje
      proc{ puts 'Entré a un mensaje' },
      # Bloque After. Se ejecuta después de cada mensaje
      proc{ puts 'Salí de un mensaje' }
  )

  @new_method = true

  def self.method_added(name)
    if @new_method
      @new_method = false

      old_method = self.instance_method(name)

      self.define_method(name) do |*arg|

        pp self.antes
        old_method.bind(self).call


      end
      @new_method = true
    end
  end

  def mensaje_1
    puts 'Mensaje 1'
    return 5
  end

  def mensaje_2
    puts 'Mensaje 2'
    return 3
  end

  end

MiClase.new.mensaje_1
MiClase.new.mensaje_2
MiClase.instance_methods(false)

#MiClase.new.mensaje_2
# Retorna 3 e imprime:
# Entré a un mensaje
# mensaje_2
# Salí de un mensaje