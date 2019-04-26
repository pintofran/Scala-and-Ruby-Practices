class Contrato

  @@antes = []
  @@despues = []

  def self.before_and_after_each_call(antes,dps)
    @@antes.push([antes,self])
    @@despues.push([dps,self])
  end

  @@new_method = true

  def self.method_added(name)
    if @@new_method
      @@new_method = false

      old_method= self.instance_method(name)

      self.define_method(name) do |*arg|

        @@antes.each { |tuplaProc|

          if(tuplaProc[1] == self.class)
            tuplaProc[0].call
          end

        }
        var = old_method.bind(self).call
        @@despues.each { |tuplaProc|

          if(tuplaProc[1] == self.class)
            tuplaProc[0].call
          end

        }
        return var

      end
      @@new_method = true
    end
  end

end

class MiClase < Contrato

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

end

class MiClase2 < Contrato

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

  def mensaje_1
    puts 'Segunda prueba'
    return 3
  end

end

pp MiClase.new.mensaje_1

pp MiClase2.new.mensaje_1


#MiClase.new.mensaje_2
# Retorna 3 e imprime:
# Entré a un mensaje
# mensaje_2
# Salí de un mensaje