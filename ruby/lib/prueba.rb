class Module

  attr_accessor :antes, :despues, :preCond, :postCond, :esContrato, :ejecutar

  def init
    @esContrato = true
    @new_method = true
    @ejecutar = true
  end


  def before_and_after_each_call(antesNuevo, dpsNuevo)
    init

    self.antes = pushPiola(self.antes, antesNuevo)
    self.despues = pushPiola(self.despues, dpsNuevo)

  end

  def invariant(&bloque)
    init

    bloqueNuevo = proc {

      if (!self.instance_eval(&bloque))
        raise "Error de consistencia de la clase"
      end

    }

    self.despues = pushPiola(self.despues, bloqueNuevo)

  end

  def pre(&bloque)
    @preCond = bloque
  end

  def post(&bloque)
    @postCond = bloque
  end

  def method_added(name)
    if (@esContrato)

      if (@new_method && name != 'initialize'.to_sym)
        @new_method = false

        old_method = instance_method(name)

        preProc=@preCond
        if (preProc.nil?)
          preProc = proc {true}
        end

        postProc=@postCond
        if (postProc.nil?)
          postProc = proc { |*args| true}
        end

        define_method(name) do |*arg|

          if (self.class.ejecutar)

              self.class.ejecutar = false

              if (!self.instance_eval(&preProc))
                raise "Error de pre condicion"
              end
              self.class.ejecutar = true

          end

          self.class.antes.each {|proc|

            if (self.class.ejecutar)
              self.class.ejecutar = false
              self.instance_eval(&proc)
              self.class.ejecutar = true
            end

          }

          var = old_method.bind(self).call(*arg)


          self.class.despues.each {|proc|

            if (self.class.ejecutar)
              self.class.ejecutar = false
              self.instance_eval(&proc)
              self.class.ejecutar = true
            end

          }

          if (self.class.ejecutar)

              self.class.ejecutar = false

              if (!self.instance_exec(var,&postProc))
                raise "Error de post condicion"
              end

              self.class.ejecutar = true

          end

          return var

        end

        @preCond = nil
        @postCond = nil

        @new_method = true
      end

    end
  end

  def pushPiola(lista, elemento)
    if (lista.nil?)
      lista = []
    end
    return lista.push(elemento)
  end


end