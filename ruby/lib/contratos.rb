class Module

  attr_accessor :bloques_before, :bloques_after, :invariants, :pre_condicion, :post_condicion, :es_ejecucion_normal

  def init
    if @bloques_before.nil?
      @bloques_before = BloquesEjecutables.new
    end
    if @bloques_after.nil?
      @bloques_after = BloquesEjecutables.new
    end
    if @invariants.nil?
      @invariants = BloquesEjecutables.new
    end
    @new_method = true
    @es_ejecucion_normal = true
  end

  def before_and_after_each_call(antes_nuevo, despues_nuevo)
    init
    @bloques_before.agregar_bloque(antes_nuevo)
    @bloques_after.agregar_bloque(despues_nuevo)
  end

  def invariant(&bloque)
    init
    @invariants.agregar_bloque_condicion(&bloque)
  end

  def pre(&bloque)
    proc_nuevo = proc { |*var|
      unless self.instance_exec(*var, &bloque)
        raise ErrorPrePost, "Error de pre condicion"
      end
    }
    @pre_condicion = proc_nuevo
  end

  def post(&bloque)
    proc_nuevo = proc { |*var|
      unless self.instance_exec(*var, &bloque)
        raise ErrorPrePost, "Error de post condicion"
      end
    }
    @post_condicion = proc_nuevo
  end

  def ejecutarSinLoop(this,var,&proc)
    if this.class.es_ejecucion_normal
      this.class.es_ejecucion_normal = false
      begin
        this.instance_exec(var,&proc)
      rescue Exception
        this.class.es_ejecucion_normal = true
        raise
      end
      this.class.es_ejecucion_normal = true
    end
  end

  def method_added(name)

    pre_proc=@pre_condicion
    if pre_proc.nil?
      pre_proc = proc {true}
    end

    post_proc=@post_condicion
    if post_proc.nil?
      post_proc = proc { |*args| true}
    end

      if @new_method && name != 'initialize'.to_sym
        @new_method = false

        old_method = instance_method(name)

        #Sobreescribimos el metodo
        define_method(name) do |*arg|

          self.class.ejecutarSinLoop(self,nil,&pre_proc)

          self.class.bloques_before.ejecutar(self)

          var = old_method.bind(self).call(*arg)

          self.class.invariants.ejecutar(self)

          self.class.bloques_after.ejecutar(self)

          self.class.ejecutarSinLoop(self,var,&post_proc)


          return var

        end

        @pre_condicion = nil
        @post_condicion = nil
        @new_method = true

      else
        if @new_method && name == 'initialize'.to_sym
             @new_method = false

             old_method = instance_method(name)

             define_method(name) do |*arg|

               self.class.ejecutarSinLoop(self,nil,&pre_proc)

               var = old_method.bind(self).call(*arg)

               self.class.invariants.ejecutar(self)
               self.class.ejecutarSinLoop(self,var,&post_proc)

               return var

             end
             @pre_condicion = nil
             @post_condicion = nil
              @new_method = true
           end
      end
  end
end

class BloquesEjecutables
  attr_accessor :bloques

  def initialize
    @bloques = []
  end

  def agregar_bloque(nuevo_bloque)

    @bloques.push(nuevo_bloque)

  end

  def agregar_bloque_condicion(&bloque)

    proc_modificado = proc {
      unless self.instance_eval(&bloque)
        raise ErrorConsistencia, "Error de consistencia de la clase"
      end
    }
    @bloques.push(proc_modificado)

  end

  def ejecutar(this)

    @bloques.each {|proc|
      this.class.ejecutarSinLoop(this,nil,&proc)
    }

  end
end

class ErrorPrePost < StandardError
  def initialize(msg="Error")
    super
  end
end

class ErrorConsistencia < StandardError
  def initialize(msg="Error")
    super
  end
end