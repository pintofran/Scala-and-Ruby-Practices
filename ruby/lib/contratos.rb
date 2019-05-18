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
    proc = condicional_proc("Error de consistencia de la clase",ErrorConsistencia,&bloque)
    @invariants.agregar_bloque(proc)
  end

  def pre(&bloque)
    init
    @pre_condicion = condicional_proc('Error de pre condicion',ErrorPrePost,&bloque)
  end

  def post(&bloque)
    init
    @post_condicion = condicional_proc('Error de post condicion',ErrorPrePost,&bloque)
  end


  def condicional_proc(mensaje_error,tipo_error,&bloque)
    return proc { |*var,this|
      unless this.instance_exec(*var, &bloque)
        raise tipo_error, mensaje_error
      end
    }
  end

  def ejecutarSinLoop(this,var,&proc)
    if this.class.es_ejecucion_normal
      this.class.es_ejecucion_normal = false
      begin
        this.instance_exec(*var,this,&proc)
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
      pre_proc = proc { |*args| true}
    end

    post_proc=@post_condicion
    if post_proc.nil?
      post_proc = proc { |*args| true}
    end

    if @new_method
      @new_method = false

      old_method = instance_method(name)

      #Sobreescribimos el metodo
      define_method(name) do |*arg|

        parameter_names = old_method.parameters.map {|lista| lista.drop(1).pop}
        parameter_values = arg
        dictionary = Hash[parameter_names.zip parameter_values]
        dictionary = dictionary.filter {|key,value| key.to_s != "element"}

        TempVars.crear_singleton_accesors_temporales(self,*dictionary.keys.map {|key| key.to_s })
        dictionary.each {|key, value|
          self.send(key.to_s++'=', value)
        }

        self.class.ejecutarSinLoop(self,nil,&pre_proc)

        TempVars.borrar_ultimos_accesors(self)

        unless name == 'initialize'.to_sym
        self.class.bloques_before.ejecutar(self)
        end

        var = old_method.bind(self).call(*arg)

        self.class.invariants.ejecutar(self)

        unless name == 'initialize'.to_sym
        self.class.bloques_after.ejecutar(self)
        end

        TempVars.crear_singleton_accesors_temporales(self,*dictionary.keys.map {|key| key.to_s })

        self.class.ejecutarSinLoop(self,var,&post_proc)

        TempVars.borrar_ultimos_accesors(self)

        return var

      end

      @pre_condicion = nil
      @post_condicion = nil
      @new_method = true
    end
  end
end

class TempVars

  def self.crear_singleton_accesors_temporales(this,*keys)
    @@keys=keys
    this.singleton_class.class_eval { attr_accessor *keys }
  end

  def self.borrar_ultimos_accesors(this)
      @@keys.each { |key|
        this.singleton_class.remove_method((key.to_s++'=').to_sym)
        this.singleton_class.remove_method(key)
      }
    @@keys = []
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

