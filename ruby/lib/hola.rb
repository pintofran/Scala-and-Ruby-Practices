def pre(&bloque)
  init
  proc_nuevo = proc { |*var|
    unless self.instance_exec(*var, &bloque)
      raise ErrorPrePost, "Error de pre condicion"
    end
  }
  @pre_condicion = proc_nuevo
end

def post(&bloque)
  init
  proc_nuevo = proc { |*var|
    unless self.instance_exec(*var, &bloque)
      raise ErrorPrePost, "Error de post condicion"
    end
  }
  @post_condicion = proc_nuevo
end

parameter_names = old_method.parameters.map {|lista| lista.drop(1).pop}
parameter_values = arg
dictionary = Hash[parameter_names.zip parameter_values]
pp dictionary