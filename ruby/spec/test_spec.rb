require_relative "../lib/contratos"
require_relative "../lib/invariantes"
require_relative "../lib/pila"

describe "before_and_after_each_call" do
  context "Ya en Module" do







    it "La Pila debería lanzar una excepcion de pre condicion al intentar popear sin elementos" do
      pila = Pila.new(3)

      expect { pila.pop()}.
          to raise_error('Error de pre condicion')
    end

    it "La Pila debería lanzar una excepcion de pre condicion al intentar pushear mas elementos de lo que es capaz" do
      pila = Pila.new(3)
      pila.push(1)
      pila.push(2)
      pila.push(3)
      expect { pila.push(4)}.
          to raise_error('Error de pre condicion')
    end

    it "El Guerrero debería lanzar una excepcion de post condicion al implementar el method romper_por_post" do
      guerrero = Guerrero.new(60)
      expect { guerrero.romper_por_post}.
          to raise_error('Error de post condicion')
    end

    it "El Guerrero debería lanzar una excepcion de pre condicion al implementar el method romper_por_pre" do
      guerrero = Guerrero.new(60)
      expect { guerrero.romper_por_pre}.
          to raise_error('Error de pre condicion')
    end

    it "El Guerrero debería poder implementar el method mensaje" do
      guerrero = Guerrero.new(60)
      expect { guerrero.romper_por_pre}.
          to_not raise_error
    end

  end

end