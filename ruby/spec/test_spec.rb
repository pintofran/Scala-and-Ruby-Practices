require_relative "../lib/prueba"
require_relative "../lib/invariantes"
require_relative "../lib/pila"

describe "before_and_after_each_call" do
  context "Ya en Module" do

    class Testing

      attr_accessor :contador, :se_ejecuto_after, :se_ejecuto_before

      before_and_after_each_call(
          Proc.new{self.se_ejecuto_after = true},
          Proc.new{self.se_ejecuto_before = true})

      def initialize
        self.contador = 0
        self.se_ejecuto_after = false
        self.se_ejecuto_before = false
      end

      def mensaje1
        @contador = @contador + 1
      end
    end

    it "Debería ejecutar el proc 'before' cuando se llama a un metodo" do
      test = Testing.new
      test.mensaje1
      expect(test.se_ejecuto_after).to be true
      expect(test.contador).to be 1
    end

    it "Debería ejecutar el proc 'after' cuando se llama a un metodo" do
      test = Testing.new
      test.mensaje1
      expect(test.se_ejecuto_before).to be true
      expect(test.contador).to be 1
    end

    it "Debería tirar una excepción por la invariante" do

      expect { Pila.new(-1)}.
          to raise_error('Error de consistencia de la clase')
    end

    it "La Pila debería poder pushear un elemento" do
     pila = Pila.new(3)

      expect { pila.push(1)}.
        to_not raise_error
    end

    it "La Pila deberia lanzar una excepcion de post condicion cuando al pushear un elemento su altura sea 0" do
      pila = Pila.new(3)
      pila.define_singleton_method('height') { 0 }

      expect { pila.push(1)}.
          to raise_error('Error de post condicion')
    end

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








  end

end

