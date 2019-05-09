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
      guerrero = Guerrero.new(60)
      expect { raise StandardError, 'Error de consistencia de la clase'}.
          to raise_error('Error de consistencia de la clase')
    end

    it "La Pila debería ejecutar el proc 'after'" do
     pila = Pila.new(3)
     pila.push(1)

      expect { raise StandardError, 'Error de pre condicion'}.
        to_not raise_error( '#<StandardError: Error de pre condicion>')
    end

   it "La Pila debería lanzar una excepcion al intentar agregar un elemento nil" do
     pila = Pila.new(3)
     pila.push(nil)

     expect { raise StandardError, 'Error de post condicion'}.
          to raise_error('Error de post condicion')
   end








  end

end

