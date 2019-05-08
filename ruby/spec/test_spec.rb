require_relative "../lib/prueba"

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



  end

end

