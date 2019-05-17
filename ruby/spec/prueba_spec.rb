require_relative "../lib/pila"
require_relative "../lib/invariantes"
require_relative "../lib/operaciones"

class Testing

  attr_accessor :contador, :se_ejecuto_after, :se_ejecuto_before

  before_and_after_each_call(
      Proc.new {self.se_ejecuto_after = true},
      Proc.new {self.se_ejecuto_before = true})

  def initialize
    self.contador = 0
    self.se_ejecuto_after = false
    self.se_ejecuto_before = false
  end

  def mensaje1
    @contador = @contador + 1
  end
end

context 'hola' do


describe 'before_and_after' do

  it "Deberia ejecutar el proc 'before' cuando se llama a un metodo" do
    test = Testing.new
    test.mensaje1
    expect(test.se_ejecuto_after).to be true
    expect(test.contador).to be 1
  end

  it "Deberia ejecutar el proc 'after' cuando se llama a un metodo" do
    test = Testing.new
    test.mensaje1
    expect(test.se_ejecuto_before).to be true
    expect(test.contador).to be 1
  end

  it "El flag no deberia ser falso luego de ejecutar el mensaje" do
    test = Testing.new
    test.mensaje1
    expect(test.se_ejecuto_before).not_to be false
  end

end

describe 'Pila comportamiento normal' do

  it "La Pila deberia poder pushear un elemento" do
    pila = Pila.new(3)

    expect {pila.push(1)}.
        to_not raise_error
  end

end

describe 'Pila falla por invariante' do

    it "Deberia tirar una excepcion por la invariante" do

      expect {Pila.new(-1)}.
          to raise_error(ErrorConsistencia, 'Error de consistencia de la clase')
    end
  end

  describe 'Pila fallando por pre' do

    it "El Guerrero deberia lanzar una excepcion de pre condicion al implementar el method romper_por_pre" do
      guerrero = Guerrero.new(60)
      expect {guerrero.romper_por_pre}.
          to raise_error('Error de pre condicion')
    end

    it "El Guerrero deberia lanzar una excepcion de pre condicion al implementar el method romper_por_pre" do
      guerrero = Guerrero.new(60)
      expect {guerrero.romper_por_post}.
          to raise_error('Error de post condicion')
    end

    it "La Pila deberia lanzar una excepcion de pre condicion al intentar pushear mas elementos de lo que es capaz" do
      pila = Pila.new(3)
      pila.push(1)
      pila.push(2)
      pila.push(3)
      expect {pila.push(4)}.
          to raise_error(ErrorPrePost, 'Error de pre condicion')
    end

  end

  describe 'Fallar por post' do

    it "La Pila deberia lanzar una excepcion de post condicion cuando al pushear un elemento su altura sea 0" do
      pila = Pila.new(3)
      pila.define_singleton_method('height') {0}

      expect {pila.push(1)}.
          to raise_error('Error de post condicion')
    end
  end

describe 'Post con parametros' do

  it "No deberia fallar" do
    operacion = Operaciones.new

    expect(operacion.dividir(10,2)).to eq(5)
  end

  it "Deberia Fallar" do
    operacion = Operaciones.new

    expect {operacion.dividirYFallar(10,2)}.
        to raise_error(ErrorPrePost,'Error de post condicion')
  end



  it "No Deberia Fallar" do
    operacion = Operaciones.new

    expect {operacion.sumar(3,2)}.
        not_to raise_error
  end


  it "Deberia Fallar" do
    operacion = Operaciones.new

    expect {operacion.sumar(4,2)}.
        to raise_error(ErrorPrePost,'Error de post condicion')
  end





end
  describe 'hola' do
    it "Deberia Fallar" do
      operacion = Operaciones.new

      expect {operacion.sumar(5,5)}.
          to raise_error(ErrorPrePost,'Error de post condicion')
    end
  end
end