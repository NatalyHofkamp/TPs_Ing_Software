package tp3;

public class MasDosCarta extends CartaColor {

    public MasDosCarta(String color) {
        super(color, "+2");
    }

    public void aplicarEfecto(juegoUNO juego) {
        juego.direccion.avanzar();
        juego.repartirCartas(2);
    }

}
