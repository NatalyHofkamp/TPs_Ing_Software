package tp3;

public class ReverseCarta extends CartaColor {

    public ReverseCarta(String color) {
        super(color, "reverse");
    }

    @Override
    public void aplicarEfecto(juegoUNO juego) {
        juego.direccion = juego.direccion.invertir();
    }
}
