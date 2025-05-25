package tp3;

public class SkipCarta extends CartaColor {

    public SkipCarta(String color) {
        super(color, "skip");
    }

    @Override
    public void aplicarEfecto(juegoUNO juego) {
        juego.direccion.avanzar();
    }
}
