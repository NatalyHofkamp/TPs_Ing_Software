package tp3;

public class SkipCarta extends CartaColor {

    public SkipCarta(String color) {
        super(color, "skip");
    }

    @Override
    public void aplicarEfecto(GameUNO juego) {
        juego.direccion.avanzar();
    }
}
