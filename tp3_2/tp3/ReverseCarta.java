package tp3;

public class ReverseCarta extends CartaColor {

    public ReverseCarta(String color) {
        super(color, "reverse");
    }

    @Override
    public void aplicarEfecto(GameUNO juego) {
        if (juego.getCantJugadores() == 2) {
            juego.direccion.avanzar();
        } else {
            juego.direccion = juego.direccion.invertir();
        }
    }
}
