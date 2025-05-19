public class ReverseCarta extends CartaColor {

    public ReverseCarta(String color) {
        super(color, "reverse");
    }

    @Override
    public void aplicarEfecto(GameUNO juego) {
        juego.ronda.reverseTurnOrder();
    }
}
