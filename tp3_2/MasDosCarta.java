public class MasDosCarta extends CartaColor {

    public MasDosCarta(String color) {
        super(color, "+2");
    }

    public void aplicarEfecto(GameUNO juego) {
        juego.direccion.saltearJugador();
        juego.repartirCartas(2);
    }
}
