package tp3;

public class MasDosCarta extends CartaColor {

    public MasDosCarta(String color) {
        super(color, "+2");
    }

    public void aplicarEfecto(GameUNO juego) {
        juego.direccion.avanzar(); // saltamos y obtenemos el penalizado
        juego.repartirCartas(2);
    }

}
