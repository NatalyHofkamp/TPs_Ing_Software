package tp3;

public class CartaWild extends Carta {

    public CartaWild() {
        super(null, "wild");
    }

    public boolean puedeJugarSobre(Carta cartaSobreMesa) {
        return true;
    }

    public void aplicarEfecto(GameUNO juego) {
        return;
    }

    public CartaColor asignarColor(String colorElegido) {
        return new NumeroCarta(colorElegido, "wild");
    }
}
