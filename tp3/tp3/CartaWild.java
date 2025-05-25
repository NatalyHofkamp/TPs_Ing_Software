package tp3;

public class CartaWild extends Carta {


    public CartaWild() {
        super(null, "wild");
    }

    public boolean puedeJugarSobre(Carta cartaSobreMesa) {
        return this.color.equals(cartaSobreMesa.getColor());
    }

    public void aplicarEfecto(juegoUNO juego) {
        return;
    }

    public void asignarColor(String colorElegido) {
        this.color = colorElegido;
    }
}
