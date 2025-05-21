package tp3;

public class CartaWild extends Carta {


    public CartaWild() {
        super(null, "wild");
    }

    public boolean puedeJugarSobre(Carta cartaSobreMesa) {
        System.out.println(cartaSobreMesa.getColor());
        return this.color.equals(cartaSobreMesa.getColor());
    }

    public void aplicarEfecto(GameUNO juego) {
        return;
    }

    public void asignarColor(String colorElegido) {
        this.color = colorElegido;
    }
}
