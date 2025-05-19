public abstract class CartaColor extends Carta {

    public CartaColor(String color, String tipo) {
        super(color, tipo);
    }

    public boolean puedeJugarSobre(Carta cartaSobreMesa) {
        return this.color.equals(cartaSobreMesa.getColor())
                || this.tipo.equals(cartaSobreMesa.getTipo());
    }
}
