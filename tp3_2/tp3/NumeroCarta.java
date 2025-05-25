package tp3;

public class NumeroCarta extends CartaColor {
    private final String numero;

    public NumeroCarta(String color, String numero) {
        super(color, numero);
        this.numero = numero;
    }

    @Override
    public void aplicarEfecto(juegoUNO juego) {
        // No tiene efecto especial
    }
}
