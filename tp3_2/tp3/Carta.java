package tp3;

public abstract class Carta {
    protected String color;  // Puede ser "rojo", "verde", etc. o null si es Wild
    protected String tipo;   // "numero", "+2", "skip", "reverse", "wild"

    public Carta(String color, String tipo) {
        this.color = color;
        this.tipo = tipo;
    }

    public String getColor() {
        return color;
    }

    public String getTipo() {
        return tipo;
    }

    public abstract boolean puedeJugarSobre(Carta cartaSobreMesa);

    public abstract void aplicarEfecto(GameUNO juego);
}

