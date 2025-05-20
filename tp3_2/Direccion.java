public abstract class Direccion {
    protected Jugador current;

    public Direccion(Jugador current) {
        this.current = current;
    }

    public Jugador getCurrentPlayer() {
        return current;
    }

    public abstract void avanzar();

    public void saltearJugador() {
        avanzar();
        avanzar();
    }

    public abstract Direccion invertir();
}

public class Derecha extends Direccion {

    public Derecha(Jugador current) {
        super(current);
    }

    @Override
    public void avanzar() {
        current = current.next;
    }

    @Override
    public Direccion invertir() {
        return new Izquierda(current);
    }
}

public class Izquierda extends Direccion {

    public Izquierda(Jugador current) {
        super(current);
    }

    @Override
    public void avanzar() {
        current = current.prev;
    }

    @Override
    public Direccion invertir() {
        return new Derecha(current);
    }
}



