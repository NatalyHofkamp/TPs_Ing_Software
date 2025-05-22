package tp3;

public abstract class Direccion {
    protected Jugador current;

    public Direccion(Jugador current) {
        this.current = current;
    }

    public Jugador getCurrentPlayer() {
        return current;
    }

    public abstract void avanzar();

    public abstract Direccion invertir();
}


class Derecha extends Direccion {

    public Derecha(Jugador current) {
        super(current);
    }

    public void avanzar() {
        current = current.next;
    }


    @Override
    public Direccion invertir() {
        return new Izquierda(current);
    }
}

class Izquierda extends Direccion {

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



