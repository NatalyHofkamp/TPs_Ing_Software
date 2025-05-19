import java.util.List;

public class Ronda {
    private CircularList<Jugador> jugadores;

    public Ronda(List<Jugador> jugadores) {
        this.jugadores = new CircularList<>(jugadores);
    }

    public Jugador getCurrentPlayer() {
        return jugadores.getCurrent();
    }

    public void reverseTurnOrder() {
        jugadores.toggleDirection();
    }

    public void skipNextPlayer() {
        jugadores.next();
    }
}

