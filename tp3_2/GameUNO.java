import java.util.*;

public class GameUNO {
    public Direccion direccion;
    public Deque<Carta> mazo_total = new LinkedList<>();
    public Carta carta_mesa;

    public GameUNO(List<Jugador> jugadores, Deque<Carta> mazo) {
        if (jugadores.isEmpty()) throw new IllegalArgumentException("Lista vacía");
        Jugador first = jugadores.get(0);
        Jugador prev = first;
        for (int i = 1; i < jugadores.size(); i++) {
            Jugador node = jugadores.get(i);
            prev.next = node;
            node.prev = prev;
            prev = node;
        }
        prev.next = first;
        first.prev = prev;
        this.direccion = new Derecha(first);

        this.carta_mesa = mazo.pop();

    }
    public void repartirCartas(int cant_cartas) {
        for (int i = 0; i < cant_cartas; i++) {
            direccion.getCurrentPlayer().recibirCarta(mazo_total.pop());
        }

    }
    public Jugador getCurrent() {
        return direccion.getCurrentPlayer();
    }
    public Carta topCard() {
        return carta_mesa;
    }

    public void playTurn(Carta carta_elegida) {
        direccion.getCurrentPlayer().jugar(this,carta_elegida);
        direccion.avanzar();
    }
}

