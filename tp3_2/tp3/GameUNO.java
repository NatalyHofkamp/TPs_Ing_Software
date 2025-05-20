package tp3;

import java.util.Deque;
import java.util.LinkedList;
import java.util.List;

public class GameUNO {
    public Direccion direccion;
    public Deque<Carta> mazo_total = new LinkedList<>();
    public Carta carta_mesa;

    public GameUNO(List<Jugador> jugadores, Deque<Carta> mazo_total) {
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
        this.mazo_total = mazo_total;
        this.carta_mesa = mazo_total.pop();

    }
    public GameUNO repartirCartas(int cant_cartas) {
        for (int i = 0; i < cant_cartas; i++) {
            direccion.getCurrentPlayer().recibirCarta(mazo_total.pop());
        }
        return this;

    }
    public Carta getCarta() {
        if (mazo_total.isEmpty()) {
            throw new IllegalStateException("El mazo está vacío");
        }
        return mazo_total.pop();}
    public Jugador getCurrent() {
        return direccion.getCurrentPlayer();
    }
    public Carta topCard() {
        return carta_mesa;
    }

    public GameUNO playTurn(Carta carta_elegida) {

        direccion.getCurrentPlayer().jugar(this,carta_elegida);

        direccion.avanzar();
        return this;
    }
    public void aplicarPenalidadUNO(Jugador jugador) {
        jugador.recibirCarta(this.getCarta());
        jugador.recibirCarta(this.getCarta());
    }

}

