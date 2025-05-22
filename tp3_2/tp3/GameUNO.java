package tp3;

import java.util.Deque;
import java.util.LinkedList;
import java.util.List;

public class GameUNO {
    public Direccion direccion;
    public Deque<Carta> mazo_total = new LinkedList<>();
    public Carta carta_mesa;
    private int cant_jugadores;
    public GameUNO(List<Jugador> jugadores, Deque<Carta> mazo_total) {
        if (jugadores.isEmpty()) throw new IllegalArgumentException("Lista vacía");
        Jugador first = jugadores.get(0);
        Jugador prev = first;
        this.cant_jugadores = jugadores.size();
        for (int i = 1; i <cant_jugadores; i++) {
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
            getCurrent().recibirCarta(mazo_total.pop());
        }
        return this;

    }
    public int getCantJugadores() { return cant_jugadores;    }

    public Carta getCarta() {
        if (mazo_total.isEmpty()) {
            throw new IllegalStateException("El mazo está vacío");
        }
        return mazo_total.pop();
    }
    public Jugador getCurrent() {
        return direccion.getCurrentPlayer();
    }
    public Carta topCard() {
        return carta_mesa;
    }

    public GameUNO playTurn(Carta carta_elegida) {
        Jugador actual_player = this.getCurrent();
        actual_player.jugar(this,carta_elegida);

        if (actual_player.getMano().size() == 1 && !carta_elegida.cantoUNO()) {
            this.aplicarPenalidadUNO(actual_player);
            System.out.println("Penalidad por no cantar UNO!");
        }
        if (actual_player.haGanado()){
            throw new Error("El jugador haGanado");
        }

        direccion.avanzar();

        return this;
    }


    public void aplicarPenalidadUNO(Jugador jugador) {
        jugador.recibirCarta(this.getCarta());
        jugador.recibirCarta(this.getCarta());
    }

}

