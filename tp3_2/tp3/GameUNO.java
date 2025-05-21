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
        Jugador actual_player = this.getCurrent();
        verificarCantoUNO(actual_player);
        actual_player.jugar(this,carta_elegida);
        if (direccion.getCurrentPlayer().haGanado()){
            throw new Error("El jugador haGanado");
        }

        direccion.avanzar();

        return this;
    }

    public GameUNO juegoYcanto(Carta carta_elegida) {
        Jugador actual_player = this.getCurrent();
        playTurn(carta_elegida);
       actual_player.cantarUNO();
       return this;
    }
    private void verificarCantoUNO(Jugador jugador) {
        if (jugador.getMano().size() == 1 && !jugador.haCantado()) {
            this.aplicarPenalidadUNO(jugador);
        }

    }
    public void aplicarPenalidadUNO(Jugador jugador) {
        jugador.recibirCarta(this.getCarta());
        jugador.recibirCarta(this.getCarta());
    }

}

