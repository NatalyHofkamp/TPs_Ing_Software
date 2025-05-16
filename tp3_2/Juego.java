import java.util.*;


public class Ronda {
    private CircularList<Jugador> jugadores;
    public Ronda(List<Jugador> jugadores) {
        this.jugadores = new CircularList<>(jugadores);
    }

    public Jugador getCurrentPlayer() {
        return jugadores.getCurrent();
    }

    public void changeCurrentPlayer() {
        jugadores.next();
    }
    public List<Jugador> getJugadores() {
        return jugadores.toList(); // si `CircularList` no lo tiene, implementalo
    }

    public Ronda reverseTurnOrder() {
        jugadores.toggleDirection();
        return this;
    }

    public Ronda skipNextPlayer() {
        jugadores.next(); // salteás uno
        return this;
    }


}


public class Mazo {
    private Deque<Card> mazo = new LinkedList<>();

    public boolean isEmpty() {
        return mazo.isEmpty();
    }
    public Card ultCarta() {
        return mazo.peekLast();
    }
    public Card robar() {
        return mazo.pollFirst(); // null si está vacío
    }
    public void colocarCarta(Card carta) {
        mazo.addLast(carta);
    }

}

public class MazoJugador {
    private Deque<Card> cartas = new LinkedList<>();

    public void recibirCarta(Card nuevaCarta) {
        cartas.addLast(nuevaCarta);
    }

    public void pedirCarta(Mazo mazoTotal) {
        if (!mazoTotal.isEmpty()) {
            Card cartaRobada = mazoTotal.robar();
            recibirCarta(cartaRobada);
        }
    }

    public Card tirarCarta(Mazo mazoMesa, Mazo mazoTotal) {
        Iterator<Card> it = cartas.iterator();
        while (it.hasNext()) {
            Card carta = it.next();
            if (carta.sePuedeJugarSobre(mazoMesa.ultCarta())) {
                it.remove();
                mazoMesa.colocarCarta(carta);
                return carta;
            }
        }
        pedirCarta(mazoTotal);
        return null;
    }

    public boolean estaVacio() {
        return cartas.isEmpty();
    }

    public List<Card> getCartas() {
        return new ArrayList<>(cartas);
    }
}


public class GameUNO {
    public Ronda ronda;
    public Mazo mazo_total;
    public Mazo mazo_mesa;

    public GameUNO(List<Jugador> jugadores, Deque<Card> mazo) {
        this.ronda = new Ronda(jugadores);
        this.mazo_total = new Mazo(mazo);
        this.mazo_mesa = new Mazo(mazo.pop());

    }
    public void repartirCartas(int cant_cartas) {

        for (Jugador jugador : ronda.getJugadores()) {
            for (int i = 0; i < cant_cartas; i++) {
                if (!mazo_total.isEmpty()) {
                    jugador.getMazo().recibirCarta(mazo_total.robar());
                }
            }
        }

    }
    public Jugador getCurrent() {
        return ronda.getCurrentPlayer();
    }
    public Card topCard() {
        return mazo_mesa.ultCarta();
    }

    public void playTurn() {
        Jugador jugador = ronda.getCurrentPlayer();
        jugador.jugar(this);
    }
}


public class Jugador {
    private String nombre;
    private MazoJugador mazo;

    public Jugador(String nombre) {
        this.nombre = nombre;
        this.mazo = new MazoJugador();
    }

    public String getNombre() {
        return nombre;
    }

    public MazoJugador getMazo() {
        return mazo;
    }

    public boolean haGanado() {
        return mazo.estaVacio();
    }

    public void recibirCarta(Card carta) {
        mazo.recibirCarta(carta);
    }

    public void jugar(GameUNO juego) {
        Card cartaJugada = mazo.tirarCarta(juego.mazo_mesa, juego.mazo_total);
        if (cartaJugada != null) {
            cartaJugada.aplicarEfecto(juego);
        }
        juego.ronda.changeCurrentPlayer();
    }
}

