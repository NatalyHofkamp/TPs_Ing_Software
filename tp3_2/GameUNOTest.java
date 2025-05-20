import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

public class GameUNOTest {

    Jugador jugador1, jugador2, jugador3;
    List<Jugador> jugadores;
    Deque<Carta> mazo;

    @BeforeEach
    void setUp() {
        jugador1 = new Jugador("Alice");
        jugador2 = new Jugador("Bob");
        jugador3 = new Jugador("Carol");

        jugadores = List.of(jugador1, jugador2, jugador3);
        mazo = new LinkedList<>();
    }

    @Test
    void testAvanceDeTurno() {
        mazo.add(new NumeroCarta("rojo", "5"));
        GameUNO juego = new GameUNO(jugadores, mazo);
        juego.repartirCartas(1); // jugador1 recibe 1 carta del mazo

        assertEquals("Alice", juego.getCurrent().getNombre());
        juego.playTurn();  // se salta Bob
        assertEquals("Carol", juego.getCurrent().getNombre());
    }

    @Test
    void testCartaReverseInvierteOrden() {
        mazo.add(new ReverseCarta("verde"));
        GameUNO juego = new GameUNO(jugadores, mazo);
        jugador1.recibirCarta(new ReverseCarta ("verde"));

        juego.playTurn();  // Alice juega reverse
        assertEquals("Alice", juego.getCurrent().getNombre());  // sigue ella por reversa + skip
        juego.playTurn();  // Alice vuelve a jugar
        assertEquals("Carol", juego.getCurrent().getNombre());  // antihorario
    }

    @Test
    void testCartaSkipSaltaJugador() {
        mazo.add(new SkipCarta("rojo"));
        GameUNO juego = new GameUNO(jugadores, mazo);
        jugador1.recibirCarta(new SkipCarta("rojo"));

        juego.playTurn();  // Alice juega skip -> Bob es saltado
        assertEquals("Carol", juego.getCurrent().getNombre());
    }

    @Test
    void testMasDosCartaRobaCartas() {
        mazo.add(new MasDosCarta("azul"));
        mazo.add(new NumeroCarta("azul", "3"));
        mazo.add(new NumeroCarta("azul", "4"));
        GameUNO juego = new GameUNO(jugadores, mazo);
        jugador1.recibirCarta(new MasDosCarta("azul"));

        juego.playTurn();  // Alice juega +2, Bob debe robar
        assertEquals(2, jugador2.mano.size());
    }

    @Test
    void testJugadorRobaSiNoPuedeJugar() {
        mazo.add(new NumeroCarta("rojo", "1"));  // carta en mesa
        mazo.add(new NumeroCarta("verde", "9")); // carta a robar

        GameUNO juego = new GameUNO(jugadores, mazo);
        jugador1.recibirCarta(new NumeroCarta("verde", "3")); // No puede jugar sobre "rojo 1"

        juego.playTurn();
        assertEquals(2, jugador1.mano.size()); // robó una carta porque no pudo jugar
    }
}
