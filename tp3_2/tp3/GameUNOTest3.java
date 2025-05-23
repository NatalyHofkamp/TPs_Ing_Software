package tp3;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import java.util.*;
import static org.junit.jupiter.api.Assertions.*;

public class GameUNOTest3 {

    GameUNO juego2, juego3, juego4;
    Jugador j2A, j2B, j3A, j3B, j3C, j4A, j4B, j4C, j4D;

    // Cartas reutilizables
    Carta rojo5 = new NumeroCarta("Rojo", "5");
    Carta rojo3 = new NumeroCarta("Rojo", "3");
    Carta rojo7 = new NumeroCarta("Rojo", "7");
    Carta rojo2 = new NumeroCarta("Rojo", "2");

    Carta azul4 = new NumeroCarta("Azul", "4");
    Carta azul3 = new NumeroCarta("Azul", "3");
    Carta verde2 = new NumeroCarta("Verde", "2");

    Carta masDosRojo = new MasDosCarta("Rojo");
    Carta masDosVerde = new MasDosCarta("Verde");

    Carta skipRojo = new SkipCarta("Rojo");
    Carta skipVerde = new SkipCarta("Verde");
    Carta skipAmarillo = new SkipCarta("Amarillo");

    Carta reverseRojo = new ReverseCarta("Rojo");
    Carta reverseAzul = new ReverseCarta("Azul");
    Carta reverseVerde = new ReverseCarta("Verde");

    Carta wild = new CartaWild();

    @BeforeEach
    public void setup() {
        // juego2
        j2A = new Jugador("A");
        j2B = new Jugador("B");
        juego2 = new GameUNO(List.of(j2A, j2B), crearMazo());
        repartirCartas(juego2, 2);

        // juego3
        j3A = new Jugador("A");
        j3B = new Jugador("B");
        j3C = new Jugador("C");
        juego3 = new GameUNO(List.of(j3A, j3B, j3C), crearMazo());
        repartirCartas(juego3, 2);

        // juego4
        j4A = new Jugador("A");
        j4B = new Jugador("B");
        j4C = new Jugador("C");
        j4D = new Jugador("D");
        juego4 = new GameUNO(List.of(j4A, j4B, j4C, j4D), crearMazo());
        repartirCartas(juego4, 2);
    }

    private Deque<Carta> crearMazo() {
        return new LinkedList<>(List.of(
                rojo5, azul4, rojo7, masDosRojo, skipVerde, reverseAzul,
                rojo3, wild, masDosVerde, rojo5, skipAmarillo, reverseVerde,
                rojo2, verde2, azul3
        ));
    }

    private void repartirCartas(GameUNO juego, int cartasPorJugador) {
        Jugador actual = juego.getCurrent();
        for (int i = 0; i < cartasPorJugador; i++) {
            Jugador j = actual;
            do {
                j.recibirCarta(juego.mazo_total.poll());
                j = j.next;
            } while (j != actual);
        }
    }

    @Test
    public void testJugarCartaValidaActualizaMesaYMano() {
        Jugador actual = juego3.getCurrent();
        actual.recibirCarta(rojo7);
        assertTrue(rojo7.puedeJugarSobre(juego3.topCard()));
        juego3.playTurn(rojo7);
        assertEquals(rojo7, juego3.topCard());
        assertFalse(actual.getMano().contains(rojo7));
    }

    @Test
    public void testPenalidadPorNoCantarUNO() {
        Jugador actual = juego3.getCurrent();
        actual.getMano().clear();
        actual.recibirCarta(rojo7);
        actual.recibirCarta(rojo2);
        juego3.playTurn(rojo7);
        assertEquals(3, actual.getMano().size()); // penaliza con 2
    }

    @Test
    public void testJugarUltimaCartaEsVictoria() {
        Jugador actual = juego3.getCurrent();
        actual.getMano().clear();
        actual.recibirCarta(rojo2);
        juego3.playTurn(rojo2); // Juega la última carta y gana
        Jugador siguiente = juego3.getCurrent();
        Carta otraCarta = new NumeroCarta("Rojo", "3");
        siguiente.recibirCarta(otraCarta);
        assertThrows(IllegalStateException.class, () -> juego3.playTurn(otraCarta));
    }


    @Test
    public void testWildCardCambiaColor() {
        Jugador actual = juego3.getCurrent();
        actual.recibirCarta(wild);
        ((CartaWild) wild).asignarColor("Rojo");
        juego3.playTurn(wild);
        assertEquals("Rojo", juego3.topCard().getColor());
    }

    @Test
    public void testSkipConDosJugadoresVuelveAlMismo() {
        j2A.recibirCarta(skipRojo);
        juego2.playTurn(skipRojo);
        assertEquals(j2A, juego2.getCurrent());
    }

    @Test
    public void testMasDosConDosJugadoresSaltaCorrectamente() {
        int cantidadAntes = j2B.getMano().size();
        j2A.recibirCarta(masDosRojo);
        juego2.playTurn(masDosRojo.uno());
        assertEquals(cantidadAntes + 2, j2B.getMano().size());
        assertEquals(j2A, juego2.getCurrent());
    }

    @Test
    public void testReverseConTresJugadoresInvierteOrden() {
        j3A.recibirCarta(reverseRojo);
        juego3.playTurn(reverseRojo);
        assertTrue(juego3.direccion instanceof Izquierda);
        assertEquals(j3A.prev, juego3.getCurrent());
    }

    @Test
    public void testSkipConTresJugadoresSaltaUno() {
        j3A.recibirCarta(skipRojo);
        Jugador esperado = j3A.next.next;
        juego3.playTurn(skipRojo);
        assertEquals(esperado, juego3.getCurrent());
    }

    @Test
    public void testMasDosConTresJugadoresSaltaCorrectamente() {
        j3A.recibirCarta(masDosRojo);
        int antes = j3B.getMano().size();
        juego3.playTurn(masDosRojo);
        assertEquals(antes + 2, j3B.getMano().size());
        assertEquals(j3C, juego3.getCurrent());
    }

    @Test
    public void testSkipConCuatroJugadoresSaltaCorrectamente() {
        j4A.recibirCarta(skipRojo);
        Jugador esperado = j4A.next.next;
        juego4.playTurn(skipRojo);
        assertEquals(esperado, juego4.getCurrent());
    }

    @Test
    public void testReverseConCuatroJugadoresInvierteDireccion() {
        j4A.recibirCarta(reverseRojo);
        juego4.playTurn(reverseRojo);
        assertTrue(juego4.direccion instanceof Izquierda);
        assertEquals(j4A.prev, juego4.getCurrent());
    }

    @Test
    public void testMasDosConCuatroJugadoresSaltaCorrectamente() {
        j4A.recibirCarta(masDosRojo);
        int antes = j4B.getMano().size();
        juego4.playTurn(masDosRojo);
        assertEquals(antes + 2, j4B.getMano().size());
        assertEquals(j4C, juego4.getCurrent());
    }

    @Test
    public void testSecuenciaDeTurnosConCartasValidas() {
        j3A.recibirCarta(rojo7);
        juego3.playTurn(rojo7);
        assertEquals(rojo7, juego3.topCard());
        assertEquals(j3B, juego3.getCurrent());

        j3B.recibirCarta(rojo3);
        juego3.playTurn(rojo3);
        assertEquals(rojo3, juego3.topCard());
        assertEquals(j3C, juego3.getCurrent());

        j3C.recibirCarta(rojo5);
        juego3.playTurn(rojo5);
        assertEquals(rojo5, juego3.topCard());
        assertEquals(j3A, juego3.getCurrent());
    }

    @Test
    public void testCantaUnoYGanaYNoSePuedeSeguirJugando() {
        j2A.getMano().clear();
        j2A.recibirCarta(rojo3);
        j2A.recibirCarta(rojo5);
        j2B.recibirCarta(rojo7);

        juego2.playTurn(rojo3.uno());
        assertEquals(1, j2A.getMano().size());

        juego2.playTurn(rojo7);
        juego2.playTurn(rojo5); // j2A gana

        assertEquals(0, j2A.getMano().size());

        j2B.recibirCarta(azul4);
        assertThrows(IllegalStateException.class, () -> juego2.playTurn(azul4));
    }
}

