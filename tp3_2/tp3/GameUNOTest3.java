package tp3;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

import java.util.*;

public class GameUNOTest3 {

    GameUNO juego2, juego3, juego4;
    Jugador j1, j2, j3, j4;

    @BeforeEach
    public void setup() {
        // juego2
        Jugador a2 = new Jugador("A");
        Jugador b2 = new Jugador("B");
        juego2 = new GameUNO(List.of(a2, b2), crearMazo());
        repartirCartas(juego2);

        // juego3
        Jugador a3 = new Jugador("A");
        Jugador b3 = new Jugador("B");
        Jugador c3 = new Jugador("C");
        juego3 = new GameUNO(List.of(a3, b3, c3), crearMazo());
        repartirCartas(juego3);

        // juego4
        Jugador a4 = new Jugador("A");
        Jugador b4 = new Jugador("B");
        Jugador c4 = new Jugador("C");
        Jugador d4 = new Jugador("D");
        juego4 = new GameUNO(List.of(a4, b4, c4, d4), crearMazo());
        repartirCartas(juego4);
    }


    private Deque<Carta> crearMazo() {
        Deque<Carta> mazo = new LinkedList<>();
        mazo.addLast(new NumeroCarta("Rojo", "5"));
        mazo.addLast(new NumeroCarta("Azul", "4"));
        mazo.addLast(new NumeroCarta("Rojo", "7"));

        mazo.addLast(new MasDosCarta("Rojo"));
        mazo.addLast(new SkipCarta("Verde"));
        mazo.addLast(new ReverseCarta("Azul"));

        mazo.addLast(new NumeroCarta("Rojo", "3"));

        mazo.addLast(new CartaWild());
        mazo.addLast(new MasDosCarta("Verde"));

        mazo.addLast(new NumeroCarta("Rojo", "5"));

        mazo.addLast(new SkipCarta("Amarillo"));
        mazo.addLast(new ReverseCarta("Verde"));

        mazo.addLast(new NumeroCarta("Rojo", "1"));
        mazo.addLast(new NumeroCarta("Verde", "2"));
        mazo.addLast(new NumeroCarta("Azul", "3"));



        return mazo;
    }

    private void repartirCartas(GameUNO juego) {
        Jugador primero = juego.getCurrent();

        for (int i = 0; i < 3; i++) {
            Jugador actual = primero;
            do {
                actual.recibirCarta(juego.mazo_total.poll());
                actual = actual.next;
            } while (actual != primero);
        }
    }


    // 🧪 Tests generales
    @Test
    public void testJugarCartaValidaActualizaMesaYMano() {
        Jugador actual = juego3.getCurrent();
        Carta carta = new NumeroCarta("Rojo", "7");
        actual.recibirCarta(carta);
        assertTrue(carta.puedeJugarSobre(juego3.topCard()));
        juego3.playTurn(carta);
        assertEquals(carta, juego3.topCard());
        assertFalse(actual.getMano().contains(carta));
    }

    @Test
    public void testJugarCartaNoEnManoLanza() {
        Carta carta = new NumeroCarta("Verde", "3");
        assertThrows(IllegalArgumentException.class, () -> juego3.playTurn(carta));
    }

    @Test
    public void testPenalidadPorNoCantarUNO() {
        Jugador actual = juego3.getCurrent();
        actual.getMano().clear();
        actual.recibirCarta(new NumeroCarta("Rojo", "7"));
        actual.recibirCarta(new NumeroCarta("Rojo", "2"));
        juego3.playTurn(actual.getMano().peek());
        assertEquals(3, actual.getMano().size()); // Penaliza con 2
    }

    @Test
    public void testJugarUltimaCartaEsVictoria() {
        Jugador actual = juego3.getCurrent();
        actual.getMano().clear();
        Carta ultima = new NumeroCarta("Rojo", "2");
        actual.recibirCarta(ultima);
        Error error = assertThrows(Error.class, () -> juego3.playTurn(ultima));
        assertEquals("El jugador haGanado", error.getMessage());
    }

    @Test
    public void testWildCardCambiaColor() {
        Jugador actual = juego3.getCurrent();
        CartaWild wild = new CartaWild();
        actual.recibirCarta(wild);
        wild.asignarColor("Rojo");
        juego3.playTurn(wild);
        assertEquals("Rojo", juego3.topCard().getColor());
    }

    // 🧪 Tests con 2 jugadores

    @Test
    public void testReverseConDosJugadoresVuelveAlMismo() {
        Jugador actual = juego2.getCurrent();
        ReverseCarta invierte = new ReverseCarta("Rojo");
        actual.recibirCarta(invierte);
        juego2.playTurn(invierte);
        assertEquals(actual, juego2.getCurrent());
    }

    @Test
    public void testSkipConDosJugadoresVuelveAlMismo() {
        Jugador actual = juego2.getCurrent();
        SkipCarta skipRojo = new SkipCarta("Rojo");
        actual.recibirCarta(skipRojo);
        juego2.playTurn(skipRojo);
        assertEquals(actual, juego2.getCurrent());
    }

    @Test
    public void testMasDosConDosJugadoresSaltaCorrectamente() {
        Jugador actual = juego2.getCurrent();
        Jugador otro = actual.next;
        int cantidadAntes = otro.getMano().size();
        MasDosCarta masDosCarta = new MasDosCarta("Rojo");
        actual.recibirCarta(masDosCarta);
        juego2.playTurn(masDosCarta.uno());
        assertEquals(cantidadAntes + 2, otro.getMano().size());
        assertEquals(actual, juego2.getCurrent());
    }

    // 🧪 Tests con 3 jugadores

    @Test
    public void testReverseConTresJugadoresInvierteOrden() {
        Jugador actual = juego3.getCurrent();
        ReverseCarta invierte = new ReverseCarta("Rojo");
        actual.recibirCarta(invierte);
        juego3.playTurn(invierte);
        assertTrue(juego3.direccion instanceof Izquierda);
        assertEquals(actual.prev, juego3.getCurrent());
    }

    @Test
    public void testSkipConTresJugadoresSaltaUno() {
        Jugador actual = juego3.getCurrent();
        Jugador esperado = actual.next.next;
        SkipCarta skipRojo = new SkipCarta("Rojo");
        actual.recibirCarta(skipRojo);
        juego3.playTurn(skipRojo);
        assertEquals(esperado, juego3.getCurrent());
    }

    @Test
    public void testMasDosConTresJugadoresSaltaCorrectamente() {
        Jugador actual = juego3.getCurrent();
        Jugador siguiente = actual.next;
        Jugador esperado = siguiente.next;
        int cantidadAntes = siguiente.getMano().size();
        MasDosCarta masRojo = new MasDosCarta("Rojo");
        actual.recibirCarta(masRojo);
        juego3.playTurn(masRojo);
        assertEquals(cantidadAntes + 2, siguiente.getMano().size());
        assertEquals(esperado, juego3.getCurrent());
    }

    // 🧪 Tests con 4 jugadores

    @Test
    public void testSkipConCuatroJugadoresSaltaCorrectamente() {
        Jugador actual = juego4.getCurrent();
        Jugador esperado = actual.next.next;
        SkipCarta skipRojo = new SkipCarta("Rojo");
        actual.recibirCarta(skipRojo);
        juego4.playTurn(skipRojo);
        assertEquals(esperado, juego4.getCurrent());
    }

    @Test
    public void testReverseConCuatroJugadoresInvierteDireccion() {
        Jugador actual = juego4.getCurrent();
        ReverseCarta invierte = new ReverseCarta("Rojo");
        actual.recibirCarta(invierte);
        juego4.playTurn(invierte);
        assertTrue(juego4.direccion instanceof Izquierda);
        assertEquals(actual.prev, juego4.getCurrent());
    }

    @Test
    public void testMasDosConCuatroJugadoresSaltaCorrectamente() {
        Jugador actual = juego4.getCurrent();
        Jugador penalizado = actual.next;
        Jugador esperado = penalizado.next;
        int antes = penalizado.getMano().size();
        MasDosCarta masDosCarta = new MasDosCarta("Rojo");
        actual.recibirCarta(masDosCarta);
        juego4.playTurn(masDosCarta);
        assertEquals(antes + 2, penalizado.getMano().size());
        assertEquals(esperado, juego4.getCurrent());
    }
}
