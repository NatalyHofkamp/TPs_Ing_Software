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
        j1 = new Jugador("A");
        j2 = new Jugador("B");
        j3 = new Jugador("C");
        j4 = new Jugador("D");

        juego2 = new GameUNO(List.of(j1, j2), crearMazo());
        juego3 = new GameUNO(List.of(j1, j2, j3), crearMazo());
        juego4 = new GameUNO(List.of(j1, j2, j3, j4), crearMazo());

        repartirCartas(juego2);
        repartirCartas(juego3);
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
        mazo.addLast(new NumeroCarta("Verde", "3"));
        mazo.addLast(new CartaWild());
        mazo.addLast(new MasDosCarta("Verde"));
        return mazo;
    }

    private void repartirCartas(GameUNO juego) {
        Jugador primero = juego.getCurrent();
        Jugador actual = primero;
        do {
            actual.recibirCarta(juego.mazo_total.poll());
            actual = actual.next;
        } while (actual != primero);
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
        actual.recibirCarta(new ReverseCarta("Rojo"));
        juego2.playTurn(new ReverseCarta("Rojo"));
        assertEquals(actual, juego2.getCurrent());
    }

    @Test
    public void testSkipConDosJugadoresVuelveAlMismo() {
        Jugador actual = juego2.getCurrent();
        actual.recibirCarta(new SkipCarta("Rojo"));
        juego2.playTurn(new SkipCarta("Rojo"));
        assertEquals(actual, juego2.getCurrent());
    }

    @Test
    public void testMasDosConDosJugadoresSaltaCorrectamente() {
        Jugador actual = juego2.getCurrent();
        Jugador otro = actual.next;
        int cantidadAntes = otro.getMano().size();
        actual.recibirCarta(new MasDosCarta("Rojo"));
        juego2.playTurn(new MasDosCarta("Rojo"));
        assertEquals(cantidadAntes + 2, otro.getMano().size());
        assertEquals(actual, juego2.getCurrent());
    }

    // 🧪 Tests con 3 jugadores

    @Test
    public void testReverseConTresJugadoresInvierteOrden() {
        Jugador actual = juego3.getCurrent();
        actual.recibirCarta(new ReverseCarta("Rojo"));
        juego3.playTurn(new ReverseCarta("Rojo"));
        assertTrue(juego3.direccion instanceof Izquierda);
        assertEquals(actual.prev, juego3.getCurrent());
    }

    @Test
    public void testSkipConTresJugadoresSaltaUno() {
        Jugador actual = juego3.getCurrent();
        Jugador esperado = actual.next.next;
        actual.recibirCarta(new SkipCarta("Rojo"));
        juego3.playTurn(new SkipCarta("Rojo"));
        assertEquals(esperado, juego3.getCurrent());
    }

    @Test
    public void testMasDosConTresJugadoresSaltaCorrectamente() {
        Jugador actual = juego3.getCurrent();
        Jugador siguiente = actual.next;
        Jugador esperado = siguiente.next;
        int cantidadAntes = siguiente.getMano().size();
        actual.recibirCarta(new MasDosCarta("Rojo"));
        juego3.playTurn(new MasDosCarta("Rojo"));
        assertEquals(cantidadAntes + 2, siguiente.getMano().size());
        assertEquals(esperado, juego3.getCurrent());
    }

    // 🧪 Tests con 4 jugadores

    @Test
    public void testSkipConCuatroJugadoresSaltaCorrectamente() {
        Jugador actual = juego4.getCurrent();
        Jugador esperado = actual.next.next;
        actual.recibirCarta(new SkipCarta("Rojo"));
        juego4.playTurn(new SkipCarta("Rojo"));
        assertEquals(esperado, juego4.getCurrent());
    }

    @Test
    public void testReverseConCuatroJugadoresInvierteDireccion() {
        Jugador actual = juego4.getCurrent();
        actual.recibirCarta(new ReverseCarta("Rojo"));
        juego4.playTurn(new ReverseCarta("Rojo"));
        assertTrue(juego4.direccion instanceof Izquierda);
        assertEquals(actual.prev, juego4.getCurrent());
    }

    @Test
    public void testMasDosConCuatroJugadoresSaltaCorrectamente() {
        Jugador actual = juego4.getCurrent();
        Jugador penalizado = actual.next;
        Jugador esperado = penalizado.next;
        int antes = penalizado.getMano().size();
        actual.recibirCarta(new MasDosCarta("Rojo"));
        juego4.playTurn(new MasDosCarta("Rojo"));
        assertEquals(antes + 2, penalizado.getMano().size());
        assertEquals(esperado, juego4.getCurrent());
    }
}
