package tp3;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.*;

public class GameUNOTest2 {

    GameUNO juego;
    Deque<Carta> mazo;

    @BeforeEach
    public void setup() {
        Jugador jugador1 = new Jugador("Alice");
        Jugador jugador2 = new Jugador("Bob");
        Jugador jugador3 = new Jugador("Charlie");

        // Mazo de prueba
        mazo = new LinkedList<>();
        mazo.addLast(new NumeroCarta("Rojo", "5"));
        mazo.addLast(new NumeroCarta("Rojo", "7"));
        mazo.addLast(new MasDosCarta("Rojo"));
        mazo.addLast(new SkipCarta("Verde"));
        mazo.addLast(new ReverseCarta("Azul"));
        mazo.addLast(new NumeroCarta("Verde", "3"));
        mazo.addLast(new NumeroCarta("Rojo", "2"));
        mazo.addLast(new NumeroCarta("Azul", "4"));
        mazo.addLast(new MasDosCarta("Verde"));
        mazo.addLast(new NumeroCarta("Azul", "1"));
        mazo.addLast(new SkipCarta("Rojo"));
        mazo.addLast(new MasDosCarta("Azul"));

        List<Jugador> jugadores = List.of(jugador1, jugador2, jugador3);
        juego = new GameUNO(jugadores, mazo);


        for (int j = 0; j < jugadores.size(); j++) {
            juego.repartirCartas(3);  // da una carta al current
            juego.direccion.avanzar(); // pasa al siguiente
        }

    }

    @Test
    public void testTurnoAvanzaCorrectamente() {
        Jugador primero = juego.getCurrent();
        juego.direccion.avanzar();
        Jugador segundo = juego.getCurrent();
        juego.direccion.avanzar();
        Jugador tercero = juego.getCurrent();



        assertEquals(primero.next.next.getNombre(), tercero.getNombre());
    }

    @Test
    public void testReverseCambiaDireccion() {
        assertTrue(juego.direccion instanceof Derecha);

        Jugador actual = juego.getCurrent();
        Carta reverse = new ReverseCarta("Rojo");
        actual.recibirCarta(reverse);

        juego.playTurn(reverse);

        assertTrue(juego.direccion instanceof Izquierda);
        assertEquals(actual.prev, juego.getCurrent());
    }

    @Test
    public void testMasDosSalteaYSumaCartas() {
        Jugador actual = juego.getCurrent();
        Jugador siguiente = actual.next;
        Jugador siguienteDelSiguiente = siguiente.next;

        int cantidadAntes = siguiente.mano.size();

        Carta masDosRojo = new MasDosCarta("Rojo");
        actual.recibirCarta(masDosRojo);


        juego.playTurn(masDosRojo);


        assertEquals(cantidadAntes + 2, siguiente.mano.size());
        assertEquals(siguienteDelSiguiente.getNombre(), juego.getCurrent().getNombre());
    }




    @Test
    public void testSkipSalteaJugadorSiguiente() {
        Jugador actual = juego.getCurrent();
        Jugador esperado = actual.next.next;

        Carta skip = new SkipCarta("Rojo");
        actual.recibirCarta(skip);

        juego.playTurn(skip);

        assertEquals(esperado.getNombre(), juego.getCurrent().getNombre());
    }

    @Test
    public void testJugarCartaValidaActualizaMesaYMano() {
        Jugador actual = juego.getCurrent();

        Carta carta = new NumeroCarta("Rojo", "7");
        actual.recibirCarta(carta);

        assertTrue(carta.puedeJugarSobre(juego.topCard()));

        juego.playTurn(carta);

        assertEquals(carta, juego.topCard());
        assertFalse(actual.mano.contains(carta));
    }

    @Test
    public void testJugarCartaNoEnManoLanza() {
        Carta carta = new NumeroCarta("Verde", "3");

        assertThrows(IllegalArgumentException.class, () -> {
            juego.playTurn(carta);
        });
    }

    @Test
    public void testPenalidadPorNoCantarUNO() {
        Jugador actual = juego.getCurrent();
        actual.mano.clear();

        Carta carta1 = new NumeroCarta("Rojo", "7");
        Carta carta2 = new NumeroCarta("Rojo", "2");

        actual.recibirCarta(carta1);
        actual.recibirCarta(carta2);

        juego.playTurn(carta1);

        // El jugador queda con 1 carta pero no cantó UNO
        // Entonces penaliza con 2 cartas
        assertEquals(3, actual.mano.size());
    }
    @Test
    public void testJugadorCantaUNOCorrectamente() {
        Jugador actual = juego.getCurrent();
        actual.mano.clear();

        Carta carta1 = new NumeroCarta("Rojo", "7");
        Carta carta2 = new NumeroCarta("Rojo", "2");

        actual.recibirCarta(carta1);
        actual.recibirCarta(carta2);


        // Se asegura que puede jugar una carta, le deja 1 y canta UNO
        juego.juegoYcanto(carta1);

        assertEquals(1, actual.mano.size());
        assertTrue(actual.haCantado());
    }

    @Test
    public void testJugadorNoCantaUNORecibePenalidad() {
        Jugador actual = juego.getCurrent();
        actual.mano.clear();

        Carta carta1 = new NumeroCarta("Rojo", "7");
        Carta carta2 = new NumeroCarta("Rojo", "2");

        actual.recibirCarta(carta1);
        actual.recibirCarta(carta2);

        juego.playTurn(carta1); // no canta UNO

        assertEquals(3, actual.mano.size()); // 1 que le quedó + 2 de penalidad
    }

    @Test
    public void testDosJugadoresReverseVuelveAlMismoJugador() {
        Jugador jugador1 = new Jugador("Ana");
        Jugador jugador2 = new Jugador("Beto");
        Deque<Carta> nuevoMazo = new LinkedList<>();
        nuevoMazo.add(new NumeroCarta("Rojo", "5"));
        GameUNO juego2 = new GameUNO(List.of(jugador1, jugador2), nuevoMazo);
        juego2.repartirCartas(1).direccion.getCurrentPlayer().recibirCarta(new ReverseCarta("Rojo"));

        Jugador actual = juego2.getCurrent();
        juego2.playTurn(new ReverseCarta("Rojo"));

        assertEquals(actual, juego2.getCurrent());
    }

    @Test
    public void testCuatroJugadoresSkipSalteaCorrectamente() {
        Jugador j1 = new Jugador("A");
        Jugador j2 = new Jugador("B");
        Jugador j3 = new Jugador("C");
        Jugador j4 = new Jugador("D");

        Deque<Carta> nuevoMazo = new LinkedList<>();
        nuevoMazo.add(new NumeroCarta("Rojo", "5"));

        GameUNO juego4 = new GameUNO(List.of(j1, j2, j3, j4), nuevoMazo);
        juego4.repartirCartas(1);
        Carta skip = new SkipCarta("Rojo");
        j1.recibirCarta(skip);

        juego4.playTurn(skip);

        // A juega -> salta B -> turno de C
        assertEquals(j3, juego4.getCurrent());
    }

    @Test
    public void testWildCardCambiaColor() {
        Jugador actual = juego.getCurrent();
        CartaWild wild = new CartaWild();

        actual.recibirCarta(wild);
        wild.asignarColor("Rojo");
        System.out.println(wild);
        juego.playTurn(wild);

        assertEquals(wild, juego.topCard());
        assertEquals("Rojo", juego.topCard().getColor());
    }




    @Test
    public void testJugarUltimaCartaEsVictoria() {
        Jugador actual = juego.getCurrent();
        actual.mano.clear();
        Carta ante_ultima = new NumeroCarta("Rojo", "3");
        actual.recibirCarta(ultima);

        Error ex = assertThrows(Error.class, () -> {
            juego.juegoYcanto(ultima);
        });

        assertEquals("El jugador haGanado", ex.getMessage());
    }


}
