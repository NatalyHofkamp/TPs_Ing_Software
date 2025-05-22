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
    public void testDosJugadoresReverseVuelveAlMismoJugador() {
        Jugador jugador1 = new Jugador("Ana");
        Jugador jugador2 = new Jugador("Beto");

        // Crear un mazo inicial con una sola carta
        Deque<Carta> nuevoMazo = new LinkedList<>();
        nuevoMazo.add(new NumeroCarta("Rojo", "5"));
        nuevoMazo.add(new NumeroCarta("Rojo", "3"));
        // Crear el juego con los dos jugadores y ese mazo
        GameUNO juego2 = new GameUNO(List.of(jugador1, jugador2), nuevoMazo);

        // Repartir una carta a cada jugador
        juego2.repartirCartas(1);

        // Mostrar las cartas que tienen los jugadores después de repartir
        System.out.println("Cartas de Ana: " + jugador1.getMano());
        System.out.println("Cartas de Beto: " + jugador2.getMano());

        // Obtener el jugador actual
        Jugador actual = juego2.getCurrent();
        Carta carta_reverse = new ReverseCarta("Rojo");
        // El jugador actual recibe una carta Reverse
        actual.recibirCarta(carta_reverse);

        // Mostrar las cartas del jugador actual antes de jugar
        System.out.println("Cartas de " + actual.getNombre() + " antes de jugar Reverse: " + actual.getMano());

        // Jugar la carta Reverse y verificar si vuelve al mismo jugador
        juego2.playTurn(carta_reverse.uno());

        // Mostrar quién es el jugador actual después del Reverse
        System.out.println("Jugador actual después del Reverse: " + juego2.getCurrent().getNombre());

        // Validar que sigue siendo el mismo jugador
        assertEquals(actual, juego2.getCurrent());
    }
@Test
    public void testDosJugadoresSkipVuelveAlMismoJugador() {
        Jugador jugador1 = new Jugador("Ana");
        Jugador jugador2 = new Jugador("Beto");

        // Crear un mazo inicial con una sola carta
        Deque<Carta> nuevoMazo = new LinkedList<>();
        nuevoMazo.add(new NumeroCarta("Rojo", "5"));
        nuevoMazo.add(new NumeroCarta("Rojo", "3"));
        // Crear el juego con los dos jugadores y ese mazo
        GameUNO juego2 = new GameUNO(List.of(jugador1, jugador2), nuevoMazo);

        // Repartir una carta a cada jugador
        juego2.repartirCartas(1);

        // Mostrar las cartas que tienen los jugadores después de repartir
        System.out.println("Cartas de Ana: " + jugador1.getMano());
        System.out.println("Cartas de Beto: " + jugador2.getMano());

        // Obtener el jugador actual
        Jugador actual = juego2.getCurrent();
        Carta carta_skip = new SkipCarta("Rojo");
        // El jugador actual recibe una carta Reverse
        actual.recibirCarta(carta_skip);

        // Mostrar las cartas del jugador actual antes de jugar
        System.out.println("Cartas de " + actual.getNombre() + " antes de jugar Reverse: " + actual.getMano());

        // Jugar la carta Reverse y verificar si vuelve al mismo jugador
        juego2.playTurn(carta_skip.uno());

        // Mostrar quién es el jugador actual después del Reverse
        System.out.println("Jugador actual después del Reverse: " + juego2.getCurrent().getNombre());

        // Validar que sigue siendo el mismo jugador
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
        nuevoMazo.add(new NumeroCarta("Rojo", "6"));
        nuevoMazo.add(new NumeroCarta("Rojo", "7"));
        GameUNO juego4 = new GameUNO(List.of(j1, j2, j3, j4), nuevoMazo);
        juego4.repartirCartas(2);
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
        Carta ultima = new NumeroCarta("Rojo", "2");
        actual.recibirCarta(ante_ultima);
        actual.recibirCarta(ultima);
        juego.playTurn(ante_ultima.uno());
        juego.playTurn(juego.getCurrent().getMano().peek());
        Error error = assertThrows(Error.class, () -> {
            juego.playTurn(ultima);
        });

        assertEquals("El jugador haGanado", error.getMessage());
    }
}


