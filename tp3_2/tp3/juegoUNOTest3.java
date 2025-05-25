package tp3;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import java.util.*;
import static org.junit.jupiter.api.Assertions.*;

public class juegoUNOTest3 {

    juegoUNO juego2, juego3, juego4;
    Jugador j2A, j2B, j3A, j3B, j3C, j4A, j4B, j4C, j4D;

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

    private Deque<Carta> crearMazo1() {
        return new LinkedList<>(List.of(
                rojo5, azul4, rojo7, masDosRojo, skipVerde,
                reverseAzul, rojo3, wild, masDosVerde, rojo5,
                skipAmarillo, reverseVerde, rojo2, verde2, azul3
        ));
    }
    private Deque<Carta> crearMazo2() {
        return new LinkedList<>(List.of(
                azul4, skipVerde,reverseVerde, rojo7,rojo5, azul3,verde2, rojo3
        ));
    }

    @BeforeEach
    public void setup() {
        // juego2
        j2A = new Jugador("A");
        j2B = new Jugador("B");
        juego2 = new juegoUNO(List.of(j2A, j2B), crearMazo1());
        repartirCartas(juego2, 2);

        // juego3
        j3A = new Jugador("A");
        j3B = new Jugador("B");
        j3C = new Jugador("C");
        juego3 = new juegoUNO(List.of(j3A, j3B, j3C), crearMazo1());
        repartirCartas(juego3, 2);

        // juego4
        j4A = new Jugador("A");
        j4B = new Jugador("B");
        j4C = new Jugador("C");
        j4D = new Jugador("D");
        juego4 = new juegoUNO(List.of(j4A, j4B, j4C, j4D), crearMazo1());
        repartirCartas(juego4, 2);
    }



    private void repartirCartas(juegoUNO juego, int cartasPorJugador) {
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
    public void test1JugarCartaValidaActualizaMesaYMano() {
        Jugador actual = juego3.getCurrent();
        actual.recibirCarta(rojo7);
        assertTrue(rojo7.puedeJugarSobre(juego3.topCard()));
        juego3.jugarTurno(rojo7);
        assertEquals(rojo7, juego3.topCard());
        assertFalse(actual.jugadorMano().contains(rojo7));
    }

    @Test
    public void test2PenalidadPorNoCantarUNO() {
        Jugador actual = juego3.getCurrent();
        actual.jugadorMano().clear();
        actual.recibirCarta(rojo7);
        actual.recibirCarta(rojo2);
        juego3.jugarTurno(rojo7);
        assertEquals(3, actual.jugadorMano().size()); // penaliza con 2
    }

    @Test
    public void test3JugarUltimaCartaEsVictoria() {
        Jugador actual = juego3.getCurrent();
        actual.jugadorMano().clear();
        actual.recibirCarta(rojo2);
        juego3.jugarTurno(rojo2);
        Jugador siguiente = juego3.getCurrent();
        Carta otraCarta = new NumeroCarta("Rojo", "3");
        siguiente.recibirCarta(otraCarta);
        assertThrows(IllegalStateException.class, () -> juego3.jugarTurno(otraCarta));
    }


    @Test
    public void test4WildCardCambiaColor() {
        Jugador actual = juego3.getCurrent();
        actual.recibirCarta(wild);
        ((CartaWild) wild).asignarColor("Rojo");
        juego3.jugarTurno(wild);
        assertEquals("Rojo", juego3.topCard().getColor());
    }

    @Test
    public void test5SkipConDosJugadoresVuelveAlMismo() {
        j2A.recibirCarta(skipRojo);
        juego2.jugarTurno(skipRojo);
        assertEquals(j2A, juego2.getCurrent());
    }

    @Test
    public void test6MasDosConDosJugadoresSaltaCorrectamente() {
        int cantidadAntes = j2B.jugadorMano().size();
        j2A.recibirCarta(masDosRojo);
        juego2.jugarTurno(masDosRojo.uno());
        assertEquals(cantidadAntes + 2, j2B.jugadorMano().size());
        assertEquals(j2A, juego2.getCurrent());
    }

    @Test
    public void test7ReverseConTresJugadoresInvierteOrden() {
        j3A.recibirCarta(reverseRojo);
        juego3.jugarTurno(reverseRojo);
        assertTrue(juego3.direccion instanceof Izquierda);
        assertEquals(j3A.prev, juego3.getCurrent());
    }

    @Test
    public void test8SkipConTresJugadoresSaltaUno() {
        j3A.recibirCarta(skipRojo);
        Jugador esperado = j3A.next.next;
        juego3.jugarTurno(skipRojo);
        assertEquals(esperado, juego3.getCurrent());
    }

    @Test
    public void test9MasDosConTresJugadoresSaltaCorrectamente() {
        j3A.recibirCarta(masDosRojo);
        int antes = j3B.jugadorMano().size();
        juego3.jugarTurno(masDosRojo);
        assertEquals(antes + 2, j3B.jugadorMano().size());
        assertEquals(j3C, juego3.getCurrent());
    }

    @Test
    public void test10SkipConCuatroJugadoresSaltaCorrectamente() {
        j4A.recibirCarta(skipRojo);
        Jugador esperado = j4A.next.next;
        juego4.jugarTurno(skipRojo);
        assertEquals(esperado, juego4.getCurrent());
    }

    @Test
    public void test11ReverseConCuatroJugadoresInvierteDireccion() {
        j4A.recibirCarta(reverseRojo);
        juego4.jugarTurno(reverseRojo);
        assertTrue(juego4.direccion instanceof Izquierda);
        assertEquals(j4A.prev, juego4.getCurrent());
    }

    @Test
    public void test12MasDosConCuatroJugadoresSaltaCorrectamente() {
        j4A.recibirCarta(masDosRojo);
        int antes = j4B.jugadorMano().size();
        juego4.jugarTurno(masDosRojo);
        assertEquals(antes + 2, j4B.jugadorMano().size());
        assertEquals(j4C, juego4.getCurrent());
    }

    @Test
    public void test13SecuenciaDeTurnosConCartasValidas() {
        j3A.recibirCarta(rojo7);
        juego3.jugarTurno(rojo7);
        assertEquals(rojo7, juego3.topCard());
        assertEquals(j3B, juego3.getCurrent());

        j3B.recibirCarta(rojo3);
        juego3.jugarTurno(rojo3);
        assertEquals(rojo3, juego3.topCard());
        assertEquals(j3C, juego3.getCurrent());

        j3C.recibirCarta(rojo5);
        juego3.jugarTurno(rojo5);
        assertEquals(rojo5, juego3.topCard());
        assertEquals(j3A, juego3.getCurrent());
    }

    @Test
    public void test14CantaUnoYGanaYNoSePuedeSeguirJugando() {
        j2A.jugadorMano().clear();
        j2A.recibirCarta(rojo3);
        j2A.recibirCarta(rojo5);
        j2B.recibirCarta(rojo7);

        juego2.jugarTurno(rojo3.uno());
        assertEquals(1, j2A.jugadorMano().size());

        juego2.jugarTurno(rojo7);
        juego2.jugarTurno(rojo5);

        assertEquals(0, j2A.jugadorMano().size());

        j2B.recibirCarta(azul4);
        assertThrows(IllegalStateException.class, () -> juego2.jugarTurno(azul4));
    }

    @Test
    public void test15JugadorRobaHastaTenerCartaValida() {
        Carta cartaMesa = rojo5;
        Carta cartaInvalida = skipVerde;
        juegoUNO juego = new juegoUNO(List.of(j2A, j2B), crearMazo2());
        repartirCartas(juego, 2);
        juego.carta_mesa = cartaMesa;
        juego.jugarTurno(cartaInvalida);

        assertTrue(j2A.jugadorMano().contains(skipVerde)); // no se pudo jugar
        assertTrue(j2A.jugadorMano().contains(azul3));
        assertTrue(j2A.jugadorMano().contains(verde2));
        assertFalse(j2A.jugadorMano().contains(rojo3));
    }


}

