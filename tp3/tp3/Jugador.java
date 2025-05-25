package tp3;

import java.util.Deque;
import java.util.LinkedList;

public class Jugador {
    Jugador next;
    Jugador prev;
    private boolean cantoUNO = false;

    private String nombre;
    public Deque<Carta> mano =  new LinkedList<>();

    public Jugador(String nombre) {
        this.nombre = nombre;
    }

    public String getNombre() {
        return nombre;
    }

    public Deque<Carta> jugadorMano() {return mano;}

    public boolean haGanado() {
        return mano.isEmpty();
    }

    public void recibirCarta(Carta nuevaCarta) {
        mano.add(nuevaCarta);
    }


    private void jugarCartaValida(juegoUNO juego, Carta carta) {
        carta.aplicarEfecto(juego);
        juego.carta_mesa = carta;
        mano.remove(carta);

    }

    public void jugar(juegoUNO juego, Carta carta) {
        while (!carta.puedeJugarSobre(juego.carta_mesa)) {
            carta = juego.getCarta();
            recibirCarta(carta);
        }
        jugarCartaValida(juego, carta);
    }

}
