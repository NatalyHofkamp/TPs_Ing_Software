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

    public boolean haGanado() {
        return mano.isEmpty();
    }

    public void recibirCarta(Carta nuevaCarta) {
        mano.add(nuevaCarta);
    }

    private void validarQueLaTengo(Carta carta) {
        if (!mano.contains(carta)) {
            throw new IllegalArgumentException("La carta no está en la mano del jugador");
        }
    }
    private void jugarHastaPoder(GameUNO juego, Carta carta) {
        while (!carta.puedeJugarSobre(juego.carta_mesa)) {
            carta = juego.getCarta();
            recibirCarta(carta);
        }
        jugarCartaValida(juego, carta);
    }

    private void jugarCartaValida(GameUNO juego, Carta carta) {
        carta.aplicarEfecto(juego);
        juego.carta_mesa = carta;
        mano.remove(carta);
        verificarCantoUNO(juego);
    }


    public void jugar(GameUNO juego, Carta cartaElegida) {
        validarQueLaTengo(cartaElegida);
        jugarHastaPoder(juego, cartaElegida);
    }

    private void verificarCantoUNO(GameUNO juego) {
        if (mano.size() == 1 && !cantoUNO) {
            juego.aplicarPenalidadUNO(this);
        }

    }




}
