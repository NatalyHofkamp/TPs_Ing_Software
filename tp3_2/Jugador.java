import java.util.Deque;
import java.util.Iterator;
import java.util.LinkedList;

public class Jugador {
    Jugador next;
    Jugador prev;

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

    public void jugar(GameUNO juego,Carta carta_elegida) {
        if (!mano.contains(cartaElegida)) {
            throw new IllegalArgumentException("La carta no está en la mano del jugador");
        }
            if (carta_elegida.puedeJugarSobre(juego.carta_mesa)):
                carta_elegida.aplicarEfecto(juego);
            juego.carta_mesa = carta_elegida;
        else:
            jugar ( juego, juego.repartirCartas(1))



}
