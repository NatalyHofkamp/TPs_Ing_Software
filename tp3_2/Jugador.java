import java.util.Deque;
import java.util.Iterator;
import java.util.LinkedList;

public class Jugador {
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

    public void jugar(GameUNO juego) {
        Iterator<Carta> it = mano.iterator();
        while (it.hasNext()) {
            Carta carta_jugada = it.next();
            if (carta_jugada.puedeJugarSobre(juego.carta_mesa)) {
                it.remove();
                carta_jugada.aplicarEfecto(juego);
                juego.carta_mesa = carta_jugada;
                return;
            }
        }
        juego.repartirCartas(1);
    }
}
