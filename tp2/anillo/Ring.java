package anillo;
import java.util.LinkedList;
import java.util.NoSuchElementException;

public class Ring {
    LinkedList<Object> listaObjetos;
    int indiceActual;
    public Ring() {
        listaObjetos = new LinkedList<>();
        indiceActual = -1;
    }

    public Ring next() {
        indiceActual -= 1;
        System.out.println("Elemento siguiente: " + listaObjetos.get((int) ((indiceActual % listaObjetos.size() + listaObjetos.size()) % listaObjetos.size())
        ));
        return this;
    }

    public Object current() {
        System.out.println("Elemento actual: " + listaObjetos.get((int) ((indiceActual % listaObjetos.size() + listaObjetos.size()) % listaObjetos.size())
        ));
        return listaObjetos.get((int) ((indiceActual % listaObjetos.size() + listaObjetos.size()) % listaObjetos.size())
        );
    }

    public Ring add( Object cargo ) {
        listaObjetos.add(cargo);
        indiceActual +=  1;
        return this;
    }

    public Ring remove() {
        listaObjetos.remove(indiceActual);
        indiceActual +=  1;
        return this;
    }
}
