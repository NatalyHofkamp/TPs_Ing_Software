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
        if (listaObjetos.isEmpty()) {
            throw new NoSuchElementException("El anillo está vacío, no hay siguiente elemento.");
        }

        // Mostrar el elemento actual (último agregado)
        System.out.println("Elemento actual: " + listaObjetos.get(indiceActual));

        // Avanzar al siguiente elemento de forma circular
        indiceActual = (indiceActual + 1) % listaObjetos.size();

        // Mostrar el siguiente elemento (después de la actualización del índice)
        System.out.println("Elemento siguiente: " + listaObjetos.get(indiceActual));

        return this;  // Retorna el objeto Ring actual para permitir la encadenación de métodos
    }


    public Object current() {

        if (listaObjetos.isEmpty()) {
            throw new NoSuchElementException("El anillo está vacío, no hay current.");
        }
        return listaObjetos.get(indiceActual);
    }

    public Ring add( Object cargo ) {
        listaObjetos.add(cargo); // Agregar el objeto a la lista
        indiceActual = listaObjetos.size() - 1;
        return this;
    }

    public Ring remove() {
        if (listaObjetos.isEmpty()) {
            System.out.println("La lista está vacía, no se puede eliminar.");
            return this; // Si la lista está vacía, no hace nada
        }

        // Eliminar el objeto en el índice actual
        listaObjetos.remove(indiceActual);

        // Ajustar el índice actual después de la eliminación
        if (listaObjetos.isEmpty()) {
            indiceActual = -1; // Si la lista quedó vacía, ponemos el índice en -1
        } else {
            // Si hay elementos, ajustamos el índice de forma circular
            // Si eliminamos el primer elemento, el current pasa a ser el siguiente
            if (indiceActual >= listaObjetos.size()) {
                indiceActual = 0; // Si eliminamos el último, vamos al primero
            }
        }

        return this;
    }
}
