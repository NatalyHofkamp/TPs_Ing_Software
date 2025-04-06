package anillo;

abstract class Nodo {
    abstract Nodo add(Object valor);
    abstract Nodo next();
    abstract Nodo remove();
    abstract Object getValor();
}

class NodoVacio extends Nodo {

    Nodo add(Object valor) {
        return new NodoUno(valor);
    }

    Nodo next() {
        return this;
    }

    Nodo remove() {
        return this;
    }

    Object getValor() {
        throw new IllegalStateException("El anillo está vacío.");
    }
}


class NodoUno extends Nodo {

    private Object valor;

    NodoUno(Object valor) {
        this.valor = valor;
    }

    Nodo add(Object nuevoValor) {
        NodoNormal nuevo = new NodoNormal(nuevoValor);
        NodoNormal actual = new NodoNormal(valor);

        nuevo.next = actual;
        nuevo.prev = actual;
        actual.next = nuevo;
        actual.prev = nuevo;

        return nuevo;
    }

    Nodo next() {
        return this;
    }

    Nodo remove() {
        return new NodoVacio();
    }

    Object getValor() {
        return valor;
    }
}



class NodoNormal extends Nodo {

    Object valor;
    NodoNormal next;
    NodoNormal prev;

    NodoNormal(Object valor) {
        this.valor = valor;
    }

    Nodo add(Object nuevoValor) {
        NodoNormal nuevo = new NodoNormal(nuevoValor);

        nuevo.next = this.next;
        nuevo.prev = this;
        this.next.prev = nuevo;
        this.next = nuevo;

        return nuevo;
    }

    Nodo next() {
        return next;
    }

    Nodo remove() {

    }

    Object getValor() {
        return valor;
    }
}


public class Ring {

    private Nodo actual;

    public Ring() {
        this.actual = new NodoVacio();
    }

    public Ring add(Object cargo) {
        actual = actual.add(cargo);
        return this;
    }

    public Ring next() {
        actual = actual.next();
        return this;
    }

    public Ring remove() {
        actual = actual.remove();
        return this;
    }

    public Object current() {
        return actual.getValor();
    }
}
