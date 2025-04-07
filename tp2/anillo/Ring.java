package anillo;

abstract class Nodo {
    Nodo next;
    Nodo prev;
    abstract Nodo add(Object valor);
    abstract Nodo next();
    abstract Nodo remove();
    abstract Object getValue();
}

class NodoVacio extends Nodo {

    Nodo add(Object valor) {return  new NodoUno(valor);
    }
    Nodo next() {throw new IllegalStateException("El anillo está vacío");
    }
    Nodo remove() {
        return this;
    }
    Object getValue() {
        throw new IllegalStateException("El anillo está vacío.");
    }
}


class NodoUno extends Nodo {
    private Object valor;
    NodoUno(Object valor) {
        this.valor = valor;
        this.next = this;
        this.prev = this;
    }
    Nodo add(Object nuevoValor) {
        NodoNormal nuevo = new NodoNormal(nuevoValor);
        NodoNormal current = new NodoNormal(this.valor);
        nuevo.next = current;
        nuevo.prev = current;
        current.next = nuevo;
        current.prev = nuevo;
        return nuevo;
    }
    Nodo next() {return this.next;}
    Nodo remove() {return new NodoVacio(); }
    Object getValue() {
        return valor;
    }
}



class NodoNormal extends Nodo {
    Object valor;
    NodoNormal(Object valor) { this.valor = valor;    }

    Nodo add(Object nuevoValor) {
        NodoNormal nuevo = new NodoNormal(nuevoValor);
        nuevo.next = this.next;
        nuevo.prev = this;
        this.next.prev = nuevo;
        this.next = nuevo;
        return nuevo;
    }

    Nodo next() {
        return this.prev;
    }
    Nodo remove() { return this.prev; }
    Object getValue() {
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
        return actual.getValue();
    }
}
