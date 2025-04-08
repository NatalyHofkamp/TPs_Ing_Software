package anillo;

abstract class Node {
    Node next;
    Node prev;
    abstract Node add(Object value);
    abstract Node next();
    abstract Node remove();
    abstract Object getValue();
}

class EmptyNode extends Node {

    Node add(Object valor) {return  new OneNode(valor);
    }
    Node next() {throw new IllegalStateException("El anillo está vacío");
    }
    Node remove() {
        return this;
    }
    Object getValue() {
        throw new IllegalStateException("El anillo está vacío.");
    }
}


class OneNode extends Node {
    private Object value;
    OneNode(Object valor) {
        this.value = valor;
        this.next = this;
        this.prev = this;
    }
    Node add(Object nuevoValor) {
        NormalNode aux = new NormalNode(nuevoValor);
        NormalNode current = new NormalNode(this.value);
        aux.next = current;
        aux.prev = current;
        current.next = aux;
        current.prev = aux;
        return aux;
    }
    Node next() {return this.next;}
    Node remove() {return new EmptyNode(); }
    Object getValue() {
        return value;
    }
}



class NormalNode extends Node {
    Object valor;
    NormalNode(Object valor) { this.valor = valor;    }

    Node add(Object nuevoValor) {
        NormalNode aux = new NormalNode(nuevoValor);
        aux.next = this.next;
        aux.prev = this;
        this.next.prev = aux;
        this.next = aux;
        return aux;
    }

    Node next() {
        return this.prev;
    }
    Node remove() { return this.prev; }
    Object getValue() {
        return valor;
    }
}


public class Ring {
    private Node curr;
    public Ring() {
        this.curr = new EmptyNode();
    }

    public Ring add(Object cargo) {
        curr = curr.add(cargo);
        return this;
    }

    public Ring next() {
        curr = curr.next();
        return this;
    }

    public Ring remove() {
        curr = curr.remove();
        return this;
    }

    public Object current() {
        return curr.getValue();
    }
}
