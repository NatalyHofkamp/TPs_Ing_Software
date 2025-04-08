package anillo;
import java.util.Stack;

abstract class Node {
    Node next;
    Node prev;
    abstract Node add(Object value);
    abstract Node next();
    abstract Node remove();
    abstract Object getValue();
}

class EmptyNode extends Node {
    Node add(Object valor) {
        return new OneNode(valor);
    }

    Node next() {
        throw new IllegalStateException("El anillo está vacío");
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
        OneNode nuevo = new OneNode(nuevoValor);

        nuevo.next = this.next;
        nuevo.prev = this;

        this.next.prev = nuevo;
        this.next = nuevo;

        return nuevo;
    }


    Node next() {
        return this.prev;
    }

    Node remove() {
        this.prev.next = this.prev;
        this.next.prev = this.next;
        return this.prev;
    }

    Object getValue() {
        return value;
    }
}



public class Ring {
    private Stack<Node> stack = new Stack<>();
    private Node curr;

    public Ring() {
        curr = new EmptyNode();
        stack.push(curr);
    }

    public Ring add(Object value) {
        curr = curr.add(value);
        stack.push(curr);
        return this;
    }

    public Ring next() {
        curr = curr.next();
        return this;
    }

    public Ring remove() {
        curr.remove();
        stack.remove(curr);               // quitamos el nodo actual del historial
        curr = stack.peek();       // si aún hay algo, ese es el nuevo actual
        return this;
    }

    public Object current() {
        return curr.getValue();
    }
}

