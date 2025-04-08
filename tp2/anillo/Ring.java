package anillo;

import java.time.LocalDate;
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
        throw new IllegalStateException("El anillo está vacío");
    }

    Object getValue() {
        throw new IllegalStateException("El anillo está vacío.");
    }

    @Override
    public String toString() {
        return "EmptyNode";
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
        return this.prev; // el anillo gira hacia atrás
    }

    Node remove() {
        this.prev.next = this.next;
        this.next.prev = this.prev;
        return this.prev;
    }

    Object getValue() {
        return value;
    }

    @Override
    public String toString() {
        return "OneNode(" + value + ")";
    }
}

public class Ring {
    private final Stack<Node> stack = new Stack<>();
    private Node curr;

    public Ring() {
        curr = new EmptyNode();
        stack.push(curr);
    }

    public Ring add(Object value) {
        curr = curr.add(value);
        stack.push(curr);
        System.out.println("add -> curr: " + curr);
        return this;
    }

    public Ring next() {
        curr = curr.next();
        stack.push(curr);
        System.out.println("next -> curr: " + curr);
        return this;
    }

    public Ring remove() {
        System.out.println("removeantes -> curr: " + curr);
        curr = curr.remove();   // esto devuelve el nodo anterior en el anillo
        stack.pop();            // quitamos del historial el nodo eliminado
        stack.push(curr);
        curr = stack.peek();// actualizamos el historial con el nuevo curr

        System.out.println("remove -> curr: " + curr);
        return this;
    }


    public Object current() {
        return curr.getValue();
    }
}


