package anillo;

class LinkFull extends Link {

    private Object value;
    LinkFull(Object newValue) {
        this.value = newValue;
        this.next = this;
        this.prev = this;
    }

    Link add(Object newValue) {
        LinkFull aux = new LinkFull(newValue);
        aux.next = this.next;
        aux.prev = this;
        this.next.prev = aux;
        this.next = aux;
        return aux;
    }

    Link next() {
        return this.prev; // el anillo gira hacia atrás
    }

    Link remove(Link curr) {
        curr.prev.next = curr.next;
        curr.next.prev = curr.prev;
        return curr.prev;
    }

    Object getValue() {
        return value;
    }
}
