package anillo;

class LinkEmpty extends Link {
    Link add(Object valor) {
        Link link = new LinkFull(valor);
        return link;
    }

    Link next() {
        throw new IllegalStateException("El anillo está vacío");
    }

    Link remove(Link curr) {
        curr.next = this;
        curr.prev = this;
        return this;
    }

    Object getValue() {
        throw new IllegalStateException("El anillo está vacío.");
    }
}
