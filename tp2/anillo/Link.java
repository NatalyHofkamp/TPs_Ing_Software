package anillo;

abstract class Link {
    Link next;
    Link prev;
    abstract Link add(Object value);
    abstract Link next();
    abstract Link remove(Link curr);
    abstract Object getValue();
}
