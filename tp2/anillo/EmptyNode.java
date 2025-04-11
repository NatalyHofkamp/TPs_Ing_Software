//package anillo;
//
//class EmptyNode extends Node {
//    Node add(Object valor) {
//        Node node = new OneNode(valor);
//        return node;
//    }
//
//    Node next() {
//        throw new IllegalStateException("El anillo está vacío");
//    }
//
//    Node remove(Node curr) {
//        curr.next = this;
//        curr.prev = this;
//        return this;
//    }
//
//    Object getValue() {
//        throw new IllegalStateException("El anillo está vacío.");
//    }
//}
