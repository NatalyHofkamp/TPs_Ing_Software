package anillo;
import java.util.Stack;


public class Ring {

    private Link curr;
    private Stack<Link> stack = new Stack<>();
    public Ring() {
        curr = new LinkEmpty();
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
        stack.pop();
        Link beforeLast = stack.peek();
        curr = beforeLast.remove(curr);
        return this;
    }


    public Object current() {
        return curr.getValue();
    }
}


