import java.util.Collection;
import runtime.primitive.LogicalInt;
import runtime.primitive.IntEqualitySolverImpl;
import runtime.primitive.IntEqualitySolver;

public class FibTd {

    public static void main(String[] args) throws Exception {
    if (args.length != 1) printUsage();
    else try {
    final int x = Integer.parseInt(args[0]);

    IntEqualitySolver solver = new IntEqualitySolverImpl();
    FibTdHandler handler = new FibTdHandler(solver);
    LogicalInt logint=new LogicalInt();
    handler.tellFib(x,logint);
    System.out.println(logint.getValue());


} catch (NumberFormatException e) {
System.err.println(e.getMessage());
printUsage();
}
}

public final static void printUsage() {
System.out.println(
"Usage: java FibTd <positive int>"
);
}
}
