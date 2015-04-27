import java.util.Collection;
import runtime.primitive.LogicalInt;
import runtime.primitive.IntEqualitySolverImpl;
import runtime.primitive.IntEqualitySolver;

public class Leq {

    public static void main(String[] args) throws Exception {
    if (args.length != 1) printUsage();
    else try {
    final int x = Integer.parseInt(args[0]);

    IntEqualitySolver solver = new IntEqualitySolverImpl();
    LeqHandler handler = new LeqHandler(solver);
    LogicalInt logints[]=new LogicalInt[x];
    for (int i=0; i<x; i++) {
      logints[i]=new LogicalInt();
    }
    for (int i=0; i<x; i++) {
      handler.tellLeq(logints[i],logints[(i+1)%x]);
    }

} catch (NumberFormatException e) {
System.err.println(e.getMessage());
printUsage();
}
}

public final static void printUsage() {
System.out.println(
"Usage: java Leq <positive int>"
);
}
}
