import java.util.Collection;
import runtime.primitive.LogicalInt;
import runtime.primitive.IntEqualitySolverImpl;
import runtime.primitive.IntEqualitySolver;

public class Tak {

    public static void main(String[] args) throws Exception {
    if (args.length != 3) printUsage();
    else try {
    final int x = Integer.parseInt(args[0]);
    final int y = Integer.parseInt(args[1]);
    final int z = Integer.parseInt(args[2]);

    IntEqualitySolver solver = new IntEqualitySolverImpl();
    TakTabHandler handler = new TakTabHandler(solver);
    LogicalInt logint=new LogicalInt();
    handler.tellTak(x,y,z,logint);
    System.out.println(logint.getValue());


} catch (NumberFormatException e) {
System.err.println(e.getMessage());
printUsage();
}
}
    
public final static void printUsage() {
System.out.println(
"Usage: java Tak <positive int>"
);
}
}
