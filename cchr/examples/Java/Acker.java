import java.util.Collection;
import java.math.BigInteger;
import runtime.primitive.LogicalInt;
import runtime.primitive.IntEqualitySolverImpl;
import runtime.primitive.IntEqualitySolver;
import runtime.EqualitySolver;
import runtime.EqualitySolverImpl;
import runtime.Logical;

public class Acker {

    public static void main(String[] args) throws Exception {
    if (args.length != 2) printUsage();
    else try {
    final BigInteger x = new BigInteger(args[0]);
    final BigInteger y = new BigInteger(args[1]);

    EqualitySolver<BigInteger> solver = new EqualitySolverImpl<BigInteger>();
    AckerHandler handler = new AckerHandler(solver);
    Logical<BigInteger> log=new Logical<BigInteger>();
    handler.tellAcker(x,y,log);
    System.out.println(log);


} catch (NumberFormatException e) {
System.err.println(e.getMessage());
printUsage();
}
}

public final static void printUsage() {
System.out.println(
"Usage: java Acker <positive int>"
);
}
}
