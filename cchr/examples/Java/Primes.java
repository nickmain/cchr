import java.util.Collection;

public class Primes {

    public static void main(String[] args) throws Exception {
    if (args.length != 1) printUsage();
    else try {
    final int i0 = Integer.parseInt(args[0]);

if (i0 < 0) {
printUsage();
return;
}

// First we create a new JCHR constraint handler:
PrimesHandler handler = new PrimesHandler();

// Next we tell the JCHR handler the following two constraints:             
handler.tellCandidate(i0);
// Afterwards we can lookup the constraints in the 
// resulting constraint store: 

/*Collection<PrimesHandler.PrimeConstraint> prims = handler.getPrimeConstraints();

for (PrimesHandler.PrimeConstraint f : prims) {
  System.out.println("prime("+f.get$0()+")");
}*/


} catch (NumberFormatException e) {
System.err.println(e.getMessage());
printUsage();
}
}
    
public final static void printUsage() {
System.out.println(
"Usage: java Primes <positive int>"
);
}
}
