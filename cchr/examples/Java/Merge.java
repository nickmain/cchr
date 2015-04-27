public class Merge {

    public static void main(String[] args) throws Exception {
    if (args.length != 1) printUsage();
    else try {
    final int i0 = Integer.parseInt(args[0]);

if (i0 < 0) {                
printUsage();
return;
}

// First we create a new JCHR constraint handler:

int test[]= {27,74,17,33,94,18,46,83,65, 2,32,53,28,85,99,47,28,82, 6,11,55,29,39,81,90,37,10, 0,66,51,7,21};

for (int j=0; j<i0; j++) {
MergesortHandler handler = new MergesortHandler();
for (int k=0; k<test.length; k++) {
  handler.tellMerge(0,test[k]);
}
}

} catch (NumberFormatException e) {
System.err.println(e.getMessage());
printUsage();
}
}
    
public final static void printUsage() {
System.out.println(
"Usage: java Merge <positive int>"
);
}
}
