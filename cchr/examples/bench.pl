#!/usr/bin/perl -w

use strict;

my $BENCHER="./bench.sh";

my $FACTOR=1.09;
my $EFACTOR=1.09;
my $MINTIME=0.000025;
my $MAXTIME=10;
my $RUNTIME=10;

my $AVGTIME=($MINTIME+$MAXTIME)/2;


my %BENCHERS=(
  gcd => sub { (5,1000*$_[0]) },
  fib => sub { ($_[0]) },
  leq => sub { ($_[0]+1) },
  primes => sub { ($_[0]+1) },
  ram => sub { ($_[0]) },
  tak => sub { (int($_[0]/0.81),int($_[0]/0.9),int($_[0])) }
);

my %SYSTEMS=(
  swi => sub {  my ($prog,@args)=@_; return ('./benchswi.sh',$prog,join(',',@args)); },
#  jchr => sub { my ($prog,@args)=@_; return ('java','-cp','/home/pw/Desktop/KULeuven_JCHR.jar:Java',$prog,@args); },
  cchr => sub { my ($prog,@args)=@_; return ("./$prog",@args); },
  c => sub {    my ($prog,@args)=@_; return ("C/$prog",@args); }
);

my %SYSTBL=(
  swi =>  {gcd => "gcd", fib => "fib", leq => "leq", primes => "primes", ram => "ram", tak => "tak" },
#  jchr => {              fib => "Fib",     leq => "Leq", primes => "Primes",  ram => "Ram", tak => "Tak"},
  cchr => {gcd => "gcd", fib => "fib_gmp", leq => "leq", primes => "primes",  ram => "ram", tak => "tak2"},
  c =>    {gcd => "gcd", fib => "fib_gmp", leq => "leq", primes => "primes",  ram => "ram", tak => "tak" }
);

#my %SYSTBL=(
#  swi =>  {fib => "fib"}
#);

my %MAX=(
  c => {leq => 450, fib => 135000, gcd => 35000000, primes => 350000, ram => 250000000, tak => 3400},
  cchr => {ram => 4500, tak => 2000, fib => 25000, gcd => 10000000, primes => 80000, leq => 250},
  swi => {gcd => 800, ram => 25000, tak => 500, fib => 750, primes => 14000, leq => 65},
#  jchr => {fib => 10000, leq => 100, primes => 9000, ram => 2500, leq => 850},
);

sub execBench {
  my ($bench,$sys,$num)=@_;
  my @nums=$BENCHERS{$bench}->($num);
  my $tbl=$SYSTBL{$sys}->{$bench};
  return (undef,undef) if (!defined $tbl);
  my @cmd=$SYSTEMS{$sys}->($tbl,@nums);
  my @val=split(/\s+/,qx($BENCHER $RUNTIME @cmd));
  return @val;
}

for my $bench (keys %BENCHERS) {
  print "## bench=$bench\n";
  SYS: for my $sys (keys %SYSTEMS) {
    print "### sys=$sys\n";
    my $lim=$MAX{$sys}->{$bench};
    my $low=30500000; # a year should do as maximal 'low'
    my ($num,$val,$run)=(0,0,0);
    my ($min,$minv,$max,$maxv)=(1,0,0,0); 
    do {
      $num=int($num*$EFACTOR)+1 if ($val<=$AVGTIME);
      $num=$lim if ((defined $lim) && $num>$lim);
      ($val,$run)=execBench($bench,$sys,$num);
      next SYS if (!defined($val));
      $val-=$low;
      if ($val<0) {
        $low+=$val; $val=0;
      }
      print "$sys/$bench:$num (${val},${low})*${run} exp\n";
      if ($val>$minv && $val<$AVGTIME) {$min=$num; $minv=$val;};
      if ($val<$maxv && $val>$AVGTIME) {$max=$num; $maxv=$val;};
    } while($val<$MINTIME && (!defined($lim) || $num<$lim) );
    my ($avg,$avgv);
    if ($val>=$MINTIME && $val<=$MAXTIME || (defined($lim) && $num==$lim) || $max==0) {
      $avg=$min;
      $avgv=$minv;
    } else {
      do {
        $avg=int($max-($max-$min)/($maxv-$minv)*($maxv-$AVGTIME)+0.5);
	last if ($avg==$max || $avg==$min);
	($avgv,$run)=execBench($bench,$sys,$avg);
	$avgv-=$low;
        if ($avgv<0) {
          $low+=$avgv;
	  $avgv=0;
        }
        print "$sys/$bench:$num (${avgv},${low})*${run} rf[$min,$max]\n";
	if ($avgv>$AVGTIME) {
	  $max=$avg;
	  $maxv=$avgv;
	} else {
	  $min=$avg;
	  $minv=$avgv;
	}
      } while ($avgv<$MINTIME || $avgv>$MAXTIME);
    }
    $num=$avg;
    $val=$avgv;
    while ($val<$MAXTIME && (!defined($lim) || $num < $lim)) {
      $num=int($num*$FACTOR+1);
      $num=$lim if ((defined $lim) && $num>$lim);
      ($val,$run)=execBench($bench,$sys,$num);
      $val-=$low;
      if ($val<0) {
        $low+=$val; $val=0;
      }
      print "$sys/$bench:$num (${val},${low})*${run} up\n";
    }
    $num=$avg;
    $val=$avgv;
    while ($val>$MINTIME) {
      $num=int($num/$FACTOR);
      last if ($num<1);
      ($val,$run)=execBench($bench,$sys,$num);
      $val-=$low;
      if ($val<0) {
        $low+=$val; $val=0;
      }
      print "$sys/$bench:$num (${val},${low})*${run} dn\n";
    }
  }
}
