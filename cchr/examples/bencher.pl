#!/usr/bin/perl -w

use strict;

$!=1;

#my %tests = (
#  c =>    { leq => [100], tak2 => [500,450,405], ram_fib => [200000] },
#  cchr => { dijkstra => [16384], leq => [100], tak2 => [500,450,405], ram_fib => [200000] },
#  swi =>  { dijkstra => [16384], leq => [100], tak2 => [500,450,405], ram_fib => [200000] },
#  yap =>  { dijkstra => [16384], leq => [100], tak2 => [500,450,405], ram_fib => [200000] }
#);


my %tests = (
  c =>    { ram_fib => [25000] },
  cchr => { ram_fib => [25000] },
  swi =>  { ram_fib => [25000] },
  yap =>  { ram_fib => [25000] }
);

my $minruns=20;
my $maxruns=30000;
my $acc=0.001;

sub stati {
  my $n=0;
  my $sum=0;
  my $qsum=0;
  for (@_) {
    $sum += $_;
    $n ++;
  }
  my $avg=$sum/$n;
  for (@_) {
    $qsum += ($_-$avg)*($_-$avg);
  }
  my $sdev=sqrt($qsum/($n-1))/sqrt($n);
  return ($avg,$sdev);
}

for my $sys (keys %tests) {
  my $sysv = $tests{$sys};
  print "sys $sys\n";
  for my $test (keys %{$sysv}) {
    print "  test $test\n";
    my $testv = $sysv->{$test};
    my $ex="./bench$sys.sh $test ".join(',',@{$testv})." |";
    my @real;
    my @cpu;
    my @stc;
    my @str;
    my $excl;
    my $run=0;
    while(1) {
      open PIPE,$ex;
      my $line=<PIPE>;
      $run++;
      if ($line =~ /(\d+)\s+(\d+)/) {
        print "    run $run: $1 $2";
        push @cpu,$1;
        push @real,$2;
        @cpu = (sort @cpu);
        @real = (sort @real);
        if ($#cpu+1 >= $minruns) {
          $excl=int(($#cpu+1)/15)+1;
          my @scpu = @cpu[$excl..($#cpu-$excl)];
          my @sreal = @real[$excl..($#real-$excl)];
          @stc=stati(@scpu);
          @str=stati(@sreal);
          last if ($stc[0]>0 && $str[0]>0 && $stc[1]/$stc[0]<$acc && $str[1]/$str[0]<$acc);
          last if ($run>$maxruns);
        }
        print "\n";
      }
      close PIPE;
    }
    printf("    # $sys $test: excl=$excl cpu=%.4g+-%.3g real=%.4g+-%.3g\n",$stc[0],$stc[1],$str[0],$str[1]);
  }
}
