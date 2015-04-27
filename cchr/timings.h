#ifndef _TIMINGS_H_
#define _TIMINGS_H_ 1

#define TIMINGS_USE_TIME

#ifdef TIMINGS_USE_TIMES
#include <sys/times.h>
#include <unistd.h>
#define TIMINGS_T struct tms
#define TIMINGS_GET(val) times(&(val))
#define TIMINGS_CALC(val1,val2) (((double)(((val2).tms_stime+(val2).tms_utime)-((val1).tms_stime+(val1).tms_utime))))
#else
#include <time.h>
#define TIMINGS_T clock_t
#define TIMINGS_GET(val) val=clock()
#define TIMINGS_CALC(val1,val2) (((double)((val2)-(val1)))/CLOCKS_PER_SEC)

#endif

typedef struct {
  double base;
  int running;
  TIMINGS_T tv;
} timing_t;

void static inline timing_start(timing_t *timing) {
  timing->base=0.0;
  timing->running=1;
  TIMINGS_GET(timing->tv);
}

double static inline timing_stop(timing_t *timing) {
  if (timing->running) {
    TIMINGS_T tv;
    TIMINGS_GET(tv);
    timing->base += TIMINGS_CALC(timing->tv,tv);
    timing->running=0;
  }
  return timing->base;
}

void static inline timing_cont(timing_t *timing) {
  if (!timing->running) {
    timing->running=1;
    TIMINGS_GET(timing->tv);
  }
}

double static timing_get(timing_t *timing) {
  double base=timing->base;
  if (timing->running) {
    TIMINGS_T tv;
    TIMINGS_GET(tv);
    base += TIMINGS_CALC(timing->tv,tv);
  }
  return base;
}

#endif
