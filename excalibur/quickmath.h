#ifndef _QUICKMATH_H_
#define _QUICKMATH_H_

#include <math.h>

#ifdef __APPLE__
  #define	FLOAT_TO_INT(in,out) out=(int)in;
  #define	FLOAT_TO_UINT(in,out) out=(unsigned int)in;
#else
  #define	FLOAT_TO_INT(in,out)		\
      __asm__ __volatile__ ("fistpl %0" : "=m" (out) : "t" (in) : "st") ;
#endif

#ifdef __cplusplus
  extern "C" {
#endif

void cos_buildtable(void);
void cos_freetable(void);
float cos_quick(float d);

void sin_buildtable(void);
void sin_freetable(void);
float sin_quick(float d);

void sincos_quick(float d, float *sin, float *cos);

#ifdef __cplusplus
  }
#endif

#endif
