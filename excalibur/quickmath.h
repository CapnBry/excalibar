#ifndef _QUICKMATH_H_
#define _QUICKMATH_H_

#ifdef __APPLE__
  #define	FLOAT_TO_INT(in,out) out=(int)in;
#else
  #define	FLOAT_TO_INT(in,out)		\
      __asm__ __volatile__ ("fistpl %0" : "=m" (out) : "t" (in) : "st") ;
#endif

#endif
