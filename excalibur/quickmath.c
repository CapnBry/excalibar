#include <malloc.h>
#include <math.h>
#include "quickmath.h"

const unsigned int TRIG_TABLE_SIZE = 256;
const float TRIG_TABLE_LERP = (float)(256.0 / 180.0);

static float *COS_TABLE;
static float *SIN_TABLE;

void cos_buildtable(void)
{
    unsigned int i;
    COS_TABLE = (float *)malloc(TRIG_TABLE_SIZE * sizeof(float));
    for (i=0; i<=TRIG_TABLE_SIZE; i++)
        COS_TABLE[i] = (float)cos(M_PI * (double)i / (double)TRIG_TABLE_SIZE);
}

void cos_freetable(void)
{
    free(COS_TABLE);
}

float cos_quick(float d)
{
    int quantized;
    d *= TRIG_TABLE_LERP;
    FLOAT_TO_INT(d, quantized);

    if (quantized < 0)
        quantized = -quantized;

    if ((unsigned int)quantized > TRIG_TABLE_SIZE)
    {
        quantized %= (2 * TRIG_TABLE_SIZE);
        if ((unsigned int)quantized > TRIG_TABLE_SIZE)
            quantized = (2 * TRIG_TABLE_SIZE) - quantized;
    }  /* if > COS_TABLE_SIZE */

    return COS_TABLE[quantized];
}

void sin_buildtable(void)
{
    unsigned int i;
    SIN_TABLE = (float *)malloc(TRIG_TABLE_SIZE * sizeof(float));
    for (i=0; i<=TRIG_TABLE_SIZE; i++)
        SIN_TABLE[i] = (float)sin(M_PI * (double)i / (double)TRIG_TABLE_SIZE);
}

void sin_freetable(void)
{
    free(SIN_TABLE);
}

float sin_quick(float d)
{
    int quantized;
    int negative;
    d *= TRIG_TABLE_LERP;
    FLOAT_TO_INT(d, quantized);

    if (quantized < 0)
    {
        quantized = -quantized;
        negative = 1;
    }
    else
        negative = 0;

    if ((unsigned int)quantized > TRIG_TABLE_SIZE)
    {
        quantized %= (2 * TRIG_TABLE_SIZE);

        if ((unsigned int)quantized > TRIG_TABLE_SIZE)
        {
            quantized = (2 * TRIG_TABLE_SIZE) - quantized;
              /* flip the negative */
            negative ^= 1;
        }
    }  /* if > TABLE_SIZE */

    if (negative)
        return -SIN_TABLE[quantized];
    else
        return SIN_TABLE[quantized];
}

void sincos_quick(float d, float *sin, float *cos)
{
    int quantized;
    int negative_s;
    d *= TRIG_TABLE_LERP;
    FLOAT_TO_INT(d, quantized);

    if (quantized < 0)
    {
        quantized = -quantized;
        negative_s = 1;
    }
    else
        negative_s = 0;

    if ((unsigned int)quantized > TRIG_TABLE_SIZE)
    {
        quantized %= (2 * TRIG_TABLE_SIZE);

        if ((unsigned int)quantized > TRIG_TABLE_SIZE)
        {
            quantized = (2 * TRIG_TABLE_SIZE) - quantized;
              /* flip the negative */
            negative_s ^= 1;
        }
    }  /* if > TABLE_SIZE */

    *cos = COS_TABLE[quantized];
    *sin = SIN_TABLE[quantized];
    if (negative_s)
        *sin = -(*sin);
}

