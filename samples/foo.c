#include <stdint.h>
#include <stdio.h>
#include <math.h>

double foo(double x, int64_t y)
{
	return pow(x, (double)y);
}

int bar(double x, int64_t y, double *result)
{
	*result = pow(x, (double)y);
	return 0;
}
