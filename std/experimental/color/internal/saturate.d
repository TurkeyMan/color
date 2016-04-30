// Written in the D programming language.

/**
This module implements support for saturating integer arithmetic.

Authors:    Manu Evans
Copyright:  Copyright (c) 2015, Manu Evans.
License:    $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0)
Source:     $(PHOBOSSRC std/experimental/color/conv.d)
*/
module std.experimental.color.internal.saturate;

import std.traits;

template Promote(U)
{
    static if(is(U == byte)) alias Promote = short;
    else static if(is(U == short)) alias Promote = int;
    else static if(is(U == int)) alias Promote = long;
    else static if(is(U == long)) alias Promote = cent;
    else static if(is(U == ubyte)) alias Promote = ushort;
    else static if(is(U == ushort)) alias Promote = uint;
    else static if(is(U == uint)) alias Promote = ulong;
    else static if(is(U == ulong)) alias Promote = ucent;
    else static assert(false, "!");
}


// HAX: http://locklessinc.com/articles/sat_arithmetic/

pure nothrow @safe @nogc:


U addSat(U)(U a, U b) if(isIntegral!U && isUnsigned!U)
{
	U c = cast(U)(a + b);
	c |= -U(c < a);
	return c;

/+
    // runner up! (same result on both compilers)
    U c = cast(U)(a + b);
    if (c < a) // Can only happen due to overflow
    c = cast(U)-1;
    return c;
+/
}

int test(int a, int b)
{
    return addSat(a, b);
//    return addSat2(a, b);
//    return addSat3(a, b);
//    return addSat4(a, b);
}

U addSat(U)(U a, U b) if(isIntegral!U && isSigned!U)
{
    Unsigned!U ux = a;
	Unsigned!U uy = b;
	Unsigned!U res = ux + uy;

	// Calculate overflowed result. (Don't change the sign bit of ux)
	ux = (ux >> 31) + U.max;

	// Force compiler to use cmovns instruction
	if (cast(U)((ux ^ uy) | ~(uy ^ res)) >= 0)
		res = ux;

	return res;
}

U subSat(U)(U a, U b) if(isIntegral!U && isUnsigned!U)
{
/+
	u32b res = x - y;
	res &= -(res <= x);

	return res;
+/
    return 0;
}
U subSat(U)(U a, U b) if(isIntegral!U && isSigned!U)
{
/+
	u32b ux = x;
	u32b uy = y;
	u32b res = ux - uy;

	ux = (ux >> 31) + INT_MAX;

	/* Force compiler to use cmovns instruction */
	if ((s32b)((ux ^ uy) & (ux ^ res)) < 0)
	{
    res = ux;
	}

	return res;
+/
    return 0;
}

U mulSat(U)(U a, U b) if(isIntegral!U && isUnsigned!U)
{
/+
	u64b res = (u64b) x * (u64b) y;

	u32b hi = res >> 32;
	u32b lo = res;

	return lo | -!!hi;
+/
    return 0;
}
U mulSat(U)(U a, U b) if(isIntegral!U && isSigned!U)
{
/+
	s64b res = (s64b) x * (s64b) y;
	u32b res2 = ((u32b) (x ^ y) >> 31) + INT_MAX;

	s32b hi = (res >> 32);
	s32b lo = res;

	if (hi != (lo >> 31)) res = res2;

	return res;
+/
    return 0;
}

U divSat(U)(U a, U b) if(isIntegral!U && isUnsigned!U)
{
    // can not overflow...
    return 0;
}
U divSat(U)(U a, U b) if(isIntegral!U && isSigned!U)
{
/+
	/* Only one way to overflow, so test for and prevent it. */
	x += !((y + 1) | ((u32b) x + INT_MIN));

	return x / y;
+/
    return 0;
}



unittest
{

}
