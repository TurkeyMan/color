// Written in the D programming language.

/**
This module implements support for normalised int's.

Authors:    Manu Evans
Copyright:  Copyright (c) 2015, Manu Evans.
License:    $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0)
Source:     $(PHOBOSSRC std/experimental/color/conv.d)
*/
module std.experimental.color.internal.normint;

import std.traits;
import std.typetuple : TypeTuple;

private alias Signed = std.traits.Signed;
private alias Unsigned = std.traits.Unsigned;
private alias isSigned = std.traits.isSigned;
private alias isUnsigned = std.traits.isUnsigned;
/+
// TODO: is there a better way to do this? I was unable to influence any of the templates in std.traits that the existing templates depend on
template Signed(T) if(is(T == NormalizedInt!U, U))
{
    alias Signed = NormalizedInt!(std.traits.Signed!(typeof(T.val)));
}
template Unsigned(T) if(is(T == NormalizedInt!U, U))
{
    alias Unsigned = NormalizedInt!(std.traits.Unsigned!(typeof(T.val)));
}
template isSigned(T) if(is(T == NormalizedInt!U, U))
{
    alias isSigned = std.traits.isSigned!(typeof(T.val));
}
template isUnsigned(T) if(is(T == NormalizedInt!U, U))
{
    alias isUnsigned = std.traits.isUnsigned!(typeof(T.val));
}


struct NormalizedInt(T) if(isIntegral!T)
{
    this(T v)
    {
        val = v;
    }
    this(F)(F v) if(isFloatingPoint!F)
    {
        val = v;
    }
    T val;
}


pragma(msg, Signed!(NormalizedInt!uint));
+/
// get the best working type for operations
template ResultType(T, U)
{
    static if(isSigned!T && isSigned!U)
    {
        static if(T.sizeof == 1 && U.sizeof == 1)
            alias ResultType = byte;
        else static if(T.sizeof <= 2 && U.sizeof <= 2)
            alias ResultType = short;
        else static if(T.sizeof <= 4 && U.sizeof <= 4)
            alias ResultType = int;
        else
            alias ResultType = long;
    }
    else static if(isUnsigned!T && isUnsigned!U)
    {
        static if(T.sizeof == 1 && U.sizeof == 1)
            alias ResultType = ubyte;
        else static if(T.sizeof <= 2 && U.sizeof <= 2)
            alias ResultType = ushort;
        else static if(T.sizeof <= 4 && U.sizeof <= 4)
            alias ResultType = uint;
        else
            alias ResultType = ulong;
    }
    else
    {
        static if(T.sizeof == 1 && U.sizeof == 1)
            alias ResultType = short;
        else static if(T.sizeof <= 2 && U.sizeof <= 2)
            alias ResultType = int;
        else
            alias ResultType = long;
    }
}
template WorkType(T, U)
{
    static if(isSigned!T || isSigned!U)
    {
        static if(T.sizeof == 1 && U.sizeof == 1)
            alias WorkType = short;
        else static if(T.sizeof <= 2 && U.sizeof <= 2)
            alias WorkType = int;
        else
            alias WorkType = long;
    }
    else
    {
        static if(T.sizeof == 1 && U.sizeof == 1)
            alias WorkType = ushort;
        else static if(T.sizeof <= 2 && U.sizeof <= 2)
            alias WorkType = uint;
        else
            alias WorkType = ulong;
    }
}

struct SS
{
    alias this x;
//    alias this y;

//    int x() { return 10; }
    float y() { return 10; }
}

R normalisedAdd(T, U, R = ResultType!(T, U))(T a, U b)
{
    R x = convertNormInt!R(a);
    R y = convertNormInt!R(b);
    ushort s = cast(ushort)(a+b);
    return cast(ubyte)(-(s>>8)) | cast(ubyte)s;
//    return cast(R)((x > R.min - y) ? R.min : x + y);
/+
    auto r = x + y;
    if(r > R.max)
        r = R.max;
    else static if(isSigned!R)
    {
        if(r < R.min)
            r = R.min;
    }
    return cast(R)r;
+/
}

R normalisedSubtract(T, U, R = ResultType!(T, U))(T a, U b)
{
    R x = convertNormInt!R(a);
    R y = convertNormInt!R(b);
    auto r = x - y;
    static if(isUnsigned!R)
    {
        if(x <= y)
            r = 0;
    }
    else
    {
        if(r < R.min)
            r = R.min;
        if(r > R.max)
            r = R.max;
    }
    return cast(R)r;
}
/+
export __gshared byte xx;
export __gshared ubyte uxx;
void adds(byte a, byte b) nothrow @nogc
{
    xx = normalisedAdd(a, b);
}
void addu(ubyte a, ubyte b) nothrow @nogc
{
    uxx = normalisedAdd(a, b);
}

void subs(byte a, byte b) nothrow @nogc
{
    xx = normalisedSubtract(a, b);
}
void subu(ubyte a, ubyte b) nothrow @nogc
{
    uxx = normalisedSubtract(a, b);
}
+/
// convert a float to a normalised int (with saturation)
To floatToNormInt(To, From)(From f) if(isFloatingPoint!From && isIntegral!To)
{
    if(f >= 1)
        return To.max;
    else static if(isSigned!To)
    {
        if(f <= -1)
            return -To.max;
    }
    else
    {
        if(f <= 0)
            return 0;
    }
    return cast(To)(T.max * f);
}

// convert a normalised int to a float
To normIntToFloat(To, From)(From i) if(isIntegral!From && isFloatingPoint!To)
{
    static if(isSigned!From)
        return max(i * To(1.0/From.max), To(-1.0));
    else
        return i * To(1.0/From.max);
}

// converts directly between fixed-point color types, without doing float conversions
// ** this should be tested for performance; we can optimise the small->large conversions with table lookups, maybe imul?
To convertNormInt(To, From)(From i) if(isIntegral!To && isIntegral!From)
{
    template Iota(alias start, alias end)
    {
        static if(end == start)
            alias Iota = TypeTuple!();
        else
            alias Iota = TypeTuple!(Iota!(start, end-1), end-1);
    }
    enum Bits(T) = T.sizeof*8;

    static if(isUnsigned!To && isUnsigned!From)
    {
        static if(Bits!To <= Bits!From)
            return To(i >> (Bits!From-Bits!To));
        else
        {
            To r;

            enum numReps = Bits!To/Bits!From;
            foreach(j; Iota!(0, numReps))
                r |= To(i) << (j*Bits!From);

            return r;
        }
    }
    else static if(isUnsigned!To)
    {
        if(i < 0) // if i is negative, return 0
            return 0;
        else
        {
            enum Sig = Bits!From-1;
            static if(Bits!To < Bits!From)
                return cast(To)(i >> (Sig-Bits!To));
            else
            {
                To r;

                enum numReps = Bits!To/Sig;
                foreach(j; Iota!(1, numReps+1))
                    r |= To(cast(Unsigned!From)(i&From.max)) << (Bits!To - j*Sig);

                enum remain = Bits!To - numReps*Sig;
                static if(remain)
                    r |= cast(Unsigned!From)(i&From.max) >> (Sig - remain);

                return r;
            }
        }
    }
    else static if(isUnsigned!From)
    {
        static if(Bits!To <= Bits!From)
            return To(i >> (Bits!From-Bits!To+1));
        else
        {
            Unsigned!To r;

            enum numReps = Bits!To/Bits!From;
            foreach(j; Iota!(0, numReps))
                r |= Unsigned!To(i) << (j*Bits!From);

            return To(r >> 1);
        }
    }
    else
    {
        static if(Bits!To <= Bits!From)
            return cast(To)(i >> (Bits!From-Bits!To));
        else
        {
            enum Sig = Bits!From-1;
            enum Fill = Bits!To - Bits!From;

            To r = To(i) << Fill;

            enum numReps = Fill/Sig;
            foreach(j; Iota!(1, numReps+1))
                r |= Unsigned!To(cast(Unsigned!From)(i&From.max)) << (Fill - j*Sig);

            enum remain = Fill - numReps*Sig;
            static if(remain)
                r |= cast(Unsigned!From)(i&From.max) >> (Sig - remain);

            return r;
        }
    }
}

unittest
{
    // static asserts since these should all ctfe:

    // unsigned -> unsigned
    static assert(convertNormInt!ubyte(ushort(0x3765)) == 0x37);
    static assert(convertNormInt!ushort(ubyte(0x37)) == 0x3737);
    static assert(convertNormInt!ulong(ubyte(0x35)) == 0x3535353535353535);

    // signed -> unsigned
    static assert(convertNormInt!ubyte(short(-61)) == 0);
    static assert(convertNormInt!ubyte(short(0x3795)) == 0x6F);
    static assert(convertNormInt!ushort(byte(0x37)) == 0x6EDD);
    static assert(convertNormInt!ulong(byte(0x35)) == 0x6AD5AB56AD5AB56A);

    // unsigned -> signed
    static assert(convertNormInt!byte(ushort(0x3765)) == 0x1B);
    static assert(convertNormInt!short(ubyte(0x37)) == 0x1B9B);
    static assert(convertNormInt!long(ubyte(0x35)) == 0x1A9A9A9A9A9A9A9A);

    // signed -> signed
    static assert(convertNormInt!byte(short(0x3795)) == 0x37);
    static assert(convertNormInt!byte(short(-28672)) == -112);
    static assert(convertNormInt!short(byte(0x37)) == 0x376E);
    static assert(convertNormInt!short(byte(-109)) == -27866);
    static assert(convertNormInt!long(byte(-45)) == -3195498973398505005);
}
