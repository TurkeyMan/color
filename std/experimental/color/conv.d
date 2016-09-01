// Written in the D programming language.

/**
    This module implements various _color type conversions.

    Authors:    Manu Evans
    Copyright:  Copyright (c) 2015, Manu Evans.
    License:    $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0)
    Source:     $(PHOBOSSRC std/experimental/color/conv.d)
*/
module std.experimental.color.conv;

import std.experimental.color;
import std.experimental.color.rgb;
import std.experimental.color.xyz;
import std.experimental.color.hsx;
import std.experimental.color.normint;

import std.traits : isNumeric, isIntegral, isFloatingPoint, isSigned, isSomeChar, TemplateOf;
import std.typetuple : TypeTuple;

@safe pure nothrow @nogc:

/**
Detect whether $(D T) is a valid color component type.
*/
enum isColorComponentType(T) = isFloatingPoint!T || is(T == NormalizedInt!U, U);

/**
Detect whether $(D T) can represent a colour component.
*/
enum isColorScalarType(T) = isNumeric!T || is(T == NormalizedInt!U, U);

/**
Convert between color types.
*/
To convertColor(To, From)(From color) if(isColor!To && isColor!From)
{
    // no conversion is necessary
    static if(is(To == From))
        return color;

    // *** XYZ is the root type ***
    else static if(isXYZ!From && isXYZ!To)
    {
        alias F = To.ComponentType;
        return To(F(color.X), F(color.Y), F(color.Z));
    }

    // following conversions come in triplets:
    //   Type!U -> Type!V
    //   Type -> Parent
    //   Parent -> type

    // *** RGB triplet ***
    else static if(isRGB!From && isRGB!To)
    {
        alias ToType = To.ComponentType;
        alias FromType = From.ComponentType;

        auto src = color.tristimulusWithAlpha;

        static if(false && From.colorSpace == To.colorSpace && isIntegral!FromType && FromType.sizeof <= 2 &&
                    (From.linear != To.linear || !is(FromType == ToType)))
        {
            alias WorkType = WorkingType!(FromType, ToType);
            enum NumValues = 1 << (FromType.sizeof*8);

            // <= 16bit type conversion should use a look-up table
            shared immutable ToType[NumValues] conversionTable = {
                ToType[NumValues] table = void;
                foreach(i; 0..NumValues)
                {
                    WorkType v = convertPixelType!WorkType(cast(FromType)i);
                    static if(From.linear == false)
                        v = toLinear!(From.colorSpace)(v);
                    static if(To.linear == false)
                        v = toGamma!(To.colorSpace)(v);
                    table[i] = convertPixelType!ToType(v);
                }
                return table;
            }();

            static if(To.hasAlpha)
                return To(conversionTable[src[0]], conversionTable[src[0]], conversionTable[src[1]], conversionTable[src[2]], convertPixelType!ToType(src[3]));
            else
                return To(conversionTable[src[0]], conversionTable[src[0]], conversionTable[src[1]], conversionTable[src[2]]);
        }
        else static if(From.colorSpace == To.colorSpace && From.linear == To.linear)
        {
            // color space is the same, just do type conversion
            return To(cast(ToType)src[0], cast(ToType)src[1], cast(ToType)src[2], cast(ToType)src[3]);
        }
        else
        {
            // unpack the working values
            alias WorkType = WorkingType!(FromType, ToType);
            WorkType r = cast(WorkType)src[0];
            WorkType g = cast(WorkType)src[1];
            WorkType b = cast(WorkType)src[2];

            static if(From.linear == false)
            {
                r = toLinear!(From.colorSpace)(r);
                g = toLinear!(From.colorSpace)(g);
                b = toLinear!(From.colorSpace)(b);
            }
            static if(From.colorSpace != To.colorSpace)
            {
                enum toXYZ = RGBColorSpaceMatrix!(From.colorSpace, WorkType);
                enum toRGB = inverse(RGBColorSpaceMatrix!(To.colorSpace, WorkType));
                enum mat = multiply(toXYZ, toRGB);
                WorkType[3] v = multiply(mat, [r, g, b]);
                r = v[0]; g = v[1]; b = v[2];
            }
            static if(To.linear == false)
            {
                r = toGamma!(To.colorSpace)(r);
                g = toGamma!(To.colorSpace)(g);
                b = toGamma!(To.colorSpace)(b);
            }

            // convert and return the output
            static if(To.hasAlpha)
                return To(cast(ToType)r, cast(ToType)g, cast(ToType)b, cast(ToType)src[3]);
            else
                return To(cast(ToType)r, cast(ToType)g, cast(ToType)b);
        }
    }
    else static if(isRGB!From && isXYZ!To)
    {
        alias ToType = To.ComponentType;
        alias FromType = From.ComponentType;
        alias WorkType = WorkingType!(FromType, ToType);

        // unpack the working values
        auto src = color.tristimulus;
        WorkType r = cast(WorkType)src[0];
        WorkType g = cast(WorkType)src[1];
        WorkType b = cast(WorkType)src[2];

        static if(From.linear == false)
        {
            r = toLinear!(From.colorSpace)(r);
            g = toLinear!(From.colorSpace)(g);
            b = toLinear!(From.colorSpace)(b);
        }

        // transform to XYZ
        enum toXYZ = RGBColorSpaceMatrix!(From.colorSpace, WorkType);
        WorkType[3] v = multiply(toXYZ, [r, g, b]);
        return To(v[0], v[1], v[2]);
    }
    else static if(isXYZ!From && isRGB!To)
    {
        alias ToType = To.ComponentType;
        alias FromType = From.ComponentType;
        alias WorkType = WorkingType!(FromType, ToType);

        enum toRGB = inverse(RGBColorSpaceMatrix!(To.colorSpace, WorkType));
        WorkType[3] v = multiply(toRGB, [ WorkType(color.X), WorkType(color.Y), WorkType(color.Z) ]);

        static if(To.linear == false)
        {
            v[0] = toGamma!(To.colorSpace)(v[0]);
            v[1] = toGamma!(To.colorSpace)(v[1]);
            v[2] = toGamma!(To.colorSpace)(v[2]);
        }

        return To(cast(ToType)v[0], cast(ToType)v[1], cast(ToType)v[2]);
    }

    // *** xyY triplet ***
    else static if(isxyY!From && isxyY!To)
    {
        alias F = To.ComponentType;
        return To(F(color.x), F(color.y), F(color.Y));
    }
    else static if(isxyY!From && isXYZ!To)
    {
        alias F = To.ComponentType;
        if(color.y == F(0))
            return To(F(0), F(0), F(0));
        else
            return To(F(color.x*color.Y/color.y), F(color.Y), F((F(1)-color.x-color.y)*color.Y/color.y));
    }
    else static if(isXYZ!From && isxyY!To)
    {
        alias F = To.ComponentType;
        auto sum = color.X + color.Y + color.Z;
        if(sum == F(0))
            return To(WhitePoint!F.D65.x, WhitePoint!F.D65.y, F(0));
        else
            return To(F(color.X/sum), F(color.Y/sum), F(color.Y));
    }

    // *** HSx triplet ***
    else static if(isHSx!From && isHSx!To)
    {
        // HACK: cast through RGB (this works fine, but could be faster)
        return cast(To)cast(From.ParentColourSpace)from;
    }
    else static if(isHSx!From && isRGB!To)
    {
        import std.math : abs;

        alias ToType = To.ComponentType;
        alias WT = FloatTypeFor!ToType;

        auto c = color.tupleof;
        WT h = convertPixelType!WT(c[0]);
        WT s = convertPixelType!WT(c[1]);
        WT x = convertPixelType!WT(c[2]);

        WT C, m;
        static if(From.type == HSxType.HSV)
        {
            C = x*s;
            m = x - C;
        }
        else static if(From.type == HSxType.HSL)
        {
            C = (1 - abs(2*x - 1))*s;
            m = x - C/2;
        }
        else static if(From.type == HSxType.HSI)
        {
            C = s;
            m = x - (r+g+b)*WT(1.0/3.0);
        }
        else static if(From.type == HSxType.HCY)
        {
            C = s;
        }

        WT H = h/60;
        WT X = C*(1 - abs(H%2.0 - 1));

        WT r, g, b;
        if(H < 1)
            r = C, g = X, b = 0;
        else if(H < 2)
            r = X, g = C, b = 0;
        else if(H < 3)
            r = 0, g = C, b = X;
        else if(H < 4)
            r = 0, g = X, b = C;
        else if(H < 5)
            r = X, g = 0, b = C;
        else if(H < 6)
            r = C, g = 0, b = X;

        static if(From.type == HSxType.HCY)
        {
            enum YAxis = RGBColorSpaceMatrix!(From.colorSpace, WT)[1];
            m = x - (YAxis[0]*r + YAxis[1]*g + YAxis[2]*b); // Derive from Luma'
        }

        return To(convertPixelType!ToType(r+m), convertPixelType!ToType(g+m), convertPixelType!ToType(b+m));
    }
    else static if(isRGB!From && isHSx!To)
    {
        import std.algorithm : min, max;
        import std.math : abs;

        alias ToType = To.ComponentType;
        alias WT = FloatTypeFor!ToType;

        auto c = color.tristimulus;
        WT r = convertPixelType!WT(c[0]);
        WT g = convertPixelType!WT(c[1]);
        WT b = convertPixelType!WT(c[2]);

        WT M = max(r, g, b);
        WT m = min(r, g, b);
        WT C = M-m;

        // Calculate Hue
        WT h;
        if(C == 0)
            h = 0;
        else if(M == r)
            h = WT(60) * ((g-b)/C % WT(6));
        else if(M == g)
            h = WT(60) * ((b-r)/C + WT(2));
        else if(M == b)
            h = WT(60) * ((r-g)/C + WT(4));

        WT s, x;
        static if(To.type == HSxType.HSV)
        {
            x = M; // 'Value'
            s = x == 0 ? WT(0) : C/x; // Saturation
        }
        else static if(To.type == HSxType.HSL)
        {
            x = (M + m)/WT(2); // Lightness
            s = (x == 0 || x == 1) ? WT(0) : C/(1 - abs(2*x - 1)); // Saturation
        }
        else static if(To.type == HSxType.HSI)
        {
            x = (r + g + b)/WT(3); // Intensity
            s = x == 0 ? WT(0) : 1 - m/x; // Saturation
        }
        else static if(To.type == HSxType.HCY)
        {
            enum YAxis = RGBColorSpaceMatrix!(To.colorSpace, WT)[1];
            x = YAxis[0]*r + YAxis[1]*g + YAxis[2]*b; // Luma'
            s = C; // Chroma
        }

        return To(convertPixelType!ToType(h), convertPixelType!ToType(s), convertPixelType!ToType(x));
    }

    // *** fallback plan ***
    else
    {
        // cast along a conversion path to reach our target conversion
        alias Path = ConversionPath!(From, To);
        return convertColor!To(convertColor!(Path[0])(color));
    }
}

unittest
{
    // test RGB format conversions
    alias UnsignedRGB = RGB!("rgb", ubyte);
    alias SignedRGBX = RGB!("rgbx", byte);
    alias FloatRGBA = RGB!("rgba", float);

    static assert(cast(UnsignedRGB)SignedRGBX(0x20,0x30,-10)               == UnsignedRGB(0x40,0x60,0));
    static assert(cast(UnsignedRGB)FloatRGBA(1,0.5,0,1)                    == UnsignedRGB(0xFF,0x80,0));
    static assert(cast(UnsignedRGB)cast(FloatRGBA)UnsignedRGB(0xFF,0x80,0) == UnsignedRGB(0xFF,0x80,0));
    static assert(cast(FloatRGBA)UnsignedRGB(0xFF,0x80,0)                  == FloatRGBA(1,float(0x80)/float(0xFF),0,0));
    static assert(cast(FloatRGBA)SignedRGBX(127,-127,-128)                 == FloatRGBA(1,-1,-1,0));

    // test greyscale conversion
    alias UnsignedL = RGB!("l", ubyte);
    assert(cast(UnsignedL)UnsignedRGB(0xFF,0x20,0x40)   == UnsignedL(0x83));


    // TODO... we can't test this properly since DMD can't CTFE the '^^' operator! >_<

    alias XYZf = XYZ!float;

    // test RGB conversions
    alias sRGBA = RGB!("rgba", ubyte, false, RGBColorSpace.sRGB);
    alias lRGBA = RGB!("rgba", ushort, true, RGBColorSpace.sRGB);
    alias gRGBA = RGB!("rgba", byte, false, RGBColorSpace.sRGB_Gamma2_2);
    alias sRGBAf = RGB!("rgba", float, false, RGBColorSpace.sRGB);
    alias lRGBAf = RGB!("rgba", double, true, RGBColorSpace.sRGB);
    alias gRGBAf = RGB!("rgba", float, false, RGBColorSpace.sRGB_Gamma2_2);

    assert(cast(lRGBA)sRGBA(0xFF, 0xFF, 0xFF, 0xFF)           == lRGBA(0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF));
    assert(cast(gRGBA)sRGBA(0xFF, 0x80, 0x01, 0xFF)           == gRGBA(0x7F, 0x3F, 0x03, 0x7F));
    assert(cast(sRGBA)cast(XYZf)sRGBA(0xFF, 0xFF, 0xFF, 0xFF) == sRGBA(0xFF, 0xFF, 0xFF, 0));
    //...

    // test PackedRGB conversions
    //...

    // test xyY conversions
    alias xyYf = xyY!float;

    static assert(cast(xyYf)XYZf(0.5, 1, 0.5) == xyYf(0.25, 0.5, 1));
    static assert(cast(XYZf)xyYf(0.5, 0.5, 1) == XYZf(1, 1, 0));

    // check the degenerate cases
    static assert(cast(xyYf)XYZf(0, 0, 0) == xyYf(WhitePoint!float.D65.x, WhitePoint!float.D65.y, 0));
    static assert(cast(XYZf)xyYf(0.5, 0, 1) == XYZf(0, 0, 0));

    // test HSx conversions
    alias HSVf = HSV!float;
    alias HSLf = HSL!float;
    alias HSIf = HSI!float;
    alias HCYf = HCY!float;

    static assert(cast(HSVf)RGB8(255, 0, 0) == HSVf(0, 1, 1));
    static assert(cast(RGB8)HSVf(0, 1, 1) == RGB8(255, 0, 0));
    static assert(cast(RGB8)HSVf(60, 0.5, 0.5) == RGB8(128, 128, 64));

    static assert(cast(HSLf)RGB8(255, 0, 0) == HSLf(0, 1, 0.5));
    static assert(cast(RGB8)HSLf(0, 1, 0.5) == RGB8(255, 0, 0));
    static assert(cast(RGB8)HSLf(60, 0.5, 0.5) == RGB8(191, 191, 64));

    static assert(cast(HSIf)RGB8(255, 0, 0) == HSIf(0, 1, 1.0/3));
    static assert(cast(HSIf)RGB8(255, 255, 0) == HSIf(60, 1, 2.0/3));
//    static assert(cast(RGB8)HSIf(0, 1, 1) == RGB8(1, 0, 0));

//    pragma(msg, cast(RGB8)HCYf(0, 0, 1));
//    static assert(cast(HCYf)RGB8(255, 0, 0) == HCYf(0, 1, 1));
//    static assert(cast(RGB8)HCYf(0, 1, 1) == RGB8(1, 0, 0));

    //...
}


/**
* Create a color from hex strings in the standard forms: (#/$/0x)rgb/argb/rrggbb/aarrggbb
*/
Color colorFromString(Color = RGB8, C)(const(C)[] hex) if(isSomeChar!C)
{
    static ubyte val(C c)
    {
        if(c >= '0' && c <= '9')
            return cast(ubyte)(c - '0');
        else if(c >= 'a' && c <= 'f')
            return cast(ubyte)(c - 'a' + 10);
        else if(c >= 'A' && c <= 'F')
            return cast(ubyte)(c - 'A' + 10);
        else
            assert(false, "Invalid hex string");
    }

    if(hex.length > 0 && (hex[0] == '#' || hex[0] == '$'))
        hex = hex[1..$];
    else if(hex.length > 1 && (hex[0] == '0' && hex[1] == 'x'))
        hex = hex[2..$];

    if(hex.length == 3)
    {
        ubyte r = val(hex[0]);
        ubyte g = val(hex[1]);
        ubyte b = val(hex[2]);
        return cast(Color)RGB8(cast(ubyte)(r | (r << 4)), cast(ubyte)(g | (g << 4)), cast(ubyte)(b | (b << 4)));
    }
    if(hex.length == 4)
    {
        ubyte a = val(hex[0]);
        ubyte r = val(hex[1]);
        ubyte g = val(hex[2]);
        ubyte b = val(hex[3]);
        return cast(Color)RGBA8(cast(ubyte)(r | (r << 4)), cast(ubyte)(g | (g << 4)), cast(ubyte)(b | (b << 4)), cast(ubyte)(a | (a << 4)));
    }
    if(hex.length == 6)
    {
        ubyte r = cast(ubyte)(val(hex[0]) << 4) | val(hex[1]);
        ubyte g = cast(ubyte)(val(hex[2]) << 4) | val(hex[3]);
        ubyte b = cast(ubyte)(val(hex[4]) << 4) | val(hex[5]);
        return cast(Color)RGB8(r, g, b);
    }
    if(hex.length == 8)
    {
        ubyte a = cast(ubyte)(val(hex[0]) << 4) | val(hex[1]);
        ubyte r = cast(ubyte)(val(hex[2]) << 4) | val(hex[3]);
        ubyte g = cast(ubyte)(val(hex[4]) << 4) | val(hex[5]);
        ubyte b = cast(ubyte)(val(hex[6]) << 4) | val(hex[7]);
        return cast(Color)RGBA8(r, g, b, a);
    }
    else
    {
        // TODO: should we look up colors from the W3C color table by name?

        assert(false, "Invalid hex string!");
    }
}

///
unittest
{
    // common hex formats supported:

    // 3 digits
    static assert(colorFromString("F80") == RGB8(0xFF, 0x88, 0x00));
    static assert(colorFromString("#F80") == RGB8(0xFF, 0x88, 0x00));
    static assert(colorFromString("$F80") == RGB8(0xFF, 0x88, 0x00));
    static assert(colorFromString("0xF80") == RGB8(0xFF, 0x88, 0x00));

    // 6 digits
    static assert(colorFromString("FF8000") == RGB8(0xFF, 0x80, 0x00));
    static assert(colorFromString("#FF8000") == RGB8(0xFF, 0x80, 0x00));
    static assert(colorFromString("$FF8000") == RGB8(0xFF, 0x80, 0x00));
    static assert(colorFromString("0xFF8000") == RGB8(0xFF, 0x80, 0x00));

    // 4/8 digita (/w alpha)
    static assert(colorFromString!RGBA8("#8C41") == RGBA8(0xCC, 0x44, 0x11, 0x88));
    static assert(colorFromString!RGBA8("#80CC4401") == RGBA8(0xCC, 0x44, 0x01, 0x80));
}


package:

// try and use the preferred float type
// if the int type exceeds the preferred float precision, we'll upgrade the float
template FloatTypeFor(IntType, RequestedFloat = float)
{
    static if(IntType.sizeof > 2)
        alias FloatTypeFor = double;
    else
        alias FloatTypeFor = RequestedFloat;
}

// find the fastest type to do format conversion without losing precision
template WorkingType(From, To)
{
    static if(isFloatingPoint!From && isFloatingPoint!To)
    {
        static if(From.sizeof > To.sizeof)
            alias WorkingType = From;
        else
            alias WorkingType = To;
    }
    else static if(isFloatingPoint!To)
        alias WorkingType = To;
    else static if(isFloatingPoint!From)
        alias WorkingType = FloatTypeFor!To;
    else
    {
        // small integer types can use float and not lose precision
        static if(From.sizeof <= 2 && To.sizeof <= 2)
            alias WorkingType = float;
        else
            alias WorkingType = double;
    }
}

// find the conversion path from one distant type to another
template ConversionPath(From, To)
{
    template isParentType(Parent, Of)
    {
        static if(isXYZ!Of)
            enum isParentType = false;
        else static if(isInstanceOf!(TemplateOf!Parent, Of.ParentColor))
            enum isParentType = true;
        else
            enum isParentType = isParentType!(Parent, Of.ParentColor);
    }

    template FindPath(From, To)
    {
        static if(isInstanceOf!(TemplateOf!To, From))
            alias FindPath = TypeTuple!(To);
        else static if(isParentType!(From, To))
            alias FindPath = TypeTuple!(FindPath!(From, To.ParentColor), To);
        else
            alias FindPath = TypeTuple!(From, FindPath!(From.ParentColor, To));
    }

    alias Path = FindPath!(From, To);
    static if(Path.length == 1 && !is(Path[0] == From))
        alias ConversionPath = Path;
    else
        alias ConversionPath = Path[1..$];
}
