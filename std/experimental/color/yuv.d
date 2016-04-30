module std.experimental.color.yuv;

import std.experimental.color;
import std.experimental.color.colorspace;
import std.experimental.color.xyz : XYZ, isXYZ;
import std.experimental.color.rgb : RGB, isRGB;

import std.traits: isFloatingPoint, isIntegral, isSigned, isUnsigned, isSomeChar, Unqual;
import std.typetuple: TypeTuple;
import std.typecons: tuple;

@safe: pure: nothrow: @nogc:

/**
Detect whether $(D_INLINECODE T) is a member of the YUV color family.
*/
enum isYUV(T) = isInstanceOf!(YUV, T);

/**
Alias for a Y'CbCr color. This is the standard used by most SDTV, HDTV, SDR UHDTV broadcasts, and compressed digital formats including MPEG, JPEG, etc.
*/
alias YCbCr(CT = ubyte, RGBColorSpace cs = RGBColorSpace.HDTV) = HSx!(YUVType.YCbCr, CT, cs);

/**
Alias for a YcCbcCrc color. Often used for higher-fidelity encoding of SDR UHDTV (Rec.2020) broadcast signals.
*/
alias YcCbcCrc(CT = ubyte, RGBColorSpace cs = RGBColorSpace.UHDTV) = HSx!(YUVType.YcCbcCrc, CT, cs);

/**
Alias for a Y'IQ color. This is a standard used by early NTSC broadcast television, but has been phased-out in favour of Y'CbCr in most broadcasts.
*/
alias YIQ(CT = ubyte, RGBColorSpace cs = RGBColorSpace.NTSC) = HSx!(YUVType.YIQ, CT, cs);

/**
Alias for a Y'UV color. This is the standard used by PAL (exclusing PAL-N) broadcast television.
*/
alias YUV(CT = ubyte, RGBColorSpace cs = RGBColorSpace.PAL_SECAM) = HSx!(YUVType.YUV, CT, cs);

/**
Alias for a Y'DbDr color. This is the standard used by SECAM and PAL-N broadcast television.
*/
alias YDbDr(CT = ubyte, RGBColorSpace cs = RGBColorSpace.PAL_SECAM) = HSx!(YUVType.YDbDr, CT, cs);

/**
Define a YUV family color type.
*/
enum YUVType
{
    /** Y′CbCr. This is the standard used by most SDTV, HDTV, SDR UHDTV broadcasts, and compressed digital formats including MPEG, JPEG, etc. */
    YCbCr,
    /** YcCbcCrc. Often used for higher-fidelity encoding of SDR UHDTV (Rec.2020) broadcast signals. */
    YcCbcCrc,
    /** Y′UV. This is the standard used by PAL (exclusing PAL-N) broadcast television. */
    YUV,
    /** Y’IQ. This is a standard used by early NTSC broadcast television, but has been phased-out in favour of Y'CbCr in most broadcasts. */
    YIQ,
    /** Y'DbDr. This is the standard used by SECAM and PAL-N broadcast television. */
    YDbDr,
}

/**
YUV color space is used primarily as an encoding scheme for broadcast television, and often compressed storage formats including MPEG and JPEG.

Params: type_ = A type from the YUVType enum.
ComponentType_ = Type for the color channels. May be unsigned integer or floating point type.
colorSpace_ = Color will be within the specified RGB color space.
*/
struct YUV(YUVType type_, ComponentType_ = ubyte, RGBColorSpace colorSpace_ = RGBColorSpace.HDTV) if (isFloatingPoint!ComponentType_ || isUnsigned!ComponentType_)
{
@safe: pure: nothrow: @nogc:

    static if (isFloatingPoint!ComponentType_)
    {
        /** Type of the color components. */
        alias ComponentType = ComponentType_;
    }
    else
    {
        /** Type of the color components. */
        alias ComponentType = NormalizedInt!ComponentType_;
    }

    /** The colors color space. */
    enum RGBColorSpace colorSpace = colorSpace_;
    /** The color space descriptor. */
    enum RGBColorSpaceDesc!F colorSpaceDesc(F = double) = rgbColorSpaceDef!F(colorSpace_);
    /** The color type from the YUV family. */
    enum YUVType type = type_;

    // mixin the color channels according to the type
    mixin("ComponentType " ~ Components!type[0] ~ " = 0;");
    mixin("ComponentType " ~ Components!type[1] ~ " = 0.5;");
    mixin("ComponentType " ~ Components!type[2] ~ " = 0.5;");

    /** Construct a color from YUV components. */
    this(ComponentType Y, ComponentType U, ComponentType V)
    {
        mixin("this." ~ Components!type[0] ~ " = Y;");
        mixin("this." ~ Components!type[1] ~ " = U;");
        mixin("this." ~ Components!type[2] ~ " = V;");
    }

    static if (!isFloatingPoint!ComponentType_)
    {
        /** Construct a color from YUV components. */
        this(ComponentType.IntType Y, ComponentType.IntType U, ComponentType.IntType V)
        {
            mixin("this." ~ Components!type[0] ~ " = ComponentType(Y);");
            mixin("this." ~ Components!type[1] ~ " = ComponentType(U);");
            mixin("this." ~ Components!type[2] ~ " = ComponentType(V);");
        }
    }

    /**
    Cast to other color types.

    This cast is a convenience which simply forwards the call to convertColor.
    */
    Color opCast(Color)() const if (isColor!Color)
    {
        return convertColor!Color(this);
    }


    // operators
    typeof(this) opBinary(string op, S)(S rh) const if(isFloatingPoint!S && (op == "*" || op == "/" || op == "^^"))
    {
        alias T = Unqual!(typeof(this));
        T res = this;
        foreach(c; XYZComponents)
            mixin(ComponentExpression!("res._ #= rh;", c, op));
        return res;
    }
    ref typeof(this) opOpAssign(string op, S)(S rh) if(isFloatingPoint!S && (op == "*" || op == "/" || op == "^^"))
    {
        foreach(c; XYZComponents)
            mixin(ComponentExpression!("_ #= rh;", c, op));
        return this;
    }

package:

//    alias ParentColor = RGB!("rgb", ComponentType_, type == YUVType.YcCbcCrc, colorSpace); // TODO: we can handle conversion for int types directly, but for simplicity, we'll use float for conversion for now.
    alias ParentColor = RGB!("rgb", FloatTypeFor!ComponentType_, type == YUVType.YcCbcCrc, colorSpace);

    static To convertColorImpl(To, From)(From color) if (isYUV!From && isYUV!To)
    {
        alias ToType = To.ComponentType;
        alias FromType = From.ComponentType;

        auto src = color.tupleof;

        static if (From.type == To.type && From.colorSpace == To.colorSpace)
        {
            // color space is the same, just do type conversion
            return To(cast(ToType)src[0], cast(ToType)src[1], cast(ToType)src[2], cast(ToType)src[3]);
        }
        else
        {
            // YUV encoding or RGB colourspace changed
            return cast(To)cast(To.ParentColor)cast(From.ParentColor)color;
        }
    }
    unittest
    {
    }

    static To convertColorImpl(To, From)(From color) if (isYUV!From && isRGB!To)
    {
        static assert(To.colorSpace == From.colorSpace, "TODO: mitmatching colorspace...");

        alias FromType = From.ComponentType;
        alias ToType = To.ComponentType;
        alias WorkType = From.ParentColor.ComponentType;

        enum mat = fromYUV!(type, WorkType, From.colorSpace);

        // TODO: if From color is only luminance, then we can do efficient conversion...
//        static if (From.hasComponent!'l')

        alias src = color.tupleof;
        WorkType[3] yuv = [cast(WorkType)src[0], cast(WorkType)src[1], cast(WorkType)src[2]];

        static if (!isFloatingPoint!FromType)
        {
            // TODO: shift U, V into [-1, 1]
            // YCbCr UV is [-0.5    - 0.5   ]
            // YIQ    I is [-0.5957 - 0.5957]
            //        Q is [-0.5226 - 0.5226]
            // YUV    U is [-0.436  - 0.436 ]
            //        V is [-0.615  - 0.615 ]
            // YDbDr UV is [-1.333  - 1.333 ]
            static assert(false, "TODO");
        }
//        static if (tv or pc Y range?)
//        {
//            // expand Y if it was clipped for TV?
//        }
        static if (type == YUVType.YcCbcCrc)
        {
            // YcCbcCrc performs gamma compression after YUV conversion
            yuv.Y = To.colorSpaceDesc!WorkType.fromGamma(yuv.Y);
        }

        WorkType[3] rgb = multiply(mat, [yuv[0], yuv[1], yuv[2]]);

        From.ParentColor t(rgb[0], rgb[1], rgb[2]);
        return cast(To)t;
    }
    // TODO: YcCbcCrc can be converted directly to XYZ more efficiently
//    static To convertColorImpl(To, From)(From color) if (isYUV!From && isXYZ!To)
    unittest
    {
    }

    static To convertColorImpl(To, From)(From color) if (isRGB!From && isYUV!To)
    {
        static assert(To.colorSpace == From.colorSpace, "TODO: mitmatching colorspace...");

        alias ToType = To.ComponentType;
        alias WorkType = To.ParentColor.ComponentType;

        enum mat = toYUV!(type, WorkType, To.colorSpace);

        // TODO: if From color is only luminance, then we can do efficient conversion...
//        static if (From.hasComponent!'l')

        auto c = cast(To.ParentColor)color;
        auto src = c.tristimulus;

        WorkType[3] yuv = multiply(mat, [src[0], src[1], src[2]]);

        static if (type == YUVType.YcCbcCrc)
        {
            // YcCbcCrc performs gamma compression after YUV conversion
            yuv.Y = To.colorSpaceDesc!WorkType.toGamma(yuv.Y);
        }
//        static if (tv or pc Y range?)
//        {
//            // clip Y to fit the TV range?
//        }
        static if (!isFloatingPoint!ToType)
        {
            // TODO: U, V need to be shifted into 0-1 space...
            // YCbCr UV is [-0.5   - 0.5]
            // YUV    U is [-0.436 - 0.436]
            //        V is [-0.615 - 0.615]
            // YDbDr UV is [-1.333 - 1.333]
            static assert(false, "TODO");
        }

        return To(cast(ToType)yuv[0], cast(ToType)yuv[1], cast(ToType)yuv[2]);
    }
    // TODO: YcCbcCrc can be converted directly from XYZ more efficiently
//    static To convertColorImpl(To, From)(From color) if (isXYZ!From && isYUV!To)
    unittest
    {
    }

private:
    template Components(YUVType type)
    {
        static if (type == YUVType.YCbCr)
            alias Components = TypeTuple!("Y","Cb","Cr");
        else static if (type == YUVType.YcCbcCrc)
            alias Components = TypeTuple!("Yc","Cbc","Crc");
        else static if (type == YUVType.YUV)
            alias Components = TypeTuple!("Y","U","V");
        else static if (type == YUVType.YIQ)
            alias Components = TypeTuple!("Y","I","Q");
        else static if (type == YUVType.YDbDr)
            alias Components = TypeTuple!("Y","Db","Dr");
    }

    alias AllComponents = Components!type_;
}

void f()
{
    import std.conv;
    alias TGT = YUV!(ubyte, false, RGBColorSpace.HDTV);
    RGB8 c;
    TGT c2;

    c.convertColor!(TGT);
    c2.convertColor!(RGB8);

    pragma(msg, "to:");
    pragma(msg, "YCbCr ", toYUV!(YUVType.YCbCr, double, RGBColorSpace.NTSC));
//    pragma(msg, "YIQ   ", toYUV!(YUVType.YIQ, double, RGBColorSpace.NTSC));
    pragma(msg, "YUV   ", toYUV!(YUVType.YUV, double, RGBColorSpace.PAL_SECAM));
    pragma(msg, "YUVHD ", toYUV!(YUVType.YUV, double, RGBColorSpace.HDTV));
    pragma(msg, "YDbDr ", toYUV!(YUVType.YDbDr, double, RGBColorSpace.PAL_SECAM));
    pragma(msg, "from:");
    pragma(msg, "YCbCr ", fromYUV!(YUVType.YCbCr, double, RGBColorSpace.NTSC));
//    pragma(msg, "YIQ   ", fromYUV!(YUVType.YIQ, double, RGBColorSpace.NTSC));
    pragma(msg, "YUV   ", fromYUV!(YUVType.YUV, double, RGBColorSpace.PAL_SECAM));
    pragma(msg, "YUVHD ", fromYUV!(YUVType.YUV, double, RGBColorSpace.HDTV));
    pragma(msg, "YDbDr ", fromYUV!(YUVType.YDbDr, double, RGBColorSpace.PAL_SECAM));
}

unittest
{
    //...
}

private:

template toYUV(YUVType type, F, RGBColorSpace colorspace)
{
    enum cs = rgbColorSpaceDef!F(colorspace);

    enum R = cs.red.Y;
    enum G = cs.green.Y;
    enum B = cs.blue.Y;

    static if (type == YUVType.YCbCr || type == YUVType.YcCbcCrc)
    {
        enum U = F(0.5/(1 - B));
        enum V = F(0.5/(1 - R));
    }
    else static if (type == YUVType.YIQ)
    {
        static assert(false, "Can't find a source for the YIQ conversion process!");
    }
    else static if (type == YUVType.YUV)
    {
        enum U = F(0.436/(1 - B));
        enum V = F(0.615/(1 - R));
    }
    else static if (type == YUVType.YDbDr)
    {
        enum U = F(1.333724/(1 - B));
        enum V = F(-1.333935/(1 - R));
    }

    enum F[3][3] toYUV = [[  R,      G,      B     ],
                          [ -R*U,   -G*U,    U-B*U ],
                          [  V-R*V, -G*V,   -B*V   ]];
}
template fromYUV(YUVType type, F, RGBColorSpace colorspace)
{
    enum cs = rgbColorSpaceDef!F(colorspace);

    enum R = cs.red.Y;
    enum G = cs.green.Y;
    enum B = cs.blue.Y;

    static if (type == YUVType.YCbCr || type == YUVType.YcCbcCrc)
    {
        enum U = F(0.5);
        enum V = F(0.5);
    }
    else static if (type == YUVType.YIQ)
    {
        static assert(false, "Can't find a source for the YIQ conversion process!");
    }
    else static if (type == YUVType.YUV)
    {
        enum U = F(0.436);
        enum V = F(0.615);
    }
    else static if (type == YUVType.YDbDr)
    {
        enum U = F(1.333724);
        enum V = F(-1.333935);
    }

    enum F[3][3] fromYUV = [[ F(1), F(0),           F(1-R)/V       ],
                            [ F(1), F(B-1)*B/(U*G), F(R-1)*R/(V*G) ],
                            [ F(1), F(1-B)/U,       F(0)           ]];
}
