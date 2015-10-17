module std.experimental.log.logformatspec;

import std.format;
import std.conv;
import std.string;
import std.array;
import std.traits;

import std.stdio;

import std.experimental.log.logger;

/** These represent the possible types of formatted items that can appear
 *  in a log format specifier string, including string literals (no formatting),
 *  runtime variadic parameters (like the log message string itself), 
 *  environment variables, timestamps, or any of pre-defined or user-defined
 *  named inline substitution values (the "..." in "%{...}").
 */
package static enum FormatElementType : ubyte
{
    Null                        = 0x00,   /// default = absence of any meaningful value   
    LiteralString               = 0x01,   /// means an unformatted, verbatim string
    VariadicArgument            = 0x02,   /// means format spec of a runtime variadic parameter
    
    // Logging-system values (0x10 to 0x3F)
    LogLevel                    = 0x10,
    Category                    = 0x11,
    Tag                         = 0x12,
    AllTags                     = 0x13,
    
    LogEntryLength              = 0x30,   /// Number of bytes of this log message
    Timestamp                   = 0x3F,
    
    // Source-level values (0x40 to 0x7F)
    SourceFileName              = 0x41,
    SourceFilePath              = 0x42,
    SourceLineNumber            = 0x43,
    PackageName                 = 0x50,
    ModuleName                  = 0x51,
    FullModuleName              = 0x52,
    FunctionName                = 0x53,
    CompilerName                = 0x7E,
    CompilerVersion             = 0x7F,
    
    // Runtime-level values (0x80 to 0xCF)
    HostName                    = 0x80,
    ExecutableName              = 0x90,
    ProcessID                   = 0xA0,
    ProcessName                 = 0xA1,
    ThreadID                    = 0xB0,
    ThreadName                  = 0xB1,
    
    EnvVariable                 = 0xC0,
    
    CPUstuff                    = 0xC1,
    
    // Exception / Error values (0xD0 to 0xFD)
    ExceptionTypeName           = 0xD0,
    ExceptionFullTypeName       = 0xD1,
    ExceptionMessage            = 0xD2,
    ExceptionFile               = 0xD3,
    ExceptionLine               = 0xD4,
    
    ErrorTypeName               = 0xE0,  // Note: AssertErrors are Errors
    ErrorFullTypeName           = 0xE1,
    ErrorMessage                = 0xE2,
    ErrorFile                   = 0xE3,
    ErrorLine                   = 0xE4,
    
    TraceInfo                   = 0xF0,
    
    UserDefined                 = 0xFE,   // Means that a user-defined value        
    Undetermined                = 0xFF,   // temporary during parsing
}

/**
 *  Holds a format specifier for a given instance of a format string
 *  and provides many of the "low level" formatting functions and
 *  pieces of the format string to enable LogFormatter to perform
 *  formatting in as efficient way as possible.
 *  This follows the FormatSpec struct in many ways and in fact
 *  stores an instance of a FormatSpec for each element in the primary
 *  format specifier string.  This implements an extension to the
 *  D / C printf format specifier syntax via the %{...} notation.
 */
class LogFormatSpec
{
    private static FormatElementType[string] fmtElemTypeLookup;
    
    private static this()
    {
        foreach(member; EnumMembers!FormatElementType)
        {
            fmtElemTypeLookup[to!string(member)] = member;
        }
        // the following values should never match a string inside {...}
        //  for inline substitution values
        fmtElemTypeLookup.remove("Null");
        fmtElemTypeLookup.remove("LiteralString");
        fmtElemTypeLookup.remove("VariadicArgument");
        fmtElemTypeLookup.remove("UserDefined");
        fmtElemTypeLookup.remove("Undetermined");
    }
    
    private static enum FormatCase : byte
    {
        Undefined = 0,
        NormalCase = 1,
        LowerCase = 2,
        UpperCase = 3
    } 
    
    /**
     *  Represents one segment of the format string, either
     *  string literal, or a reference to a run-time string to be
     *  inserted/formatted along with associated format specifier.
     */
    private struct LogFormatSpecElement
    {
        auto elemType = FormatElementType.Null;
        string literal;
        FormatSpec!char singleSpec;
        FormatCase elemCase;
    }
    
    package LogFormatSpecElement[] elems;
    
    this(string fmtString)
    {
        auto f = FormatSpec!char(fmtString);
        // pass 1: build the array of elements by "munching" the format string
        while (f.trailing.length > 0) 
        {
            auto a = appender!string();
            f.writeUpToNextSpec(a);
            if ( a.data.length > 0)
            {
                elems ~= LogFormatSpecElement(FormatElementType.LiteralString, a.data, singleSpec("%s"));
            }
            if ( f.spec == '{' )   // we have one of our "extended syntax" keywords to follow
            {
                uint pos = 0;
                while ( f.trailing.length > pos && f.trailing[pos] != '}' )
                {
                    pos++;
                }
                auto keywordSpec = f;    // to capture any formatting specifiers that preceded the '{'
                keywordSpec.spec = 's';   // may be reset later with default
                keywordSpec.trailing = null;   // this will be a "singleSpec"
                if ( keywordSpec.indexStart != 0 || keywordSpec.indexEnd != 0)
                {
                    //  should log error/warning that positional parameters do
                    //     not apply to inline {...} substitution values and will be ignored.
                    keywordSpec.indexStart = keywordSpec.indexEnd = 0;                    
                }
                elems ~= LogFormatSpecElement(FormatElementType.Undetermined, f.trailing[0 .. pos].idup, keywordSpec);
                // push f.trailing so that it effectively "skips" over the %{...}
                pos++;
                if ( f.trailing.length <= pos )
                    f.trailing = "";
                else
                    f.trailing = f.trailing[pos .. $];
            }
            else   // some other format specifier
            {
                auto fmtSpec = f;    // to capture the format specification
                fmtSpec.trailing = "";
                elems ~= LogFormatSpecElement(FormatElementType.VariadicArgument, "", fmtSpec);
            }
        }
        // pass 2: determine the FormatElementType enum values for each of the elements
        //         and peel off optional format spec suffix "%."
        for(int i = 0; i < elems.length; i++)
        {
            auto elem = &elems[i];
            if ( elem.elemType == FormatElementType.Undetermined )
            {
                bool useDefaultFormat = true;
                if( elem.literal.length >= 2 && elem.literal[$ - 2] == '%' )
                {
                    useDefaultFormat = false;
                    elem.singleSpec.spec = elem.literal[$ - 1];
                    elem.literal = elem.literal[0 .. $ - 2];
                }
                auto cleanLiteral = removechars(elem.literal, " \t");
                if(cleanLiteral in fmtElemTypeLookup)
                {
                    elem.elemType = fmtElemTypeLookup[cleanLiteral];
                    elem.elemCase = FormatCase.NormalCase;
                }
                else
                {
                    // maybe it's upper or lower case matching
                    foreach(strType, typeVal ; fmtElemTypeLookup)
                    {
                        if( toLower(strType) == cleanLiteral )
                        {
                            elem.elemType = typeVal;
                            elem.elemCase = FormatCase.LowerCase;
                            break;
                        }
                        else if ( toUpper(strType) == cleanLiteral )
                        {
                            elem.elemType = typeVal;
                            elem.elemCase = FormatCase.UpperCase;
                            break;
                        }
                    }
                    if ( elem.elemCase == FormatCase.Undefined )  // no match :(
                    {
                        // replace this "%{...}" with the failed internal string "..."
                        //   should also, throw an exception or log this failure somehow 
                        elems = elems[0 .. i] ~ LogFormatSpecElement(
                            FormatElementType.LiteralString, elem.literal, singleSpec("%s"))
                            ~ elems[i + 1 .. $];
                        useDefaultFormat = false;
                    }
                }
                if (useDefaultFormat) 
                {
                    // additionally check that the user defined NO other FormatSpec options/flags
                    FormatSpec!char fs;   //default initialized
                    if ( elem.singleSpec == fs )
                    {
                        // we can assume the user specified nothing special and can use the
                        //  default spec for the given element type
                        elem.singleSpec = getDefaultFormat(elem.elemType);
                    }
                }
            }
        }
        // "pass 3"?  contract consecutive string literals that
        //   may have been introduced above in pass 2
        contractLiterals();        
    }
    
    private FormatSpec!char getDefaultFormat(FormatElementType elemType)
    {
        // default assumption is everything is a string (%s), with a 
        //  few numeric exceptions
        switch (elemType)
        {
            case FormatElementType.ProcessID:
            case FormatElementType.ThreadID:
            case FormatElementType.LogEntryLength:
            case FormatElementType.SourceLineNumber:
            case FormatElementType.ExceptionLine:
            case FormatElementType.ErrorLine: 
                return singleSpec("%d");
            default:
                return singleSpec("%s");  // otherwise use assumed default "%s"
        }
    }
    
    /**
     *  Iterates over the elems array and combines any consecutive
     *  StringLiteral's into 1.
     */
    private void contractLiterals()
    {
        for(int i = 0; i + 1 < elems.length ; i++)
        {
            if ( elems[i].elemType == FormatElementType.LiteralString &&
                 elems[i+1].elemType == FormatElementType.LiteralString)
            {
                elems[i].literal ~= elems[i+1].literal;
                elems = elems[0 .. i + 1] ~ elems[i + 2 .. $];
                i--;
            }
        }
    }
    
    package void bakeValue(FormatElementType fmtElemType, string value)
    {
        foreach(ref elem ; elems)
        {
            if (elem.elemType == fmtElemType)
            {
                switch (elem.elemCase)
                {
                    case FormatCase.NormalCase:
                        elem.literal = value;
                        break;
                    case FormatCase.LowerCase:
                        elem.literal = toLower(value);
                        break;
                    case FormatCase.UpperCase:
                        elem.literal = toUpper(value);
                        break;
                    default:
                        elem.literal = "Case undefined in bakeValue - should never happen!";
                }
                auto w = appender!string();
                formatValue(w, elem.literal, elem.singleSpec);
                elem.literal = w.data;
                elem.elemType = FormatElementType.LiteralString;
            }
        }
        contractLiterals();
    }
    
    package void bakeValue(FormatElementType fmtElemType, int value)
    {
        foreach(ref elem ; elems)
        {
            if (elem.elemType == fmtElemType)
            {
                auto w = appender!string();
                formatValue(w, value, elem.singleSpec);
                elem.literal = w.data;
                elem.elemType = FormatElementType.LiteralString;
            }
        }
        contractLiterals();
    }
    
    override string toString()
    {
        auto a = appender!string();
        foreach(elem ; fmtElemTypeLookup)
        {
            a.put(to!string(elem) ~ ", ");
        }
        a.put('\n');
        foreach(ref elem ; elems)
        {
            a.formattedWrite("Element: %s, Spec: %s, Str: '%s', Case: %s\n",
                to!string(elem.elemType), elem.singleSpec.spec,
                elem.literal, to!string(elem.elemCase));
        }
        
        return a.data;
    }
}

unittest
{
    alias LogFormatSpec.FormatCase FormatCase;
    alias LogFormatSpec.LogFormatSpecElement LogFormatSpecElement;
    auto sSpec = singleSpec("%s");
    
    auto spec = new LogFormatSpec("%s");
    assert(spec.elems.length == 1);
    assert(spec.elems[0] == LogFormatSpecElement(FormatElementType.VariadicArgument, "", sSpec));
        
    spec = new LogFormatSpec("%{LogLevel} | %0x");
    assert(spec.elems.length == 3);
    assert(spec.elems[0] == LogFormatSpecElement(FormatElementType.LogLevel, "LogLevel", sSpec, FormatCase.NormalCase));
    assert(spec.elems[1] == LogFormatSpecElement(FormatElementType.LiteralString, " | ", sSpec));
    assert(spec.elems[2] == LogFormatSpecElement(FormatElementType.VariadicArgument, "", singleSpec("%0x")));
    
    spec = new LogFormatSpec("%{Category}%{CATEGORY}%{category}");
    assert(spec.elems.length == 3);
    assert(spec.elems[0] == LogFormatSpecElement(FormatElementType.Category, "Category", sSpec, FormatCase.NormalCase));
    assert(spec.elems[1] == LogFormatSpecElement(FormatElementType.Category, "CATEGORY", sSpec, FormatCase.UpperCase));
    assert(spec.elems[2] == LogFormatSpecElement(FormatElementType.Category, "category", sSpec, FormatCase.LowerCase));
    
    spec = new LogFormatSpec("%{ should not match } - %{Undetermined}");
    assert(spec.elems.length == 1);
    assert(spec.elems[0] == LogFormatSpecElement(FormatElementType.LiteralString, " should not match  - Undetermined", sSpec));
    
    spec = new LogFormatSpec("%{Process ID} | %#06{ProcessID %x}");
    assert(spec.elems.length == 3);
    assert(spec.elems[0] == LogFormatSpecElement(FormatElementType.ProcessID, "Process ID", singleSpec("%d"), FormatCase.NormalCase));
    assert(spec.elems[2] == LogFormatSpecElement(FormatElementType.ProcessID, "ProcessID ", singleSpec("%#06x"), FormatCase.NormalCase));
    spec.bakeValue(FormatElementType.ProcessID, 12);
    assert(spec.elems.length == 1);
    //writeln(spec);
    assert(spec.elems[0] == LogFormatSpecElement(FormatElementType.LiteralString, "12 | 0x000c", singleSpec("%d"), FormatCase.NormalCase));    
}
