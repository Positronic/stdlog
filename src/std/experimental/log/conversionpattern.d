/**
 * Provides facilities for working with a log conversion pattern which
 *  is a printf-like string that specifies how values of various sources
 *  and types are assembled and formatted into a string or byte array.
 *
 * Copyright: Copyright Doug Nickerson 2015
 * License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Authors:   Doug Nickerson
 */

//          Copyright Doug Nickerson 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module std.experimental.log.conversionpattern;

import std.experimental.log.conversionpattern_data;
import std.exception;

/**
 * Similar to std.format we define a small-ish domain specific language
 * which serves 2 primary purposes:
 *  1) to specify the formatting of each item into string or byte-array type
 *     and any intervening literal strings (e.g. the format string to printf)
 *  2) to uniquely identify a variable via it's source and/or name (including
 *     mapping to user-defined values) (e.g. the arguments to printf)
 * The first purpose is already handled, for the most part, by the std.format
 *  FormatSpec and formattedWrite which function similarly to printf.  But we
 *  expand upon that syntax here to support some additional formatting and
 *  alignment and more-significantly to implement the second purpose.
 * Note for the second purpose a so called "conversion word" is used to identify
 *  a single variable like "%threadId", but it can also refer to a mapping or array
 *  like "%mdc{key}" which requires an additional key for the lookup, or it can
 *  even refer to a "conversion function" like "%replace(){}" which takes a
 *  sub-pattern as input and modifies it.  So, a conversion "word" is not always a
 *  single variable name.  In the examples above, the conversion words are "threadId"
 *  "mdc" and "replace".  Conversion words are the central part of conversion pattern
 *  "items" (see below), because they define the "thing" that is being converted or
 *  type of conversion.
 *
 * Conversion Pattern String:
 *  Conversion patterns are (UTF-8) strings of characters interspersed with 
 *  conversion pattern items.  Conversion pattern items start with a '%' in the printf-like
 *  syntax, but are decorated not only by the usual format flags, width, precision,
 *  conversion specifier character, and conversion word, but also 3 other optional
 *  parts: format arguments (in square brackets [] ), a sub-pattern (in parenthesis () )
 *  or conversion parameters (in curly brackets {} ).
 * 1. Format arguments [] are needed when either the width or precision format modifiers,
 *  or both are indicated by an asterisk '*'.  Just like in printf implementations,
 *  this allows the width and/or precision formatting to be controlled by a variable and
 *  can enable some unique dynamic logging capabilities (data itself controls formatting
 *  of other data).
 *  Format arguments must contain 1 or 2 comma-separated integer-typed variables which
 *  must evaluate or be convertible to an integer at the time of the log event.  Their
 *  values will be treated as std.format.formattedWrite does. 
 * 2. A sub-pattern () contains a conversion pattern string which is first evaluated to
 *  an output string and operated on as if it were a single input value (of string
 *  type).  See the 'replace(){}' conversion word (function) for example.  Sub-patterns
 *  allow grouping of multiple values which can then be formatted as a whole (aligned,
 *  padded, truncated) via the usual format modifiers applicable to strings.  In this
 *  case, the sub-pattern enclosed in parenthesis need not be preceded by a conversion
 *  word, but must have some format specifier between the '%' and '('.  In fact, this is
 *  the only case where the conversion word is not required.
 * 3. Conversion parameters {} come last, after the conversion word and sub-pattern ()
 *  if any.  Certain conversion words require 1 or more (sometimes optional) parameters.
 *  For instance, %mdc{key} takes 1 parameter, the key to lookup user-defined values.
 *  The %replace(p){'find','rep'} always takes 2 parameters, the first is a RegEx to find
 *  and the second is the replacement. Note, it also requires a sub-pattern, p, inside the
 *  parenthesis over which to do the replacement.
 * Lastly, be aware that an empty conversion parameters list "{}" can be used on ANY
 *  conversion pattern item to terminate it.  This might be needed in ambiguous cases,
 *  to denote where a conversion pattern item ends and literal characters start.
 *  For example, "%date%nHello" would cause the parser to try to find a conversion
 *  word of "nHello" which isn't defined and probably not what you intended.  So, you can 
 *  insert an empty parameter list: "%date%n{}Hello" which clarifies that "%n" is the
 *  conversion pattern item, followed by the literal "Hello". 
 * 
 * ConversionPattern's have the following grammar:
 *  ConversionPattern:
 *      ConversionPatternItem*
 *  ConversionPatternItem:
 *      $(B '%') $(I FormatSpec) $(B '!')? $(I ConversionWord) $(I SubPattern) $(I ConvParams) 
 *      $(B '%') $(I FormatSpec) $(I SubPattern)  //note: FormatSpec and SubPattern must both be non-empty
 *      $(B '%(') $(I FormatString) $(B '%)') $(I ConversionWord) $(I SubPattern) $(I ConvParams) // see std.format
 *      $(B '%%')
 *      $(I OtherCharacterExceptPercent)
 *  FormatSpec:
 *      $(I Position) $(I Flags) $(I MinWidth) $(I MaxWidth) $(I Precision) $(I FormatChar)? $(I FormatArgs)
 *  ConversionWord:
 *      'm' | 'msg' | 'pid' | 'threadId' | 'mdc' | 'date' | 'replace' | 'env'
 *  SubPattern:
 *      empty
 *      '(' $(I ConversionPattern) ')'
 *  ConvParams:
 *      empty
 *      '{' $(I ParamList) '}'
 *      '{' $(I DateParam) '}'
 *      '{' $(I MDCParam) '}'
 *  FormatArgs:
 *      empty
 *      '[' $(I ParamList) ']'
 *  ParamList:
 *      empty
 *      Parameter (',' Parameter)*
 *  MDCParam:
 *      Parameter (':-' Parameter)?          //mimicking Logback syntax
 *  Parameter:
 *      ('-' | '_' | '.' | Alpha | Digit)+   //Note: whitespace is allowed before and after Parameter, but will be ignored
 *      ''' (AnyCharExceptSingleQuote)* '''  //Note: use ' or " to include whitespace or other special chars in the Parameter value, but again surrounding whitespace is optional
 *      '"' (AnyCharExceptDoubleQuote)* '"'
 *  DateParam:
 *      something   //it's own internal conversion pattern
 *      something ',' Timezone
 *
 *  Note: this domain specific language is weakly typed in a few places (not just strings everywhere).
 *    In general, the fall-back type is "string" since ultimately most log output gets formatted into
 *     a string.  However, check the documentation for given conversion words and conversion functions
 *     as several operate with numeric values, date/time values, arrays, etc. prior to the rendered string type.
 *    Also, be aware of std.format's capability to format arrays and any D type via an overloaded toString().
 *     This can be very useful in the context of the logging system as it allows the developer to
 *     easily "dump" any object with a runtime-controllable format specifier and logging logic.
 */

/** **/

/**
 * These represent the possible types of formatted items (conversion words) that
 *  can appear in a conversion pattern string, including: string literals (no formatting),
 *  runtime variadic parameters (like the log message string itself), logging system values
 *  (like log level, category, tags, mode, Event stats, etc.), environment variables,
 *  source info (line, function, class, etc. of log statement), run-time values (process Id,
 *  thread ID, any MDC value, etc.), timestamps, error/exception/trace info, string conversion
 *  functions, colors, or any of pre-defined or user-defined conversion word (the "..." in "%...").
 */
package static enum WordType : ubyte
{
    Null                    = 0x00,   /// default = absence of any meaningful value   
    LiteralString           = 0x01,   /// means an unformatted, verbatim string
    VariadicArgument        = 0x02,   /// means a (lazy) variadic parameter to the .log call 
    LogMessage              = 0x03,   /// THE log message content (m, msg)
    NewLine                 = 0x04,   /// %n or %endl -- just a shorthand so conversion patterns be be on one line
                                          ///  gets inserted as part of a LiteralString segment, equivalent to \n
    
    // Logging-system values (0x10 to 0x3F)
    LogLevel                = 0x10,
    Category                = 0x11,
    CategoryRoot            = 0x12,
    CategoryLeaf            = 0x13,
    Tag                     = 0x18,
    AllTags                 = 0x19,
    LogMode                 = 0x1E,
    PrevLogMode             = 0x1F,
        
    LogLength               = 0x20,   /// Number of bytes of this log message
    EventOccurenceCount     = 0x21,
    EventFilteredCount      = 0x22,
    EventLoggedCount        = 0x23,
    EventSuppressedCount    = 0x24,
    EventSuppressedTime     = 0x25,
    EventRate               = 0x26,
    EventLoggedRate         = 0x27,
    //ResetCount{filtered}  etc. -- is an action and not an output -- set in rules somewhere
    //ResetMetric{logRate}  etc. -- is an action (useful for controlling quality of statistics)
    // probably just have reset(...)  where ... is any metric, counter, sequence numbers, etc.
    Metric                  = 0x2B,   /// Lookup some named or user-defined metric
    Rate                    = 0x2C,   /// the rate at which a given parameter/metric occurs/elapses/increments, etc. (numeric timestamped array input)
    Stat                    = 0x2D,   /// compute a stat (min,mean,max) of some value (numeric array input)
    Average                 = 0x2E,   /// compute an average over a given time window (numeric timestamped array input) or the last N samples, etc.
    TimeSince               = 0x2F,
    
    LogConfigFileName       = 0x30,
    LogConfigFilePath       = 0x31,
    GlobalSeqNumber         = 0x32,   /// Global (shared in same process) sequence number, incremented for every event in the whole program
    ThreadLocalSeqNumber    = 0x33,   /// Thread-local sequence number, incremented for every event originating in that thread
    FiberLocalSeqNumber     = 0x34,   /// Fiber-local sequence number, incremented for events origination within the fiber
    LocalSeqNumber          = 0x35,   /// developer-provided sequence integer

    // Shouldn't be needed: see conversionpattern_data "WD[WT.Array...." for explanation
    //ArrayFormat             = 0x38,   /// %( ... %) -- indicates std.format's array (grouping) syntax
    SubPatternOnly          = 0x39,   /// %-20.30(subpat) -- only case where NO conversion word is needed, but format must exist
    Date                    = 0x3E,
    Timestamp               = 0x3F,
    
    // Source-level values (0x40 to 0x7F)
    SourceFileName          = 0x41,
    SourceFilePath          = 0x42,
    SourceLineNumber        = 0x43,
    FunctionName            = 0x44,  /// caller function, method, delegate, etc. name 
    FunctionFQN             = 0x45,  /// as above, but fully qualified name
    FunctionSig             = 0x46,  /// basic caller function, method, delegate signature
    FunctionPrettySig       = 0x47,  /// full function signature, pretty-printed with all attributes, parameters, return type, annotations, etc.
    Location                = 0x48,  /// compound format similar to Java "%funcFQN(%file:%line)"
    
    ClassName               = 0x50,  /// containing class, struct, union, etc. name
    ClassFQN                = 0x51,  /// as above, but fully qualified name
    PackageName             = 0x52,
    Module                  = 0x53,
    ModuleFQN               = 0x54,
    
    CompileTimestamp        = 0x60,
    CompilerName            = 0x7E,
    CompilerVersion         = 0x7F,
    
    // Runtime-level values (0x80 to 0xAF)
    HostName                = 0x80,
    ShortHostName           = 0x81,
    HostIPAddress           = 0x82,
    CPUStuff                = 0x83,
    ExecutableName          = 0x88,
    ExecutablePath          = 0x89,
    CommandLineArg          = 0x8A,
    CommandLineFull         = 0x8B,
    WorkingDirectory        = 0x8F,
    ProcessID               = 0x90,
    ProcessName             = 0x91,
    EnvVariable             = 0x9F,  /// env
    ThreadID                = 0xA0,
    ThreadName              = 0xA1,
    MDC                     = 0xA2,
    
    // Exception / Error values (0xB0 to 0xDF)
    ExceptionTypeName       = 0xB0,
    ExceptionFullTypeName   = 0xB1,
    ExceptionMessage        = 0xB2,
    ExceptionFile           = 0xB3,
    ExceptionLine           = 0xB4,
    
    ErrorTypeName           = 0xC0,  // Note: AssertErrors are Errors
    ErrorFullTypeName       = 0xC1,
    ErrorMessage            = 0xC2,
    ErrorFile               = 0xC3,
    ErrorLine               = 0xC4,
    
    TraceInfo               = 0xD0,
    
    // String conversion functions (0xE0 to 0xEF)
    Replace                 = 0xE0,   /// RegEx find and replace in sub-pattern
    Crop                    = 0xE1,   /// RegEx match, then crop anything not matching
    Strip                   = 0xE2,   /// Strip away any leading or trailing whitespace
    ToLower                 = 0xE3,   /// all lowercase version of sub-pattern
    ToUpper                 = 0xE4,   /// all uppercase version of sub-pattern
    Capitalize              = 0xE5,   /// Captialize first letter of each word and lowercase all other letters
    Center                  = 0xE6,   /// Center the sub-pattern within a given field width
    Fill                    = 0xE7,   /// Fill leading and trailing whitespace with a given character 
    Wrap                    = 0xE8,   /// Wrap long text to a fixed-width multiline "paragraph"
    ToBase64                = 0xE9,   /// Create a Base64 encoded string from binary data / raw bytes
    EncodeURI               = 0xEA,   /// Take any UTF-8 string and escape any URI-invalid characters
    DecodeURI               = 0xEB,   /// Take any URI possibly with escaped characters and return the unescaped UTF-8
    Round                   = 0xEC,   /// Round numeric value to certain decimal place (accepts numeric format specifiers)
    ToJSON                  = 0xED,   /// Output a JSON-formatted version of any D-type
    ToUUID                  = 0xEE,   /// Generate a UUID for a sub-pattern
    HTML                    = 0xEF,   /// Wraps the sub-pattern in a given HTML tag
                                      /// example: "%html(subpat){td}" produces "<td>subpat<\td>"
        
    // Colors (0xF0 to 0xFD)
    Black                   = 0xF0,   /// Colors are formatted like %black(sub-pattern)
    Red                     = 0xF1,   ///  but can also be made bold with the option: %red(sub-pattern){bold}
    Green                   = 0xF2,
    Yellow                  = 0xF3,
    Blue                    = 0xF4,
    Magenta                 = 0xF5,
    Cyan                    = 0xF6,
    White                   = 0xF7,
    Gray                    = 0xF8,
    Color                   = 0xFB,   /// Specify color code (for HTML
    Highlight               = 0xFC,   /// %highlight colors the sub-pattern based on the LogLevel of the LogEvent
    Style                   = 0xFD,   /// ANSI style
    
    CustomCW                = 0xFE,   /// Represents a developer-defined conversion word/function        
    Undetermined            = 0xFF,   // temporary during parsing
}

/**
 * Hash indexed on all pre-defined conversion word strings (synonyms) mapping
 *  to WordType.  This can be used as a fast look-up during pattern
 *  parsing to then get WordInfo via WordData defined in module
 *  conversionpattern_data.
 */
/*private immutable WordType[string] WordTypeLookup;

static this()
{
    import std.stdio;
 
}
*/

/**********************************************************************
 * Signals an error during conversion pattern parsing.
 */
class PatternParseException : Exception
{
    @safe pure nothrow
    this()
    {
        super("conversion pattern parse error");
    }

    @safe pure nothrow
    this(string msg, string fn = __FILE__, size_t ln = __LINE__, Throwable next = null)
    {
        super(msg, fn, ln, next);
    }
}

private alias enforcePat = enforce!PatternParseException;

/**
 * The common 'unit' of a conversion pattern string: a single conversion item.
 *  Represents the components: FormatSpec and any formatting arguments,
 *  conversion word, reference to sub-pattern, and conversion parameters.
 *  These are 'stored' as they are at parse-time in a manner that enables
 *  as efficient a lookup / resolution / evaluation as possible at conversion-time.
 */
struct ConvPatternItem(Char)
    if (is(Unqual!Char == Char))
{
    FormatSpec!Char fmtSpec;  /// Contains flags, precision, minimum width, etc.

    /**
       Maximum width, default $(D 0).
     */
    int maxWidth = 0;   // not part of the std.format FormatSpec, but very common
                        //  in well-known logging libraries
    /**
       Special value for widths or precision ('*')
          - same meaning as in FormatSpec
     */
    alias FormatSpec!Char.DYNAMIC DYNAMIC;
    
    const(Char)[] fmtArgs;  // for now, but should implement as a concrete value/enum for quick lookup of the int(s) during logging
    auto wordType = WordType.Null;
    const(Char)[] literal;
    ConversionPattern!Char subPattern;
    //ConvParameters params;  implement with a Variant[] or maybe have specialized ConvPatternItems for CWs that take parameters
    const(Char)[] params;   // for now
}

/**
 * A General handler for $(D printf) style format specifiers augmented with conversion 'words'
 *  sub-patterns, and optional parameters -- the "conversion pattern" (a.k.a. "layout" in some
 *  logging frameworks). At parse-time, used for building an array of ConvPatternItems
 *  which comprise the entire pattern.  Construction proceeds like std.format.FormatSpec
 *  (much code shamelessly copied) by incrementally parsing off items from the front until
 *  the "trailing" string is empty.
 *
 * During logging, the conversion pattern is used to build the actual log entry output.
 *  This is done incrementally, so a given instance of a ConversionPattern can hold the
 *  original pattern as the user provided in config, any intermediate partially-rendered
 *  state, or the final, completely rendered log output (a single item containing a literal). 
 */
struct ConversionPattern(Char)
    if (is(Unqual!Char == Char))
{
    // the items or "words" that each contain their own formatting,
    // sub-patterns, parameters, literal string value, etc.
    ConvPatternItem[] items;
    
    /**
       $(D _trailing) contains the rest of the conversion pattern string.
     */
    const(Char)[] trailing;
    
    /**
       Construct a new $(D ConversionPattern) using the pattern string $(D pattern),
       no processing is done until needed.
     */
    this(in Char[] pattern) @safe pure
    {
        trailing = pattern;
    }
    
    bool writeUpToNextSpec(OutputRange)(OutputRange writer)
    {
        if (trailing.empty)
            return false;
        for (size_t i = 0; i < trailing.length; ++i)
        {
            if (trailing[i] != '%') continue;
            put(writer, trailing[0 .. i]);
            trailing = trailing[i .. $];
            enforcePat(trailing.length >= 2, `Unterminated pattern specifier: "%"`);
            trailing = trailing[1 .. $];

            if (trailing[0] != '%')
            {
                // Spec found. Fill up the spec, and bailout
                fillUp();
                return true;
            }
            // Doubled! Reset and Keep going
            i = 0;
        }
        // no conversion pattern item spec found
        put(writer, trailing);
        trailing = null;
        return false;
    }
    
    // shameless duplication of code from std.format.FormatSpec and
    //  trivial modification was cheap, but may prove useful as some
    //  functional logic may change over time.
    // Also, due to the similarity of this struct and FormatSpec,
    //  specific tests for bugs discovered in FormatSpec in the past
    //  would be wise to regression test here
    unittest
    {
        import std.array;
        auto w = appender!(char[])();
        auto f = ConversionPattern("abc%ndef%nghi");
        f.writeUpToNextSpec(w);
        assert(w.data == "abc", w.data);
        assert(f.trailing == "def%nghi", text(f.trailing));
        f.writeUpToNextSpec(w);
        assert(w.data == "abcdef", w.data);
        assert(f.trailing == "ghi");
        // test with embedded %%s
        f = ConversionPattern("ab%%cd%%ef%ng%%h%nij");
        w.clear();
        f.writeUpToNextSpec(w);
        assert(w.data == "ab%cd%ef" && f.trailing == "g%%h%nij", w.data);
        f.writeUpToNextSpec(w);
        assert(w.data == "ab%cd%efg%h" && f.trailing == "ij");
        // bug4775
        f = ConversionPattern("%%%n");
        w.clear();
        f.writeUpToNextSpec(w);
        assert(w.data == "%" && f.trailing == "");
        f = ConversionPattern("%%%%%n%%");
        w.clear();
        while (f.writeUpToNextSpec(w)) continue;
        assert(w.data == "%%%");

        f = ConversionPattern("a%%b%%c%");
        w.clear();
        assertThrown!FormatException(f.writeUpToNextSpec(w));
        assert(w.data == "a%b%c" && f.trailing == "%");
    }
    
    private void fillUp()
    {
        // Now the real 'meat' of the parser implementation for the
        //  domain-specific language as defined above (for ConvPatternItem)
        // Assume that trailing is on the character immediately after the '%'
        //   '%%' case is already taken care of
        
        // Check for "%(" or "%-(" cases, which are std.format's array (grouping)
        //  syntax - let singleSpec handle it? -- but still need to look-ahead
        //  scan for the closing "%)"
        //   Skip format spec, conversion word and sub-pattern below and enforce
        //     that there be a {} single parameter (which must also resolve to an array type)
        
        // First, determine if we have any part of a FormatSpec at all
        //   without actually parsing it (FormatSpec will do that).
        // However, we need to remove the max width if present and
        //   add the default format specifier character 's' if none is given
        // then call singleSpec
        
        // Note: if positional argument was given '$' then remember to enforce
        //   that below if the conversion word does not use position
        // However, only 'WARN' if it's extraneous and will be ignored
        
        // Second, record format arguments, if any
        //  remove the optional '!' separator if any
        
        // Next, lookup the conversion word using the defining AA
        //  and save the WordType enum value
        // if it's a custom user-defined conversion word,
        //  verify that we have it registered and if not, throw an
        //  exception for 'unknown conversion word'
        // If no conversion word at all, next char must be '(' which starts
        //   a "sub-pattern only" case - otherwise, exception "no conv word"
        // use defined properties of the conversion word for the logic below
        
        // Enforce constraints that the conversion word may have, or that the format
        //  implies (is a numeric fmt spec being applied to a string value, etc.?)
        
        // If the conversion word expects a sub-pattern enforce it, if
        //  it's optional, check, if it's not used at all but a '(' is next,
        //  assume it's part of the pattern literal.
        // Extract the entire sub-pattern string (counting non-escaped parens
        //   etc.) and recursively construct the sub-ConversionPattern
        //  noting to use specialized sub-pattern treatment for %date or 
        //   functions that expect a single value of a given type, etc.
        
        // Parse the conversion params '{}' even if the conversion word doesn't
        //  take any parameters (it's always optional to end a %convWord with "{}")
        // But, if the conversion word requires a particular number of parameters
        //  enforce that.  Also enforce required types of parameters, etc.
        // Call the delegate associated with the conversion word to parse each parameter
                
        //lastly add a new ConvPatternItem onto the items array 
    }
    
    unittest
    {
        // oh where to being!
        //check each type of overall syntax in the DSL
        // check various whitespace in argument and parameter lists
        // check each code path that throws
        // try different array (grouping) syntax with and without {}, etc.
        // exercise each of the conversion words that needs special sub-pattern or
        //  parameter value parsing logic.
        // check some deeply nested sub-patterns
        // check parens and curly brackets within quoted params, etc.
        //   or anything that might confuse the look-ahead parts of the scanner
        // check "stopping" logic for a '(' immediately following a conversion word
        //   which doesn't use a sub-pattern, or similar cases, if any
    }
}  

/**
Helper function that returns a $(D ConvPatternItem) for a single
specifier given in $(D pat)

Used exclusively in unit tests ?

Params:
    pat = A conversion specifier (conversion pattern item string)

Returns:
    A $(D ConvPatternItem) with the specifier parsed.

Enforces giving only one specifier to the function.
  */
ConvPatternItem!Char singleItem(Char)(Char[] pat)
{
    import std.conv : text;
    enforce(fmt.length >= 2, new Exception("pattern must be at least 2 characters long"));
    enforce(fmt.front == '%', new Exception("pattern must start with a '%' character"));

    static struct DummyOutputRange {
        void put(C)(C[] buf) {} // eat elements
    }
    auto a = DummyOutputRange();
    auto spec = FormatSpec!Char(fmt);
    //dummy write
    spec.writeUpToNextSpec(a);

    enforce(spec.trailing.empty,
            new Exception(text("Trailing characters in fmt string: '", spec.trailing)));

    return spec;
}
