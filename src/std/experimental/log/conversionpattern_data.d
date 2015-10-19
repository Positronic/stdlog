/**
 * Declares a large amount of static data which enumerates the properties,
 *  types, allowed values, descriptions, examples, etc. for
 *  Conversion Patterns (std.experimental.log.conversionpattern).
 * Used as not only a definitive reference for the details of the domain
 *  specific language, but also as input and expected output used to drive
 *  unit tests and generation of user documentation.
 *
 * Copyright: Copyright Doug Nickerson 2015
 * License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Authors:   Doug Nickerson
 */

//          Copyright Doug Nickerson 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module std.experimental.log.conversionpattern_data;

import std.experimental.log.conversionpattern : WordType;
import std.traits : EnumMembers;
import std.conv : to;
import std.typecons : Tuple;

/**
 * Denotes possible data types that a "symbol" - be that a conversion word
 *  it's sub-pattern or one of the conversion parameters must be.
 */
enum SymbolDataType : ubyte
{
    Null         = 0x00,   // type not applicable -  could apply to paramDataType since
                           //   not all words take params, but every word evaluates to something
                           //   therefore, this value is invalid for WordInfo.dataType
    String       = 0x01,   // the typical type (only type in some logging frameworks)
    Ident        = 0x02,   // a string, but it must also meet certain requirements (no spaces, etc.)
                           //   usually an identifier (symbol name) for something like MDC key or evaluator name
    Integer      = 0x10,   // ProcessID, ThreadID, occurrence count, etc.
    Float        = 0x11,   // floating point -- statistics, time intervals, etc.
    Numeric      = 0x12,   // either Integer or Float
    Metric       = 0x13,   // a numeric optionally with an associated timestamp
    IntegerSlice = 0x1F,   // slice notation with integer values:  "3..12"
    Array        = 0x20,   // an array of elements of any type (FormatSpec implements %(...%) )
    Varies       = 0x70,   // means the symbol could take on any number of types
    
    // Conversion Parameter only types (parse types)
    ConstIdent   = 0x80,   // an Ident, but must be exactly 1 of a pre-defined constant list of literals (a keyword)
    KeyWithDef   = 0x81,   // an Ident as a lookup key, but with optional ":-defaultVal" suffix in case key doesn't exist
    DateFormat   = 0xD0,   // date/time format string (it's own internal syntax)
    TimePeriod   = 0xD1,   // a duration of time with it's own syntax ("5M", "30sec", etc.)
    RegEx        = 0xD2,   // RegularExpression matching string
    
}

enum SubPatternRequired : ubyte
{
    No           = 0x00,   // Never has sub-pattern (or will be ignored)
    Optional     = 0x01,   // May or may not have a sub-pattern
    Yes          = 0x02,   // Must always have a sub-pattern
}

/**
 * Holds reference information for a single conversion parameter.
 */
immutable struct ParameterInfo
{
    // name the parameter can be called
    string name;
    // can the parameter be omitted
    bool isOptional;
    // default value used if the optional parameter is omitted
    string defaultVal;
    // the data type of the parameter must parse into
    //  can be any value other than Null, including the
    //  Conversion Parameter only types
    SymbolDataType parseType;
    // an array listing the only allowed string literals "idents"
    //  a.k.a. "keywords" for a parameter of type ConstIdent
    // For a keyword that has synonyms use "|" as a separator
    string[] acceptedValues;
    // Used for usage / exception strings - just briefly describes
    //   what the parameter indicates or what it does
    string description;
}

/**
 * Holds all the reference information for a given conversion word.
 * Intended for static, read-only use.
 */
immutable struct WordInfo
{
    // 1 or more "words" that identify the conversion word
    //  any 2 or more words are simply aliases / synonyms for each other
    string[] synonyms = ["NotImplementedYet"];
    /**
     * Denotes the data type that a conversion word will evaluate
     *  to.  This allows checking (at pattern parse time) if, for instance
     *  a word that returns a string is attempted to be formatted with a 'd'
     *  or any other format specifier that doesn't apply to that type.
     */
    SymbolDataType dataType;
    
    // Sub-pattern ()
    // true if the sub-pattern is required for the conversion word
    SubPatternRequired requiresSubPattern;
    // Sub-pattern evaluation type
    SymbolDataType subPatType;
    
    // Conversion Parameters {}
    // Array of 0 or more conversion parameter infos
    ParameterInfo[] params;
    // Used for usage / exception strings - just briefly describes
    //   what the word evaluates to or what it does to it's input.
    string description;
}

/**
 * Stores the master list of all conversion word types, synonyms, properties, etc.
 * Used for reference as it's content is intended to be static.
 */
public immutable WordInfo[WordType.max] WordData;

static this()
{   
    // convenience aliases so that we can shorten the lines below
    alias WordData WD;
    alias WordType WT;
    alias WordInfo WI;
    alias ParameterInfo Param;

    alias SymbolDataType.Null         Null;
    alias SymbolDataType.String       String;
    alias SymbolDataType.Ident        Ident;
    alias SymbolDataType.Integer      Integer;
    alias SymbolDataType.Float        Float;
    alias SymbolDataType.Numeric      Numeric;
    alias SymbolDataType.Metric       Metric;
    alias SymbolDataType.IntegerSlice IntSlice;
    alias SymbolDataType.Array        Array;
    alias SymbolDataType.Varies       Varies;
    // Conversion Parameter types only (parse types)
    alias SymbolDataType.ConstIdent   ConstIdent;   // an Ident, but must be exactly 1 of a pre-defined constant list of literals (a keyword)
    alias SymbolDataType.KeyWithDef   KeyWithDef;   // an Ident as a lookup key, but with optional ":-defaultVal" suffix in case key doesn't exist
    alias SymbolDataType.DateFormat   DateFormat;   // date/time format string (it's own internal syntax)
    alias SymbolDataType.TimePeriod   TimePeriod;   // a duration of time with it's own syntax ("5M", "30sec", etc.)
    alias SymbolDataType.RegEx        RegEx;        // RegularExpression matching string
    
    alias SubPatternRequired.No       No;
    alias SubPatternRequired.Optional Opt;
    alias SubPatternRequired.Yes      Yes;

    WD[WT.Null]                 = WI(null,                            Null,    No,  Null, null, "Not a real conversion word type");
    WD[WT.LiteralString]        = WI(null,                            String,  No,  Null, null, "An unformatted, literal, verbatim string");
//    WD[WT.VariadicArgument]     = WI(["arg","arg1"],                  Varies,  No,  Null,
//        [Param("index", true, "1", Integer, null, "1-based index of the argument")], "A (lazy) variadic argument from the .log call");
    WD[WT.LogMessage]           = WI(["m","msg","message"],           String,  No,  Null, null, "THE log message content (m, msg)");
    WD[WT.NewLine]              = WI(["n","endl"],                    String,  No,  Null, null, "Equivalent to '\\n' - inserts a newline character");

    // Logging-system values
    WD[WT.LogLevel]             = WI(["p","le","level","sev"],        String,  No,  Null, null, "Level or 'priority' or 'severity' of the logging event");
/*    WD[WT.Category]             = WI(["c","cat","category","logger"], String,  No,  Null,
        [Param("length",true,"inf",Integer,null, "target length to attempt to abbreviate category down to")], "Full Category of the logging event");
    WD[WT.CategoryRoot]         = WI(["cRoot","catRoot"],             String,  No,  Null, null, "Root (or top-level) category of the logging event");
    WD[WT.CategoryLeaf]         = WI(["cLeaf","catLeaf"],             String,  No,  Null, null, "Leaf (or bottom-most) sub-category of the logging event");
    WD[WT.Tag]                  = WI(["T","tag","marker"],            String,  No,  Null, null, "Tag applied to log entry - if more than 1: returns the last tag applied");
    WD[WT.AllTags]              = WI(["tags","allTags"],              Array,   No,  Null, null, "Array of all tags applies to log entry");
    WD[WT.LogMode]              = WI(["mode"],                        String,  No,  Null, null, "Logging mode currently enabled");
    WD[WT.PrevLogMode]          = WI(["lastMode"],                    String,  No,  Null, null, "Previous logging mode - before switching to current mode");
*/
    WD[WT.LogLength]            = WI(["len","length"],                Integer, No,  Null, null, "Number of bytes of this log message");
/*    WD[WT.EventOccurenceCount]  = WI(["oCount","occurCount"],         Integer, No,  Null, null, "Count of all occurrences of log event");
    WD[WT.EventFilteredCount]   = WI(["fCount","filterCount"],        Integer, No,  Null, null, "Count of the filtered log events");
    WD[WT.EventLoggedCount]     = WI(["lCount","logCount"],           Integer, No,  Null, null, "Count of logged output log events");
    WD[WT.EventSuppressedCount] = WI(["sCount","suppressCount"],      Integer, No,  Null, null, "Count of suppressed log events");
    WD[WT.EventSuppressedTime]  = WI(["sTime","suppressTime"],        Float,   No,  Null, null, "Period of time that log entry was last suppressed, in seconds, floating-point");
    WD[WT.EventRate]            = WI(["oRate","occurRate"],           Float,   No,  Null, null, "Rate or speed at which log entry occurs, in log entries per second, floating-point");
    WD[WT.EventLoggedRate]      = WI(["lRate","logRate"],             Float,   No,  Null, null, "Rate or speed at which log entry is output, in log entries per second, floating-point");
    WD[WT.Metric]               = WI(["met","metric","data"],         Metric,  No,  Null, 
        [Param("metricName",false,null,KeyWithDef,null,"The key or name of the metric value to retrieve and optionally a default value to use if the metric is not found")],
        "Returns a value or array of (timestamped) values, either by name or given a user-defined key, floating-point or integer");
    WD[WT.Rate]                 = WI(["rate"],                        Float,   Yes, Metric, 
        [Param("sampleSize",true,"inf",TimePeriod,null,"Defines a time interval or a number N of recent samples over which to compute the average rate")],
        "Returns a rate at which some (timestamped) metric is changing, in metric/second, floating-point");
    WD[WT.Stat]                 = WI(["stat","statistic"],            Float,   Yes, Metric, [
        Param("statType",false,null,ConstIdent,["mean","min","max","stddev"],"The type of statistic to compute"),
        Param("sampleSize",true,"inf",TimePeriod,null,"Defines a time interval or a number N of recent samples over which to compute the statistic")],
        "Returns the computed statistic of a given metric since it was last reset or (optionally) over a time window T or the last N samples, floating-point");
    WD[WT.Average]              = WI(["avg","average"],               Float,   Yes, Metric, 
        [Param("sampleSize",true,"inf",TimePeriod,null,"Defines a time interval or a number N of recent samples over which to compute the average")],
        "Returns the average of a given metric or statistic since it was last reset or (optionally) over a time window T or the last N samples, floating-point\n" ~
        "Shorthand for %stat(%metric){mean} which is equivalent");
    WD[WT.TimeSince]            = WI(["r","relative","timeSince","elapsed"],Float,Opt,Metric, [
        Param("pastEvent",true,"procStart",ConstIdent,["procStart","threadStart","lastLog","lastLevel","lastCat","lastTag","modeChange","reset"],
                         "Identifies the past event type where the time interval starts"),
        Param("nth",true,"1",Integer,null,"For a sequence of past events, specifies the nth most recent event. 0 means the oldest event")],
        "Time elapsed between some past event: program start-up, previous log entry, 10 ERROR logs ago, etc. and this log event, in seconds, floating-point");
*/
    WD[WT.LogConfigFileName]    = WI(["configName"],      String,  No,  Null, null, "Filename of the currently loaded log config, if any");
    WD[WT.LogConfigFilePath]    = WI(["configPath"],      String,  No,  Null, null, "Full path and filename of the currently loaded log config, if any");
    
    // TODO: either in counters above of these "sequence numbers" below:
    //       add "level counter", "category counter" "tag counter", etc.
    //       and a generic "event count" -- maybe taking 'rules' as a parameter
/*    WD[WT.GlobalSeqNumber]      = WI(["gsn","globalSeqNum","programSeqNum"],                Integer, No,  Null,
        [Param("key",true,null,KeyWithDef,null,"The key or name of a user-defined global counter")],
        "Global (shared across threads) sequence counter, incremented for any events in the whole program, including threads and fibers");
    WD[WT.ThreadLocalSeqNumber] = WI(["tsn","threadSeqNum","sn","seqNum","sequenceNumber"], Integer, No,  Null,
        [Param("key",true,null,KeyWithDef,null,"The key or name of a user-defined thread-local counter")],
        "Thread-local sequence counter, incremented for events originating in that thread, including any fibers");
    WD[WT.FiberLocalSeqNumber]  = WI(["fsn","fiberSeqNum"],                                 Integer, No,  Null,
        [Param("key",true,null,KeyWithDef,null,"The key or name of a user-defined fiber-local counter")],
        "Fiber-local sequence counter, incremented for events originating in that fiber");
    WD[WT.LocalSeqNumber]       = WI(["lsn","localSeqNum","dsn","devSeqNum"],               Integer, No,  Null,
        [Param("key",true,null,KeyWithDef,null,"The key or name of a developer-defined local counter")],
        "Developer-provided sequence number.  Locality is defined by where in the code the developer places the counter.\n" ~
        "For instance, it might be within a class instance and so would only 'live' as long as that object.");
*/    // Note: ArrayFormat not needed as a "conversion word" since it is really a format specifier (albeit a long one)
    //   The conversion pattern item's FormatSpec should reference the entire string slice from %( to %)
    //   Obviously only conversion words that are of array type can accept the array FormatSpec
    //     and in the case of %(...%)(....) we have no conversion word, it's just a "sub-pattern only"
    //     that happens to have an array FormatSpec.  Of course, the sub-pattern needs to evaluate
    //     to an array type in that case 
    //WD[WT.ArrayFormat]          = WI(null,                String,  Yes, Array, null, "Indicates std.format's array (grouping) syntax: %(...%) not a real word, sub-pattern must evaluate to an array type");
    WD[WT.SubPatternOnly]       = WI([""],                String,  Yes, String, null, "No word - subpattern only, but formatting spec must exist - Ex. %-20.30(subpat)");
    WD[WT.Date]                 = WI(["d","date"],        String,  Opt, String, 
        [Param("dateTimeFormat",true,"ISO8601",String,null,"Specifies the exact format of the date/time stamp in a notation similar to java.text.SimpleDateFormat (see Logback)")],
        "Timestamp of the log event, supporting a multitude of formats.\n" ~
        "The format is specified in either a special sub-pattern following the strftime library function (time.h) format notation " ~
        "OR in the parameter dataTimeFormat which, like Logback, uses a more readable format notation similar to java.text.SimpleDateFormat.\n" ~
        "Either a sub-pattern or a conversion parameter or neither must be present; it is an error if both are.");
//    WD[WT.Timestamp]            = WI(["time","stamp"],    String,  Opt, Numeric, String,  "format", "Timestamp supporting time-of-day and formatting sub-pattern time intervals. Similar to %date without year, month, week or day.");

    // Source-level values
    WD[WT.SourceFileName]       = WI(["f","fileName"],                  String,  No,  Null, null, "Filename of the source file where the log entry originated. D keyword __FILE__");
//    WD[WT.SourceFilePath]       = WI(["F","file"],                      String,  No,  Null, null, "Path and filename of the source file where the log entry originated");
    WD[WT.SourceLineNumber]     = WI(["L","line"],                      Integer, No,  Null, null, "Line number within the source file where the log entry originated. D keyword __LINE__");
//    WD[WT.FunctionName]         = WI(["fn","U","func"],                 String,  No,  Null, null, "Name of the function where the log entry originated. From D keyword __FUNCTION__");
    WD[WT.FunctionFQN]          = WI(["FN","funcFQN","method"],         String,  No,  Null, null, "Fully qualified name of the function where the log entry originated. D keyword __FUNCTION__");
//    WD[WT.FunctionSig]          = WI(["fnSig","funcSig"],               String,  No,  Null, null, "Function signature of the function where the log entry originated. From D keyword __PRETTY_FUNCTION__");
    WD[WT.FunctionPrettySig]    = WI(["FNSig","fnPretty","prettyFunc"], String,  No,  Null, null, "Pretty-printed function signature of the function where the log entry originated. D keyword __PRETTY_FUNCTION__");
//    WD[WT.Location]             = WI(["l","location","caller"],         String,  No,  Null, Varies,  "|depth",  "Short-hand for '%funcFQN(%f:%L)'");
//    WD[WT.ClassName]            = WI(["cls","className"],               String,  No,  Null, null, "Name of the class, struct, or union where the log entry originated");
//    WD[WT.ClassFQN]             = WI(["C","class","classFQN"],          String,  No,  Null, null, "Fully qualified name of the class, struct, or union where the log entry originated");
//    WD[WT.PackageName]          = WI(["pkg","package"],                 String,  No,  Null, null, "Fully qualified name of the package where the log entry originated");
//    WD[WT.Module]               = WI(["mod","module"],                  String,  No,  Null, null, "Name of module where the log entry originated");
//    WD[WT.ModuleFQN]            = WI(["MOD","modFQN","moduleFQN"],      String,  No,  Null, null, "Fully qualified name of module where the log entry originated");
//    WD[WT.CompileTimestamp]     = WI(["compT","compileTime"],           String,  No,  Null, String,  "|format", "Timestamp of the time of source code compilation. Same format options as %date");
//    WD[WT.CompilerName]         = WI(["comp","compName"],               String,  No,  Null, null, "Compiler vendor string");
//    WD[WT.CompilerVersion]      = WI(["compV","compVersion"],           String,  No,  Null, null, "Compiler version");

    // Runtime-level values
//    WD[WT.HostName]             = WI(["H","host","hostname"],   String,  No,  Null, null, "");
//    WD[WT.ShortHostName]        = WI(["h","shorthost"],         String,  No,  Null, null, "");
//    WD[WT.HostIPAddress]        = WI(["ip","hostIP"],           String,  No,  Null, null, "");
//    WD[WT.CPUStuff]             = WI(["cpu"],                   String,  No,  Null, null, "");
//    WD[WT.ExecutableName]       = WI(["execName"],              String,  No,  Null, null, "");
//    WD[WT.ExecutablePath]       = WI(["execPath"],              String,  No,  Null, null, "");
//    WD[WT.CommandLineArg]       = WI(["cmd"],                   String,  No,  Null, Ident,    "|argName", "");
//    WD[WT.CommandLineFull]      = WI(["cmdLine"],               String,  No,  Null, null, "");
//    WD[WT.WorkingDirectory]     = WI(["wd","pwd","workingDir"], String,  No,  Null, null, "");
//    WD[WT.ProcessID]            = WI(["pid"],                   String,  No,  Null, null, "");
//    WD[WT.ProcessName]          = WI(["procName"],              String,  No,  Null, null, "");
//    WD[WT.EnvVariable]          = WI(["E","env"],               String,  No,  Null, Ident,    "|var:-defaultVal",    "");
//    WD[WT.ThreadID]             = WI(["tid","threadID"],        Integer, No,  Null, null, "");
//    WD[WT.ThreadName]           = WI(["t","thread"],            String,  No,  Null, null, "");
    // synonyms are from zLog, Logback, and Log4j
//    WD[WT.MDC]                  = WI(["M","X","mdc","MDC","K","map","MAP"],
//                                                                String,  No,  Null, Ident, "|key:-defaultVal",     "");
    
    // apparently K map and MAP are Log4J's "MapMessage"  -- log message itself is a map -- use cases?
    // TODO, also consider adding NDC (Nested Diagnostic Context) - or a stack of strings/objects
    //    still part of the thread context, just like MDC (a map), but this is a stack -- use cases?
    //    x, NDC

    // Exception / Error values
//    WD[WT.ExceptionTypeName]    = WI(["exName"],               String,  No,  Null, null, "");
//    WD[WT.ExceptionFullTypeName]= WI(["exFullName"],           String,  No,  Null, null, "");
//    WD[WT.ExceptionMessage]     = WI(["exMsg"],                String,  No,  Null, null, "");
//    WD[WT.ExceptionFile]        = WI(["exFile"],               String,  No,  Null, null, "");
//    WD[WT.ExceptionLine]        = WI(["exLine"],               String,  No,  Null, null, "");
    
    // consider wrapped Exception "stacks"
    //  ex, exception, throwable   -- std LogBack and Log4J
    //  rEx, rException, rThrowable -- Log4J, reversed order stack trace
    //  xEx, xException, xThrowable -- Logback and Log4J, adds "packaging information" -- includes jar version number

//    WD[WT.ErrorTypeName]        = WI(["errName"],              String,  No,  Null, null,"");  // Note: AssertErrors are Errors
//    WD[WT.ErrorFullTypeName]    = WI(["errFullName"],          String,  No,  Null, null,"");
//    WD[WT.ErrorMessage]         = WI(["errMsg"],               String,  No,  Null, null,"");
//    WD[WT.ErrorFile]            = WI(["errFile"],              String,  No,  Null, null,"");
//    WD[WT.ErrorLine]            = WI(["errLine"],              String,  No,  Null, null,"");

//    WD[WT.TraceInfo]            = WI(["trace"],                String,  No,  Null, null,"");

/*  testing the UserDoc generation combinations for optional/required sub-pat and parameters
    WD[WT.ErrorFullTypeName]      = WI(["errFullName"],          String,  Opt, String,null,"temp for debug");
    WD[WT.ErrorMessage]         = WI(["errMsg"],               String,  Opt, String, 
        [Param("parm1",true,"def",String,null,"parm1 desc")],"temp for debug");
    WD[WT.ErrorFile]            = WI(["errFile"],              String,  Opt, String,
        [Param("parm1",true,"def",String,null,"parm1 desc"),
         Param("parm2",true,"def",String,null,"parm2 desc"),
         Param("parm3",true,"def",String,null,"parm3 desc")],"temp for debug");
    WD[WT.ErrorLine]            = WI(["errLine"],              String,  Opt, String,
        [Param("parmA",false,"def",String,null,"parmA desc"),
         Param("parmB",false,"def",String,null,"parmB desc"),
         Param("parmC",false,"def",String,null,"parmC desc")],"temp for debug");
    WD[WT.TraceInfo]            = WI(["trace"],                String,  Opt, String,
        [Param("parmA",false,"def",String,null,"parmA desc"),
         Param("parmB",false,"def",String,null,"parmB desc"),
         Param("parm1",true,"def",String,null,"parm1 desc")],"temp for debug");
    WD[WT.Replace]              = WI(["replace"],                String,  Opt, String,
        [Param("parmA",false,"def",String,null,"parmA desc"),
         Param("parm1",true,"def",String,null,"parm1 desc"),
         Param("parm2",true,"def",String,null,"parm2 desc")],"temp for debug");
*/


    // String conversion functions
/*    WD[WT.Replace]              = WI(["replace"],              String,  Yes, String, [
        Param( String,  "find" ),
        Param(           "repl"),
        ], "RegEx find and replace in sub-pattern");
    WD[WT.Crop]                 = WI(["crop"],                 String,  Yes, String,
        [Param( String,  "match" )], "RegEx match, then crop anything not matching");
    WD[WT.Strip]                = WI(["strip"],                String,  Yes, String,   null, "Strip away any leading or trailing whitespace");
    WD[WT.ToLower]              = WI(["lower"],                String,  Yes, String,   null, "all lowercase version of sub-pattern");
    WD[WT.ToUpper]              = WI(["upper"],                String,  Yes, String,   null, "all uppercase version of sub-pattern");
    WD[WT.Capitalize]           = WI(["capital"],              String,  Yes, String,   null, "Captialize first letter of each word and lowercase all other letter");
    WD[WT.Center]               = WI(["center"],               String,  Yes, String,   Integer, "|width",  "Center the sub-pattern within a given field width");
    WD[WT.Fill]                 = WI(["fill"],                 String,  Yes, String,   String,  "|filler", "Fill leading and trailing whitespace with a given character");
    WD[WT.Wrap]                 = WI(["wrap"],                 String,  Yes, String,   Integer, "|width",  "Wrap long text to a fixed-width multiline 'paragraph'");
    WD[WT.ToBase64]             = WI(["base64"],               String,  Yes, Varies,   null, "Create a Base64 encoded string from binary data / raw bytes");
    WD[WT.EncodeURI]            = WI(["encURI","encodeURI"],   String,  Yes, String,   null, "Take any UTF-8 string and escape any URI-invalid characters");
    WD[WT.DecodeURI]            = WI(["decURI","decodeURI"],   String,  Yes, String,   null, "Take any URI possibly with escaped characters and return the unescaped UTF-8");
    // TODO .. add enc, encHTML .. equivalent to Log4J's HTML symbol escaping
    //  perhaps a mroe generic "esc" or "escape" and "unescape" which takes an encoding name as parameter:
    //  %esc(stuff){xml} 
    WD[WT.Round]                = WI(["round"],                Numeric, Yes, Numeric, Integer, "|number", "Round numeric sub-pattern value to certain decimal place (accepts numeric format specifiers)");
    WD[WT.ToJSON]               = WI(["json"],                 String,  Yes, Varies, null, "Output a JSON-formatted version of any D-type");
    WD[WT.ToUUID]               = WI(["u","uuid"],             String,  Opt, Varies, null, "Generate a UUID for a sub-pattern");
    WD[WT.HTML]                 = WI(["html"],                 String,  Yes, String, String,  "tag",    "Wraps the sub-pattern in a given HTML tag");
                             /// example: "%html(subpat){td}" produces "<td>subpat<\td>");
*/
    Param colorStyleParam = Param("style",true,null,ConstIdent,["bold","dim","ul|UL|underline","blink|blinking","bg|BG|back|background"],
                                  "Also applies the ANSI style to the text as well. Multiple styles can be combined with '&' between them.");
    // Colors
/*    WD[WT.Black]                = WI(["black"],                String,  Yes, String, [colorStyleParam], "Colors the sub-pattern black");
    WD[WT.Red]                  = WI(["red"],                  String,  Yes, String, [colorStyleParam], "Colors the sub-pattern red");
    WD[WT.Green]                = WI(["green"],                String,  Yes, String, [colorStyleParam], "Colors the sub-pattern green");
    WD[WT.Yellow]               = WI(["yellow"],               String,  Yes, String, [colorStyleParam], "Colors the sub-pattern yellow");
    WD[WT.Blue]                 = WI(["blue"],                 String,  Yes, String, [colorStyleParam], "Colors the sub-pattern blue");
    WD[WT.Magenta]              = WI(["magenta"],              String,  Yes, String, [colorStyleParam], "Colors the sub-pattern magenta");
    WD[WT.Cyan]                 = WI(["cyan"],                 String,  Yes, String, [colorStyleParam], "Colors the sub-pattern cyan");
    WD[WT.White]                = WI(["white"],                String,  Yes, String, [colorStyleParam], "Colors the sub-pattern white");
    WD[WT.Gray]                 = WI(["gray"],                 String,  Yes, String, [colorStyleParam], "Colors the sub-pattern gray");
    WD[WT.Color]                = WI(["color"],                String,  Yes, String,  [
        Param( String, "colorCode" ),
        Param( ConstIdent, "|bold|dim|ul/UL/underline|blink/blinking|bg/BG/back/background|strikethrough" ),
        ], "Specify color code (for HTML)");
    WD[WT.Highlight]            = WI(["highlight"],            String,  Yes, String,  String, "|bold|dim|ul/UL/underline|blink/blinking|bg/BG/back/background", "Colors the sub-pattern based on the LogLevel of the LogEvent");
    WD[WT.Style]                = WI(["style","font"],         String,  Yes, String,  Ident,  "|bold|dim|ul/UL/underline|blink/blinking|strikethrough|rv/revVid/reverseVideo", "Changes the style of the sub-pattern");

    WD[WT.CustomCW]             = WI(null,                     String,  Opt, Varies, null, "Represents a developer-defined conversion word/function");
*/
    //WD[WT.Undetermined]         = WI([""], String, false, Null, "");   // temporary during parsing
}

unittest
{
    // Sanity check for language design -- make sure ALL synonyms are unique
    //  also, make sure all enum values except 0xFF have a WordInfo assigned
    import std.format : format;
    import std.conv : to;
    import std.algorithm.comparison : among;
    import std.algorithm : sort, findAdjacent;
    string[] allSynonyms = [];
    foreach(type ; [EnumMembers!WordType])
    {
        // Undetermined has no info (off the end of the array) intentionally
        if ( type == WordType.Undetermined ) continue;
        // Check that each valid enum value of WordType has a non-default WordInfo assigned
        //  this is to ensure that no enum value was missed when creating the master WordData structure
        
        // FIXME: until ALL are truely implemented we will skip this check
        if(WordData[type].synonyms !is null && WordData[type].synonyms[0] == "NotImplementedYet")
            continue;
        
        assert(WordData[type] != WordInfo.init, format("ERROR: WordData[%s] is default-initialized", type));
        
        
        // Check that no synonym list is null or 0-length
        //  except for the expected ones, explicitly listed
        if((WordData[type].synonyms is null ||
            WordData[type].synonyms.length == 0) &&
            !type.among(WordType.Null,
                        WordType.LiteralString,
                        WordType.CustomCW) )
        {
            assert(false, format("ERROR: %s has null or 0-length synonym list", type));           
        }
        // accumulate the list of all synonyms
        allSynonyms ~= WordData[type].synonyms;
    }
    // Finally, confirm that the set of all conversion word synonyms has no duplicates
    sort(allSynonyms);
    auto hasADup = allSynonyms.findAdjacent();
    if(hasADup.length != 0)
        assert(false, format("ERROR: Found at least 1 pair of duplicated synonyms in %s", hasADup));
}

debug {
    /**
     * Stores 1 or more example conversion words (with sub-pat
     *  and/or parameter(s) as necessary) and the expected rendered
     *  output value to be used for documentation and in unit tests.
     * Intended to be used for a single conversion word type.
     */
    immutable struct WordExamples(ContextTupleSpec...)
    {
        // the conversion word that these examples illustrate
        //   note other conversion words, surrounding text, and
        //   sub-patterns may appear in the examples
        WordType convWord;
        
        // Extra description / info / context to elaborate or
        //  frame the examples contained herein
        string description;
        // Optional (but often used) context variables / data
        //  that need to be loaded for the example expected Output
        //  to be reproducible (needed for unit testing)
        // Stored as Tuple of values -- the names should indicate
        //  what logging system variables the values apply to.
        Tuple!ContextTupleSpec context;
              
        // Stores the pair of "%input" and "conversion output"
        //  strings, a.k.a. a single example
        immutable struct Example
        {
            string input;
            string expectedOutput;
        }
        // The example text (as the user would have in a conversion pattern)
        //  and the corresponding rendered output we would expect to see
        Example[] examples;    
    }

    /**
     * Similarly to WordData, this is a master collection of example data,
     *  with input and expected output for each tested conversion word.  This
     *  is used for unit testing and documentation generation.
     * It is static read-only content, but also only compiled if in debug.
     */
    // since WordExamples is a template (because the context data is a Tuple)
    //  and since each ConvWord example has varying context data, we must make this
    //  one big Tuple where the names of each element are the WordType
    //  enum member string value.  Unit tests can then access the examples for a
    //  given conversion word by name.
 /*   public immutable Tuple!(
        to!string(WordType.LogMessage), WordExamples!(string, "logMessage"),
        to!string(WordType.NewLine), WordExamples!(),                     ,
        WordExamples!(uint, "threadID"),     to!string(WordType.ThreadID),
        ) AllWordExamples;
     
     */
 //    public immutable WordExamples[WordType.max]  AllWordExamples;
}

/*
 * Built only when we need to generate user docs (beyond
 *  Ddoc because we need to dynamically generate based on content
 *  within the code).
 * If there is some fancy way to do this with DDoc, I'd be very interested to know how
 */
version(Logging_UserDocs) 
{
    bool isNotImplYet(WordType type)
    {
        return type != WordType.Undetermined
            // for now since eventually all WordType should be implemenated
            && WordData[type] == WordInfo.init;
    }
    
    bool isNotValidType(WordType type)
    {
        // There will be special documentation for Array format syntax,
        //  sub-pattern only, etc. (i.e. no real conversion *word* to speak of)
        import std.algorithm : among;
        // remove this once the unittest passes (i.e. all WordTypes are implemented)
        if(isNotImplYet(type)) return true;
        return type.among(WordType.Null,
                          WordType.LiteralString,
                          WordType.SubPatternOnly,
                          WordType.CustomCW,
                          WordType.Undetermined) > 0;
    }
    
    // dump WordData as a JSON string?  any use for this format?
    
    /**
     * Generates a nice HTML tabular format of all the conversion
     *  word information
     */
    void generateConvWordHTML(OutputRange)(OutputRange writer)
    {
        //import std.stdio : write, writef, writeln, writefln;
        import std.range.primitives;
        // Generate a definitive table of all conversion word information (with CTFE)
        //   in a nicely formatted way - a la Log4J and Logback tables
        
        // -------------- start of HTML ---------------------------
        put(writer, q"HTML
<style>
table {
  border-color:gray;
  border-spacing:2px;
  font-family: Verdana, Arial, SunSans-Regular, Sans-Serif;
  font-size: 11pt;
  color: #000;
}
table td {
  background-color: #f0f0f0;
  padding: 3px;
  border-top: 1px dotted #888888;
}
table tr:nth-child(odd) td {
  background-color: #f9f9f9;
}
table.convWordTable th {
  background-color: #888;
  font-weight: bold;
  min-width: 300px
}
td.word {
  text-align: right;
  vertical-align: text-top;
}
span.wordSubPat {
 
}
span.wordParam {
  font-style: italic;
}
span.wordType {
  font-style: italic;
  font-weight: bold;
}
pre {
  display: inline;
  font-family: Courier, Monaco, Monospace;
  font-size: 10pt;
  padding-left: 1ex;
  padding-right: 1ex;
  line-height: 3ex;
}
pre.logInput {
  
}
pre.logOutput {
  border: 1px dashed #AAAAAA;
  background: #D0D0D0;
}
</style>
HTML"); 
       
        import std.format : format;
        import std.string : string;
        import std.algorithm : joiner, all, filter, map;
        import std.range;
        import std.conv : text;
        
        //import std.string : translate;
        void writeRow(WordType cwType)
        {
            WordInfo cwInfo = WordData[cwType];
            // list all possible suffixes: no sub-pat (if not required) and no parameters (if none or all optional)
            //                             no sub-pat (if not required) and required parameters
            //                             no sub-pat (if not required) and required + optional parameters
            //                             sub-pat (if any) and <each of the 3 cases above for parameters>
            bool areAllParamsOptional(WordType type)
            {
                return WordData[type].params == null || WordData[type].params.all!"a.isOptional";
            }
            immutable(ParameterInfo[]) filterRequiredParams(WordType type)
            {
                return WordData[type].params.filter!("!a.isOptional").array;
            }
            immutable(ParameterInfo[]) filterOptionalParams(WordType type)
            {
                return WordData[type].params.filter!("a.isOptional").array;
            }
            string[] wordSuffix;
            // first build up any/all parameter combinations
            if(cwInfo.params != null) {
                if(areAllParamsOptional(cwType)) {
                    wordSuffix ~= "";
                    // incrementally add each optional parameter, starting with the first
                    foreach(i, param; cwInfo.params) {
                        wordSuffix ~= wordSuffix[$-1] ~ (i == 0 ? "" : ", ") ~ param.name;
                    }
                } else { // at least 1 required parameter (req params should come before any optional ones
                    wordSuffix ~= text(joiner(filterRequiredParams(cwType).map!("a.name").array.dup, ", "));
                    if(filterOptionalParams(cwType).length > 0) { // additionally we have at least 1 optional parameter
                        foreach(param; filterOptionalParams(cwType)) {
                            wordSuffix ~= wordSuffix[$-1] ~ ", " ~ param.name;
                        }
                    }
                }
            } else {  // case of 0 parameters, so the "" (no suffix) entry needs to be added
                wordSuffix ~= "";
            }
            // now wrap each parameter list, if any, in the { } and <span>
            foreach(ref wSuf ; wordSuffix) {
                if(wSuf != "") wSuf = "<span class=\"wordParam\">{" ~ wSuf ~ "}</span>";
            }
            // now we can prepend the "(sub-pat)" if any
            if(cwInfo.requiresSubPattern == SubPatternRequired.Yes) {
                // required, so replace the current {} with (){} (i.e. prepend but don't add new entries)
                foreach(ref wSuf ; wordSuffix) {
                    wSuf = "<span class=\"wordSubPat\">(sub-pat)</span>" ~ wSuf;
                }
            } else if(cwInfo.requiresSubPattern == SubPatternRequired.Optional) {
                // since sub-pat is optional, copy all the wordSuffix's for both with and without the (sub-pat)
                wordSuffix ~= wordSuffix.map!("\"<span class=\\\"wordSubPat\\\">(sub-pat)</span>\" ~ a").array;                    
            }
            // else - there is no sub-pattern - nothing to do since any parameters are already taken care of
            // Special case: %date forbids both the sub-pattern and parameter to appear simultaneously
            //    they are optional individually since either can be used, but not at the same time
            //    so we need to delete the last suffix of "(sub-pat){dateTimeFormat}"
            if(cwType == WordType.Date)
                wordSuffix.length = wordSuffix.length - 1;
            string tdConvWord = "\n      ";
            foreach(syn ; cwInfo.synonyms)
                tdConvWord ~= format("<b>%s</b>%s<br>\n      ", syn, 
                                     cwInfo.requiresSubPattern == SubPatternRequired.Yes || 
                                     !areAllParamsOptional(cwType) ? "..." : "");
            foreach(wordSuf ; wordSuffix) {
                if(wordSuf != "")  // empty case taken care of above
                    tdConvWord ~= format("<b>%s</b>%s<br>\n      ", cwInfo.synonyms[0], wordSuf);
            }
            tdConvWord = tdConvWord[0..$-2];
            string tdDesc = format(q"HTML
<span class="wordType">%s</span>: %s
      <br>Example:   <pre class="logInput">%%%s</pre>
      <br>Generates: <pre class="logOutput">%s</pre>
HTML", cwType, cwInfo.description, cwInfo.synonyms[0], "result of example");
            put(writer, format("  <tr>\n    <td class=\"word\">%s</td>\n", tdConvWord));
            put(writer, "    <td>" ~ tdDesc ~ "    </td>\n  </tr>\n");
        }
        // end void writeRow(WordType cwType)
        
        void writeWholeTable(bool sortByFirstSyn)
        {
            put(writer, q"HTML
<table class="convWordTable">
  <tr>
    <th>Conversion Word</th>
    <th>Description</th>
  </tr>
HTML");
            import std.algorithm : sort, remove, each;
            import std.array : array;
            import std.string : toLower;
            auto orderedTypes = [EnumMembers!WordType].remove!(a => isNotValidType(a));
            if (sortByFirstSyn)
            {
                orderedTypes = orderedTypes.sort!((a, b) => std.string.toLower(WordData[a].synonyms[0]) <
                                                            std.string.toLower(WordData[b].synonyms[0])
                                                 ).array;
            }
            orderedTypes.each!writeRow;
            //foreach(type ; orderedTypes) { writeRow(type); }
            put(writer, "</table>\n");
        }
        
        writeWholeTable(false);
        put(writer, "<br>Same information but sorted alphabetically on the first conversion word:<br>\n");
        writeWholeTable(true);
    }
}
