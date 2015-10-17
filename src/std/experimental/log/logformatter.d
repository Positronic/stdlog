module std.experimental.log.logformatter;

import std.array;
import std.format;
import std.conv;
import std.string;
import std.process;

import std.stdio;

import std.experimental.log.logger;
import std.experimental.log.logformatspec;

/**
 *  Represents a log format string and performs the formatting operation
 *  on provided inputs.  It provides compile-time optimization, template 
 *  expansions, format string constants, configuration / load-time optimizations
 *  all designed to speed up the output of formatted log entries.
 */
class LogFormatter
{   
    private string formatString;
    private LogFormatSpec formatSpec;
    
    package this(string fmtString)
    {
        formatString = fmtString;
        formatSpec = new LogFormatSpec(formatString);
        stdout.writeln(formatSpec.toString());
        bakeRuntimeConstants();   // can only instanciate a class at runtime
    }
    
    package void formatLog(LogLevel ll, string logMessage, ref Appender!(ubyte[]) writer)
    {
        /*string replacedFormatString = formatString.replace("%{LOGLEVEL}", toUpper(to!string(ll))).
                                                   replace("%{loglevel}", toLower(to!string(ll))).
                                                   replace("%{LogLevel}", to!string(ll));
        */
        formatSpec.bakeValue(FormatElementType.LogLevel, to!string(ll));
        stdout.writeln(formatSpec.toString());
        foreach(elem ; formatSpec.elems)
        {
            switch (elem.elemType)
            {
                case FormatElementType.LiteralString:
                    writer ~= representation(elem.literal);
                    break;
                case FormatElementType.VariadicArgument:
                    writer.formatValue(logMessage, elem.singleSpec);
                    break;
                default:
                    writer ~= representation("Format element not implemented yet: " ~ to!string(elem.elemType));
            }
        }
        //writer.formattedWrite(replacedFormatString, logMessage);      
    }
    
    private void bakeRuntimeConstants()
    {
        int pid = thisProcessID();
        formatSpec.bakeValue(FormatElementType.ProcessID, pid);
    }
    
    package void bakeEnvVariables()
    {
        
    }
}