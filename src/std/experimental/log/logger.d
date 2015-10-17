module std.experimental.log.logger;

import std.stdio;
import std.format;
import std.array;

import std.experimental.log.logformatter;

enum LogLevel : ubyte
{
    Debug = 20,
    Info = 40,
    Notice = 60,
    Warn = 80,
    Error = 100,
    Fatal = 120
}

class Logger
{
    string filename;
    // would like to have immutable(ubyte)[] similar to strings
    //  the values of the bytes never change once put in this buffer
    //  unfortunately, it seems write.clear is now @disabled for 
    //  immutable elements --- sooo ??
    // Ideally, we don't want to re-allocate the array each time it's content
    //  is dumped to file - just reuse it, the whole point of efficiency here
    // So, perhaps we need some other structure, circular buffer, etc.
    Appender!(ubyte[]) writer;
    LogFormatter logForm;
    
    this(string filename)
    {
        this.filename = filename;
        writer = Appender!(ubyte[])();
        writer.reserve(4096);
        //check if directory of log filename is writable by this process
        //  if not, flag warning at this point, but don't error until an
        //  attempt is made to actually write to the file
        logForm = new LogFormatter("Static stuff | %{ProcessID} | %#06{ProcessID %x} | " ~
                                   "%{ LOGLEVEL }%{ log  level  }%{LogLevel}|%{LogLasdevel}|%{ThreadID}| %s");
    }
    
    void log(LogLevel level, string message)
    {
        logForm.formatLog(level, message, writer);
        // should either check the file for write permissions (or creation if
        //   it doesn't exist, etc) and throw an error if not ... or just try catch and 
        //   log the exception.  Note: preferable to log an error in Dalog's log
        //   rather than throw, so the user's code can proceed despite logging failure
        // TODO: in future add a constructor option with the above behavior as default
        //   but let the user specify to throw errors from the log() call
        File f = File(filename, "w");
        f.lockingTextWriter.put(writer.data);
        writer.clear;
    }
}
