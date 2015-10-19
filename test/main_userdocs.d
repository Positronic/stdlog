/**
 * Entry point and other "post-processing" for generation of documentation
 *  beyond the basic DDoc. 
 */
module main_userdocs;

import std.stdio;
import std.file;

void main()
{
    version(Logging_UserDocs)
    {
        import std.experimental.log.conversionpattern_data;
        string docRoot = "./docs/";
        mkdirRecurse(docRoot);
        string convWordTableHTML = "conversion_words.html";
        File f = File(docRoot ~ convWordTableHTML, "w");
        generateConvWordHTML(f.lockingTextWriter);
        writeln("User docs generated and output to: " ~ f.name);
    }
    else
    {
        writeln("Nothing performed!  This entry point is meant to be used only for documentation generation (dub -c userdocs)");        
    } 
}