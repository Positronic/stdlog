/**
 * Define an entry point for unit tests since this is a library.
 */
module main_unittests;

import std.stdio;

void main()
{
    version(unittest)
    {
        writeln("All unit tests passed!");
    }
    else
    {
        writeln("No unit tests were built or executed!");
        writeln("This entry point is meant to be used only when unittests are built (dub -c unittests -b unittest)");
    }
}