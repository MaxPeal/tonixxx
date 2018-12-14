/** CLI app for generating potato facts */

import pt;

import std.stdio;

/** CLI entry point */
version(APP) {
    void main() {
        writeln(factoid());
    }
}
