import std.stdio;

void main() {
    auto factoidFile = File("factoids.txt");
    auto factoidContent = readText(factoidFile);
    auto factoids = splitLines(factoidContent);
    writeln(factoids.randomShuffle())
}
