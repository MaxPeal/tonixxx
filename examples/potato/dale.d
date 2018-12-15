/** Common tasks */

import dl;

import core.exception;
import std.file;
import std.stdio;
import std.string;
import std.path;
import std.process;

/** Generate documentation */
@(TASK)
void doc() {
    exec("doxygen");
}

/** Run D-Scanner */
@(TASK)
void dscanner() {
    exec("dub", ["run", "dscanner", "--", "--styleCheck"]);
}

/** Static code validation */
@(TASK)
void lint() {
    // deps(&doc);
    deps(&dscanner);
}

/** Lint, and then install artifacts */
@(TASK)
void install() {
    auto cwd = getcwd();
    exec("dub", ["add-local", cwd]);
}

/** Uninstall artifacts */
@(TASK)
void uninstall() {
    auto cwd = getcwd();
    wait(execMut("dub", ["remove-local", cwd]).pid);
}

/** Lint, and then run unit tests */
@(TASK)
void unitTest() {
    deps(&lint);
    exec("dub", ["test"]);
}

/** Lint, and then run integration tests */
@(TASK)
void integrationTest() {
    deps(&install);
    exec("dub", ["run", "--verror", "--config", "potato"]);
}

/** Lint, and then run tests */
@(TASK)
void test() {
    deps(&unitTest);
    deps(&integrationTest);
}

/** Lint, unittest, and build debug binaries */
@(TASK)
void buildDebug() {
    deps(&unitTest);
    exec("dub", ["build"]);
    exec("dub", ["build", "--config", "potato"]);
}

/** Lint, unittest, and build release binaries */
@(TASK)
void buildRelease() {
    deps(&unitTest);
    exec("dub", ["build", "-b", "release"]);
    exec("dub", ["build", "-b", "release", "--config", "potato"]);
}

/** Run valgrind */
@(TASK)
void valgrind() {
    version(Windows) {
        immutable extension = ".exe";
    } else {
        immutable extension = "";
    }

    immutable app = "potato" ~ extension;
    immutable potatoBin = buildPath(".dub", "build", "bin", app);
    exec("valgrind", ["--error-exitcode=1", "--leak-check=full", potatoBin]);
}

/** Lint, unittest, build (release) binaries, and run any stable leak checks */
@(TASK)
void leaks() {
    deps(&buildRelease);

    version(Windows) { return; }
    version(DragonFlyBSD) { return; }
    version(Haiku) { return; }
    version(Solaris) { return; }
    version(NetBSD) { return; }
    version(OpenBSD) { return; }
    version(OSX) { return; }
    else {
        try {
            auto lsbReleaseAll = execStdoutUTF8("lsb_release", ["-a"]);

            if (lsbReleaseAll.indexOf("Void") != -1) { return; }
        } catch (ProcessException e) {} // Non-Linux distribution

        try {
            auto unameAll = execStdoutUTF8("uname", ["-a"]);

            if (unameAll.indexOf("HBSD") != -1 ||
                unameAll.indexOf("Minix") != -1) { return; }
        } catch (ProcessException e) {} // Non-UNIX distribution

        deps(&valgrind);
    }
}

/** Lint, unittest, build (release) binaries, and optionally run leak checks. */
@(TASK)
void build() {
    deps(&leaks);
}

/** Run dub clean */
@(TASK)
void cleanDub() {
    exec("dub", ["clean"]);
}

/** Clean workspaces */
@(TASK)
void clean() {
    deps(&cleanDub);
}

/** CLI entrypoint */
version(unittest) {} else
void main(string[] args) {
    phony([
        &uninstall,
        &cleanDub,
        &clean
    ]);

    mixin(yyyup!("args", "build"));
}
