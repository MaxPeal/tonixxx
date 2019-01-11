// Copyright 2018 Andrew Pennebaker

#pragma once

// Show CLI options.
void usage(char* program);

// Returns system-relative exit code signifying test success/failure.
int test();

// CLI entrypoint.
int main(int argc, char** argv);
