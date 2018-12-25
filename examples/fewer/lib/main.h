#pragma once

// Copyright 2017 Andrew Pennebaker

// Launch application.
int m(int console_out, int console_err, int console_in, int root, bool test);

#ifndef __CloudABI__
    // Display command line syntax information.
    void usage(int console_err, char *program);
#endif
