// rdap: a Debug Adapter Protocol server for R. It speaks DAP on stdin/stdout and runs R itself
// (embedded), on this, the main, thread.
#define _GNU_SOURCE
#include "rdap.h"

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static void usage(FILE *f) {
  fprintf(f, "usage: rdap [--log FILE]\n"
             "A debug adapter (DAP, on stdin/stdout) for R scripts. Start it from a DAP client.\n"
             "  --log FILE   write the protocol traffic to FILE (also: RDAP_LOG=FILE)\n"
             "  --version    print the version\n");
}

int main(int argc, char **argv) {
  const char *log_path = getenv("RDAP_LOG");
  for (int i = 1; i < argc; i++) {
    if (strcmp(argv[i], "--version") == 0) {
      printf("rdap %s\n", RDAP_VERSION);
      return 0;
    } else if (strcmp(argv[i], "--log") == 0 && i + 1 < argc) {
      log_path = argv[++i];
    } else if (strcmp(argv[i], "--help") == 0 || strcmp(argv[i], "-h") == 0) {
      usage(stdout);
      return 0;
    } else {
      usage(stderr);
      return 2;
    }
  }

  // the protocol keeps the original stdin/stdout; R (and whatever it runs) gets /dev/null and
  // pipes that become output events
  int in = fcntl(0, F_DUPFD_CLOEXEC, 3);
  int out = fcntl(1, F_DUPFD_CLOEXEC, 3);
  int devnull = open("/dev/null", O_RDONLY);
  dup2(devnull, 0);
  close(devnull);
  dap_init(in, out, log_path);
  capture_start();
  dap_start_reader(debugger_reader_hook);
  debugger_loop();
}
