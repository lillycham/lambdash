#define _GNU_SOURCE
#include <signal.h>
#include <spawn.h>
#include <sys/types.h>
#include <unistd.h>

extern char **environ;

/* posix_spawn instead of fork: only the calling thread survives a fork, and
   the GHC runtime has several. Returns 0 or an errno value. */
static int spawn(const char *file, char *const argv[], char *const envp[],
                 int search, int in, int out, pid_t pgid, pid_t *pid) {
  posix_spawn_file_actions_t actions;
  posix_spawnattr_t attr;
  sigset_t defaults, mask;
  int err;

  posix_spawn_file_actions_init(&actions);
  posix_spawnattr_init(&attr);

  if (in != STDIN_FILENO)
    posix_spawn_file_actions_adddup2(&actions, in, STDIN_FILENO);
  if (out != STDOUT_FILENO)
    posix_spawn_file_actions_adddup2(&actions, out, STDOUT_FILENO);

  /* A child gets stdin, stdout and stderr, and nothing else: no pipe of
     another stage, no descriptor the shell inherited, no terminal that
     the line editor opened. A background job that kept one would keep its
     reader waiting for end of input, or keep a terminal it does not own. */
  short flags = POSIX_SPAWN_SETPGROUP | POSIX_SPAWN_SETSIGDEF | POSIX_SPAWN_SETSIGMASK;
#if defined(__APPLE__)
  if (in == STDIN_FILENO)
    posix_spawn_file_actions_addinherit_np(&actions, STDIN_FILENO);
  if (out == STDOUT_FILENO)
    posix_spawn_file_actions_addinherit_np(&actions, STDOUT_FILENO);
  posix_spawn_file_actions_addinherit_np(&actions, STDERR_FILENO);
  flags |= POSIX_SPAWN_CLOEXEC_DEFAULT;
#elif defined(__GLIBC__) && (__GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 34))
  posix_spawn_file_actions_addclosefrom_np(&actions, STDERR_FILENO + 1);
#endif

  /* Ignored signals stay ignored across exec. The shell ignores the job
     control signals and the GHC runtime ignores SIGPIPE, so a child would
     inherit all of them: `yes | head -1` would never end. */
  sigemptyset(&defaults);
  sigaddset(&defaults, SIGINT);
  sigaddset(&defaults, SIGQUIT);
  sigaddset(&defaults, SIGTSTP);
  sigaddset(&defaults, SIGTTIN);
  sigaddset(&defaults, SIGTTOU);
  sigaddset(&defaults, SIGPIPE);
  sigemptyset(&mask);

  posix_spawnattr_setpgroup(&attr, pgid);
  posix_spawnattr_setsigdefault(&attr, &defaults);
  posix_spawnattr_setsigmask(&attr, &mask);
  posix_spawnattr_setflags(&attr, flags);

  err = search ? posix_spawnp(pid, file, &actions, &attr, argv, envp)
               : posix_spawn(pid, file, &actions, &attr, argv, envp);

  posix_spawn_file_actions_destroy(&actions);
  posix_spawnattr_destroy(&attr);
  return err;
}

/* Search the shell's own PATH, with its own environment. */
int lsh_spawn(const char *file, char *const argv[], int in, int out,
              pid_t pgid, pid_t *pid) {
  return spawn(file, argv, environ, 1, in, out, pgid, pid);
}

/* An exact path, with an explicit environment. posix_spawnp would search the
   PATH of this process, not the PATH in envp, and an overlay changes envp. */
int lsh_spawn_env(const char *path, char *const argv[], char *const envp[],
                  int in, int out, pid_t pgid, pid_t *pid) {
  return spawn(path, argv, envp, 0, in, out, pgid, pid);
}
