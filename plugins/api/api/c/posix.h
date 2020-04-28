// To keep this file independent on a
// users system we will define all types here
// instead of trying to parse system headers

typedef unsigned long int size_t;
typedef long int ssize_t;
typedef int div_t;
typedef int ldiv_t;
typedef int lldiv_t;
typedef int wchar_t;

int main(int argc, const char **argv);

// stdlib.h

void          _Exit(int code)
    __attribute__((noreturn));
long          a64l(const char *)
    __attribute__((nonnull(1), warn_unused_result));

void          abort(void) __attribute__((noreturn));
int           abs(int x) __attribute__((__const__, warn_unused_result));
int           atexit(void (*callback)(void)) __attribute__((nonnull(1)));
double        atof(const char *)  __attribute__((pure, nonnull(1), warn_unused_result));
int           atoi(const char *)  __attribute__((pure, nonnull(1), warn_unused_result));
long          atol(const char *)  __attribute__((pure, nonnull(1), warn_unused_result));
long long     atoll(const char *) __attribute__((pure, nonnull(1), warn_unused_result));
void         *bsearch(const void *key,
                      const void *base,
                      size_t nmemb,
                      size_t size,
                      int (*compare)(const void *, const void *))
    __attribute__((pure, nonnull(1,2,5), warn_unused_result));

void         *calloc(size_t nmemb, size_t size)   __attribute__((malloc, warn_unused_result, alloc_size(1,2)));
div_t         div(int numenator, int denominator) __attribute__((__const__, warn_unused_result));
double        drand48(void);
double        erand48(unsigned short [3]);
void          exit(int code) __attribute__((noreturn));
void          free(void *ptr);
char         *getenv(const char *name) __attribute__((nonnull(1), warn_unused_result));
int           getsubopt(char **restrict optionp, char *const *tokens, char **restrict value)
    __attribute__((nonnull(1,2,3), warn_unused_result));
int           grantpt(int);
char         *initstate(unsigned, char *, size_t) __attribute__((nonnull(2)));
long          jrand48(unsigned short [3]);
char         *l64a(long) __attribute__((warn_unused_result));
long          labs(long) __attribute__((__const__, warn_unused_result));
void          lcong48(unsigned short [7]);
ldiv_t        ldiv(long, long) __attribute__((__const__, warn_unused_result));
long long     llabs(long long) __attribute__((__const__, warn_unused_result));
lldiv_t       lldiv(long long, long long) __attribute__((__const__, warn_unused_result));
long          lrand48(void);
void         *malloc(size_t size)
    __attribute__((malloc, warn_unused_result, alloc_size(1)));

int           mblen(const char *, size_t);
size_t        mbstowcs(wchar_t *, const char *, size_t);
int           mbtowc(wchar_t *, const char *, size_t);
char         *mkdtemp(char *template) __attribute__((nonnull(1), warn_unused_result));
int           mkstemp(char *) __attribute__((nonnull(1),warn_unused_result));
long          mrand48(void);
long          nrand48(unsigned short [3]);
int           posix_memalign(void **memptr, size_t align, size_t size)
    __attribute__((nonnull(1), warn_unused_result, alloc_size(3)));
int           posix_openpt(int) __attribute__((warn_unused_result));
char         *ptsname(int fd);
int           putenv(char *name) __attribute__((nonnull(1)));
void          qsort(void *base, size_t nmemb, size_t size, int (*compare)(const void *, const void *))
    __attribute__((nonnull(1,4)));
int           rand(void);
int           rand_r(unsigned *seed);
long          random(void);
void         *realloc(void *ptr, size_t size) __attribute__((warn_unused_result));
char         *realpath(const char * restrict input, char * restrict output) __attribute__((warn_unused_result));
unsigned short *seed48(unsigned short [3]);
int           setenv(const char *name, const char *value, int replace) __attribute__((nonnull(2)));
void          setkey(const char *) __attribute__((nonnull(1)));
char         *setstate(char *buf) __attribute__((nonnull(1)));
void          srand(unsigned);
void          srand48(long);
void          srandom(unsigned);
double        strtod (const char * restrict nptr, char ** restrict endptr);
float         strtof (const char * restrict nptr, char ** restrict endptr);
long          strtol (const char * restrict nptr, char ** restrict endptr, int base);
long double   strtold(const char * restrict nptr, char ** restrict endptr);
long long     strtoll(const char * restrict nptr, char ** restrict endptr, int base);
unsigned long strtoul(const char * restrict nptr, char ** restrict endptr, int base);
unsigned long long
              strtoull(const char * restrict nptr, char ** restrict endptr, int base);
int           system(const char *command) __attribute__((warn_unused_result));
int           unlockpt(int);
int           unsetenv(const char *name) __attribute__((nonnull(1)));
size_t        wcstombs(char *, const wchar_t *, size_t);
int           wctomb(char *, wchar_t);

// stdio.h
typedef void * FILE;

int printf(const char * restrict format, ...) __attribute__((format(printf,1,2)));
int fprintf(FILE * restrict stream, const char * restrict format, ...) __attribute__((format(printf,2,3)));
int sprintf(char * restrict str, const char * restrict format, ...) __attribute__((format(printf,2,3)));
int snprintf(char * restrict str, size_t size, const char * restrict format, ...) __attribute__((format(printf,3,4)));
int scanf(const char * restrict format, ...) __attribute__((format(scanf,1,2)));
int fscanf(FILE * restrict stream, const char * restrict format, ...) __attribute__((format(scanf,2,3)));
int sscanf(const char * restrict str, const char * restrict format, ...) __attribute__((format(scanf,2,3)));


char *fgets(char * restrict s, int size, FILE * restrict stream)
    __attribute__((warn_unused_result));
char *gets(char * restrict s) __attribute__((warn_unused_result, deprecated));
FILE *fdopen(int fd, const char *mode) __attribute__((warn_unused_result));
FILE *fopen(const char * restrict path, const char * restrict mode) __attribute__((warn_unused_result));
FILE *freopen(const char * restrict path, const char * restrict mode, FILE * restrict stream) __attribute__((warn_unused_result));
int fclose(FILE *fp);
int feof(FILE *stream) __attribute__((warn_unused_result));
int ferror(FILE *stream) __attribute__((warn_unused_result));
int fgetc(FILE *stream);
int fgetc(FILE *stream);
int fileno(FILE *stream) __attribute__((warn_unused_result));
int fputc(int c, FILE *stream);
int fputs(const char *s, FILE *stream);
int getc(FILE *stream);
int getchar(void);
int putchar(int c);
int putc(int c, FILE *stream);
int puts(const char *s);
int remove(const char *);
int rename(const char *, const char *);
int ungetc(int c, FILE *stream);

size_t fread(void * restrict ptr, size_t size, size_t nmemb, FILE * restrict stream)
    __attribute__((warn_unused_result, storage(1,2,3)));
size_t fwrite(const void * restrict ptr, size_t size, size_t nmemb, FILE * restrict stream)
    __attribute__((warn_unused_result, storage(1,2,3)));
void clearerr(FILE *stream);

int __isoc99_fscanf (FILE *__restrict __stream, const char *__restrict __format, ...)  __attribute__((warn_unused_result));
int __isoc99_scanf (const char *__restrict __format, ...)  __attribute__((warn_unused_result));
int __isoc99_sscanf (const char *__restrict __s, const char *__restrict __format, ...)  __attribute__((warn_unused_result));

int _IO_feof(FILE *stream);
int _IO_getc(FILE *stream);
int _IO_putc(int c, FILE *stream);
int _IO_puts(const char *s);


// string.h

void    *memccpy(void *restrict dst, const void *restrict src, int stop, size_t max)
    __attribute__((nonnull(1,2), storage(1,4), storage(2,4)));

void    *memchr(const void *s, int c, size_t n)
    __attribute__((pure,nonnull(1),storage(1,3)));

void *memrchr(const void *s, int c, size_t n)
    __attribute__((pure,nonnull(1),storage(1,3)));

int      memcmp(const void *s1, const void *s2, size_t n)
    __attribute__((pure, nonnull(1,2), storage(1,3), storage(2,3)));

void    *memcpy(void * restrict dst, const void * restrict src, size_t n)
    __attribute__((nonnull(1,2), storage(1,3), storage(2,3))) ;

void    *memmove(void *dst, const void *src, size_t n)
    __attribute__((nonnull(1,2), storage(1,3), storage(2,3)));

void    *memset(void *buf, int c, size_t n)
    __attribute__((nonnull(1), storage(1,3)));

int strcasecmp(const char *s1, const char *s2)
    __attribute__((pure, nonnull(1,2), storage(1), storage(2)));

int strncasecmp(const char *s1, const char *s2, size_t n)
    __attribute__((pure, nonnull(1,2), storage(1,3), storage(2,3)));

char *index(const char *s, int c)
    __attribute__((pure,nonnull(1),storage(1)));

char *rindex(const char *s, int c)
    __attribute__((pure,nonnull(1),storage(1)));

char *stpcpy(char * restrict dst, const char * restrict src)
    __attribute__((nonnull(1,2), storage(1), storage(2)));

char *strcat(char * restrict dst, const char * restrict src)
    __attribute__((nonnull(1,2), storage(1), storage(2)));

char *strchr(const char *s, int c)
    __attribute__((pure,nonnull(1),storage(1)));

int strcmp(const char *s1, const char *s2)
    __attribute__((pure, nonnull(1,2), storage(1), storage(2)));

int strcoll(const char *s1, const char *s2)
    __attribute__((pure, nonnull(1,2), storage(1), storage(2)));

char *strcpy(char * restrict dst, const char * restrict src)
    __attribute__((nonnull(1,2), storage(1), storage(2)));

size_t strcspn(const char *s, const char *reject)
    __attribute__((pure, nonnull(1,2), storage(1), storage(2)));

char *strdup(const char *s) __attribute__((malloc, storage(1), nonnull(1)));

char *strfry(char *string) __attribute__((nonnull(1), storage(1)));

size_t strlen(const char *s) __attribute__((pure, nonnull(1), storage(1)));

char *strncat(char * restrict dst, const char * restrict src, size_t n)
    __attribute__((nonnull(1,2), storage(1), storage(2,3))) ;

int strncmp(const char * s1, const char * s2, size_t n)
    __attribute__((pure, nonnull(1,2), storage(1,3), storage (2,3)));

char *strncpy(char * restrict dst, const char * restrict src, size_t n)
    __attribute__((pure, nonnull(1,2), storage(1,3), storage (2,3)));

char *strpbrk(const char *s, const char *accept)
    __attribute__((pure, nonnull(1,2), storage(1), storage(2)));

char *strrchr(const char *s, int c)
    __attribute__((pure, nonnull(1), storage(1)));

char *strsep(char **stringp, const char *delim) __attribute__((nonnull(1,2)));

size_t strspn(const char *s, const char *accept)
    __attribute__((pure, nonnull(1,2), storage(1), storage(2)));

char *strstr(const char *haystack, const char *needle)
    __attribute__((pure, nonnull(1,2), storage(1), storage(2)));
char *strtok(char *s, const char *delim)
    __attribute__((pure, nonnull(1,2), storage(1), storage(2)));


size_t strxfrm(char * restrict dst, const char * restrict src, size_t n)
    __attribute__((nonnull(1,2), warn_unused_result, storage(dst,n), storage(src,n)))
;


// unix
typedef int mode_t;
int open(const char *pathname, int flags, ...) __attribute__((nonnull(1)));
int creat(const char *pathname, mode_t mode) __attribute__((nonnull(1)));

// socket
struct sockaddr;
typedef size_t socklen_t;

int     accept(int fd, struct sockaddr *addr, socklen_t *len_ptr)
    __attribute__((warn_unused_result));

int     bind(int fd, const struct sockaddr *addr, socklen_t len)
    __attribute__((warn_unused_result));

int     connect(int fd, const struct sockaddr *addr, socklen_t len)
    __attribute__((warn_unused_result));

int     getpeername(int fd, struct sockaddr *addr, socklen_t *len_ptr)
    __attribute__((warn_unused_result));

int     getsockname(int fd, struct sockaddr *addr, socklen_t *len_ptr)
    __attribute__((warn_unused_result));

int     getsockopt(int fd, int level, int optname, void *optval, socklen_t *len)
    __attribute__((warn_unused_result));

int     listen(int fd, int n)
    __attribute__((warn_unused_result));

ssize_t recv(int fd, void *buf, size_t n, int flags)
    __attribute__((warn_unused_result));

ssize_t recvfrom(int fd, void * restrict buf, size_t n, int flags,
                 struct sockaddr * restrict addr,
                 socklen_t * restrict len_ptr)
    __attribute__((warn_unused_result));

ssize_t recvmsg(int fd, struct msghdr *message, int flags)
    __attribute__((warn_unused_result));

ssize_t send(int fd, const void *buf, size_t size, int flags)
    __attribute__((warn_unused_result));

ssize_t sendmsg(int fd, const struct msghdr *message, int flags)
    __attribute__((warn_unused_result));

ssize_t sendto(int fd, const void *buf, size_t n, int flags,
               const struct sockaddr *addr, socklen_t addr_len)
    __attribute__((warn_unused_result));

int     setsockopt(int fd, int level, int optnmae, const void *optval, socklen_t len)
    __attribute__((warn_unused_result));

int     shutdown(int fd, int command)
    __attribute__((warn_unused_result));

int     sockatmark(int fd) __attribute__((warn_unused_result));
int     socket(int domain, int type, int protocol) __attribute__((warn_unused_result));
int     socketpair(int domain, int type, int protocol, int fds[2])
    __attribute__((warn_unused_result));

// unistd.h
typedef int uid_t;
typedef int gid_t;
typedef int pid_t;
typedef int off_t;

int          access(const char *path, int mode)
    __attribute__((warn_unused_result, nonnull(1)));

unsigned     alarm(unsigned seconds);

int          chdir(const char *)
    __attribute__((warn_unused_result, nonnull(1)));

int          chown(const char *parh, uid_t owner, gid_t group)
    __attribute__((warn_unused_result, nonnull(1)));

int          close(int fd);
size_t       confstr(int name, char *buf, size_t n);
char        *crypt(const char *key, const char *salt)
    __attribute__((nonnull(1,2)));

int          dup(int fd) __attribute__((warn_unused_result));
int          dup2(int fd, int fd2);
void         _exit(int status) __attribute__((noreturn));
void         encrypt(char *block, int direction) __attribute__((nonnull(1)));
int          execl(const char *path, const char *arg, ...) __attribute__((nonnull(1,2)));
int          execle(const char *path, const char *arg, ...) __attribute__((nonnull(1,2)));
int          execlp(const char *path, const char *arg, ...) __attribute__((nonnull(1,2)));
int          execv(const char *path, char *const argv[]) __attribute__((nonnull(1,2)));
int          execve(const char *path, char *const argv[], char *const envp[]) __attribute__((nonnull(1,2)));
int          execvp(const char *path, char *const argv[]) __attribute__((nonnull(1,2)));
int          faccessat(int, const char *, int, int) __attribute__((warn_unused_result));
int          fchdir(int fd) __attribute__((warn_unused_result));
int          fchown(int fd, uid_t owner, gid_t group) __attribute__((warn_unused_result));
int          fchownat(int fd, const char *path, uid_t owner, gid_t group, int flag)
    __attribute__((warn_unused_result, nonnull(2)));
int          fdatasync(int fd);
int          fexecve(int fd, char *const argv[], char *const envp[]) __attribute__((nonnull(2)));
pid_t        fork(void) __attribute__((returns_twice));
long         fpathconf(int fd, int name);
int          fsync(int fd);
int          ftruncate(int fd, off_t len) __attribute__((warn_unused_result));
char        *getcwd(char *, size_t) __attribute__((warn_unused_result));
gid_t        getegid(void);
uid_t        geteuid(void);
gid_t        getgid(void);
int          getgroups(int size, gid_t list[]) __attribute__((warn_unused_result));
long         gethostid(void);
int          gethostname(char *buf, size_t len) __attribute__((nonnull(1), storage(1,2)));
char        *getlogin(void) __attribute__((warn_unused_result));
int          getlogin_r(char *buf, size_t size) __attribute__((warn_unused_result, nonnull(1), storage(1,2)));
int          getopt(int argc, char * const *argv, const char *shortopts);
pid_t        getpgid(pid_t pid) __attribute__((warn_unused_result));
pid_t        getpgrp(void) __attribute__((warn_unused_result));
pid_t        getpid(void) __attribute__((warn_unused_result));
pid_t        getppid(void) __attribute__((warn_unused_result));
pid_t        getsid(pid_t pid) __attribute__((warn_unused_result));
uid_t        getuid(void) __attribute__((warn_unused_result));
int          isatty(int fd);
int          lchown(const char *file, uid_t owner, gid_t group) __attribute__((warn_unused_result,nonnull(1)));
int          link(const char *from, const char *to) __attribute__((warn_unused_result,nonnull(1,2)));
int          linkat(int fromfd, const char *from, int tofd, const char *to, int flags) __attribute__((warn_unused_result,nonnull(2,4)));
int          lockf(int fd, int cmd, off_t len) __attribute__((warn_unused_result));
off_t        lseek(int fd, off_t off, int whence);
int          nice(int) __attribute__((warn_unused_result));
long         pathconf(const char *path, int name);
int          pause(void);
int          pipe(int [2]) __attribute__((warn_unused_result));
ssize_t      pread(int fd, void *buf, size_t nbytes, off_t off)
    __attribute__((warn_unused_result));
ssize_t      pwrite(int fd, const void *buf, size_t nbytes, off_t off)
    __attribute__((warn_unused_result));
ssize_t      read(int fd, void *buf, size_t nbytes) __attribute__((warn_unused_result));
ssize_t      readv(int fd, const struct iovec *iov, int iovcnt)
    __attribute__((warn_unused_result));
ssize_t      readlink(const char *restrict path, char *restrict buf, size_t len)
    __attribute__((warn_unused_result, nonnull(1,2), storage(2,3)));
ssize_t      readlinkat(int fd, const char *restrict path, char *restrict buf, size_t len)
    __attribute__((warn_unused_result, nonnull(2,3), storage(3,4)));
int          rmdir(const char *path);
int          setegid(gid_t) __attribute__((warn_unused_result));
int          seteuid(uid_t) __attribute__((warn_unused_result));
int          setgid(gid_t) __attribute__((warn_unused_result));
int          setpgid(pid_t pid, pid_t pgid);
pid_t        setpgrp(void);
int          setregid(gid_t, gid_t) __attribute__((warn_unused_result));
int          setreuid(uid_t, uid_t) __attribute__((warn_unused_result));
pid_t        setsid(void);
int          setuid(uid_t) __attribute__((warn_unused_result));
unsigned     sleep(unsigned);
void         swab(const void *from, void *to, ssize_t n);
int          symlink(const char *from, const char *to) __attribute__((warn_unused_result));
int          symlinkat(const char *from, int fromfd, const char *to) __attribute__((warn_unused_result));
void         sync(void);
long         sysconf(int);
pid_t        tcgetpgrp(int);
int          tcsetpgrp(int, pid_t);
int          truncate(const char *file, off_t length) __attribute__((warn_unused_result));
char        *ttyname(int);
int          ttyname_r(int, char *, size_t);
int          unlink(const char *name);
int          unlinkat(int fd, const char *name, int flags);
ssize_t      write(int, const void *, size_t) __attribute__((warn_unused_result));

FILE *popen(const char *cmd, const char *type)
    __attribute__((warn_unused_result));


int posix_spawn(pid_t *pid, const char *path,
                const void *file_actions,
                const void *attrp,
                char *const argv[], char *const envp[]);

int posix_spawnp(pid_t *pid, const char *file,
                 const void *file_actions,
                 const void *attrp,
                 char *const argv[], char *const envp[]);

// errno.h

int *__errno_location(void);

// unistd.h

int brk(void *addr);
void *sbrk(int increment);


// ctypes.h
int isalnum(int c);
int isalpha(int c);
int iscntrl(int c);
int isdigit(int c);
int isgraph(int c);
int islower(int c);
int isprint(int c);
int ispunct(int c);
int isspace(int c);
int isupper(int c);
int isxdigit(int c);

int isascii(int c);
int isblank(int c);
