
#include <err.h>
#include <sys/types.h>
#include <unistd.h>

void socat_fork(int *pid)
{
    *pid = fork();
    if(*pid < 0){
        warn("%s: fork()", __func__);
    }
}

void socat_exec(const char *dest, const char *src)
{
    warn("%s, %s", dest, src);
    if(execlp("socat", "socat", dest, src, 0) < 0){
        warn("%s: execlp(socat, socat, %s, %s, 0)", __func__, dest, src);
    }
}
