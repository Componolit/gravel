
#include <err.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>

void socat_exec(const char *dest, const char *src)
{
    if(execlp("socat", "socat", dest, src, 0) < 0){
        warn("%s: execlp(socat, socat, %s, %s, 0)", __func__, dest, src);
    }
}

void socat_check_pid(int pid, int *status)
{
    usleep(10000); //give socat 10ms to connect
    *status = waitpid(pid, 0, WNOHANG);
}
