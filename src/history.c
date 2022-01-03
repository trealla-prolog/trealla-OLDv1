#include <ctype.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <assert.h>

#include <readline/readline.h>
#include <readline/history.h>
#include "history.h"
#include "utf8.h"

#include <termios.h>
#include <unistd.h>

#include "cdebug.h"

int history_getch(void)
{
	struct termios oldattr, newattr;
	tcgetattr(STDIN_FILENO, &oldattr);
	newattr = oldattr;
	newattr.c_lflag &= ~(ICANON | ECHO);
	tcsetattr(STDIN_FILENO, TCSANOW, &newattr);
	int ch = fgetc_utf8(stdin);
	tcsetattr(STDIN_FILENO, TCSANOW, &oldattr);
	return ch;
}

int history_getch_fd(int fd)
{
	struct termios oldattr, newattr;
	tcgetattr(fd, &oldattr);
	newattr = oldattr;
	newattr.c_lflag &= ~(ICANON | ECHO);
	tcsetattr(fd, TCSANOW, &newattr);
	int ch = fgetc_utf8(stdin);
	tcsetattr(fd, TCSANOW, &oldattr);
	return ch;
}

char *history_readline_eol(const char *prompt, __attribute__((unused)) char eol)
{
	char *line;

	if ((line = readline(prompt)) == NULL)
		return NULL;

	if (line[0] && (line[0] != '\r') && (line[0] != '\n'))
		add_history(line);

	return line;
}

static char g_filename[1024];

void history_load(const char *filename)
{
	snprintf(g_filename, sizeof(g_filename), "%s", filename);
	using_history();
	read_history(g_filename);
}

void history_save(void)
{
	write_history(g_filename);
	//rl_clear_history();
	clear_history();
}
