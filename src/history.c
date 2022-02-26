#include <ctype.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <assert.h>

//#include <readline/readline.h>
//#include <readline/history.h>
#include "isocline/include/isocline.h"

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

char *history_readline_eol(const char *prompt, char eol)
{
	char *cmd = NULL;
	char *line;

	ic_set_prompt_marker("", "");

LOOP:

	if ((line = ic_readline(prompt)) == NULL)
		return NULL;

	if (cmd) {
		size_t n = strlen(cmd) + strlen(line);
		cmd = realloc(cmd, n+1);
		ensure(cmd);
		strcat(cmd, line);
	} else {
		cmd = strdup(line);
	}

	free(line);
	const char *s = cmd;

	for (;;) {
		int ch = get_char_utf8(&s);
		const char *end_ptr = cmd + strlen(cmd) - (strlen(cmd) ? 1 : 0);

		while (isspace(*end_ptr) && (end_ptr != cmd))
			end_ptr--;

		if ((ch == 0) && (*end_ptr == eol)) {
			if (strcmp(cmd, "halt.") && strcmp(cmd, ".")) {
				;//add_history(cmd);
			}

			break;
		}

		if (ch == 0) {
			cmd = realloc(cmd, strlen(cmd)+1+1);
			strcat(cmd, "\n");
			prompt = "";
			goto LOOP;
		}
	}

	return cmd;
}

static char g_filename[1024];

void history_load(const char *filename)
{
	snprintf(g_filename, sizeof(g_filename), "%s", filename);
	ic_set_history(g_filename, 999);
}

void history_save(void)
{
}
