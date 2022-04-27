#include <stdlib.h>
#include <stdio.h>
#include <stddef.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>

#ifdef USE_ISOCLINE
#include "isocline/include/isocline.h"
#else
#include <readline/readline.h>
#include <readline/history.h>
#endif

#ifndef _WIN32
#include <termios.h>
#endif

#include "history.h"
#include "utf8.h"
#include "cdebug.h"

int history_getch(void)
{
#ifndef _WIN32
	struct termios oldattr, newattr;
	tcgetattr(STDIN_FILENO, &oldattr);
	newattr = oldattr;
	newattr.c_lflag &= ~(ICANON | ECHO);
	tcsetattr(STDIN_FILENO, TCSANOW, &newattr);
#endif
	int ch = fgetc_utf8(stdin);
#ifndef _WIN32
	tcsetattr(STDIN_FILENO, TCSANOW, &oldattr);
#endif
	return ch;
}

int history_getch_fd(int fd)
{
#ifndef _WIN32
	struct termios oldattr, newattr;
	tcgetattr(fd, &oldattr);
	newattr = oldattr;
	newattr.c_lflag &= ~(ICANON | ECHO);
	tcsetattr(fd, TCSANOW, &newattr);
#endif
	int ch = fgetc_utf8(stdin);
#ifndef _WIN32
	tcsetattr(fd, TCSANOW, &oldattr);
#endif
	return ch;
}

static char g_filename[1024];


#if !USE_ISOCLINE
char *history_readline_eol(const char *prompt, char eol)
{
	char *cmd = NULL;
	char *line;

LOOP:

	if ((line = readline(prompt)) == NULL)
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
			if (strcmp(cmd, "halt.") && strcmp(cmd, "."))
				add_history(cmd);

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
#else
char *history_readline_eol(const char *prompt, char eol)
{
	char *cmd = NULL;
	char *line;

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
			if (strcmp(cmd, "halt.") && strcmp(cmd, "."))
				ic_history_add(cmd);

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

void history_load(const char *filename)
{
	snprintf(g_filename, sizeof(g_filename), "%s", filename);
	ic_set_history(g_filename, 999);

	ic_enable_brace_matching(false);
	ic_enable_brace_insertion(false);
	ic_enable_completion_preview(false);
	ic_enable_color(false);

	ic_set_default_completer(NULL, NULL);
	ic_set_default_highlighter(NULL, NULL);

	ic_set_prompt_marker("", "");
}

void history_save(void)
{
}
#endif
