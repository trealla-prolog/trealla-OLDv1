#include "library.h"

extern unsigned char library_builtins_pl[];
extern unsigned int library_builtins_pl_len;
extern unsigned char library_lists_pl[];
extern unsigned int library_lists_pl_len;
extern unsigned char library_apply_pl[];
extern unsigned int library_apply_pl_len;
extern unsigned char library_http_pl[];
extern unsigned int library_http_pl_len;
extern unsigned char library_atts_pl[];
extern unsigned int library_atts_pl_len;
extern unsigned char library_dcgs_pl[];
extern unsigned int library_dcgs_pl_len;
extern unsigned char library_format_pl[];
extern unsigned int library_format_pl_len;
extern unsigned char library_charsio_pl[];
extern unsigned int library_charsio_pl_len;
extern unsigned char library_assoc_pl[];
extern unsigned int library_assoc_pl_len;
extern unsigned char library_ordsets_pl[];
extern unsigned int library_ordsets_pl_len;
extern unsigned char library_dict_pl[];
extern unsigned int library_dict_pl_len;
extern unsigned char library_freeze_pl[];
extern unsigned int library_freeze_pl_len;
extern unsigned char library_dif_pl[];
extern unsigned int library_dif_pl_len;
extern unsigned char library_when_pl[];
extern unsigned int library_when_pl_len;
extern unsigned char library_pairs_pl[];
extern unsigned int library_pairs_pl_len;
extern unsigned char library_random_pl[];
extern unsigned int library_random_pl_len;
extern unsigned char library_lambda_pl[];
extern unsigned int library_lambda_pl_len;
extern unsigned char library_ugraphs_pl[];
extern unsigned int library_ugraphs_pl_len;
extern unsigned char library_sqlite3_pl[];
extern unsigned int library_sqlite3_pl_len;
extern unsigned char library_sqlite3_register_pl[];
extern unsigned int library_sqlite3_register_pl_len;

//extern unsigned char library_clpb_pl[];
//extern unsigned int library_clpb_pl_len;
//extern unsigned char library_clpz_pl[];
//extern unsigned int library_clpz_pl_len;

library g_libs[] = {
	 {"apply", library_apply_pl, &library_apply_pl_len},
	 {"assoc", library_assoc_pl, &library_assoc_pl_len},
	 {"atts", library_atts_pl, &library_atts_pl_len},
	 {"builtins", library_builtins_pl, &library_builtins_pl_len},
	 {"charsio", library_charsio_pl, &library_charsio_pl_len},
	 {"dcgs", library_dcgs_pl, &library_dcgs_pl_len},
	 {"dict", library_dict_pl, &library_dict_pl_len},
	 {"dif", library_dif_pl, &library_dif_pl_len},
	 {"format", library_format_pl, &library_format_pl_len},
	 {"freeze", library_freeze_pl, &library_freeze_pl_len},
	 {"http", library_http_pl, &library_http_pl_len},
	 {"lambda", library_lambda_pl, &library_lambda_pl_len},
	 {"lists", library_lists_pl, &library_lists_pl_len},
	 {"ordsets", library_ordsets_pl, &library_ordsets_pl_len},
	 {"pairs", library_pairs_pl, &library_pairs_pl_len},
	 {"random", library_random_pl, &library_random_pl_len},
	 {"sqlite3", library_sqlite3_pl, &library_sqlite3_pl_len},
	 {"sqlite3_register", library_sqlite3_register_pl, &library_sqlite3_register_pl_len},
	 {"ugraphs", library_ugraphs_pl, &library_ugraphs_pl_len},
	 {"when", library_when_pl, &library_when_pl_len},

	 //{"clpb", library_clpb_pl, &library_clpb_pl_len},
	 //{"clpz", library_clpz_pl, &library_clpz_pl_len},

	 {0}
};
