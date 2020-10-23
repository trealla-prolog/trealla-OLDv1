#include "library.h"

extern uint8_t _binary_library_lists_pl_start[];
extern uint8_t _binary_library_lists_pl_end[];

extern uint8_t _binary_library_dict_pl_start[];
extern uint8_t _binary_library_dict_pl_end[];

extern uint8_t _binary_library_apply_pl_start[];
extern uint8_t _binary_library_apply_pl_end[];

extern uint8_t _binary_library_http_pl_start[];
extern uint8_t _binary_library_http_pl_end[];

extern uint8_t _binary_library_atts_pl_start[];
extern uint8_t _binary_library_atts_pl_end[];

extern uint8_t _binary_library_error_pl_start[];
extern uint8_t _binary_library_error_pl_end[];

library g_libs[] = {
     {"lists", _binary_library_lists_pl_start, _binary_library_lists_pl_end},
     {"dict", _binary_library_dict_pl_start, _binary_library_dict_pl_end},
     {"apply", _binary_library_apply_pl_start, _binary_library_apply_pl_end},
     {"http", _binary_library_http_pl_start, _binary_library_http_pl_end},
     {"atts", _binary_library_atts_pl_start, _binary_library_atts_pl_end},
     {"error", _binary_library_error_pl_start, _binary_library_error_pl_end},
     {0}
};
