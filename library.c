#include "library.h"

#if USE_LDLIBS
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
extern uint8_t _binary_library_dcgs_pl_start[];
extern uint8_t _binary_library_dcgs_pl_end[];
extern uint8_t _binary_library_format_pl_start[];
extern uint8_t _binary_library_format_pl_end[];
extern uint8_t _binary_library_charsio_pl_start[];
extern uint8_t _binary_library_charsio_pl_end[];
#endif

library g_libs[] = {
#if USE_LDLIBS
     {"lists", _binary_library_lists_pl_start, _binary_library_lists_pl_end},
     {"dict", _binary_library_dict_pl_start, _binary_library_dict_pl_end},
     {"apply", _binary_library_apply_pl_start, _binary_library_apply_pl_end},
     {"http", _binary_library_http_pl_start, _binary_library_http_pl_end},
     {"atts", _binary_library_atts_pl_start, _binary_library_atts_pl_end},
     {"error", _binary_library_error_pl_start, _binary_library_error_pl_end},
     {"dcgs", _binary_library_dcgs_pl_start, _binary_library_dcgs_pl_end},
     {"format", _binary_library_format_pl_start, _binary_library_format_pl_end},
     {"charsio", _binary_library_charsio_pl_start, _binary_library_charsio_pl_end},
#endif
     {0}
};

