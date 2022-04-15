#pragma once

// Defines a generic 'map' interface...

#ifndef MAP_TYPE
#define MAP_TYPE SKIPLIST
#endif

#if MAPTYPE == SKIPLIST

#include "skiplist.h"

#define map skiplist
#define miter sliter

#define m_create sl_create
#define m_allow_dups sl_allow_dups
#define m_set_tmp sl_set_tmp
#define m_set sl_set
#define m_app sl_app
#define m_get sl_get
#define m_del sl_del
#define m_count sl_count
#define m_find sl_find
#define m_is_next sl_is_next
#define m_next sl_next
#define m_find_key sl_find_key
#define m_is_next_key sl_is_next_key
#define m_next_key sl_next_key
#define m_first sl_first
#define m_done sl_done
#define m_destroy sl_destroy

#endif
