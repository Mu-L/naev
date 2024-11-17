/*
 * See Licensing and Copyright notice in naev.h
 */
#pragma once

/** @cond */
#include <stddef.h>
/** @endcond */

char *base64_encode( size_t *len, const char *src, size_t sz );
char *base64_decode( size_t *len, const char *src, size_t sz );
char *base64_encode_to_cstr( const char *src, size_t sz );
char *base64_decode_cstr( size_t *len, const char *src );
