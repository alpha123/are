/**
 * u.c - unicode and utf8 utilities
 */

#pragma once

#include "a.h"

/* https://github.com/skeeto/branchless-utf8
   modified to branch so that it doesn't require three trailing \0 bytes */
E u8d(USZ z, U8B buf[z], CP *cp, USZ *out_len){
    static const uint8_t lengths[] = {
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 3, 3, 4, 0
    };
    static const uint8_t masks[]  = {0x00, 0x7f, 0x1f, 0x0f, 0x07};
    static const uint32_t mins[] = {4194304, 0, 128, 2048, 65536};
    static const uint8_t shiftc[] = {0, 18, 12, 6, 0};
    static const uint8_t shifte[] = {0, 6, 4, 2, 0};

    U8B *s = buf, len = lengths[s[0] >> 3];
    *out_len = len;

    /* Assume a four-byte character and load four bytes. Unused bits are
     * shifted out.
     */
    U8B s0=s[0],s1=z>1?s[1]:0,s2=z>2?s[2]:0,s3=z>3?s[3]:0;
    *cp  = (CP)(s0 & masks[len]) << 18;
    *cp |= (CP)(s1 & 0x3f) << 12;
    *cp |= (CP)(s2 & 0x3f) <<  6;
    *cp |= (CP)(s3 & 0x3f) <<  0;
    *cp >>= shiftc[len];

    /* Accumulate the various error conditions. */
    E err = (*cp < mins[len]) << 6; // non-canonical encoding
    err |= ((*cp >> 11) == 0x1b) << 7;  // surrogate half?
    err |= (*cp > 0x10FFFF) << 8;  // out of range?
    err |= (s[1] & 0xc0) >> 2;
    err |= (s[2] & 0xc0) >> 4;
    err |= (s[3]       ) >> 6;
    err ^= 0x2a; // top two bits of each tail byte correct?
    err >>= shifte[len];

    return err;
}