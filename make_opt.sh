#!/bin/sh

gcc -Os -s -fno-strict-aliasing -fno-unwind-tables -fno-asynchronous-unwind-tables -fno-ident -ffast-math -fomit-frame-pointer -ffunction-sections -fdata-sections -Wl,--gc-sections -Wl,-z,norelro -o are a.c
strip -s --strip-unneeded -R .comment -R .gnu.version -R note -R .eh_frame -R .eh_frame_hdr are
