#include "intergalactic_transmission.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>

static uint8_t parity(uint8_t b) {
    uint8_t p = 0;
    while (b) {
        p ^= b & 1;
        b >>= 1;
    }
    return p;
}

/*
0        1        2        3        4        5        6
00000000 11111111 22222222 33333333 44444444 55555555 66666666
0000000_ 0111111_ 1122222_ 2223333_ 3333444_ 4444455_ 5555556_ 6666666_
0        1        2        3        4        5        6        7
*/
int transmit_sequence(uint8_t* const out, const uint8_t* const in,
                      int const in_len) {
    int out_ix = 0;
    uint8_t byte;  // buffer for bits
    for (int i = 0; i < in_len; i += 7) {
        // out + 0
        byte = in[i] & 0xFE;
        out[out_ix++] = byte | parity(byte);

        // out + 1
        if (i + 1 >= in_len) {
            byte = ((in[i] & 0x1) << 7);
            out[out_ix++] = byte | (in[i] & 1);
            break;
        }
        byte = ((in[i] & 0x1) << 7) | ((in[i + 1] >> 1) & 0x7E);
        out[out_ix++] = byte | parity(byte);

        // out + 2
        if (i + 2 >= in_len) {
            byte = ((in[i + 1] & 0x3) << 6);
            out[out_ix++] = byte | parity(byte);
            break;
        }
        byte = ((in[i + 1] & 0x3) << 6) | ((in[i + 2] >> 2) & 0x3E);
        out[out_ix++] = byte | parity(byte);

        // out + 3
        if (i + 3 >= in_len) {
            byte = ((in[i + 2] & 0x7) << 5);
            out[out_ix++] = byte | parity(byte);
            break;
        }
        byte = ((in[i + 2] & 0x7) << 5) | ((in[i + 3] >> 3) & 0x1E);
        out[out_ix++] = byte | parity(byte);

        // out + 4
        if (i + 4 >= in_len) {
            byte = ((in[i + 3] & 0xF) << 4);
            out[out_ix++] = byte | parity(byte);
            break;
        }
        byte = ((in[i + 3] & 0xF) << 4) | ((in[i + 4] >> 4) & 0xE);
        out[out_ix++] = byte | parity(byte);

        // out + 5
        if (i + 5 >= in_len) {
            byte = ((in[i + 4] & 0x1F) << 3);
            out[out_ix++] = byte | parity(byte);
            break;
        }
        byte = ((in[i + 4] & 0x1F) << 3) | ((in[i + 5] >> 5) & 0x6);
        out[out_ix++] = byte | parity(byte);

        // out + 6
        if (i + 6 >= in_len) {
            byte = ((in[i + 5] & 0x3F) << 2);
            out[out_ix++] = byte | parity(byte);
            break;
        }
        byte = ((in[i + 5] & 0x3F) << 2) | ((in[i + 6] >> 6) & 0x2);
        out[out_ix++] = byte | parity(byte);

        // out + 7
        byte = in[i + 6] << 1;
        out[out_ix++] = byte | parity(byte);
    }
    return out_ix;
}

static bool check_parity(uint8_t b) {
    uint8_t p = 0;
    while (b) {
        p ^= b & 1;
        b >>= 1;
    }
    return p == 0;
}

/*
0        1        2        3        4        5        6        7
0000000_ 0111111_ 1122222_ 2223333_ 3333444_ 4444455_ 5555556_ 6666666_
00000000 11111111 22222222 33333333 44444444 55555555 66666666
0        1        2        3        4        5        6
*/
int decode_message(uint8_t* const out, const uint8_t* const in,
                   int const in_len) {
    int out_ix = 0;
    uint16_t bitbuf = 0;  // buffer for bits
    bool expecting_more = false;
    for (int i = 0; i < in_len; i++) {
        if (!check_parity(in[i])) return -1;
        bitbuf = bitbuf << 8 | in[i];
        uint8_t const m = i % 8;
        switch (m) {
            case 0:
                expecting_more = true;
                continue;
            case 1:
                out[out_ix++] = (bitbuf & 0b11111110'00000000) >> 8 |
                                (bitbuf & 0b10000000) >> 7;
                expecting_more = bitbuf & 0b01111110;
                break;
            case 2:
                out[out_ix++] = (bitbuf & 0b01111110'00000000) >> 7 |
                                (bitbuf & 0b11000000) >> 6;
                expecting_more = bitbuf & 0b00111110;
                break;
            case 3:
                out[out_ix++] = (bitbuf & 0b00111110'00000000) >> 6 |
                                (bitbuf & 0b11100000) >> 5;
                expecting_more = bitbuf & 0b00011110;
                break;
            case 4:
                out[out_ix++] = (bitbuf & 0b00011110'00000000) >> 5 |
                                (bitbuf & 0b11110000) >> 4;
                expecting_more = bitbuf & 0b00001110;
                break;
            case 5:
                out[out_ix++] = (bitbuf & 0b00001110'00000000) >> 4 |
                                (bitbuf & 0b11111000) >> 3;
                expecting_more = bitbuf & 0b00000110;
                break;
            case 6:
                out[out_ix++] = (bitbuf & 0b00000110'00000000) >> 3 |
                                (bitbuf & 0b11111100) >> 2;
                expecting_more = bitbuf & 0b00000010;
                break;
            case 7:
                out[out_ix++] = (bitbuf & 0b00000010'00000000) >> 2 |
                                (bitbuf & 0b11111110) >> 1;
                expecting_more = false;
                break;
            default:
                assert(!"TODO");
        }
    }
    if (expecting_more) return -2;
    return out_ix;
}
