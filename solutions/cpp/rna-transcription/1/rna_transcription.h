#pragma once

#include <boost/range/adaptor/transformed.hpp>
#include <stdexcept>
#include <string>

namespace rna_transcription {

using namespace boost;
using namespace boost::adaptors;
using namespace std;

inline char to_rna(char nuc) {
    switch (nuc) {
        case 'A':
            return 'U';
        case 'C':
            return 'G';
        case 'G':
            return 'C';
        case 'T':
            return 'A';
        default:
            throw invalid_argument{string{nuc}};
    }
}

inline string to_rna(string dna) {
    return copy_range<string>(
        dna | transformed(static_cast<char (*)(char)>(to_rna)));
}

}  // namespace rna_transcription
