#include "protein_translation.h"

#include <map>
#include <optional>
#include <set>

namespace protein_translation {

using namespace std;

optional<string> lookup_protein(string codon) {
    const map<string, set<string>> name_to_codon{
        {"STOP", {"UAA", "UAG", "UGA"}},
        {"Cysteine", {"UGU", "UGC"}},
        {"Leucine", {"UUA", "UUG"}},
        {"Methionine", {"AUG"}},
        {"Phenylalanine", {"UUU", "UUC"}},
        {"Serine", {"UCU", "UCC", "UCA", "UCG"}},
        {"Tryptophan", {"UGG"}},
        {"Tyrosine", {"UAU", "UAC"}},
    };
    static map<string, string> codon_to_name;
    if (codon_to_name.empty()) {
        for (const auto& [name, codons] : name_to_codon) {
            for (const auto& codon : codons) {
                codon_to_name.emplace(codon, name);
            }
        }
    }
    const auto it = codon_to_name.find(codon);
    return it != codon_to_name.end() ? optional{it->second} : nullopt;
}

vector<string> proteins(string rna) {
    vector<string> proteins;
    for (size_t i = 0; i < rna.size() / 3; i++) {
        const auto codon = rna.substr(3 * i, 3);
        const auto mprotein = lookup_protein(codon);
        if (not mprotein) {
            return {};
        }
        if (*mprotein == "STOP") {
            break;
        }
        proteins.push_back(*mprotein);
    }
    return proteins;
}

}  // namespace protein_translation
