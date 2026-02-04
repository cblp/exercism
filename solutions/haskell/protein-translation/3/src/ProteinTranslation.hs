{-# LANGUAGE LambdaCase #-}

module ProteinTranslation (proteins) where

proteins :: String -> Maybe [String]
proteins "" = Just []
proteins rna =
    lookup codon proteinTable >>= \case
        "STOP" -> Just []
        protein -> (protein :) <$> proteins rest
  where
    (codon, rest) = splitAt 3 rna

proteinTable :: [(String, String)]
proteinTable =
    [ (codon, protein)
    | (codons, protein) <-
        [ (["AUG"], "Methionine")
        , (["UUU", "UUC"], "Phenylalanine")
        , (["UUA", "UUG"], "Leucine")
        , (["UCU", "UCC", "UCA", "UCG"], "Serine")
        , (["UAU", "UAC"], "Tyrosine")
        , (["UGU", "UGC"], "Cysteine")
        , (["UGG"], "Tryptophan")
        , (["UAA", "UAG", "UGA"], "STOP")
        ]
    , codon <- codons
    ]
