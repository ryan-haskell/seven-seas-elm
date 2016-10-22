module RandomHelper exposing (makeSeeds, makeSeedTuples)

import Random


makeSeeds : Random.Seed -> Int -> List Random.Seed
makeSeeds seed numSeeds =
    let
        seedTuples =
            makeSeedsHelper seed numSeeds 0 []

        ( _, seeds ) =
            List.unzip seedTuples
    in
        seeds


makeSeedTuples : Random.Seed -> Int -> List ( Int, Random.Seed )
makeSeedTuples seed numSeeds =
    makeSeedsHelper seed numSeeds 0 []


makeSeedsHelper : Random.Seed -> Int -> Int -> List ( Int, Random.Seed ) -> List ( Int, Random.Seed )
makeSeedsHelper seed maxIndex index seedTuples =
    let
        ( num, newSeed ) =
            Random.step (Random.int Random.minInt Random.maxInt) seed
    in
        if index == maxIndex then
            seedTuples
        else
            makeSeedsHelper newSeed maxIndex (index + 1) (List.append seedTuples [ ( index, newSeed ) ])
