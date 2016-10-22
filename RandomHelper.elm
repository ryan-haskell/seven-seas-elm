module RandomHelper exposing (makeSeeds)

import Random


makeSeeds : Random.Seed -> Int -> List Random.Seed
makeSeeds seed numSeeds =
    makeSeedsHelper seed numSeeds 0 []


makeSeedsHelper : Random.Seed -> Int -> Int -> List Random.Seed -> List Random.Seed
makeSeedsHelper seed maxIndex index seeds =
    let
        ( num, newSeed ) =
            Random.step (Random.int Random.minInt Random.maxInt) seed
    in
        if index == maxIndex then
            seeds
        else
            makeSeedsHelper newSeed maxIndex (index + 1) (List.append seeds [ newSeed ])
