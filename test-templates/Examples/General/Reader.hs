
prg1 =  ask

prg1run x = runPure . handle (readerHandler x)

prg1res :: Int -> Int
prg1res x = prg1run x prg1



