
handleException = runPure . handle exceptionHandler

-------------------------------------------------------

prg1 = throw "something went wrong"

prg1res :: Either String a
prg1res = handleException prg1

-------------------------------------------------------

prg2 n = return (n + 1)

prg2res :: Integer -> Either String Integer
prg2res n = handleException  $ prg2 n

-------------------------------------------------------

prg3 = do
  v <- ask
  if v < 15 
  then throw $ show v
  else return (v+1)

prg3run n = runPure 
                 . handle exceptionHandler 
                 . handle (readerHandler n)

prg3res :: Integer -> Either String Integer
prg3res n = prg3run n prg3

-------------------------------------------------------

prg4 = do
  v <- ask
  if v < 10
  then throw "Not a valid number"
  else return (v + 1)


prg4run :: Integer -> Integer -> Eff '[Reader Integer, Exception String] Integer -> Integer
prg4run d n = runPure
                  . handle (defValueExceptionHandler d) 
                  . handle (readerHandler n)

prg4res :: Integer -> Integer -> Integer
prg4res d n = prg4run d n prg4

-------------------------------------------------------
