--  This generates a gdate binary for doing calendar calculations.


import BasePrelude hiding (option)
import Options.Applicative

import CTE.Date


ifSwitch :: Mod FlagFields Bool -> a -> Parser a -> Parser a
''  x y z = (\a b c -> if a then b else c) <$> switch x <*> pure y <*> z

ifOptional :: Parser a -> (a -> b) -> Parser b -> Parser b
''  op f alt = maybe <$> alt <*> pure f <*> optional op

parseCutoff :: Parser Cutoff
''  =
	ifSwitch
		(short 'e' <> long "english" <> help "Use English cutoff")
		englishCutoff $
	ifSwitch
		(short 'p' <> long "proleptic" <> help "Use proleptic Gregorian")
		NegInf $
	ifSwitch
		(short 'j' <> long "julian" <> help "Use proleptic Julian")
		PosInf $
	ifOptional
		(option auto
			(short 'c' <> long "cutoff" <>
				help "Use specified cutoff" <> metavar "JULIAN_DAY"))
		Finite $
	switch (short 'g' <> long "gregorian" <> help "Use standard Gregorian (default)") $>
	gregorianCutoff

dateStats :: JulianDay -> String
''  jd = unlines $
	( ["Mon","Tues","Wednes","Thurs","Fri","Satur","Sun"] !! fromIntegral (jd`mod`7) ++ "day" ) :
	( "Julian day: " ++ show jd ) :
	( "Gregorian: " ++ show (jdToDate gregorianCutoff jd) ) :
	( (if jd < dateToJD NegInf (Date 4 3 1) then ("Proleptic " ++) else id)
		("Julian: " ++ show (jdToDate PosInf jd)) ) :
	[ "Proleptic Gregorian: " ++ show (jdToDate NegInf jd) | Finite jd < gregorianCutoff ] ++
	[ "English: " ++ dualDate (jdToDate englishCutoff jd)
		| Finite jd < englishCutoff && jd > dateToJD PosInf (Date 1000 3 25) ] ++
	( "Classic Gryd: " ++ let (y, d) = jdToGryd jd in printf "%04d'%03d" y d ) :
	( "Modern Gryd: " ++
		(let (y, m, d) = jdToGrydmon jd in printf "%04d-%s-%02d" y (grydMonth m) (d::Int)) ++
		"   " ++
		let (_, w, d) = jdToGrydwk jd in printf "'%02d%c" (w::Int) (weekdays!!d) ) :
	[]

main = do
	(cut, diffd, arg') <- execParser $
		info (helper <*>
			((,,) <$>
				parseCutoff <*>
				optional (option auto (short 'd' <> long "diff" <> help "Different date" <> metavar "DATE")) <*>
				optional (argument str (metavar "DATE|JULIAN_DAY"))
				))
			(fullDesc <> progDesc "parse and translate dates")
	jd <- case arg' of
		Nothing -> today
		Just arg | '-' `elem` drop 1 arg -> pure $ dateToJD cut (read arg)
					|                       -> pure $ read arg
	case diffd of
		Just dt -> let jd' = dateToJD cut dt in do
			putStr $ unlines $
				("Difference: " ++ show (jd - jd')) :
				"__Date 1__" :
				(dateStats jd' ++ "__Date 2__") :
				[]
		Nothing -> pure ()
	putStr $ dateStats jd

