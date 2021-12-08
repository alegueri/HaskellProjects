module MarkClassList where

import Data.Char (toLower)

classList01 = [
     ("dalvescb","1045805")
    ]

lookupStudentNum :: String -> [(String,String)] -> String
lookupStudentNum mac_id classList
  | isStudNum mac_id classList = mac_id
  | otherwise = case lookup (map toLower mac_id) classList of
                            Just stud_num -> stud_num
                            Nothing -> error $
                              "failed to lookup stud num for: "
                              ++ mac_id
                              ++ "\nmake sure its set (i.e. replace macid = \"TODO\")"

isStudNum :: String -> [(String,String)] -> Bool
isStudNum stud_num classList = stud_num `elem` (map snd classList)
