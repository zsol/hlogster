module Metric1 (metrics1) where

import Data.List.Split (splitOn)

aux True acc = acc + 1
aux _ acc = acc
countTrue l = (foldr aux 0 l)

match_category_and_function cname fname fields = if ( fields !! 3 == cname) && (((splitOn ":" (fields !! 5 )) !! 1) == fname) then True else False

metrics1 tokenized =
	("hlogster.usage.vacak", countTrue (map filter_usage_presentations_delete tokenized))
	where 
		filter_usage_presentations_delete = match_category_and_function "usage" "presentations_delete"


