positives :: [Integer] -> [Integer]
-- We assume 0 is included
positives [xs] = if xs >= 0 then [xs] else []
positives (x:xs) = if x >= 0 then [x] ++ (positives xs) else positives xs