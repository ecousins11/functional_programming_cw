import System.Random

----Part 1 ----

--defining the type for coordinates of the slide. 
--A Float is used which means a decimal place is within the number. 

type Point = (Float,Float)

--y0 = start point
--yi = height ith supporting point

--the velocity function takes the start point and a supporting point height and returns a float for velocity at this point
--for the velocity function I rearranged the formula from g*(y0-yi) = 0.5�v^2 
--v^2 = (9.81*(y0 -yi))/0.5

velocity :: Float -> Float -> Float
velocity y0 yi = ((9.81*(y0 -yi))/0.5)**0.5

--distance function takes 2 points and returns the distance between them
--I rearranged the formula for distance: d^2 = (x1 - x0)**2 + (y1 - y0)**2

distance :: Point -> Point -> Float
distance (x0, y0) (x1, y1) = ((x1 - x0)**2 + (y1 - y0)**2)**0.5

--the time function takes 3 floats and returns a float
--the floats are a distance and 2 velocities
--time is length/average velocity ; average velocity can take the mean so is divided by 2

time :: Float -> Float -> Float -> Float
time d vi vj = d / ((vi + vj)/2)

--arrange in pairs takes the start point, end point and a list of supporting points
--the function will return a list of pairs of points. To do this the Haskell zip function is used. 

arrange_in_pairs :: Point -> Point -> [Point] -> [(Point, Point)]
arrange_in_pairs (x0,y0) (xn,yn) xs = zip xz ys
    where xz = [(x0, y0)] ++ xs
          ys = xs ++ [(xn, yn)]

--total time

--these values will be overwritten once total time is called:
y0=100
x0 =0
xn=100
yn=0

--time_point gets each pair from arrange_in_pairs function and returns the time taken to slide between these points.
--time_point uses distance, velocity and time functions defined above.
time_point :: (Point, Point)-> Float 
time_point ((xm,ym),(xo, yo)) = time d vi vj
        where d = distance (xm,ym) (xo, yo)
              vi = velocity y0 (snd (xm,ym))
              vj = velocity y0 (snd (xo, yo))

--totals_list takes the start point, end point and a list of supporting points and returns a list of floats.
--The list contains calculations of time taken between each point.
totals_list :: Point -> Point -> [Point] -> [Float]
totals_list (x0,y0) (xn,yn) xs = map time_point (arrange_in_pairs (x0,y0) (xn,yn) xs)

--total_time takes the start point, end point and a list of supporting points and results in a float.
--total time uses time_point and totals_list to sum the individual times between each pair.
total_time :: Point -> Point -> [Point] -> Float 
total_time (x0,y0) (xn,yn) xs = sum (totals_list (x0,y0) (xn,yn) xs)

----Part 2----
-- A candidate type is a tuple containing the start point, end point, list of supporting points and the total time relevant. 
type Candidate = (Point, Point, [Point], Float)

--takes end point, start point, a list of lists of supporting points and creates a list of candidates
--make_candidates takes 2 Points (start and end) and a list of lists of supporting points to result in a candidate (including sliding time)
--the function uses recurssion and the base case is reached when the list of supporting points is empty

make_candidates :: Point -> Point -> [[Point]] -> [Candidate]
make_candidates (x0,y0) (xn,yn) [] = []
make_candidates (x0,y0) (xn,yn) (x:xs) = [((x0,y0), (xn,yn), x, (total_time (x0,y0) (xn,yn) x))] ++ (make_candidates (x0,y0) (xn,yn) xs)

--sort by time takes a list of candidates and sorts by sliding time

--get_time is an function to take the time of each candidate in the function below. It takes a candidate and result in just the total sliding time.

get_time :: Candidate -> Float
get_time (p1, p2, ps, t) = t

--sort_by_time takes a list of candidates and orders these candidates into a new list
--recurssion is used with the base case being when the list of candidates taken is empty
--the list comprehension shows the time is taken for every candidate in the set and compared to the next candidates time
sort_by_time :: [Candidate] -> [Candidate]

sort_by_time [] = []
sort_by_time (x:xs) = 
    sort_by_time smaller ++ [x] ++ sort_by_time larger 
    where
        y = get_time x
        smaller = [a | a <- xs, get_time a <= y]
        larger = [b | b <- xs, get_time b > y]

--candidate to string

--point_to_string takes a point and converts it to a string. It is needed for list_to_string and consequently for candidate_to_string
point_to_string :: Point -> String
point_to_string (v1,v2) = show (fst (v1,v2)) ++ " " ++ show (snd (v1,v2)) ++"\n "

--list_to_string helps for the function below. It takes a list of points and converts them into a string
--recurssion is used with the base case being when the list of points turns to an empty list
list_to_string :: [Point] -> String
list_to_string [] = []
list_to_string (x:xs) = point_to_string x ++ list_to_string xs

--candidate_to_string takes a candidate and results in a string
--the list_to_string and point_to string functions above are used; then the string is formatted such as adding "Time" and new lines
candidate_to_string :: Candidate -> String 

candidate_to_string (p1, p2, ps, t) = point_to_string p1 ++ list_to_string ps ++ point_to_string p2 ++ "Time: " ++ show t
        
--divide list

--the each_list function takes 2 lists of floats and creates a list of list of floats.
--the function splits the second (longer list) into the seperate lists the same length as the first list. 
--the function uses recurssion with the base case being when the second list is empty.
each_list :: [Float] -> [Float] -> [[Float]]
each_list xd [] = []
each_list xd yd = [fst (splitAt (length xd) yd)] ++ each_list xd (snd (splitAt (length xd) yd))

--divide_list takes 2 lists of float and makes a list of lists of points. 
--the function uses recurssion; the base case is reached when the second list (longer list) is empty
--each element of the second list will be zipped to an element of the first list in order, even though the first list is longer.
--the each_list function is needed for this

divide_list :: [Float] -> [Float] -> [[Point]] 
divide_list xd [] = []
divide_list xd yd =  map (zip xd) (each_list xd yd)

----Part 3----
--random_list takes a float, a pair of floats and a standard generator
--the result of random list is a list of floats and a new standard generator in a pair. 
--this function uses recurssion based on the integer n specified in the question. n determines the length of the list.
random_list :: Float -> (Float, Float) -> StdGen-> ([Float], StdGen) 
random_list 0 _ gen = ([], gen)
random_list n minmax gen = ((r:rs), g2)
    where 
        (r, g) = randomR minmax gen 
        (rs, g2) = random_list (n-1) minmax g


--create_random_candidates randomly creates a population of candidates
--int = number of candidates
--Point 1 and 2 is start and end
--the x values of supporting points
--min and max value for creating random y values and a random number generator
{-
create_random_candidates :: Int -> Point -> Point -> [Float] -> (Float, Float) -> StdGen -> ([Candidate], StdGen)
create_random_candidates 0 _ _ support (x, y) gene  = ([], gene)
create_random_candidates i sta end support minmax gene  = ((make_candidates sta end (r:rs) ++ make_candidates sta end rs), g2)
    where 
        (r, g) = randomR minmax gene
        (rs, g2) = create_random_candidates (i - 1) sta end gene

-}


---- Part 4----

--crossover function takes a list of candidates, an integer and a standard generator; 
--this will result in a tuple of a list of candidates and a standard generator. 
--The function uses a list comprehension where a pair of candidates (each from the list given) is chosen based on the 0-nth candidate where no 2 candidates can be the same at the same time. 
--the number of candidates that will be created when cross over is called depends on n (for n candidates)

crossover :: [Candidate]-> Int-> StdGen-> ([Candidate], StdGen) 
crossover cs n g = (cs ++ cs_new, g1) 
    where 
        pairs = [((cs !! c1), (cs !! c2)) | c1 <- [0..(n-1)], c2 <-[(c1+1)..(n-1)]] 
        (cs_new, g1) = cross_pairs pairs g

--cross_pairs takes a list of pairs of candidates and a standard generator
--It result in a tuple of a list of candidates and a new standard generator
--The function is recursive and the base case is reached when there is an empty list for the list of candidate pairs. 

cross_pairs :: [(Candidate, Candidate)]-> StdGen-> ([Candidate], StdGen) 
cross_pairs [] g = ([], g) 
cross_pairs (cp:cps) g = (c:cs, g2) 
    where 
        (c, g1) = cross_pair cp g 
        (cs, g2) = cross_pairs cps g1

--The function cross_pair takes a pair of candidates and a standard generator
--It results in a candidate and a new standard generator in a pair
--from the 2 candidates, only 1 start and end point, both lists of supporting points and the standard generator is used. 
--The function cross_supp is used on both lists of supporting points with the standard generator 
--From cross_supp a total time is calculated with the start, list of supporting points and end point.

cross_pair :: (Candidate, Candidate)-> StdGen-> (Candidate, StdGen)
cross_pair ((s, e, ps1, _), (_, _, ps2, _)) g = ((s, e, ps, t), g1) 
    where 
        (ps, g1) = cross_supp ps1 ps2 g 
        t = total_time s e ps

--The function cross_supp takes 2 lists of points and a standard generator
--cross_supp results in a list of one of the list of points and a standard generator in a pair. 
--The function decides whether to take the first candidate or second candidate based on whether 'r' is > or < 0.5.
--r depends on the random generator but must be between 0.0 and 1.1
--the function is recursive and the base case is reached when there is 2 empty lists of points
--each list of (supporting) points must be the same length (otherwise the function i non-exhaustive)
--0::Float means convert 0 to a float

cross_supp :: [Point] -> [Point]-> StdGen-> ([Point], StdGen) 
cross_supp [] [] g = ([], g) 
cross_supp (c1:cs1) (c2:cs2) g = (( if r < 0.5 then c1 else c2) : xs, g2) 
    where 
        (r, g1) = randomR (0 :: Float, 1 :: Float) g 
        (xs, g2) = cross_supp cs1 cs2 g
