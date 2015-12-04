implementation module Data.List

import Data.Nat
import Data.Int
import Data.Range

import Control.Compare
import Control.Function

import Control.Functor
import Control.Applicative
import Control.Monad

import Control.Foldable
import Control.Traversable
import Control.Sliceable

import Algebra.Group

/// # Instances

/// ## Comparisson

instance Eq [a] | Eq a where
    (==) [] [] = True
    (==) [] _  = False
    (==) _  [] = False
    (==) [a:as] [b:bs]
        | a == b    = as == bs
        | otherwise = False

instance Ord [a] | Ord a where
    (<) [] [] = False
    (<) [] _  = True
    (<) _  [] = False
    (<) [a:as] [b:bs]
        | a < b     = True
        | a > b     = False
        | otherwise = as < bs

/// ## Algebra

instance Semigroup [a] where
    (+) [x:xs] ys = [x : xs + ys]
    (+) []     ys = ys

instance Monoid [a] where
    neutral = []

/// ## Control

instance Functor [] where
    map f xs = [f x \\ x <- xs]

instance Applicative [] where
    pure x = [x]

    (<*>) fs xs = [f x \\ f <- fs, x <- xs]

instance Alternative [] where
    empty = []

    (<|>) xs ys = xs + ys

instance Monad [] where
    (>>=) xs f = [y \\ x <- xs, y <- f x]

/// ## Collection

instance Foldable [] where
    foldr f y xs = go xs
        where
            go [x:xs] = f x (go xs)
            go [] = y

    foldr` f b xs = defaultFoldr` f b xs

    foldl f y xs = go y xs
        where
            go y [x:xs] = go (f y x) xs
            go y [] = y

    foldl` f y xs = go y xs
        where
            go y [x:xs]
                #! y` = f y x
                = go y` xs
            go y [] = y

instance Traversable [] where
    traverse f x = foldr cons_f (pure []) x
        where
            cons_f x ys = (\x xs -> [x:xs]) <$> f x <*> ys

instance Sliceable [] where
    (%) xs r
        = go (int $ start r) (int $ end r) xs
        where
            go _ _ [] = []
            go 0 0 _ = []
            go 0 m [x:xs] = [x : go 0 (m - 1) xs]
            go n m [x:xs] = go (n - 1) (m - 1) xs

/// # Functions

/*
instance length []
    where
//    length ::![a] -> Int
    length xs = acclen 0 xs
    where
        acclen n [x:xs] = acclen (inc n) xs
        acclen n []     = n

instance % [a]
    where
//    (%) :: ![a] !(!Int,!Int) -> [a]
    (%) list (frm,to) = take (to - frm + 1) (drop frm list)

instance toString [x] | toChar x
    where
//    toString::![x] -> {#Char} | toChar x
    toString xs = ltosacc 0 xs (createArray l ' ')
    where
        l                    = length xs
        ltosacc i [h:t] arr    = ltosacc (inc i) t {arr & [i]=toChar h}
        ltosacc _ []    arr    = arr

instance fromString [x] | fromChar x
    where
//    fromString::!{#Char} -> [x] | fromChar x
    fromString s = stolacc s (size s - 1) []
    where
        stolacc :: !String !Int u:[a] -> u:[a] | fromChar a
        stolacc s i acc
            | i >= 0
                = stolacc s (dec i) [fromChar (s.[i]) : acc]
            // otherwise
                = acc


// ****************************************************************************************
// standard operators
// ****************************************************************************************
(!!) infixl 9::![.a] !Int -> .a
(!!) [] _
    = subscript_error
(!!) list i
    =    index list i
    where
        index ::![.a] !Int -> .a
        index [hd:tl] 0
            = hd
        index [hd:tl] n
            = index tl (n - 1)
        index [] _
            = subscript_error

subscript_error = abort "Subscript error in !!,index too large"

flatten :: ![[.a]] -> [.a]
flatten [h:t]    = h ++ flatten t
flatten []        = []

isEmpty::![.a] -> Bool
isEmpty    [] = True
isEmpty    _  = False

// ****************************************************************************************
// standard functions
// ****************************************************************************************

hd::![.a] -> .a
hd [a:x]    = a
hd []        = abort "hd of []"

tl::!u:[.a] -> u:[.a]
tl [a:x]    = x
tl []        = abort "tl of []"

last::![.a] -> .a
last [a]    = a
last [a:tl]    = last tl
last []        = abort "last of []"

init :: ![.a] -> [.a];
init []       = []
init [x]     = []
init [x:xs] = [x: init xs]

take :: !Int [.a] -> [.a]
take n xs
    | n<=0
        = []
        = take n xs
    where
        take :: !Int ![.a] -> [.a]
        take n [x:xs]
            | n<=1
                = [x]
                = [x:take (n-1) xs]
        take n [] = []

takeWhile::(a -> .Bool) !.[a] -> .[a]
takeWhile f [a:x] | f a    = [a:takeWhile f x]
                        = []
takeWhile f []            = []

drop :: !Int !u:[.a] -> u:[.a]
drop n xs | n<=0 = xs
drop n [a:x] = drop (n - 1) x
drop n [] = []

dropWhile :: (a -> .Bool) !u:[a] -> u:[a]
dropWhile f cons=:[a:x]    | f a    = dropWhile f x
                                = cons
dropWhile f []                    = []

span :: (a -> .Bool) !u:[a] -> (.[a],u:[a])
span p list=:[x:xs]
    | p x
        = ([x:ys],zs)
         with    (ys,zs) = span p xs
        = ([],list)
span p []
    = ([], [])

filter::(a -> .Bool) !.[a] -> .[a]
filter f [a:x]    | f a    = [a:filter f x]
                        = filter f x
filter f []                = []

reverse::![.a] -> [.a]
reverse list = reverse_ list []
where
    reverse_ :: ![.a] u:[.a] -> v:[.a], [u <= v]
    reverse_ [hd:tl] list    = reverse_ tl [hd:list]
    reverse_ [] list        = list

insert :: (a a -> .Bool) a !u:[a] -> u:[a];
insert r x ls=:[y : ys]
    | r x y        =     [x : ls]
                =    [y : insert r x ys]
insert _ x []     =     [x]

insertAt :: !Int .a u:[.a] -> u:[.a]
insertAt 0 x ys    = [x:ys]
insertAt _ x []    = [x]
insertAt n x [y:ys]    = [y : insertAt (n-1) x ys]

removeAt :: !Int !u:[.a] -> u:[.a];
removeAt 0 [y : ys]    = ys
removeAt n [y : ys]    = [y : removeAt (n-1) ys]
removeAt n []        = []

updateAt :: !Int .a !u:[.a] -> u:[.a]
updateAt 0 x []    = []
updateAt 0 x [y:ys]    = [x:ys]
updateAt _ x []    = []
updateAt n x [y:ys]    = [y : updateAt (n-1) x ys]

splitAt :: !Int u:[.a] -> ([.a],u:[.a])
splitAt 0     xs    =    ([],xs)
splitAt _     []    =    ([],[])
splitAt n [x:xs]    =    ([x:xs`],xs``)
    where
        (xs`,xs``) = splitAt (n-1) xs

// foldl::(.a -> .(.b -> .a)) .a ![.b] -> .a
foldl op r l
    :==    foldl r l
    where
        foldl r []        = r
        foldl r [a:x]    = foldl (op r a) x

// foldr :: (.a -> .(.b -> .b)) .b ![.a] -> .b    //    op e0 (op e1(...(op r e##)...)
foldr op r l :== foldr l
    where
        foldr []    = r
        foldr [a:x]    = op a (foldr x)

indexList::!.[a] -> [Int]
indexList x = f 0 x
where
    f::!Int ![a] -> [Int]
    f n [a:x]    = [n:f (n+1) x]
    f n []        = []

iterate::(a -> a) a -> .[a]
iterate f x    = [x:iterate f (f x)]

map::(.a -> .b) ![.a] -> [.b]
map f [a:x]    = [f a:map f x]
map f []    = []

repeatn::!.Int a -> .[a]
repeatn n x    = take n (repeat x)

repeat::a -> [a]
repeat x =     cons
where
    cons = [x:cons]

scan:: (a -> .(.b -> a)) a ![.b] -> .[a]
scan op r [a:x]    = [r:scan op (op r a) x]
scan op r []    = [r]

unzip::![(.a,.b)] -> ([.a],[.b])
unzip []    =     ([], [])
unzip [(x,y) : xys] = ([x : xs],[y : ys])
where
    (xs,ys) = unzip xys

zip2::![.a] [.b] -> [(.a,.b)]
zip2 [a:as] [b:bs]    = [(a,b):zip2 as bs]
zip2 as bs            = []

zip::!(![.a],[.b]) -> [(.a,.b)]
zip (x,y) = zip2 x y

diag3:: !.[a] .[b] .[c]-> [.(a,b,c)]
diag3 xs ys zs = [ (x,y,z) \\ ((x,y),z) <- diag2 (diag2 xs  ys) zs ]

//    diagonalisation: basic idea (for infinite lists):
//
//    diag2 xs ys = flatten [ dig2n n xs ys \\ n <- [1..] ]
//    where dig2n n xs ys = [ (a,b) \\ a <- reverse (take n xs) & b <- take n ys ]
//
//    in the definition below this idea is adapted in order to deal with finite lists too

diag2:: !.[a] .[b] -> [.(a,b)]
diag2 [] ys = []
diag2 xs [] = []
diag2 xs ys = [ (ae,be) \\ (a,b) <- takeall xs [] ys [], ae <- a & be <- b ]
where
    takeall xin xout yin yout
    | morex&&morey    = [(nxout,   nyout) : takeall nxin nxout nyin     nyout ]
    | morey            = [( xout,tl nyout) : takeall  xin  xout nyin (tl nyout)]
    | morex            = [(nxout,    yout) : takeall nxin nxout  yin      yout ]
    // otherwise
                    = shift xout yout
    where
        (morex,nxin,nxout) = takexnext xin xout
        (morey,nyin,nyout) = takeynext yin yout

        takexnext [x:xs] accu    = (True, xs,[x:accu])
        takexnext []     accu     = (False,[],accu)

        takeynext [y:ys] accu    = (True, ys,accu++[y])
        takeynext []     accu    = (False,[],accu)

        shift xout [_:ys]    = [(xout,ys): shift xout ys]
        shift _    []         = []

// ****************************************************************************************
// Boolean list
// ****************************************************************************************

and::![.Bool] -> Bool
and []
    = True
and [b : tl]
    = b && and tl

or::![.Bool] -> Bool
or []
    = False
or [b : tl]
    = b || or tl

any::(.a -> .Bool) ![.a] -> Bool
any p []
    = False
any p [b : tl]
    = p b || any p tl

all::(.a -> .Bool) ![.a] -> Bool
all p []
    =    True
all p [b : tl]
    = p b && all p tl

isMember::a !.[a] -> Bool | Eq a
isMember x [hd:tl] = hd==x || isMember x tl
isMember x []    = False

isAnyMember    :: !.[a] !.[a] -> Bool | Eq a        // Is one of arg1 an element arg2
isAnyMember [x:xs] list = isMember x list || isAnyMember xs list
isAnyMember [] list = False

removeDup :: !.[a] -> .[a] | Eq a
removeDup [x:xs] = [x:removeDup (filter ((<>) x) xs)]
removeDup _      = []

removeMember:: a !u:[a] -> u:[a] | Eq a
removeMember e [a:as]
    | a==e        = as
                = [a:removeMember e as]
removeMember e [] = []

removeMembers::!u:[a] !.[a] -> u:[a] | Eq a
removeMembers x []        = x
removeMembers x [b:y]    = removeMembers (removeMember b x) y

removeIndex :: !a !u:[a] -> (Int,u:[a]) | Eq a
removeIndex e xs = removei e xs 0
where
    removei :: !a u:[a] !Int -> (Int,u:[a]) | == a;
    removei e [x:xs] i
        | x==e
            = (i,xs)
            = (j,[x:res])
            with
                (j,res) = removei e xs (inc i)
    removei e [] i = abort "Error in removeIndex: element not found"

limit::!.[a] -> a | Eq a
limit [a:cons=:[b:x]]
    | a==b        = a
    // otherwise
                = limit cons
limit other        = abort "incorrect use of limit"

// ****************************************************************************************
// On PlusMin
// ****************************************************************************************

sum:: !.[a] -> a |  + , zero  a
sum xs = accsum xs zero
where
    accsum [x:xs] n = accsum xs (n + x)
    accsum []     n = n

// ****************************************************************************************
// On Arith
// ****************************************************************************************

prod:: !.[a] -> a | * , one  a
prod xs = accprod one xs
where
    accprod n [x:xs] = accprod (n * x) xs
    accprod n []     = n

avg:: !.[a] -> a | / , IncDec a
avg [] = abort "avg called with empty list"
avg x  = accavg zero zero x
where
    accavg n nelem [x:xs] = accavg (n + x) (inc nelem) xs
    accavg n nelem []     = n / nelem
cons :: u:a v:[u:a] -> w:[u:a], [w <= u,v <= w]
cons x xs = [x:xs]

singleton :: .a -> [.a]
singleton x = [x]

// Safe functions

headDef :: a [a] -> a
headDef d l
    | null l = d
    | otherwise = head l

keep :: Int [a] -> [a]
keep n xs = drop (length xs - n) xs

unzip3 :: ![(.a,.b,.c)] -> ([.a],[.b],[.c])
unzip3 []        = ([], [], [])
unzip3 [(x,y,z) : xyzs]  = ([x : xs],[y : ys],[z : zs])
where
  (xs,ys,zs) = unzip3 xyzs

unzip4 :: ![(.a,.b,.c,.d)] -> ([.a],[.b],[.c],[.d])
unzip4 []          = ([], [], [], [])
unzip4 [(w,x,y,z) : wxyzs]  = ([w : ws],[x : xs],[y : ys],[z : zs])
where
  (ws,xs,ys,zs) = unzip4 wxyzs
unzip5 :: ![(.a,.b,.c,.d,.e)] -> ([.a],[.b],[.c],[.d],[.e])
unzip5 []            = ([], [], [], [], [])
unzip5 [(v,w,x,y,z) : vwxyzs]  = ([v : vs],[w : ws],[x : xs],[y : ys],[z : zs])
where
  (vs,ws,xs,ys,zs) = unzip5 vwxyzs

replaceInList :: !(a a -> Bool) !a ![a] -> [a]
replaceInList cond new []         = [new]
replaceInList cond new [x:xs]
    | cond new x            = [new : xs]
    | otherwise             = [x : replaceInList cond new xs]

splitWith :: !(a -> Bool) ![a] -> (![a],![a])
splitWith f [] = ([],[])
splitWith f [x:xs]
  | f x  = let (y,n) = splitWith f xs in ([x:y],n)
      = let (y,n)  = splitWith f xs in (y,[x:n])

sortByIndex :: ![(!Int,!a)] -> [a]
sortByIndex l = map snd (sortBy (\(a,_) (b,_) -> a < b) l)

intersperse :: !a ![a] -> [a]
intersperse i []      = []
intersperse i [x]     = [x]
intersperse i [x:xs]  = [x,i:intersperse i xs]

intercalate :: .[a] [.[a]] -> .[a]
intercalate xs xss = flatten (intersperse xs xss)

transpose :: ![[a]] -> [.[a]]
transpose []  = []
transpose [[]     : xss] = transpose xss
transpose [[x:xs] : xss] = [[x : [h \\ [h:t] <- xss]] : transpose [xs : [t \\ [h:t] <- xss]]]

subsequences :: .[a] -> .[[a]]
subsequences xs = [[] : nonEmptySubsequences xs]

nonEmptySubsequences :: .[a] -> .[[a]]
nonEmptySubsequences []      =  []
nonEmptySubsequences [x:xs]  =  [[x] : foldr f [] (nonEmptySubsequences xs)]
  where f ys r = [ys : [x : ys] : r]

permutations :: [a] -> .[[a]]
permutations xs0        =  [xs0 : perms xs0 []]
  where
    perms []     _  = []
    perms [t:ts] is = foldr interleave (perms ts [t:is]) (permutations is)
      where interleave    xs     r = let (_,zs) = interleave` id xs r in zs
            interleave` _ []     r = (ts, r)
            interleave` f [y:ys] r = let (us,zs) = interleave` (f o (\xs -> [y:xs])) ys r
                                     in  ([y:us], [f [t:y:us] : zs])

foldl1 :: (.a -> .(.a -> .a)) [.a] -> .a
foldl1 f [x:xs]         =  foldl f x xs

concatMap :: (.a -> [.b]) [.a] -> [.b]
concatMap f ls = flatten (map f ls)

maximum :: .[a] -> a | < a
maximum [x]     = x
maximum [x:xs]  = max x (maximum xs)

minimum :: .[a] -> a | Ord a
minimum xs =  foldl1 min xs

getItems :: ![a] ![Int] -> [a]
getItems list indexes = [x \\ x <- list & idx <- [0..] | isMember idx indexes]

scanl :: (a -> .(.b -> a)) a [.b] -> .[a]
scanl f q ls            =  [q : (case ls of
                                  []     -> []
                                  [x:xs] -> scanl f (f q x) xs)]

scanl1 :: (a -> .(a -> a)) .[a] -> .[a]
scanl1 f [x:xs]         =  scanl f x xs
scanl1 _ []             =  []

foldr1 :: (.a -> .(.a -> .a)) [.a] -> .a
foldr1 _ [x]            =  x
foldr1 f [x:xs]         =  f x (foldr1 f xs)

replicate :: .Int a -> .[a]
replicate n x           =  take n (repeat x)

cycle :: .[a] -> [a]
cycle xs                = xs`
  where xs` = xs ++ xs`

unfoldr :: (.a -> Maybe (.b,.a)) .a -> [.b]
unfoldr f b  =
  case f b of
   Just (a,new_b) -> [a : unfoldr f new_b]
   Nothing        -> []

break :: (a -> .Bool) .[a] -> .([a],[a])
break _ xs=:[]           =  (xs, xs)
break p xs=:[x:xs`]
           | p x        =  ([],xs)
           | otherwise  =  let (ys,zs) = break p xs` in ([x:ys],zs)

stripPrefix :: .[a] u:[a] -> Maybe v:[a] | == a, [u <= v]
stripPrefix [] ys = Just ys
stripPrefix [x:xs] [y:ys]
 | x == y = stripPrefix xs ys
stripPrefix _ _ = Nothing

group :: .(.[a] -> [.[a]]) | == a
group                   =  groupBy (==)

groupBy :: (a -> a -> .Bool) .[a] -> [.[a]]
groupBy _  []           =  []
groupBy eq [x:xs]       =  [[x:ys] : groupBy eq zs]
                           where (ys,zs) = span (eq x) xs

inits :: .[a] -> [.[a]]
inits xs                =  [[] : case xs of
                                  []        -> []
                                  [x : xs`] -> map (\ys -> [x : ys]) (inits xs`)]

tails :: [a] -> .[[a]]
tails xs                =  [xs : case xs of
                                  []        -> []
                                  [_ : xs`] -> tails xs`]

isPrefixOf :: .[a] .[a] -> .Bool | == a
isPrefixOf [] _          =  True
isPrefixOf _  []         =  False
isPrefixOf [x:xs] [y:ys] =  x == y && isPrefixOf xs ys

isSuffixOf :: .[a] .[a] -> .Bool | == a
isSuffixOf x y          =  isPrefixOf (reverse x) (reverse y)

isInfixOf :: .[a] .[a] -> Bool | == a
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

elem :: a .[a] -> .Bool | == a
elem _ []       = False
elem x [y:ys]   = x == y || elem x ys

notElem :: a .[a] -> .Bool | == a
notElem _ []     =  True
notElem x [y:ys] =  x <> y && notElem x ys

lookup :: a [(a,.b)] -> Maybe .b | == a
lookup  _   []          =  Nothing
lookup  key [(x,y):xys]
    | key == x          =  Just y
    | otherwise         =  lookup key xys

find :: (a -> .Bool) -> .(.[a] -> .(Maybe a))
find p          = listToMaybe o filter p

partition :: (a -> .Bool) .[a] -> (.[a],.[a])
partition p xs = foldr (select p) ([],[]) xs
  where select :: .(a -> .Bool) a (u:[a],v:[a]) -> (w:[a],x:[a]), [u <= w,v <= x]
        select p x t =
          let (ts,fs) = t
          in if (p x) ([x:ts],fs) (ts, [x:fs])

elemIndex :: a -> .(.[a] -> .(Maybe Int)) | == a
elemIndex x     = findIndex (\y -> x==y)

elemIndices :: a -> .(.[a] -> .[Int]) | == a
elemIndices x   = findIndices (\y -> x==y)

findIndex :: (.a -> .Bool) -> .([.a] -> .(Maybe Int))
findIndex p     = listToMaybe o findIndices p

findIndices :: (.a -> .Bool) [.a] -> .[Int]
findIndices p xs = [ i \\ (x,i) <- zip2 xs [0..] | p x]

zip3 :: ![.a] [.b] [.c] -> [(.a,.b,.c)]
zip3 [a:as] [b:bs] [c:cs]
  = [(a, b, c): zip3 as bs cs]
zip3 _ _ _
  = []

zip4 :: ![.a] [.b] [.c] [.d] -> [(.a,.b,.c,.d)]
zip4 [a:as] [b:bs] [c:cs] [d:ds]
  = [(a, b, c, d): zip4 as bs cs ds]
zip4 _ _ _ _
  = []

zip5 :: ![.a] [.b] [.c] [.d] [.e] -> [(.a,.b,.c,.d,.e)]
zip5 [a:as] [b:bs] [c:cs] [d:ds] [e:es]
  = [(a, b, c, d, e): zip5 as bs cs ds es]
zip5 _ _ _ _ _
  = []

zipWith :: (.a -> .(.b -> .h)) ![.a] [.b] -> [.h]
zipWith z [a:as] [b:bs]
                   = [ z a b : zipWith z as bs]
zipWith _ _ _ = []

zipSt :: (.a -> .(.b -> (.st -> .st))) ![.a] [.b] .st -> .st
zipSt z [a:as] [b:bs] st
  # st = z a b st
  = zipSt z as bs st
zipSt _ _ _ st = st

zipWithSt :: (.a -> .(.b -> (.st -> .(.h, .st)))) ![.a] [.b] .st -> .([.h], .st)
zipWithSt z [a:as] [b:bs] st
  # (x, st)  = z a b st
  # (xs, st) = zipWithSt z as bs st
  = ([x : xs], st)
zipWithSt _ _ _ st = ([], st)

zipWith3 :: (.a -> .(.b -> .(.c -> .h))) ![.a] [.b] [.c] -> [.h]
zipWith3 z [a:as] [b:bs] [c:cs]
                   = [ z a b c : zipWith3 z as bs cs]
zipWith3 _ _ _ _ = []

zipWith4 :: (.a -> .(.b -> .(.c -> .(.d -> .h))))
      ![.a] [.b] [.c] [.d] -> [.h]
zipWith4 z [a:as] [b:bs] [c:cs] [d:ds]
                   = [ z a b c d : zipWith4 z as bs cs ds]
zipWith4 _ _ _ _ _ = []

zipWith5 :: (.a -> .(.b -> .(.c -> .(.d -> .(.e -> .h)))))
      ![.a] [.b] [.c] [.d] [.e] -> [.h]
zipWith5 z [a:as] [b:bs] [c:cs] [d:ds] [e:es]
                   = [ z a b c d e : zipWith5 z as bs cs ds es]
zipWith5 _ _ _ _ _ _ = []

nub :: .[a] -> .[a] | == a
nub l                   = nub` l []
  where
    nub` [] _           = []
    nub` [x:xs] ls
        | elem x  ls    = nub` xs ls
        | otherwise     = [x : nub` xs [x:ls]]

nubBy :: (a -> .(a -> .Bool)) .[a] -> .[a]
nubBy eq l              = nubBy` l []
  where
    nubBy` [] _         = []
    nubBy` [y:ys] xs
       | elem_by eq y xs = nubBy` ys xs
       | otherwise       = [y : nubBy` ys [y:xs]]

elem_by :: (a -> .(.b -> .Bool)) a [.b] -> .Bool
elem_by _  _ []         =  False
elem_by eq y [x:xs]     =  eq y x || elem_by eq y xs

delete :: u:(a -> v:(w:[a] -> x:[a])) | == a, [v <= u,w <= x]
delete                  =  deleteBy (==)

deleteBy :: (a -> .(b -> .Bool)) a u:[b] -> v:[b], [u <= v]
deleteBy _  _ []        = []
deleteBy eq x [y:ys]    = if (eq x y) ys [y : deleteBy eq x ys]

deleteFirstsBy :: (a -> .(b -> .Bool)) -> u:(v:[b] -> w:(.[a] -> x:[b])), [w <= u,w v <= x]
deleteFirstsBy eq       =  foldl (flip (deleteBy eq))

difference :: u:(v:[a] -> w:(.[a] -> x:[a])) | == a, [w <= u,w v <= x]
difference                    =  differenceBy (==)

differenceBy :: (a -> a -> .Bool) u:[a] .[a] -> v:[a], [u <= v]
differenceBy eq as bs             =  foldl (flip (deleteBy eq)) as bs

intersect :: u:(.[a] -> v:(.[a] -> .[a])) | == a, [v <= u]
intersect               =  intersectBy (==)

intersectBy :: (a -> b -> .Bool) .[a] .[b] -> .[a]
intersectBy _  [] _     =  []
intersectBy _  _  []    =  []
intersectBy eq xs ys    =  [x \\ x <- xs | any (eq x) ys]

union :: u:(.[a] -> v:(.[a] -> .[a])) | == a, [v <= u]
union                   = unionBy (==)

unionBy :: (a -> .(a -> .Bool)) .[a] .[a] -> .[a]
unionBy eq xs ys        =  xs ++ foldl (flip (deleteBy eq)) (nubBy eq ys) xs

isMemberGen :: !a !.[a] -> Bool | gEq{|*|} a
isMemberGen x [hd:tl]   = hd === x || isMemberGen x tl
isMemberGen x []        = False

strictFoldr :: !(.a -> .(.b -> .b)) !.b ![.a] -> .b
strictFoldr _ b []     = b
strictFoldr f b [x:xs] = f x (strictFoldr f b xs)

strictFoldl :: !(.a -> .(.b -> .a)) !.a ![.b] -> .a
strictFoldl f b [] = b
strictFoldl f b [x:xs]
  #! r = f b x
  = strictFoldl f r xs
*/
