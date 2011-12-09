module ISO where
import Data.List
import Data.Char
import System.Random
-- import Data.Graph.Inductive
import Data.Tree

type N = Integer
cons :: N->N->N
cons x y  = (2^x)*(2*y+1)

hd :: N->N
hd n | n>0 = if odd n then 0 else 1+hd  (n `div` 2)

tl :: N->N
tl n = n `div` 2^((hd n)+1)

as_nats_nat :: N->[N]
as_nats_nat 0 = []
as_nats_nat n = hd n : as_nats_nat (tl n)
 
as_nat_nats :: [N]->N  
as_nat_nats [] = 0
as_nat_nats (x:xs) = cons x (as_nat_nats xs)

append 0 ys = ys
append xs ys = cons (hd xs) (append (tl xs) ys)

data Iso a b = Iso (a->b) (b->a)

compose :: Iso a b -> Iso b c -> Iso a c
compose (Iso f g) (Iso f' g') = Iso (f' . f) (g . g')

itself = Iso id id

invert (Iso f g) = Iso g f

type Encoder a = Iso a N

nat :: Encoder N
nat = itself

nats :: Encoder [N] 
nats = Iso as_nat_nats as_nats_nat

as :: Encoder a -> Encoder b -> b -> a
as that this x = g x where 
   Iso _ g = compose that (invert this)

mset :: Encoder [N]
mset = compose (Iso as_nats_mset as_mset_nats) nats

as_mset_nats ns = tail (scanl (+) 0 ns)
as_nats_mset ms = zipWith (-) (ms) (0:ms)

set :: Encoder [N]
set = compose (Iso as_nats_set as_set_nats) nats

as_set_nats = (map pred) . as_mset_nats . (map succ)
as_nats_set = (map pred) . as_nats_mset . (map succ)

data T = H [T] deriving (Eq,Ord,Read,Show)
--type Ts = [T]

unrank :: (a -> [a]) -> a -> T
mapUnrank :: (a -> [a]) -> [a] -> [T]

rank :: ([b] -> b) -> T -> b
mapRank :: ([b] -> b) -> [T] -> [b]

unrank f n = H (mapUnrank f (f n))
mapUnrank f ns = map (unrank f) ns

rank g (H ts) = g (mapRank g ts)
mapRank g ts = map (rank g) ts

lift :: Iso b [b] -> Iso T b
lift (Iso f g) = Iso (rank g) (unrank f)

mapLift :: Iso b [b] -> Iso [T] [b]
mapLift (Iso f g) = Iso (mapRank g) (mapUnrank f)

hfs :: Encoder T
hfs = lift (Iso (as set nat) (as nat set))

ackermann = as nat hfs
inverse_ackermann = as hfs nat

hfm :: Encoder T
hfm = lift (Iso (as mset nat) (as nat mset))

hff :: Encoder T
hff = lift (Iso (as nats nat) (as nat nats))

unpair z = (hd (z+1), tl (z+1))
pair (x,y) = (cons x y)-1

type N2 = (N,N)
nat2 :: Encoder N2
nat2 = Iso pair unpair

inflate = (as nat set) . (map (*2)) . (as set nat)

bpair (i,j) = inflate i + ((*2) . inflate) j
bunpair k = (deflate xs,deflate ys) where 
  (xs,ys) = partition even (as set nat k)
  deflate = (as nat set) . (map (`div` 2))

nat2' :: Encoder N2
nat2' = Iso bpair bunpair

pair2unord_pair (x,y) = as set nats [x,y]
unord_pair2pair [a,b] = (x,y) where 
  [x,y]=as nats set [a,b]   

unord_unpair = pair2unord_pair . bunpair
unord_pair = bpair . unord_pair2pair

set2 :: Encoder [N]
set2 = compose (Iso unord_pair unord_unpair) nat

pair2mset_pair (x,y) = (a,b) where [a,b]=as mset nats [x,y]
mset_unpair2pair (a,b) = (x,y) where [x,y] = as nats mset [a,b]

mset_unpair = pair2mset_pair . bunpair
mset_pair = bpair . mset_unpair2pair

mset2 :: Encoder N2
mset2 = compose (Iso mset_unpair2pair pair2mset_pair) nat2

digraph2set ps = map bpair ps
set2digraph ns = map bunpair ns

digraph :: Encoder [N2]
digraph = compose (Iso digraph2set set2digraph) set

graph2set ps = map unord_pair ps
set2graph ns = map unord_unpair ns

graph :: Encoder [[N]]
graph = compose (Iso graph2set set2graph) set

mdigraph :: Encoder [N2]
mdigraph = compose (Iso digraph2set set2digraph) nats

mgraph :: Encoder [[N]]
mgraph = compose (Iso graph2set set2graph) nats

set2hypergraph = map (as set nat)
hypergraph2set = map (as nat set)

hypergraph :: Encoder [[N]]
hypergraph = compose (Iso hypergraph2set set2hypergraph) set

hff_pars :: Encoder [N]
hff_pars = compose (Iso pars2hff hff2pars) hff

pars2hff cs = parse_pars 0 1 cs

parse_pars l r cs | newcs == [] = t where
  (t,newcs)=pars_expr l r cs
  pars_expr l r (c:cs) | c==l = ((H ts),newcs) where
    (ts,newcs) = pars_list l r cs    
  pars_list l r (c:cs) | c==r = ([],cs)
  pars_list l r (c:cs) = ((t:ts),cs2) where 
    (t,cs1)=pars_expr l r (c:cs)
    (ts,cs2)=pars_list l r cs1

hff2pars = collect_pars 0 1

collect_pars l r (H ns) =
  [l]++ (concatMap (collect_pars l r) ns)++[r] 

z:: Encoder Z
z = compose (Iso z2nat nat2z) nat

nat2z n = if even n then n `div` 2 else (-n-1) `div` 2
z2nat n = if n<0 then -2*n-1 else 2*n

type Z = Integer
type Z2 = (Z,Z)

z2 :: Encoder Z2
z2 = compose (Iso zpair zunpair) nat

zpair (x,y) = (nat2z . bpair) (z2nat x,z2nat y)
zunpair z = (nat2z n,nat2z m) where 
  (n,m)= (bunpair . z2nat) z

mz2 :: Encoder Z2
mz2 = compose (Iso mzpair mzunpair) nat

mzpair (x,y) = (nat2z . mset_pair) (z2nat x,z2nat y)
mzunpair z = (nat2z n,nat2z m) where 
  (n,m)= (mset_unpair . z2nat) z

set2sat = map (set2disj . (as set nat)) where
  shift0 z = if (z<0) then z else z+1
  set2disj = map (shift0. nat2z)
  
sat2set = map ((as nat set) . disj2set) where
  shiftback0 z = if(z<0) then z else z-1
  disj2set = map (z2nat . shiftback0)

sat :: Encoder [[Z]]
sat = compose (Iso sat2set set2sat) set

string :: Encoder String
string = Iso string2nat nat2string

string2nat cs = from_base 128 (map (fromIntegral . ord) cs)

nat2string xs = map (chr . fromIntegral) (to_base 128 xs)

b x = pred x -- begin
o x = 2*x+0  -- bit 0
i x = 2*x+1  -- bit 1
e = 1        -- end

data D = E | O D | I D deriving (Eq,Ord,Show,Read)
data B = B D deriving (Eq,Ord,Show,Read)

funbits2nat :: B -> N
funbits2nat = bfold b o i e

bfold fb fo fi fe (B d) = fb (dfold d) where
  dfold E = fe
  dfold (O x) = fo (dfold x)
  dfold (I x) = fi (dfold x)

b' x = succ x
o' x | even x = x `div` 2
i' x | odd x = (x-1) `div` 2
e' = 1

nat2funbits :: N -> B
nat2funbits = bunfold b' o' i' e'

bunfold fb fo fi fe x = B (dunfold (fb x)) where
  dunfold n | n==fe = E
  dunfold n | even n = O (dunfold (fo n))
  dunfold n | odd n = I (dunfold (fi n))

funbits :: Encoder B
funbits = compose (Iso funbits2nat nat2funbits) nat

bsucc (B d) = B (dsucc d) where
  dsucc E = O E
  dsucc (O x) = I x
  dsucc (I x) = O (dsucc x) 

bits :: Encoder [N]
bits = Iso as_nat_bits as_bits_nat

as_bits_nat = drop_last . (to_base 2) . succ where
  drop_last = reverse . tail . reverse

as_nat_bits bs = pred (from_base 2 (bs ++ [1]))
    
to_base base n = 
  d : (if q==0 then [] else (to_base base q)) where
     (q,d) = quotRem n base
    
from_base base [] = 0
from_base base (x:xs) | x>=0 && x<base = 
   x+base*(from_base base xs)

s [] = [0]
s (0:xs) = 1:xs
s (1:xs) = 0:s xs

p [0] = []
p (0:xs) = 1:p xs 
p (1:xs) = 0:xs

nf f = (as nat bits) . f . (as bits nat)
nf2 f x y = as nat bits (f (as bits nat x) (as bits nat y))

borrow_from lender borrower f = 
  (as borrower lender) . f . (as lender borrower)

borrow_from2 lender borrower f x y = 
  (as borrower lender) 
  (f (as lender borrower x) (as lender borrower y))

db  = p . (0:)
hf = tail . s

h :: [N]->[N]
h = h' . p

h' [] = []
h' (1:_) = []
h' (0:xs) = s (h' xs)

t :: [N]->[N]
t = t' . p
t' = hf . t''

t'' (0:xs) = t'' xs
t'' xs = xs

c x ys = s (c' x ys)
c' x xs = c'' x  (db xs)

c'' [] ys = ys
c'' xs ys = 0: c'' (p xs) ys

sm xs ys = p (sm0 xs ys)

sm0 [] [] = [0]
sm0 [] (0:ys) = 1:ys
sm0 [] (1:ys) = 0:(s ys)
sm0 (0:xs) [] = 1:xs
sm0 (1:xs) [] = 0:(s xs)
sm0 (0:xs) (0:ys) = 0:(sm0 xs ys)
sm0 (0:xs) (1:ys) = 1:(sm0 xs ys)
sm0 (1:xs) (0:ys) = 1:(sm0 xs ys)
sm0 (1:xs) (1:ys) = 0:(sm1 xs ys)

sm1 xs ys = s (sm0 xs ys)

m [] _ = []
m _ [] = []
m xs ys = s (m1 (p xs) (p ys)) where
  m1 [] ys = ys
  m1 (0:xs) ys = 0:(m1 xs ys)
  m1 (1:xs) ys = sm0 ys (0:(m1 xs ys))

lst nat = cons nat 0

len 0 = 0
len xs = succ (len (tl xs))

nzipWith _ 0 _ = 0
nzipWith _ _ 0 = 0
nzipWith f xs ys = 
  cons (f (hd xs) (hd ys)) 
       (nzipWith f (tl xs) (tl ys))

nzip xs ys = nzipWith cons xs ys

nunzip zs = (xs,ys) where 
  xs = nMap hd zs 
  ys = nMap tl zs

getAssoc _ 0 = 0
getAssoc k ps = 
  if 0==xy then 0 
  else if k== x then y 
  else getAssoc k (tl ps) where 
    xy=hd ps
    x=hd xy
    y=tl xy
 
addAssoc k v ps=cons (cons k v) ps

nfoldl _ z 0    =  z
nfoldl f z xs =  nfoldl f (f z (hd xs)) (tl xs)

nfoldr f z 0     =  z
nfoldr f z xs =  f (hd xs) (nfoldr f z (tl xs))

nscanl _ q 0 = lst q
nscanl f q xs = cons q (nscanl f (f q (hd xs)) (tl xs))

rev = nfoldl (flip cons) 0

nMap f ns = nfoldr (\x xs->cons (f x) xs) 0 ns
 
nconcat xss = nfoldr append 0 xss 
 
nconcatMap f xs = nconcat (nMap f xs)

infixl 1  >>-
m >>- k = nconcatMap k m
nreturn = lst
 
nsequence = foldr mcons (nreturn 0) where 
  mcons p q = p>>- \x -> q >>- \y ->nreturn (cons x y)

nth thing = as thing nat
nths thing = map (nth thing)
stream_of thing = nths thing [0..]

ran thing seed largest = 
  head (random_gen thing seed largest 1)

random_gen thing seed largest n = genericTake n
  (nths thing (rans seed largest))
  
rans seed largest = 
    randomRs (0,largest) (mkStdGen seed)

