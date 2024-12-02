module Queue where

import qualified Data.Set as S
import qualified Data.Sequence as Seq

newtype Queue a  = Q (Seq.Seq a) deriving (Eq,Show)
empty  = Q Seq.empty
isempty :: Queue a -> Bool 
isempty (Q q)  = case q of
  Seq.Empty -> True
  _ -> False

enqueue :: Queue a -> a -> Queue a
enqueue (Q q)  a =
  Q (a Seq.<| q) 

dequeue :: Queue a -> (a , Queue a)
dequeue (Q (new_q Seq.:|> elem))  =  (elem, Q new_q)

enqueueMulti :: Queue a -> [a] -> Queue a
enqueueMulti = foldl enqueue

singleton :: a -> Queue a
singleton a = Q (Seq.singleton a)
