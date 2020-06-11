
data Color = Red | Black deriving (Show, Eq)

data RBTree = NIL | Node Color Int RBTree RBTree deriving (Show, Eq)

----------------------------------------------------------

colorNode :: Color -> RBTree -> RBTree 
colorNode c (Node _ x tl tr) = Node c x tl tr
colorNode c NIL = NIL


rotateLeft :: RBTree -> RBTree
rotateLeft (Node c x tl (Node cr y (Node crl z trll trlr) trr)) = (Node cr y (Node c x tl (Node crl z trll trlr)) trr)

rotateRight :: RBTree -> RBTree
rotateRight (Node c x (Node cl y tll (Node clr z tlrl tlrr)) tr) = (Node cl y tll (Node c x (Node clr z tlrl tlrr) tr))

searchValue :: Int -> RBTree -> Bool
searchValue v NIL = False
searchValue v (Node c x tl tr)
    | v == x = True
    | v > x = searchValue v tr
    | v < x = searchValue v tl

balance :: RBTree -> RBTree
balance (Node Black b (Node Red r1 (Node Red r2 tr2l tr2r) tr1r) btr) = (Node Red r1 (Node Black r2 tr2l tr2r) (Node Black b tr1r btr))
balance (Node Black b (Node Red r1 tr1l (Node Red r2 tr2l tr2r)) btr) = (Node Red r2 (Node Black r1 tr1l tr2l) (Node Black b tr2r btr))
balance (Node Black b btl (Node Red r1 (Node Red r2 tr2l tr2r) tr1r)) = (Node Red r2 (Node Black b btl tr2l) (Node Black r1 tr2r tr1r))
balance (Node Black b btl (Node Red r1 tr1l (Node Red r2 tr2l tr2r))) = (Node Red r1 (Node Black b btl tr1l) (Node Black r2 tr2l tr2r))
balance (Node color x tl tr) = (Node color x tl tr)
balance NIL = NIL

----------------------------------------------------------

insertV :: Int -> RBTree -> RBTree
insertV v NIL = (Node Red v NIL NIL)
insertV v (Node c x tl tr)
    | v==x = (Node c x tl tr)  
    | v>x = (balance (Node c x tl (insertV v tr)))
    | v<x = (balance (Node c x (insertV v tl) tr)) 

insertValue :: Int -> RBTree -> RBTree
insertValue v NIL = Node Black v NIL NIL
insertValue v (Node c x tl tr) = colorNode Black (insertV v (Node c x tl tr))

----------------------------------------------------------

deleteValue :: Int -> RBTree -> RBTree
deleteValue v tree = colorNode Black (del v tree)

del :: Int -> RBTree -> RBTree
del v NIL = NIL
del v (Node c x tl tr)
  | v < x = delL v (Node c x tl tr)
  | v > x = delR v (Node c x tl tr)
  | v==x = balance ( uni tl tr )

delL :: Int -> RBTree -> RBTree
delL v (Node c x tl tr) = balL (Node c x (del v tl) tr)


delR :: Int -> RBTree -> RBTree
delR v (Node c x tl tr) = balR (Node c x tl (del v tr))


balL :: RBTree -> RBTree
balL (Node Black x NIL (Node Black y NIL NIL)) = (Node Black x NIL (Node Red y NIL NIL))
balL (Node Black x NIL (Node Black y trl trr)) = (balance (Node Black x NIL (Node Red y trl trr)))
balL (Node Black x (Node Black y (Node Red z tlll tllr) (Node Red w tlrl tlrr)) (Node Red u trl trr)) = (Node Black x (Node Black y (Node Black z tlll tllr) (Node Black w tlrl tlrr)) (Node Red u trl trr))
balL (Node Black x (Node Black y tll NIL) (Node Red z trl trr)) = rotateLeft (Node Red x (Node Black y tll NIL) (Node Black z trl trr))
balL (Node Red x (Node Black y (Node Red z NIL NIL) NIL) (Node Black w trl trr)) = rotateLeft (Node Red x (Node Black y (Node Red z NIL NIL) NIL) (Node Black w trl trr)) 
balL (Node Red x (Node Black y NIL (Node Red z NIL NIL)) (Node Black w trl trr)) = rotateLeft (Node Red x (Node Black y NIL (Node Red z NIL NIL)) (Node Black w trl trr))
balL (Node Red x (Node Red y tll tlr) tr) = (Node Red x (Node Black y tll tlr) tr)
balL (Node c x tl tr) = (Node c x tl tr)

balR :: RBTree -> RBTree
balR (Node Black x (Node Red y tll tlr) (Node Black z NIL trr)) = (Node Black y tll (Node Black x (colorNode Red tlr) (Node Black z NIL (colorNode Red trr))))
balR (Node Black x (Node Red y tll tlr) (Node Black z trl NIL)) = (Node Black y tll (Node Black x (colorNode Red tlr) (Node Black z (colorNode Red trl) NIL)))
balR (Node Black x (Node Black y NIL NIL) NIL) = (Node Black x (Node Red y NIL NIL) NIL)
balR (Node Black x (Node Black y tll tlr) NIL) = colorNode Black (balance (Node Black x (Node Red y tll tlr) NIL))
balR (Node Red x tl (Node Black y NIL (Node Red z NIL NIL))) = (Node Red x tl (Node Black y NIL (Node Black z NIL NIL)))
balR (Node Red x tl (Node Black y NIL (Node Red z trl trr))) = (Node Red x tl (Node Black z (insertValue y trl) trr))
balR (Node Red x tl (Node Red y trl trr)) = (Node Red x tl (Node Black y trl trr))
balR (Node c x tl tr) = (Node c x tl tr)

uni :: RBTree -> RBTree -> RBTree
uni NIL NIL = NIL
uni tree NIL = colorNode Black tree
uni NIL tree = colorNode Black tree
uni (Node Black x tl tr) (Node Red x2 tl2 tr2 ) = (Node Red x2 (uni (Node Black x tl tr) tl2) tr2)
uni (Node Red x tl tr) (Node Black x2 tl2 tr2) = (Node Red x tl (uni tr (Node Black x2 tl2 tr2)))
uni (Node Red x tl tr) (Node Red x2 tl2 tr2)  =
    let c1 = uni tr tl2
    in case c1 of
        (Node Red x3 tl3 tr3) -> (Node Red x3 (Node Red x tl tl3) (Node Red x2 tr3 tr2))
        (Node Black x3 tl3 tr3)   -> (Node Red x tl (Node Red x2 c1 tr2))
        NIL -> (Node Black x tl (Node Red x2 tl2 tr2))
uni (Node Black x tl tr) (Node Black x2 tl2 tr2)  =
    let c2 = uni tr tl2
    in case c2 of
        (Node Red x3 tl3 tr3) -> (Node Red x3 (Node Black x tl tl3) (Node Black x2 tr3 tr2))
        (Node Black x3 tl3 tr3)   -> balL (Node Black x tl (Node Red x2 c2 tr2))
        NIL -> (Node Black x tl (Node Red x2 tl2 tr2))

ex = Node Black 5 (Node Black 3 (Node Black 2 (Node Red 1 NIL NIL) NIL) (Node Black 4 NIL NIL)) (Node Red 9 (Node Black 7 (Node Black 6 NIL NIL) (Node Black 8 NIL NIL)) (Node Black 11 (Node Black 10 NIL NIL) (Node Black 12 NIL NIL)))
ex2= Node Black 5 (Node Black 3 (Node Black 2 (Node Red 1 NIL NIL) NIL) (Node Black 4 NIL NIL)) (Node Red 9 (Node Black 7 (Node Black 6 NIL NIL) (Node Black 8 NIL NIL)) (Node Black 11 (Node Black 10 NIL NIL) (Node Red 13 (Node Black 12 NIL NIL) (Node Black 14 NIL (Node Red 15 NIL NIL)))))
ex11 = Node Black 5 (Node Black 3 (Node Black 2 (Node Red 1 NIL NIL) NIL) (Node Black 4 NIL NIL)) (Node Black 7 (Node Black 6 NIL NIL) (Node Red 9 (Node Black 8 NIL NIL) (Node Black 10 NIL (Node Red 11 NIL NIL))))


 