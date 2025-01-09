/*******************************************************************/
/*                           CONTAINERS                            */
/*******************************************************************/
// This file has an example implementation of lists, sets and maps
// (a.k.a dictionaries) using recursive types.
//
// The implementation uses a Red-Black tree balanced binary tree
// with projection (k -> a) to get sets and dictionaries (compare
// only first element of tuple). It is based on Haskell code in:
// https://abhiroop.github.io/Haskell-Red-Black-Tree/

/*******************************************************************/
/*                    (TYPE) DECLARATIONS                          */
/*******************************************************************/

tydef Maybe a = Unit + a end

tydef List a = rec l. Maybe (a * l) end

tydef RBTree k = rec b. Maybe [red: Bool, k: k, l: b, r: b] end

tydef ISet s a = 
  [ empty: s
  , insert: a -> s -> s 
  , delete: a -> s -> s
  , contains: a -> s -> Bool
  , from_list: List a -> s
  , pretty: s -> Text
  ]
end

def undefined_set : any -> ISet any s =\empty.
    [empty=empty, insert=\x.undefined, delete=\x.undefined, contains=\x.undefined, from_list=\x.undefined, pretty=\x.undefined]
end

tydef IDict d k v =
  [ empty: d
  , insert: k -> v -> d -> d
  , delete: k -> d -> d
  , get: k -> d -> Maybe v
  , contains: k -> d -> Bool
  , pretty: d -> Text
  ]
end

def undefined_dict : any -> IDict any k v =\empty.
    [empty=empty, insert=\x.undefined, delete=\x.undefined, get=\x.undefined, contains=\x.undefined, pretty=\x.undefined]
end

tydef FlatSet a = List a end
def flat_set : ISet (FlatSet a) a = undefined_set (inl ()) end

tydef FlatDict k v = List (k * v) end
def flat_dict : IDict (FlatDict k v) k v = undefined_dict (inl ()) end

tydef Set k = RBTree k end
def tree_set : ISet (Set a) a = undefined_set (inl ()) end

tydef Dict k v = RBTree (k * v) end
def tree_dict : IDict (Dict k v) k v = undefined_dict (inl ()) end

/*******************************************************************/
/*                              MAYBE                              */
/*******************************************************************/

def mmap : (a -> b) -> Maybe a -> Maybe b = \f.\m. case m (\_. inl ()) (\a. inr (f a)) end


/*******************************************************************/
/*                              LISTS                              */
/*******************************************************************/

def emptyL : List a = inl () end
def cons : a -> List a -> List a = \a.\l. inr (a, l) end
def pureL : a -> List a = \a. cons a emptyL end

def foldr : (a -> b -> b) -> b -> List a -> b = \f. \z. \xs.
  case xs
    (\_. z)
    (\c. f (fst c) (foldr f z (snd c)))
end

def foldl : (b -> a -> b) -> b -> List a -> b = \f. \z. \xs.
  case xs
    (\_. z)
    (\c. (foldl f (f z $ fst c) (snd c)))
end

def flip : (a -> b -> c) -> (b -> a -> c) = \f. (\b.\a.f a b) end

def map : (a -> b) -> List a -> List b = \f.
  foldr (\y. cons (f y)) emptyL
end

def appendL : List a -> List a -> List a = \xs. \ys.
  foldr cons ys xs
end

def lengthL : List a -> Int = foldl (\acc.\_. acc + 1) 0 end 

def safeGetL : Int -> List a -> Maybe a = \i.\l.
  case l (\_. inl ()) (\n. if (i <= 0) {inr $ fst n} {safeGetL (i - 1) (snd n)})
end 

def getL : Int -> List a -> a = \i.\l.
  case (safeGetL i l) (\_. fail $ "INDEX " ++ format i ++ " OUT OF BOUNDS!") (\a. a)
end

def formatL : List a -> Text = \l.
 let f : List a -> Text = \l. case l (\_. "]") (\n. ", " ++ format (fst n) ++ f (snd n))
 in case l (\_. "[]") (\n. "[" ++ format (fst n) ++ f (snd n))
end

/*******************************************************************/
/*                          ORDERED LISTS                          */
/*******************************************************************/

// get from ordered list
def getKeyL : (a -> k) -> k -> List a -> Maybe a = \p.\x.\l.
  case l (\_. inl ()) (\n.
    let nx = p (fst n) in
    if (nx < x) {
      getKeyL p x $ snd n
    } {
      if (nx > x) {
        inl ()
      } {
        inr $ fst n
      }
    }
  )
end

def containsKeyL : (a -> k) -> k -> List a -> Bool = \p.\x.\l.
  case (getKeyL p x l) (\_. false) (\_. true)
end

// insert into ordered list - replaces elements, like set
def insertKeyL : (a -> k) -> a -> List a -> List a = \p.\y.\l.
  case l (\_. pureL y) (\n.
    let nx = p (fst n) in
    let x = p y in
    if (nx == x) {
      inr (y, snd n)
    } {
      if (nx > x) {
        cons y l
      } {
        cons (fst n) (insertKeyL p y $ snd n)
      }
    }
  )
end

// delete in ordered list
def deleteKeyL : (a -> k) -> k -> List a -> List a = \p.\x.\l.
  case l (\_. emptyL) (\n.
    let nx = p (fst n) in
    if (nx == x) {
      snd n
    } {
      if (nx > x) {
        l
      } {
        cons (fst n) (deleteKeyL p x $ snd n)
      }
    }
  )
end

tydef FlatSet a = List a end

def flat_set : ISet (FlatSet a) a =
  [ empty=emptyL
  , insert=insertKeyL (\x.x)
  , delete=deleteKeyL (\x.x)
  , contains=containsKeyL (\x.x)
  , from_list=foldl (flip $ insertKeyL (\x.x)) emptyL
  , pretty=formatL
  ]
end

tydef FlatDict k v = List (k * v) end

def flat_dict : IDict (FlatDict k v) k v =
  [ empty=emptyL
  , insert=\k.\v. insertKeyL fst (k,v)
  , delete=deleteKeyL fst
  , get=\k.\d. mmap snd (getKeyL fst k d)
  , contains=containsKeyL fst
  , pretty=\d.
    let p : (k * v) -> Text = \kv. format (fst kv) ++ ": " ++ format (snd kv) in 
    let f : List (k * v) -> Text = \l. case l (\_. "}") (\n. ", " ++ p (fst n) ++ f (snd n))
    in case d (\_. "{}") (\n. "{" ++ p (fst n) ++ f (snd n))
  ]
end

/*******************************************************************/
/*                         RED BLACK TREE                          */
/*******************************************************************/


// equirecursive types allow us to get the node type without mutual recursion
tydef RBNode k = rec n. [red: Bool, k: k, l: Maybe n, r: Maybe n] end

def emptyT : RBTree k = inl () end

def getT : (k -> a) -> a -> RBTree k -> Maybe k = \p.\x.\t.
  case t (\_. inl ()) (\n.
    let nx = p n.k in
    if (x < nx) {
        getT p x n.l
    } {
      if (x > nx) {
          getT p x n.r
      } {
        inr n.k
      }
    }
  )
end

def containsT : (k -> a) -> a -> RBTree k -> Bool = \p.\x.\t. getT p x t != inl () end

def inorder : RBTree k -> List k = \t.
  case t (\_. inl ()) (\n.
    appendL (inorder n.l) (appendL (pureL n.k) $ inorder n.r)
  )
end

def formatT : RBTree k -> Text = \t.
  case t (\_. "N") (\n.
    "(T "
    ++ if n.red {"R "} {"B "}
    ++ formatT n.l ++ " " ++ format n.k ++ " " ++ formatT n.r ++ ")"
  ) 
end

def indent_ = \c.\i. if (i <= 0) {" "} {c ++ indent_ c (i - 1)} end
def indent = indent_ " " end

def debugT : RBTree k -> Cmd Unit =
  let d : Text -> Text -> Text -> RBTree k -> Cmd Unit = \i.\li.\ri.\t. case t (\_. log $ i ++ "+ N") (\n.
    d (i ++ li) "  " "| " n.l;
    log $ i ++ "+ " ++ if n.red {"R "} {"B "} ++ format n.k;
    d (i ++ ri) "| " "  " n.r;
  ) in d "" "" ""
end

/*
balance B (T R (T R a x0 b) x1 c) x2 d = T R (T B a x0 b) x1 (T B c x2 d) -- case 1: LL red
balance B (T R a x0 (T R b x1 c)) x2 d = T R (T B a x0 b) x1 (T B c x2 d) -- case 2: LR red
balance B a x0 (T R (T R b x1 c) x2 d) = T R (T B a x0 b) x1 (T B c x2 d) -- case 3: RL red
balance B a x0 (T R b x1 (T R c x2 d)) = T R (T B a x0 b) x1 (T B c x2 d) -- case 4: RR red
balance color a x b = T color a x b -- case 5
*/
def balanceT : RBNode k -> RBTree k = \t.
  let balanced : [ a: RBTree k, x0: k, b: RBTree k,   x1: k,   c: RBTree k, x2: k, d: RBTree k] -> RBTree k
    = \v.
    inr [red=true, l=inr [red=false, l=v.a, k=v.x0, r=v.b], k=v.x1, r=inr [red=false, l=v.c, k=v.x2, r=v.d]]
  in let case5 : RBNode k -> RBTree k =
    inr  // just id - TODO: inline?
  in let balanceRR : RBNode k -> RBNode k -> RBTree k = \t.\rn.
    case rn.r (\_. case5 t) (\rrn.
      if rrn.red {
        balanced [a=t.l, x0=t.k, b=rn.l, x1=rn.k, c=rrn.l, x2=rrn.k, d=rrn.r]
      } {
        case5 t
      }
    )
  in let balanceRL : RBNode k -> RBNode k -> RBTree k = \t.\rn.
    case rn.l (\_. balanceRR t rn) (\rln.
      if rln.red {
        balanced [a=t.l, x0=t.k, b=rln.l, x1=rln.k, c=rln.r, x2=rn.k, d=rn.r]
      } {
        balanceRR t rn
      }
    )
  in let balanceR : RBNode k -> RBTree k = \t.
    case t.r (\_.
      case5 t
    ) (\lr.
      if lr.red {
        balanceRL t lr
      } {
        case5 t
      }
    )
  in let balanceLR : RBNode k -> RBNode k -> RBTree k = \t.\ln.
    case ln.r (\_. balanceR t) (\lrn.
      if lrn.red {
        balanced [a=ln.l, x0=ln.k, b=lrn.l, x1=lrn.k, c=lrn.r, x2=t.k, d=t.r]
      } {
        balanceR t
      }
    )
  in let balanceLL : RBNode k -> RBNode k -> RBTree k = \t.\ln.
    case ln.l (\_. balanceLR t ln) (\lln.
      if lln.red {
        balanced [a=lln.l, x0=lln.k, b=lln.r, x1=ln.k, c=ln.r, x2=t.k, d=t.r]
      } {
        balanceLR t ln
      }
    )
  in let balanceL : RBNode k -> RBTree k = \t.
    case t.l (\_.
      balanceR t
    ) (\ln.
      if ln.red {
        balanceLL t ln
      } {
        balanceR t
      }
    )
  in if t.red {
    case5 t
  } {
    balanceL t
  }
end

def insertT : (k -> a) -> k -> RBTree k -> RBTree k = \p.\x.\t.
  let ins : (k -> a) -> k -> RBTree k -> RBTree k =\p.\x.\t. case t
    (\_.
      inr [red=true, k=x, l=inl (), r=inl ()]
    ) (\n.
      if (p x == p n.k) { inr [red=n.red, k=x, l=n.l, r=n.r] } {
        if (x < n.k) {
            balanceT [red=n.red, k=n.k, l=ins p x n.l, r=n.r]
        } {
            balanceT [red=n.red, k=n.k, l=n.l, r=ins p x n.r]
        }
      }
    )
  in let makeBlack : RBTree k -> RBTree k = \t. case t (\_.
        fail "makeBlack will always be called on nonempty"
      ) (\n.
        inr [red=false, k=n.k, l=n.l, r=n.r]
      )
  in makeBlack $ ins p x t
end

def deleteT : (k -> a) -> a -> RBTree k -> RBTree k = \p.\x.\t.
  // ------------------------------------------------------------------------------------------------------------
  tydef Del k a = (k -> a) -> a -> RBTree k -> RBTree k end
  // ------------------------------------------------------------------------------------------------------------
  let makeBlack : RBTree k -> RBTree k = \t.
    case t (\_. t) (\n. inr [red=false, k=n.k, l=n.l, r=n.r])
  in let makeRed : RBNode k -> RBTree k = \n.
    inr [red=true, k=n.k, l=n.l, r=n.r]
  // ------------------------------------------------------------------------------------------------------------
  /*
  balL (T B (T R t1 x t2) y t3) = T R (T B t1 x t2) y t3
  balL (T B t1 y (T B t2 z t3)) = balance' (T B t1 y (T R t2 z t3))
  balL (T B t1 y (T R (T B t2 u t3) z t4@(T B l value r)))
    = T R
      (T B t1 y t2)
      u
      (balance' (T B t3 z
        (T R l value r)))
  */
  in let balL : RBNode k -> RBTree k = \n.
    let balL2 : RBNode k -> RBNode k -> RBTree k = \n.\rn.
      case rn.l (\_. inr n) (\rln.
        if rln.red {inr n} {
          case rn.r (\_. inr n) (\rrn.
            if rrn.red {inr n} {
              inr [red=true,
                   l=inr [red=false, l=n.l, k=n.k, r=rln.l],
                   k=rln.k,
                   r=balanceT [red=false, l=rln.r, k=rn.k, r=inr [red=true, l=rrn.l, k=rrn.k, r=rrn.r]]]
            }
          )
        }
      )
    in let balL1 : RBNode k -> RBTree k = \n.
      case n.r (\_. inr n) (\rn. 
        if rn.red {balL2 n rn} {balanceT [red=false, k=n.k, l=n.l, r=makeRed rn]}
      )
    in case n.l (\_. balL1 n) (\ln. if ln.red {inr [red=true, k=n.k, l=makeBlack n.l, r=n.r]} {balL1 n})
  // ------------------------------------------------------------------------------------------------------------
  /*
  balR (T B t1 y (T R t2 x t3)) = T R t1 y (T B t2 x t3)
  balR (T B (T B t1 z t2) y t3) = balance' (T B (T R t1 z t2) y t3)
  balR (T B
    ln@(T R
      lln@(T B lll llk llr)
      lk
      lrn@(T B lrl lrk lrr))
    k
    rn)
    =
    T R
      (balance' (T B (T R lll llk llr) lk lrl))
      lrk
      (T B lrr k rn)
  */
  in let balR : RBNode k -> RBTree k = \n.
    let balR2 : RBNode k -> RBNode k -> RBTree k = \n.\ln.
      case ln.l (\_. inr n) (\lln.
        if lln.red {inr n} {
          case ln.r (\_. inr n) (\lrn.
            if lrn.red {inr n} {
              inr [red=true,
                   l=balanceT [red=false, l=inr [red=true, l=lln.l, k=lln.k, r=lln.r], k=ln.k, r=lrn.l],
                   k=lrn.k,
                   r=inr [red=false, l=lrn.r, k=n.k, r=n.r]]
            }
          )
        }
      )
    in let balR1 : RBNode k -> RBTree k = \n.
      case n.l (\_. inr n) (\ln. 
        if ln.red {balR2 n ln} {balanceT [red=false, k=n.k, l=makeRed ln, r=n.r]}
      )
    in case n.r (\_. balR1 n) (\rn. if rn.red {inr [red=true, k=n.k, l=n.l, r=makeBlack n.r]} {balR1 n})
  // ------------------------------------------------------------------------------------------------------------
  in let delL : Del k a -> (k -> a) -> a -> RBNode k -> RBTree k = \del.\p.\x.\n.
    let dl = [red=n.red, k=n.k, l=del p x n.l, r=n.r] in
    if n.red { inr dl } { balL dl }
  // ------------------------------------------------------------------------------------------------------------
  in let delR : Del k a -> (k -> a) -> a -> RBNode k -> RBTree k = \del.\p.\x.\n.
    let dr = [red=n.red, k=n.k, l=n.l, r=del p x n.r] in
    if n.red { inr dr } { balR dr }
  // ------------------------------------------------------------------------------------------------------------
  /*
  fuse E t = t
  fuse t E = t
  fuse t1@(T B _ _ _) (T R t3 y t4) = T R (fuse t1 t3) y t4
  fuse (T R t1 x t2) t3@(T B _ _ _) = T R t1 x (fuse t2 t3)
  fuse (T R t1 x t2) (T R t3 y t4)  =
    let s = fuse t2 t3
    in case s of
        (T R s1 z s2) -> (T R (T R t1 x s1) z (T R s2 y t4))
        (T B _ _ _)   -> (T R t1 x (T R s y t4))
  fuse (T B t1 x t2) (T B t3 y t4)  =
    let s = fuse t2 t3
    in case s of
        (T R s1 z s2) -> (T R (T B t1 x s1) z (T B s2 y t4))
        (T B s1 z s2) -> balL (T B t1 x (T B s y t4))
  */
  in let fuse: RBTree k -> RBTree k -> RBTree k = \l.\r.
    case l (\_. r) (\ln. case r (\_. l) (\rn.
      if ln.red {
        if rn.red {
          /* RED RED */
          let s = fuse ln.r rn.l in
          let b_case = {inr [red=true, l=ln.l, k=ln.k, r=inr [red=true, l=s, k=rn.k, r=rn.r]]} in
          case s (\_. force b_case) (\sn.
            if sn.red {
              inr [red=true, l=inr [red=true, l=ln.l, k=ln.k, r=sn.l], k=sn.k, r=inr [red=true, l=sn.r, k=rn.k, r=rn.r]]
            } {
              force b_case
            }
          ) 
        } {
          /* RED BLACK */
          inr [red=true, l=ln.l, k=ln.k, r=fuse ln.r (inr rn)]
        }
      } {
        if rn.red {
          /* BLACK RED */
          inr [red=true, l=fuse (inr ln) rn.l, k=rn.k, r=rn.r]
        } {
          /* BLACK BLACK */
          let s = fuse ln.r rn.l in
          let b_case = {balL [red=false, l=ln.l, k=ln.k, r=inr [red=false, l=s, k=rn.k, r=rn.r]]} in
          case s (\_. force b_case) (\sn.
            if sn.red {
              inr [red=true, l=inr [red=false, l=ln.l, k=ln.k, r=sn.l], k=sn.k, r=inr [red=false, l=sn.r, k=rn.k, r=rn.r]]
            } {
              force b_case
            }
          )
        }
      }
    ))
  // ------------------------------------------------------------------------------------------------------------
  in let del : (k -> a) -> a -> RBTree k -> RBTree k = \p.\x.\t.
    case t (\_.t) (\n. 
      if (p n.k == x) {fuse n.l n.r} {
        if (x < p n.k) {delL del p x n} {delR del p x n}
      }
    )
  // ------------------------------------------------------------------------------------------------------------
  in makeBlack $ del p x t
end

/*******************************************************************/
/*                           DICTIONARY                            */
/*******************************************************************/

tydef Dict k v = RBTree (k * v) end

def tree_dict : IDict (Dict k v) k v =
 [ empty = emptyT
 , get  = \k.\d. mmap snd (getT fst k d)  
 , contains = containsT fst 
 , insert = \k.\v. insertT fst (k, v) 
 , delete = \k. deleteT fst k 
 , pretty = \d. flat_dict.pretty (inorder d)
 ]
end

/*******************************************************************/
/*                              SET                                */
/*******************************************************************/

tydef Set k = RBTree k end

def tree_set : ISet (Set a) a =
  [ empty=emptyT
  , insert=insertT (\x.x)
  , delete=deleteT (\x.x)
  , contains=containsT (\x.x)
  , from_list=foldl (flip $ insertT (\x.x)) emptyT
  , pretty=\s. formatL $ inorder s
  ]
end

/*******************************************************************/
/*                           UNIT TESTS                            */
/*******************************************************************/

def assert = \i. \b. \m. if b {log $ indent i ++ "OK: " ++ m} {log "FAIL:"; fail m} end

def assert_eq 
  = \i. \exp. \actual. \m.
  if (exp == actual) {log $ indent i ++ "OK: " ++ m} {
    log (indent i ++ "FAIL: expected " ++ format exp ++ " actual " ++ format actual);
    fail m
  }
end

def group = \i. \m. \tests.
  log $ indent i ++ "START " ++ m ++ ":";
  tests $ i + 1;
  return ()
end

def test_empty: Int -> Cmd Unit = \i.
  let d = tree_dict in
  group i "EMPTY TESTS" (\i.
    assert_eq i (inl ()) (d.get 0 $ d.empty) "no element should be present in an empty tree";
    assert i (not $ d.contains 0 $ d.empty) "empty tree should not contain any elements";
  )
end

def test_insert_1235: ISet s Int -> Int -> Cmd Unit =\s.\i.
    let expected = cons 1 $ cons 2 $ cons 3 $ cons 5 emptyL in
    let actual = s.insert 2 $ s.insert 5 $ s.insert 1 $ s.insert 3 s.empty in
    assert_eq i (formatL expected) (formatL actual) "insertKeyL should insert in order";
    assert i (not $ s.contains 0 actual) "not contains 0 [1,2,3,5]";
    assert i (s.contains 1 actual) "contains 1 [1,2,3,5]";
    assert i (s.contains 2 actual) "contains 2 [1,2,3,5]";
    assert i (s.contains 3 actual) "contains 3 [1,2,3,5]";
    assert i (not $ s.contains 4 actual) "not contains 4 [1,2,3,5]";
    assert i (s.contains 5 actual) "contains 5 [1,2,3,5]";
    assert i (not $ s.contains 6 actual) "not contains 6 [1,2,3,5]";
end

def test_ordered_list : Int -> Cmd Unit = \i.
  let s = flat_set in
  group i "ORDERED LIST TESTS" (test_insert_1235 flat_set)
end

def test_insert: Int -> Cmd Unit = \i.
  let d = tree_dict in
  let s = tree_set in
  group i "INSERT TESTS" (\i.
    group i "test {0: 1}" (\i.
      let tree0 = d.insert 0 1 d.empty in
      assert i (d.empty != tree0) "nonempty tree should not be empty";
      assert_eq i (inr 1) (d.get 0 $ tree0) "one element should be present in a one element tree";
      assert i (d.contains 0 $ tree0) "one element tree should contain that elements";
      assert i (not $ d.contains 1 $ tree0) "one element tree should contain only that elements";
    );
    group i "test {0: 1, 2: 3}" (\i.
      let tree02 = d.insert 2 3 $ d.insert 0 1 d.empty in
      assert_eq i (inr 1) (d.get 0 $ tree02) "get 0 {0: 1, 2: 3} == 1";
      assert_eq i (inr 3) (d.get 2 $ tree02) "get 2 {0: 1, 2: 3} == 3";
    );
    group i "test {1,2,3,5}" (test_insert_1235 tree_set)
  );
end

def randomTestS: Int -> Set Int -> (Set Int -> Cmd a) -> Cmd (Set Int) = \i.\s.\test.
  if (i <= 0) {return s} {
    x <- random 20;
    let ns = tree_set.insert x s in
    test ns;
    randomTestS (i - 1) ns test
  }
end

def delete_insert_prop: Int -> Set Int -> Cmd Unit = \i.\t.
  let s = tree_set in
  x <- random 20;
  let i_t = inorder t in
  let f_t = s.pretty t in
  if (not $ s.contains x t) {
    // log $ format x;
    assert_eq i
      (formatL i_t)
      (s.pretty $ s.delete x $ s.insert x t)
      (f_t ++ " == delete " ++ format x ++ " $ insert " ++ format x ++ " " ++ f_t)
  } { delete_insert_prop i t /* reroll */}
end

def delete_delete_prop: Int -> Set a -> Cmd Unit = \i.\t.
  let s = tree_set in
  let i_t = inorder t in
  ix <- random (lengthL i_t);
  let x = getL ix i_t in
  let ds = s.delete x t in
  let dds = s.delete x ds in
  let f_t = s.pretty t in
  let f_dx = "delete " ++ format x in
  assert_eq i (s.pretty ds) (s.pretty dds)
    (f_dx ++ f_t ++ " == " ++ f_dx ++ " $ " ++ f_dx ++ " " ++ f_t)
end

def test_delete: Int -> Cmd Unit = \i.
  group i "DELETE TESTS" (\i.
    randomTestS 15 tree_set.empty (\s.
      group i (tree_set.pretty s) (\i.
        delete_insert_prop i s;
        delete_delete_prop i s;
      )
    )
  )
end

def test_tree: Cmd Unit =
  group 0 "TREE TESTS" (\i.
    test_empty i;
    test_ordered_list i;
    test_insert i;
    test_delete i;
  );
  log "ALL TREE TESTS PASSED!"
end

/*******************************************************************/
/*                           BENCHMARKS                            */
/*******************************************************************/

def benchmark: Int -> s -> (s -> s) -> Cmd (Int * (Int * Int)) = \times.\s.\act.
  let min = \x.\y. if (x > y) {y} {x} in
  let max = \x.\y. if (x > y) {x} {y} in
  let runM: (Int * Maybe (Int * Int)) -> s -> Int -> Cmd (Int * Maybe (Int * Int)) = \acc.\s.\n.
    if (n <= 0) {
      return acc
    } {
      t0 <- time;
      //log $ "START " ++ format t0;
      let ns = act s in
      t1 <- time;
      //log $ "END " ++ format t1;
      let t = t1 - t0 in
      //log $ format s ++ " " ++ format t ++ " ticks";
      let lim = case (snd acc) (\_. (t, t)) (\l. (min t $ fst l, max t $ snd l)) in
      runM ((fst acc + t), inr lim) ns (n - 1)
    } in
  //log "start run";
  res <- runM (0, inl ()) s times;
  //log "end run";
  let avg = fst res / times in
  let lim = case (snd res) (\_. fail "BENCHMARK NOT RUN") (\l.l) in
  return (avg, lim)
end

def cmp_bench : Int -> Text -> (Int * (Int * Int)) -> Text -> (Int * (Int * Int)) -> Cmd Unit
 = \i.\base_name.\base_res.\new_name.\new_res.
  let formatLim = \l. "(min " ++ format (fst l) ++ ", max " ++ format (snd l) ++ ")" in
  log $ indent i ++ "* " ++ base_name ++ ": "
    ++ format (fst base_res) ++ " ticks " ++ formatLim (snd base_res);

  let d = (fst new_res * 100) / fst base_res in
  let cmp = if (d > 100) {
     format (d - 100) ++ "% slower"
    } {
      format (100 - d) ++ "% faster"
    } in
     
  log $ indent i ++ "* " ++ new_name ++ ": "
    ++ format (fst new_res) ++ " ticks " ++ formatLim (snd new_res)
    ++ " <- " ++ cmp;
end

// Get a list of random integers of given length and maximum element number
def gen_random_list: Int -> Int -> Cmd (List Int) = 
  let gen = \acc.\n.\rlim.
    if (n <= 0) { return acc } {
      x <- random rlim;
      gen (cons x acc) (n - 1) rlim
    }
  in gen emptyL
end

// Get a number of lists of random integers of given length and maximum element number
def gen_random_lists: Int -> Int -> Int -> Cmd (List (List Int)) = \m.\n.\rlim.
  let gen = \acc.\m.
    if (m <= 0) { return acc } {
      l <- gen_random_list n rlim;
      gen (cons l acc) (m - 1)
    }
  in gen emptyL m
end


def bench_insert = \i.
  // Use the given function to construct (Flat)Set from the head of provided list of lists and return the tail
  let set_from_first_list : (List a -> s) -> List (List a) -> List (List a) =\from_list.\lls.
    case lls (\_. lls) (\ls_nlls.
      let ns = from_list (fst ls_nlls) in
      snd ls_nlls
    )
  in

  group i "INSERT BENCHMARK" (\i.
    group i "INSERT 10" (\i.
      let n = 10 in
      let m = 5 in
      lls10 <- gen_random_lists m n (3 * n);
      flat_res <- benchmark m lls10 (set_from_first_list flat_set.from_list);
      tree_res <- benchmark m lls10 (set_from_first_list tree_set.from_list);
      cmp_bench i "flat set" flat_res "tree set" tree_res
    );
    group i "INSERT 100" (\i.
      let n = 100 in
      let m = 3 in
      lls100 <- gen_random_lists m n (3 * n);
      flat_res <- benchmark m lls100 (set_from_first_list flat_set.from_list);
      tree_res <- benchmark m lls100 (set_from_first_list tree_set.from_list);
      cmp_bench i "flat set" flat_res "tree set" tree_res
    )
  )
end

def bench_contains = \i.
  let contains_int : s -> (Int -> s -> Bool) -> Int -> Int = \s.\contains.\n.
    let _ = contains n s in n + 1
  in
  group i "CONTAINS BENCHMARK" (\i.
    group i "CONTAINS 10" (\i.
      let n = 10 in
      let m = 3 * n in
      ls10 <- gen_random_list n m;
      let fls = flat_set.from_list ls10 in
      flat_res <- benchmark m 0 (contains_int fls flat_set.contains);
      let tls = tree_set.from_list ls10 in
      tree_res <- benchmark m 0 (contains_int tls tree_set.contains);
      cmp_bench i "flat set" flat_res "tree set" tree_res
    );
    group i "CONTAINS 100" (\i.
      let n = 100 in
      let m = 3 * n in
      ls100 <- gen_random_list n m;
      //log $ formatL ls100;
      let fls = flat_set.from_list ls100 in
      //log $ formatL fls;
      flat_res <- benchmark m 0 (contains_int fls flat_set.contains);
      let tls = tree_set.from_list ls100 in
      //log $ formatS tls;
      tree_res <- benchmark m 0 (contains_int tls tree_set.contains);
      cmp_bench i "flat set" flat_res "tree set" tree_res
    )
  )
end

def benchmark_tree: Cmd Unit =
  group 0 "TREE BENCHMARKS" (\i.
    bench_insert i;
    bench_contains i;
  );
  log "ALL BENCHMARKS DONE!"
end