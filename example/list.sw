
/*******************************************************************/
/*                       LIST ENCODING IN INT                      */
/*                                                                 */
/* This file defines functions that allow storing arbitrary list   */
/* of values in one integer. It is possible because an integer can */
/* have unlimited length and can be split into chunks of smaller   */
/* integers. See Haskell docs for Integer (used by swarm for int). */
/*                                                                 */
/* For examples of usage see the unit tests in testLIST function.  */
/*                                                                 */
/* NOTE THAT THIS IS NOT EFFICIENT OR ERGONIMIC!!! YOU SHOULD USE: */
tydef NormalListType a = rec l. Unit + (a * l) end


/*                                                                 */
/* This code was written before swarm had 'format' or 'rec', but   */
/* it still serves as an example of pure swarm code with tests.    */
/*******************************************************************/
//
// Definitions:
// - MAIN: nil (Haskell []), cons (Haskell (:)), head, tail
// - BONUS: headTail, index, for, for_each, for_each_i
// - ARITH: len, mod, shiftL, shiftR, shiftLH, shiftRH
// - HELPERS: consP, getLenPart, setLenPart, to1numA, getLenA
// - TESTS: assert, testLIST, testLIST_BIG, testLIST_ALL
//
/*******************************************************************/
/*                              TYPE                               */
/*******************************************************************/
// Type of list of ints - the inner ints can themselves be lists,
// so *any* type can be encoded inside.
//
// It is the callers responsibility to make sure a program using this
// "type" is type safe. Notably 2 == [0] != [] == 0 but [] !! x == 0.
tydef ListI = Int end


/*******************************************************************/
/*                             LAYOUT                              */
/*******************************************************************/
// The length of a number is kept in the header and split into 7bit
// chunks each prefixed by 1bit that marks if the byte is last in
// the header (0=YES).
/* EXAMPLE - [short_x,long_y] - concretly e.g. [42, 2^(2^7)]

0   < len short_x < 2^7
2^7 < len long_y  < 2^14

cons short_x   $          cons long_y          $ nil
vvvvvvvvvvvv       vvvvvvvvvvvvvvvvvvvvvvvvv     vvv
  0|len x|x    |   1|len y%2^7|0|len y/2^7|y   |  0
^^^^^^^^^^^^       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  head                         tail
*/
/*******************************************************************/
/*                         ARITH PRIMITIVES                        */
/*******************************************************************/
// bitlength of a number (shifting by one)
def naive_len: Int -> Int -> Int 
  = \acc. \i.
  if (i == 0) {acc} {naive_len (1 + acc) (i / 2)}
end


// modulus function (%)
def mod: Int -> Int -> Int = \i. \m. i - m * (i / m) end


// f X Y = Y * 2^(-X)
def shiftL: Int -> Int -> Int = \s. \i. i / 2 ^ s end


// f X Y = Y * 2^(X)
def shiftR: Int -> Int -> Int = \s. \i. i * 2 ^ s end


// shift by (8bit) bytes
def shiftLH: Int -> Int -> Int = \s. shiftL (s * 8) end

def shiftRH: Int -> Int -> Int = \s. shiftR (s * 8) end


// bitlength of a number using an accumulator (shifting by 64)
def len_accum: Int -> Int -> Int 
  = \acc. \i.
  let next = i / 2 ^ 64 in
  if (next == 0) {naive_len acc i} {len_accum (acc + 64) next}
end


// bitlength of a number (uses an accumulator and shifts by 64 bits)
def len: Int -> Int = len_accum 0 end


/*******************************************************************/
/*                       helper functions                          */
/*******************************************************************/
def getLenPart: Int -> Int = \i. mod (i / 2) (2 ^ 7) end

def setLenPart: Int -> Int = \i. 2 * mod i (2 ^ 7) end


// Split length into 7-bit parts and prefix all but last with 1
def to1numA: Int -> (Int * Int) 
  = \i.
  let nextPart = shiftL 7 i in
  if (nextPart == 0) {

    // last part
    (2 * i, 1)
  } {
    let p = to1numA nextPart in
    (1 /* header isEnd bit */ + setLenPart i + shiftRH 1 (fst p), 1 + snd p)
  }
end


// Get bitlength of the first number in the list
// and also the list starting at the number itself
def getLenA: ListI -> (Int * Int) 
  = \xs.
  let isEnd = 0 == mod xs 2 in
  let l = getLenPart xs in
  let nextPart = shiftLH 1 xs in
  if isEnd {(l, nextPart)} {
    let p = getLenA nextPart in (l + shiftR 7 (fst p), snd p)
  }
end


/*******************************************************************/
/*                         LIST FUNCTIONS                          */
/*******************************************************************/
def headTail: ListI -> (Int * ListI) 
  = \xs.
  let sign = mod xs 2 in
  let ns = xs / 2 in
  let p = getLenA ns in
  ( let x = mod (snd p) $ 2 ^ fst p in if (sign == 0) {x} {-x}
  , shiftL (fst p) (snd p) )
end

def head: ListI -> Int 
  = \xs.
  let sign = mod xs 2 in
  let ns = xs / 2 in
  let p = getLenA ns in
  let x = mod (snd p) $ 2 ^ fst p in if (sign == 0) {x} {-x}
end

def tail: ListI -> ListI 
  = \xs.
  let sign = mod xs 2 in
  let ns = xs / 2 in let p = getLenA ns in shiftL (fst p) (snd p)
end

def nil: ListI = 0 end


// Add non-negative number to beginning of list (cons adds the sign)
def consP: Int -> ListI -> Int 
  = \x. \xs.
  if (x == 0) {2 /* header says one bit length */ + shiftR (8 + 1) xs} {
    let l = len x in
    let pl = to1numA l in fst pl + shiftRH (snd pl) (x + shiftR l xs)
  }
end


// Add integer to the beginning of the list
def cons: Int -> ListI -> ListI 
  = \x. \xs.
  if (x >= 0) {2 * consP x xs} {1 + 2 * consP (-x) xs}
end


/*******************************************************************/
/*                       MORE LIST FUNCTIONS                       */
/*******************************************************************/
def index: Int -> ListI -> Int 
  = \i. \xs.
  if (i <= 0) {head xs} {index (i - 1) (tail xs)}
end

def for: ∀ a. Int -> Int -> (Int -> Cmd a) -> Cmd Unit 
  = \s. \e. \f.
  if (s == e) {} {f s; for (s + 1) e f}
end

def for_each_i: Int -> ListI -> (Int -> Int -> Cmd Unit) -> Cmd Unit 
  = \i. \xs. \f.
  if (xs == nil) {} {
    let ht = headTail xs in f i (fst ht); for_each_i (i + 1) (snd ht) f
  }
end

def for_each: ListI -> (Int -> Cmd Unit) -> Cmd Unit 
  = \xs. \f.
  for_each_i 0 xs (\i. f)
end


/*******************************************************************/
/*                           UNIT TESTS                            */
/*******************************************************************/
def assert = \b. \m. if b {} {log "FAIL:"; fail m} end

def assert_eq 
  = \exp. \actual. \m.
  if (exp == actual) {} {
    log ("FAIL: expected " ++ format exp ++ " actual " ++ format actual);
    fail m
  }
end

def testLIST =
  log "STARTING TESTS OF LIST:";
  log "check [0]";
  let l0 = cons 0 nil in
  assert_eq l0 4 "[0] ~ 4";
  assert_eq (head l0) 0 "head [0] == 0";
  assert_eq (tail l0) nil "tail [0] == []";
  log "check [1]";
  let l1 = cons 1 nil in
  assert_eq l1 516 "[1] ~ 516";
  assert_eq (head l1) 1 "head [1] == 1";
  assert_eq (tail l1) nil "tail [1] == []";
  log "check [-1]";
  let ln1 = cons (-1) nil in
  assert_eq ln1 517 "[-1] ~ 517";
  assert_eq (head ln1) (-1) "head [-1] == -1";
  assert_eq (tail ln1) nil "tail [-1] == []";
  log "check [42]";
  let l42 = cons 42 nil in
  assert_eq l42 21528 "[42] ~ 21528";
  assert_eq (head l42) 42 "head [42] == 42";
  assert_eq (tail l42) nil "tail [42] == []";
  log "check [499672]";
  let l499672 = cons 499672 nil in
  assert_eq l499672 255832140 "[499672] ~ 255832140";
  assert_eq (head l499672) 499672 "head [499672] == 499672";
  assert_eq (tail l499672) nil "tail [499672] == []";
  log "check [1,0]";
  let l1_0 = cons 1 l0 in
  assert_eq l1_0 4612 "[1,0] ~ 4612";
  assert_eq (head l1_0) 1 "head [1,0] == 1";
  assert_eq (tail l1_0) l0 "tail [1,0] == [0]";
  log "check [11,1,0]";
  let l11_1_0 = cons 11 l1_0 in
  assert_eq (head l11_1_0) 11 "head [11,1,0] == 11";
  assert_eq (tail l11_1_0) l1_0 "tail [11,1,0] == [1,0]";
  log "check [0..9]";
  let l0__9 =
    cons 0 $ cons 1 $ cons 2 $ cons 3 $ cons 4 $ cons 5 $ cons 6 $ cons 7 $ cons 8 $ cons 9 nil in
  assert_eq (head l0__9) 0 "head [0..9] == 0";
  log "- check indices";
  assert_eq (index 0 l0__9) 0 "[0..9] !! 0 == 0";
  assert_eq (index 9 l0__9) 9 "[0..9] !! 9 == 9";
  log "check for + index";
  for 0 9 (
    \i. log ("- at index " ++ format i);
    assert_eq (index i l0__9) i "[0..9] - every index I has value I"
  );
  log "check for_each";
  for_each l0__9 (
    \x. log ("- at value " ++ format x);
    assert (x < 10) "[0..9] - every value X < 10"
  );
  log "check for_each_i";
  for_each_i 0 l0__9 (
    \i. \x. log ("- at index " ++ format i);
    assert_eq i x "[0..9] - I == X for every value X at index I"
  );
  log "OK - ALL TEST PASSED\a"
end


// This tests uses VERY LONG lists of size up to 2097332 bits.
// TIP: increase the game speed (ticks/s) when you run this test.
def testLIST_BIG =
  log "STARTING TESTS OF BIG LISTS:";
  log "check [2^(2^7)] aka [big]";
  let big = 2 ^ 2 ^ 7 in
  assert_eq (len big) 129 "len (2^2^7) == 2^7+1";
  assert_eq (to1numA 129) ( 515
  , 2 ) "to1numA: 129 == 1000_0001 --> 515 == [0000 001 0] [0000 001 1]";
  assert_eq (getLenA 515) (129, nil) "getLenA: 515 --> 129";
  let ibig = 2 * (shiftR 16 big + 515) in
  let lbig = cons big nil in
  assert_eq lbig ibig "[big] ~ 2 * ((big * 2^(2*8)) + 515)";
  assert_eq (head lbig) big "head [big] == big";
  assert_eq (tail lbig) nil "tail [big] == []";
  log "check [2^(2^21)] aka [bigger]";
  let bigger = 2 ^ 2 ^ 21 in
  let lbigger = cons bigger nil in
  assert_eq (head lbigger) bigger "head [bigger] == bigger";
  assert_eq (tail lbigger) nil "tail [bigger] == []";
  log "check [2^(2^21),2^(2^7)] aka [bigger,big]";
  let lbiggest = cons bigger lbig in
  assert_eq (head lbiggest) bigger "head [bigger,big] == bigger";
  assert_eq (tail lbiggest) lbig "tail [bigger,big] == [big]";
  log "OK - ALL TEST PASSED"
end

def testLIST_ALL = testLIST; testLIST_BIG end