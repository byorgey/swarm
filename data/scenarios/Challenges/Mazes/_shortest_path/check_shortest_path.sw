def orElse : cmd bool -> cmd bool -> cmd bool = \c1. \c2.
  b <- c1;
  if b {return true} {c2}
end

def inverse : dir -> dir = \d.
  if (d == left) {right} {if (d == right) {left} {d}}
end

def followPathTo : cmd bool -> dir -> cmd bool = \followPath. \d.
  turn d;
  b <- blocked;
  if b
    {return false}
    { move;
      res <- followPath;
      if res {return true} {turn back; move; turn back; turn (inverse d); return false}
    }
end

def followPath : text -> cmd bool = \thing.
  loc <- whereami;
  h <- ishere thing;
  if (not h)
  { return false }
  { if (loc == (27, -17))
    { return true }
    { orElse
      (followPathTo (followPath thing) left)
      (orElse
        (followPathTo (followPath thing) forward)
        (followPathTo (followPath thing) right)
      )
    }
  }
end

def checkPath : cmd bool =
  teleport self (27, -17);
  g <- ishere "goal";
  if g {return false}
  {
    n <- resonate "path" ((0,0), (27, -17));
    if (n != 51) {return false} {
      loc <- as base {whereami};
      teleport self loc;
      followPath "path"
    }
  }
end;

checkPath
