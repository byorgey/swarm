def goDir = \d.
  let direction = fst d in
  if (direction == down) {
    
    // Found the base. We can stop.

  } {
    turn direction;

    // An obstruction might arise after
    // navigation direction is determined
    // but before we move.
    try {
      move;
    } {};
  }
  end;

def drillWhileBlocked = \baseLoc.

    isBlocked <- blocked;
    if isBlocked {
        drill forward;
        return ();
    } {
        myLoc <- whereami;
        if (fst myLoc > fst baseLoc) {
            move;
        } {};
    }
    end;

def chaseBase =
    baseLoc <- as base {whereami};

    nextDir <- path (inL ()) (inL baseLoc);
    case nextDir (\_. drillWhileBlocked baseLoc) goDir;
    end;

def go =
    chaseBase;
    go;
    end;

go;