def goDir = \f. \r.
  let d = fst r in
  if (d == down) {
    grapesHere <- ishere "grapes";
    if grapesHere {
        grab; return ()
    } {};
    return ();
  } {
    turn d;

    // An obstruction might arise after
    // navigation direction is determined
    // but before we move.
    try {
      move;
    } {};
    f;
  }
  end;

def followRoute = \item.
    nextDir <- path (inL ()) (inR item);
    case nextDir return $ goDir $ followRoute item;
    end;

def getGrapes =
    let targetItem = "grapes" in
    emperorHasThem <- has targetItem;
    if emperorHasThem {
        say "Tally ho!";
    } {
        grapesDropped <- as base {
            baseHasThem <- has targetItem;
            return $ not baseHasThem;
        };

        if grapesDropped {
            followRoute targetItem;
        } {
            wait 10;
            getGrapes;
        };
    }
    end;

def avoidSides =

    toLeft <- scan back;
    case toLeft return (\_.
        turn right;
    );

    toRight <- scan back;
    case toRight return (\_.
        turn left;
    );

    behind <- scan back;
    case behind return (\_.
        isBlocked <- blocked;
        if isBlocked {} {move;}
    );
    
    watch forward;
    watch back;
    watch left;
    watch right;
    wait 1000;
    end;

def go =
    getGrapes;
    avoidSides;
    go;
    end;

go;