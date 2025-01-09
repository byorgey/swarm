
def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def moveUntilBlocked =
    b <- blocked;
    if b {} {move; moveUntilBlocked}
    end;

def drillUntilStuck : cmd (unit + text) =
    try {
        maybeRetrieved <- drill forward;
        case maybeRetrieved (\_. return $ inL ()) (\_.
            move;
            drillUntilStuck;
        );
        return maybeRetrieved;
    } {
        return $ inL ();
    }
    end;

def go =
    turn west;
    doN 47 move;
    turn right;

    doN 6 (
        moveUntilBlocked;
        maybeItem <- drillUntilStuck;

        case maybeItem return (\x.
            iCount <- count x;
            if (iCount < 3) {
                turn left; drill forward; turn left
            } {
                turn back;
            }
        );
    );

    ignite south;

    end;


go;