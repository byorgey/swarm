def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def intersperse = \n. \f2. \f1. if (n > 0) {
        f1;
        if (n > 1) {
            f2;
        } {};
        intersperse (n - 1) f2 f1;
    } {};
    end;

def shiftRightForPush =
    turn back;
    move;
    turn left;
    move;
    turn left;
    end;

/*
Precondition:
Positioned behind the wall, facing it, on the leftmost cell.
*/
def pushWall =
    intersperse 3 shiftRightForPush push;
    end;

def moveToBackWall =
    turn back;
    doN 5 move;
    turn right;
    doN 2 move;
    turn right;
    end;

def moveRightSide =
    turn right;
    push;
    turn right;
    move;
    turn left;
    move;
    turn left;
    doN 5 push;
    turn right;
    move;
    turn left;
    move;
    turn left;
    push;
    end;

def moveLeftSide =
    doN 4 move;
    turn left;
    doN 5 move;
    turn right;
    push;
    turn left;
    move;
    turn right;
    move;
    turn right;
    doN 5 push;
    turn left;
    move;
    turn right;
    move;
    turn right;
    push;
    end;

def initialSetup =
    turn south;
    doN 3 move;
    turn right;
    move;
    turn right;
    end;

def waitUntilGrapesGrabbed =
    itemHere <- scan forward;
    case itemHere return (\_.
        watch forward;
        wait 1000;
        waitUntilGrapesGrabbed;
    );
    end;

def placeGrapes =

    doN 4 move;
    place "grapes";
    turn back;
    move;
    doN 2 push;
    turn back;
    doN 2 move;
    waitUntilGrapesGrabbed;

    turn back;
    doN 4 move;
    turn back;
    doN 2 push;
    end;

def go =
    placeGrapes;

    // initialSetup;
    // pushWall;
    // moveToBackWall;
    // pushWall;
    // moveRightSide;
    // moveLeftSide;
    end;

go;
