/*
Leap over an object.
The direction has already been established.
Moves at most two spaces.
If the destination cell is blocked,
go back to the original cell.
*/
def leap = \canGrab.
    move;

    // Use a try block to protect against race condition
    // of the bubble being removed before we get there.
    try {
        item <- if canGrab {
            grab;
        } {
            return "";
        };
        
        isBlocked <- blocked;
        if isBlocked {
            turn back;
            move;
        } {
            move;
            if (item == "log") {
                place item;
            } {};
        };
    } {};
    end;

def isLeapable = \item.
    item == "log" || item == "bubble" || item == "hurdle";
    end;

def isGrabbable = \item.
    item != "hurdle";
    end;


def canDo = \thing. \f.
    case thing (\_. false) f
    end;

/*
Look around for something to leap over.
Continues in same direction if the option
is available.
*/
def findBubble =
    thing <- scan forward;
    let canLeap = canDo thing isLeapable in
    let canGrab = canDo thing isGrabbable in

    if canLeap {
        leap canGrab;

        // Waiting for a moment between leaps makes
        // the motion appear more frog-like.
        wait 4;
    } {
        turn right;
    };

    findBubble;
    end;

findBubble;