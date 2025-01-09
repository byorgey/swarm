
// modulus function (%)
def mod : int -> int -> int = \i.\m.
  i - m * (i / m)
end

def placeObstacle =
    emptyHere <- isempty;
    if emptyHere {
        place "wall"
    } {
        swap "wall";
        return ();
    }
    end

def drillWhileBlocked = \xPos.
    isBlocked <- blocked;
    if isBlocked {
        try {
            push;
        } {
            drill forward;
            move;
        }
    } {
        move;
    };


    if (mod xPos 3 == 0) {
        placeObstacle;
    } {};

    end;

def go =
    myLoc <- whereami;

    let xPos = fst myLoc in

    if (xPos > -20) {
        drillWhileBlocked xPos;
        go;
    } {}

    end;

turn west;
go;