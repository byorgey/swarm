def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;
def until = \p. \c. q <- p; if q {} {c; until p c} end;

def leap = \leapable.
    move;
    until (ishere leapable) (wait 1);
    b <- grab;
    move;
    place b;
    end;

let leapable = "log" in

move;
place leapable;
turn right;
doN 7 $ leap leapable;
