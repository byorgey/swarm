def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def findPigRow = \maxDist.
    foundPig <- scout east;
    if foundPig {
        // Move one more row beyond the pig
        // so as not to scare it.
        move;
    } {
        move;
        if (maxDist > 0) {
            findPigRow $ maxDist - 1;
        } {};
    };
    end;

def moveIfNotInline =
    
    foundPig <- scout east;
    if foundPig {
        // Move to a different row than the pig
        turn right;
        move;
        turn left;
    } {
        move;
    };

    // Players will determine this value experimentally
    // by observing the pig's reaction
    wait 50;
    end;

turn south;
findPigRow 30;

turn left;
doN 5 move;

doN 20 moveIfNotInline;