def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def approachAndRetreat =
    doN 2 $ (
        wait 10;
        turn back;
        doN 10 move;
    );
    end;

turn right;
doN 4 $ approachAndRetreat;