def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def intersperse = \n. \f2. \f1. if (n > 0) {
        f1;
        if (n > 1) {
            f2;
        } {};
        intersperse (n - 1) f2 f1;
    } {};
    end;

def waitUntilUnblocked =
    x <- blocked;
    if x {
        wait 1;
        waitUntilUnblocked;
    } {};
    end;

def chooseLetter = \i.
    if (i == 0) {
        return "capital C";
    } {
        if (i == 1) {
            return "capital O";
        } {
            return "capital W";
        }
    };
    end;

// Go to upper-left corner
def goToCorner =
    move;
    doN 25 move;
    turn right;
    doN 10 move;
    turn right;
    end;

def highlightLetter =
    drill down;
    end;

def traverseRow = \progress. \n.

    if (n > 0) {

        targetItem <- chooseLetter progress;
        targetHere <- ishere targetItem;

        newProgress <- if targetHere {
            return $ progress + 1;
        } {
            // Reset the progress
            return 0;
        };

        if (newProgress == 3) {
            turn back;

            intersperse 3 move highlightLetter;
        } {
            move;
            traverseRow newProgress (n - 1);
        };
    } {};
    end;


def solve =
    waitUntilUnblocked;
    goToCorner;

    // TODO: Visit all rows, then
    // visit all columns.
    traverseRow 0 25;

    end;

solve;
