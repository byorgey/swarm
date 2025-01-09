def abs = \n. if (n < 0) {-n} {n} end
def min = \x. \y. if (x < y) {x} {y} end
def max = \x. \y. if (x < y) {y} {x} end
def elif = \t. \then. \else. {if t then else} end
def else = \t. t end

def dist : int * int -> int * int -> int = \l1. \l2.
    let xDelta = fst l1 - fst l2 in
    let yDelta = snd l1 - snd l2 in
    abs xDelta + abs yDelta;
    end;

def evade = \consecutiveInlineApproaches. \oldIsStartled. \oldHasShouted. \oldAlertness. \prevBaseLoc.

    newBaseLoc <- as base {whereami};
    myLoc <- whereami;

    let oldXDelta = fst myLoc - fst prevBaseLoc in
    let oldYDelta = snd myLoc - snd prevBaseLoc in
    let oldDistance = dist myLoc prevBaseLoc in
    let newXDelta = fst myLoc - fst newBaseLoc in
    let newYDelta = snd myLoc - snd newBaseLoc in
    let newDistance = dist myLoc newBaseLoc in

    let horizontallyInline = oldXDelta == 0 && newXDelta == 0 in
    let verticallyInline = oldYDelta == 0 && newYDelta == 0 in

    let alertRadius = 30 in
    let isWithinAlertRadius = newDistance < alertRadius in
    let directlyApproached = horizontallyInline || verticallyInline in

    let newConsecutiveInlineApproaches = if directlyApproached {consecutiveInlineApproaches + 1} {0} in

    let alertnessAdjustment = if (newDistance < oldDistance && isWithinAlertRadius) {
        if (newConsecutiveInlineApproaches > 1) {8} {4};
    } {
        if (isWithinAlertRadius && oldIsStartled) {0} {-1};
    } in

    let newAlertness = min 12 (max 0 (alertnessAdjustment + oldAlertness)) in

    // DEBUGGING
    if (newAlertness != oldAlertness) {
        say $ "New alertness: " ++ format newAlertness;
    } {};

    let newIsStartled = newAlertness > 5 in

    newHasShouted <- if newIsStartled {

        appear "!";

        styHere <- ishere "sty";
        if styHere {
            if oldHasShouted {
                return oldHasShouted;
            } {
                say "Go away!";
                return true;
            };
        } {

            direction <- chirp "sty";
            turn direction;
            move;

            return oldHasShouted;
        };
    } {
        // Feeling safe

        appear "P";

        cloverHere <- ishere "clover";

        if cloverHere {
            _clover <- harvest;
            return ();
        } {
            r <- random 6;
            if (r == 0) {

                direction <- chirp "clover";
                turn direction;
                move;
            } {};
        };
        return false;
    };

    evade newConsecutiveInlineApproaches newIsStartled newHasShouted newAlertness newBaseLoc;
    end;

baseLoc <- as base {whereami};
evade 0 false false 0 baseLoc;