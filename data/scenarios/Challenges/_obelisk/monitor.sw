def elif = \t. \then. \else. {if t then else} end
def else = \t. t end
def abs = \n. if (n < 0) {-n} {n} end

def manhattanDist = \d1. \d2.
  abs (fst d2 - fst d1) + abs (snd d2 - snd d1);
  end;

def calcDistance = \obeliskLoc.
  newBaseLoc <- as base {whereami};
  return $ manhattanDist obeliskLoc newBaseLoc;
  end;

def reapproachPhrase = \i.
  if (i == 0) {
    "Oh, you're approaching me?"
  } $ elif (i == 1) {
    "Then get as close as you need to."
  } $ elif (i == 2) {
    "Instead of running away, you're coming right to me?";
  } $ else {"..."};
  end;

def accumulateApproaches = \myLoc. \reapproachCount. \hadPreviouslyMovedAway. \prevDistance. \cumulativeApproach.
  
  currentDistance <- calcDistance myLoc;

  let approachedDist = prevDistance - currentDistance in
  let isMovingAway = approachedDist < 0 || (approachedDist == 0 && hadPreviouslyMovedAway) in
  let clampedApproach = if isMovingAway {0} {approachedDist} in

  let newCumulativeApproach = clampedApproach + cumulativeApproach in

  // Debugging
  newReapproachCount <- if (hadPreviouslyMovedAway && newCumulativeApproach > cumulativeApproach) {
    say $ format newCumulativeApproach;
    say $ reapproachPhrase reapproachCount;
    return $ reapproachCount + 1;
  } {return reapproachCount};

  accumulateApproaches myLoc newReapproachCount isMovingAway currentDistance newCumulativeApproach;
  end;

def go =
  myLoc <- whereami;
  initialDistance <- calcDistance myLoc;
  accumulateApproaches myLoc 0 true initialDistance 0;
  selfdestruct;
  end;

go;