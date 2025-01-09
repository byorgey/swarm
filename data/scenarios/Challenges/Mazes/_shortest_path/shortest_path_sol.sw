def tL = turn left end;
def tR = turn right end;
def tB = turn back end;
def ifM = \p.\t.\e. b <- p; if b t e end;
// Returns true if blocked, false if it may be a viable path
def DFSn : int -> cmd bool = \n.
  // say $ "DFSn at level " ++ format n;
  ifM (ishere "goal") {swap "path"; selfdestruct} {};
  ifM (ishere "rock") {} {
    if (n == 0) {} {
      ifM (ishere "path") {} {
        place "path";
        // Weird stuff with let bindings is a workaround for
        // https://github.com/swarm-game/swarm/issues/681
        // (see https://github.com/swarm-game/swarm/issues/1032#issuecomment-1402465755)
        tL; b <- blocked; dead <- if b {return true} {move; DFSn (n-1)};
        let deadL = dead in
        tR; b <- blocked; dead <- if b {return true} {move; DFSn (n-1)};
        let deadF = dead in
        tR; b <- blocked; dead <- if b {return true} {move; DFSn (n-1)};
        let deadR = dead in
        tL; if (deadL && deadF && deadR) {swap "rock"} {grab}; return ()
      }
    }
  };
  rockhere <- ishere "rock";
  tB; move; tB;
  return rockhere
end;
def startDFS = \n.
  b <- blocked; if b {} {move; DFSn n; return ()}
end;
def clear_rocks =
  ifM (ishere "rock") {
    grab;
    ifM blocked {} {move; clear_rocks};
    tR; ifM blocked {} {move; clear_rocks};
    tR; ifM blocked {} {move; clear_rocks};
    tR; ifM blocked {} {move; clear_rocks};
    tR
  } {};
  tB; ifM blocked {} {move}; tB
end;
def DFS = \n.
  // say ("Searching with depth " ++ format n);
  place "path";
  startDFS n; tL; startDFS n; tL; startDFS n; tL; startDFS n;
  grab; return ()
  //  swap "rock"; clear_rocks
end;
def for : int -> int -> (int -> cmd unit) -> cmd unit = \lo. \hi. \m.
  if (lo > hi) {} {m lo; for (lo+1) hi m}
end;
build {
  require 500 "rock"; require 500 "path";
  log "hi";
  for 1 500 (\n. DFS n);
}
