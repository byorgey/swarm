def goal_length : int -> cmd int = \len.
  g <- ishere "goal";
  if g {return len}
  {
    md <- path (inl ()) (inr "goal");
    case md (\_. return len) (\d. turn d; move; goal_length (len+1))
  }
end
