module Purity
       (CPatch, replay)
where
  
data Bogus = Bogus Bogus

type File = [String]

data CPatch = CPatch { _old :: Commit File, _young :: Commit File }
 -- a CPatch represents the difference between a Commit and its parent

data Commit a = Commit a

data ConflictPatches = CP Bogus

data Outcome a = Success | Conflict a

-- XXX MR has convinced me that this 'apply' function is not actually the way
-- that nor works.  So, let's figure out what is the simple pure story that
-- *does* describe how nor works, and what are the algebraic laws that
-- go with it!


apply :: CPatch -> Commit a -> Either (Commit a) ConflictPatches
apply = unimp

replay :: Commit a -> [CPatch] -> (Commit a, Outcome (ConflictPatches, [CPatch]))
-- when replay foundation ps == (fc', Succ)
--      then rebase has succeeded and has produced a whole bunch
--      of new commits, of which fc' is the new head.
--      N.B.  (distance fc' foundation) == (length ps).
-- when replay foundation ps == (fc', Conflict (cp, ps')),
--      then rebase has failed after a certain number of steps,
--      and cp contain the conflict patches to be written to the
--      working tree, fc' is the new head, and ps' is the work not
--      yet done, which will be done by rebase --continue.
--      N.B. (distance fc' foundation + length ps' + 1) == (length ps)

replay fc [] = (fc, Success)
replay fc (p:ps) = case apply p fc of Left fc' -> replay fc' ps
                                      Right cp -> (fc, Conflict (cp, ps))



unimp :: a
unimp = error "not implemented"
