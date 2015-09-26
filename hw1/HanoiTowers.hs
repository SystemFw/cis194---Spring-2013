type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -- ^ The number of disks, >= 0
         -> Peg  -- ^ Source peg
         -> Peg  -- ^ Destination peg
         -> Peg  -- ^ Support peg 
         -> [Move]
hanoi 0 _ _ _ = []
hanoi n src dest support = hanoi (n-1) src support dest ++
                           [(src,dest)]
                           ++ hanoi (n-1) support dest src
