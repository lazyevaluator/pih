{- 
Note on Pattern Matching:
  See also https://www21.in.tum.de/teaching/fpv/WS20/assets/slides.pdf Slides 97-99
  - Constructors can be pattern matched because the structure can be *uniquely* constructed from them,
    e.g. lists with constructors (:) and []
    ++ is not a constructor, because [1,2,3] = [1] ++ [2,3] = [1,2] ++ [3]
    -> ++ cannot be pattern matched.
    So a pattern can be 
      - a variable such as x or _ 
      - a constructor pattern C p1    pn
        where C is a constructor and p1 to pn are patterns (e.g. C = [] is also valid because patterns p1 to pn are empty)

  
-}
