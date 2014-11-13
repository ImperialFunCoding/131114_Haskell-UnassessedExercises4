module ComplexNumbers where

data Complex = Real Float | Img Float | Cpx Float Float

conjugate :: Complex -> Complex
conjugate (Real a) = Real a
conjugate (Img a)  = -(Img a)
conjugate (Cpx r i)= Cpx r (-i)

instance Num Complex where
    (+) (Real a) (Real b)     = Real (a+b)
    (+) (Img a) (Img b)       = Img (a+b)
    (+) (Cpx r i) (Cpx r' i') = Img (a+b)
    (+) (Cpx r (Img i)) (Cpx r' (Img i'))
        = Cpx (r+r') (Img (i+i'))
