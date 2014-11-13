module ComplexNumbers where

data Complex = Real Float | Img Float | Cpx Float Float

conjugate :: Complex -> Complex
conjugate (Real a) = Real a
conjugate (Img a)  = -(Img a)
conjugate (Cpx r i)= Cpx r (-i)

instance Num Complex where
    (+) (Real a) (Real b)     
        = Real (a+b)
    (+) (Real a) (Img b)       
        = Cpx a b
    (+) (Real a) (Cpx r i)
        = Cpx (a+r) i
    (+) (Img b) (Real a)
        = Cpx a b
    (+) (Img a) (Img b)       
        = Img (a+b)
    (+) (Img a) (Cpx r i)       
        = Cpx r (a+i)
    (+) (Cpx r i) (Real a)
        = Cpx (r+a) (i)
    (+) (Cpx r i) (Img b)
        = Cpx (r) (i+b)
    (+) (Cpx r i) (Cpx r' i')
        = Cpx (r+r') (i+i')
    (*) (Real a) (Real b)     
        = Real (a*b)
    (*) (Real a) (Img b)       
        = Img (a*b)
    (*) (Real a) (Cpx r i)
        = Cpx (a*r) (a*i)
    (*) (Img b) (Real a)
        = Img (a*b)
    (*) (Img a) (Img b)       
        = Real (-a*b)
    (*) (Img a) (Cpx r i)       
        = Cpx (-a*i) (a*r)
    (*) (Cpx r i) (Real a)
        = Cpx (a*r) (a*i)
    (*) (Cpx r i) (Img b)
        = Cpx (-i*b) (r*b)
    (*) (Cpx r i) (Cpx r' i')
<<<<<<< HEAD
        = Cpx (r*r') (i*i')

    negate (Real a)
        = Real (-a)
    negate (Img b)
        = Img (-b)
    negate (Cpx r i)
        = Cpx (-r) (-i)
    
    abs (Real a)
        = Real a
    abs (Img a)
        = Real a
    abs (Cpx r i)
        = Real (sqrt (r^2 + i^2))

    signum (Real a)
        | a == 0 = Real 0
        | a > 1  = Real 1
        | a < -1 = Real -1
    signum a = undefined
    fromInteger n
        = Real (fromInteger n)
=======
        = Cpx (r*r' - i*i') (r*i' + i*r')
>>>>>>> refs/remotes/origin/master
