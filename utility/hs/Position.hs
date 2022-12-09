module Position where

-- | A position of two values.
data Pos a = Pos {
    -- | Gets the x position.
    xpos :: a,
    -- | Gets the y position.
    ypos :: a }
    deriving (Eq)

instance (Show a) => Show (Pos a) where
    show (Pos x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

instance Functor Pos where
    fmap f (Pos x y) = Pos (f x) (f y)

instance Applicative Pos where
    pure v = Pos v v
    (<*>) (Pos fx fy) (Pos x y) = Pos (fx x) (fy y)

instance Monad Pos where
  (>>=) (Pos x y) f = Pos (xpos $ f x) (ypos $ f y)

instance Num a => Num (Pos a) where
    (+) = posCombine (+)
    (-) = posCombine (-)
    (*) = posCombine (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger v = pure (fromInteger v)

-- | Constructs a new position by applying a function
-- to the elements of two other positions.
posCombine :: (a -> b -> c) -> Pos a -> Pos b -> Pos c
posCombine f (Pos x1 y1) (Pos x2 y2) = Pos (f x1 x2) (f y1 y2)
