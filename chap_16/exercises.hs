{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

data LR b a = L a a a | R b a b


data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor $ f b

-- 1
newtype K a b = K a deriving ( Show )
instance Functor (K a) where
  fmap _ (K a) = K a

-- 2
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
type KM = Flip K Int String
-- should remind you of an
-- instance you've written before 

instance Functor (Flip K a) where
  fmap f (Flip (K b)) = Flip (K (f b))
--  fmap f (Fl b) = Fl $ fmap f b

-- 4
data EvilGoateeConst a b = GoatyConst b
instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

-- 5
data LiftItOut f a = LiftItOut (f a) deriving ( Show )
instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut a) = LiftItOut $ fmap f a

-- 6
data Parappa f g a = DaWrappa (f a) (g a) deriving ( Show )
instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa a b) = DaWrappa (fmap f a) (fmap f b)

-- 7
data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving ( Show )
instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething a b) = IgnoringSomething a (fmap f b)

-- 8
data Notorious g o a t = Notorious (g o) (g a) (g t)
instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious a b c) = Notorious a b (fmap f c)

data List a = Nil | Cons a (List a) deriving ( Show )
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a xs) = Cons (f a) (fmap f xs)


data GoatLord a =
      NoGoat
    | OneGoat a
    | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a) deriving ( Show )

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b)  (fmap f c)

data TalkToMe a = 
      Halt
    | Print String a 
    | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print words a) = Print words (f a)
  fmap f (Read g) = Read $ f . g
