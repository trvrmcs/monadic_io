module Bits where 

data Bit = One | Zero deriving  Eq

instance Show Bit where 
    show One = "1"
    show Zero = "0"

 

not_ ::  Bit -> Bit
not_ One = Zero 
not_ Zero = One 

and_ :: Bit -> Bit -> Bit 
and_ One One = One 
and_ _ _ = Zero



data World   = World [Bit] [Bit] deriving (Show, Eq)

data Byte = Byte Bit Bit Bit Bit Bit Bit Bit Bit

byte :: [Bit]->Byte 
byte (a:b:c:d:e:f:g:h:[]) = (Byte a b c d e f g h)
byte _ = error "incorrect number of bits"


instance Show Byte where 
    show (Byte a b c d e f g h)  = "<" ++ (show a ) ++ (show b)++ (show c) ++ (show d) ++ (show e) ++ (show f) ++ (show g) ++ (show h) ++">"


toBits :: Byte -> [Bit] 
toBits (Byte a b c d e f g h) = [a, b, c, d, e, f, g, h]



-- get first byte from a list of bits, return byte plus remaining bits
getByte :: [Bit]->(Byte, [Bit])
getByte input = let (xs,ys) = splitAt 8 input 
          in (byte xs, ys)


-- Introduce this alias to make all this marginally more readable to the 
-- Python programmer
type Unit = () 
unit = ()

