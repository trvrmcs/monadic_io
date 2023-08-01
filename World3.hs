import Bits 

-- last time we did this: 
-- type Program a = World -> (a, World)

--Now we create a new type rather than an alias.

newtype Program a = Program ( World -> (World, a) )


-- https://github.com/ghc/ghc/blob/master/libraries/ghc-prim/GHC/Types.hs#L244

run :: Program a -> World -> ( World,a ) 
run (Program f)   = f


readBit :: Program Bit 
readBit = Program $ \(World (bit:remaining) output) -> ( World remaining output, bit)

writeBit :: Bit -> Program Unit
writeBit bit = Program $ \(World input output ) -> ( World input (output ++ [bit]), unit)

readByte :: Program Byte
readByte = Program $ \(World input output) -> let (byte, remaining) = getByte input in 
                                          ( World remaining output, byte)

writeByte :: Byte -> Program Unit 
writeByte byte = Program $  \(World input output) -> (World input (output ++ toBits byte ), unit)


andThen :: Program  a -> (a -> Program b ) -> Program  b
andThen (Program program0) programMaker = Program $ \world0 -> 
                        let (world1, bit1) = program0 world0 in 
                        let (Program program1) = programMaker bit1 in 
                        let ( world2, bit2) = program1 world1 in 
                        (world2, bit2)
                       

-- Now let's make it a Monad

instance Functor Program where 
 --  fmap :: ((a -> b) -> Program (World -> (World,a ))    ->  Program (World -> ( World, b))
 fmap f program = Program $ \world -> let ( world',a) = run program world in ( world', f a)
 
p1 :: Program Int 
p1 = Program (\world -> (world,42))

p3 :: Program Bool 
p3 = fmap (>10) p1


-- This isn't particularly useful yet but necessary to keep Haskell happy.
instance Applicative Program where 
   pure x = Program (\world -> (world, x))

--   (<*>) :: f (a -> b) -> f a -> f b
--   (<*>) :: Program (World -> (  World ), (a->b)  ) -> Program (World -> ( World, a)) -> Program (World -> (World, b))
--                          f1                               f2                              f3 
   (Program p1) <*> (Program p2) = Program $ \world -> 
                    let ( world', a_b) = p1 world in 
                    let (world'', a) = p2 world' in 
                    (world'', a_b a)
                    


-- This is the magic
instance Monad Program where 
    (>>=) = andThen

 
programRunner :: Program a -> [Bit]
programRunner p = let initial = World [One, Zero, One, One, One, One, One, One] [] in 
                  let ((World input output),_) = run p initial in
                  output


p :: Program Unit
p = readByte >>= writeByte

-- (_, final) = run p initial

main :: IO ()
main = print $ programRunner p
