import Bits  
 

-- Now we go one step further and have programs work on ANY type, rather 
-- than just Bit
-- 

type Program a = World -> (World,a)


writeBit :: Bit -> Program Unit
writeBit bit = \ (World input output ) ->(  World input (output ++ [bit]),  unit)


readBit :: Program Bit
readBit (World (bit:remaining) output  ) = (World remaining output , bit)



readByte :: Program Byte 
readByte (World input output) = let (byte, remaining) = getByte input in 
                            ( World remaining output, byte)

writeByte :: Byte -> Program Unit 
writeByte byte = \ (World input output) -> (World input (output ++ toBits byte ), unit)


andThen :: Program a -> (a -> Program b ) -> Program b
andThen program0 programMaker = \world0 -> 
                        let (world1, x) = program0 world0 in 
                        let program1 = programMaker x in 
                        let ( world2, y) = program1 world1 in 
                        ( world2, y)
                       
   

programRunner :: Program a -> [Bit]
programRunner p = let initial = World [One, Zero, One, One, One, One, One, One] [] in 
                  let ((World input output), _) = p initial in 
                  output




{--
    Can be read as 'p is a program that, if applied to a World, will produce a new 
    World with one byte moved from the input channel to the output channel


    (In other words, `cat`)
--}

p :: Program Unit
p = (readByte `andThen` writeByte)


main :: IO ()
main = print $ programRunner p

 