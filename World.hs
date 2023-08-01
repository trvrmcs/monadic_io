{--
Let's build a very simple computer. 

It only operates on bits, that are either One or Zero
--}

data Bit = One | Zero deriving (Show, Eq)


-- We can do regular boolean logic on bits:

not_ ::  Bit -> Bit
not_ One = Zero 
not_ Zero = One 

and_ :: Bit -> Bit -> Bit 
and_ One One = One 
and_ _ _ = Zero
 
or_ :: Bit -> Bit -> Bit 
or_ Zero Zero = Zero
or_ _ _ = One

{-- 

Now let's say we also want to do IO. Imagine there's some external 
world with two channels of bits: an input channel that we can 
read from, and an output channel we can write to.  The state of 
the world at any given time is the state of these channels
--} 


data World   = World [Bit] [Bit] deriving (Show, Eq)

{--

And then let's say that a 'Program' is something that takes a 'World' 
and returns a new one.

--}

type Program = World -> (World, Bit) 

{--
`writeBit` takes a bit and produces a program that takes an existing 
world and produces a new one with that bit added to its output

Note that `writeBit` *isn't* a program: it is a program constructor. This is 
vital, if we're going to do anything meaningful we need to be able to build 
programs from smaller building blocks.

--}

writeBit :: Bit -> Program
writeBit bit = \ (World input output ) ->( World input (output ++ [bit]), Zero)

{--
def writeBit(bit):
    return lambda world: (newWorld, Zero)
--}

{--

Likewise, `readBit`, when called, creates a program that, given some input 
world returns a new world with `bit` extracted from the input channel

--}


readBit :: Program 
readBit (World (bit:remaining) output) =   (World remaining output, bit )

{--

Finally we need some way of sequencing. (Insert several hundred pages 
of discussion about category theory here...).

But in short, `andThen` takes a program and a program constructor and 
returns a new program.

--} 


andThen :: Program -> (Bit -> Program) -> Program  
andThen program0 programMaker = \world0 -> 
                        let (world1, bit1) = program0 world0 in 
                        let program1 = programMaker bit1 in 
                        let ( world2, bit2) = program1 world1 in 
                        (world2, bit2)
                       
   
{--

Ok, on to some examples.  
--}

 

p1 :: Program 
p1 = (readBit `andThen` (writeBit.not_))


p2 :: Program 
p2 = (readBit `andThen` (\ _ -> readBit) `andThen` writeBit)

p3 :: Program
p3 = (readBit `andThen` (\x -> readBit `andThen` (\y -> writeBit (and_ x y))))

{--

The actually program runner. Note that `initial` is hidden inside this 
function and not accessible by any other code - this is roughly saying 
'you can't directly do IO, you can just build Program objects'

--}


programRunner :: Program -> [Bit]
programRunner p = let initial = World [One, One] [] in 
                  let ((World input output),_) = p initial in 
                  output
     

main :: IO ()
main = print $ programRunner p1

