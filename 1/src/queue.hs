-- why the hell doesn't haskell provide a queue data structure??????

data Queue a = Queue [a] deriving (Show)

pop :: Queue a -> a, Queue a

