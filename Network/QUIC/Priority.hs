module Network.QUIC.Priority
  where
-- Prioritazation is stream prioritazation abstraction
data Priority = Priority {}
                    deriving Show

defaultPrioritazation :: Priority
defaultPrioritazation = undefined

incrPriority :: Priority
incrPriority = undefined

decrPriority :: Priority
decrPriority = undefined
