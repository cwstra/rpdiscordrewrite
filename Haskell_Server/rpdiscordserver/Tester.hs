import qualified Data.HashMap.Strict as HM

smartInc :: Maybe Word -> Maybe Word
smartInc Nothing = Just $ 1
smartInc (Just v) = Just $ v + 1
smartMerge :: Word -> Maybe (HM.HashMap Word Word) -> Maybe (HM.HashMap Word Word)
smartMerge n Nothing = Just $ HM.singleton n 1
smartMerge n (Just wordMap) = Just $ HM.alter smartInc n wordMap
