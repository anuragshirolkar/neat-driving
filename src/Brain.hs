module Brain where


newtype Brain = Brain {network :: Int}

data Sensors =
    Sensors { velocitySensor    :: Float
            , rightMargin       :: Float
            , leftMargin        :: Float
            , frontMargin       :: Float
            }

data Actions = Actions { moveAction :: Float, turnAction :: Float }

think :: Brain -> Sensors -> Actions
think _ _ = Actions 1 0