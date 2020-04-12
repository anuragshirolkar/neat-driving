module Brain where


newtype Brain = Brain {network :: Int}

newtype Sensors = Sensors {velocitySensor :: Float}

data Actions = Actions { moveAction :: Float, turnAction :: Float }

think :: Brain -> Sensors -> Actions
think _ _ = Actions 1 0