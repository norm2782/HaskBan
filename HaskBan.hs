data EnvObj = Wall
            | Box
            | Path
            | Target

data Surrounding = Left EnvObj
                 | Right EnvObj
                 | Up EnvObj
                 | Down EnvObj


