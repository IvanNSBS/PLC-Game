module Main where

    import Graphics.UI.Fungen
    import Graphics.Rendering.OpenGL (GLdouble)
    
    data GameAttribute = Score Int
    
    type PongObject = GameObject ()
    type PongAction a = IOGame GameAttribute () () () a

    movespeed = 12

    width = 400
    height = 400
    w = fromIntegral width :: GLdouble
    h = fromIntegral height :: GLdouble
    
    main :: IO ()
    main = do
      let winConfig = ((100,80),(width,height),"A brief example!")
          bmpList = [("tex.bmp", Nothing)]
          gameMap = textureMap 0 30 30 w h
          player     = objectGroup "playerGroup"  [createPlayer]
          initScore = Score 0
          input = [
            (SpecialKey KeyRight, StillDown, moveBarToRight)
            ,(SpecialKey KeyLeft,  StillDown, moveBarToLeft)
            ,(Char 'q',            Press,     \_ _ -> funExit)
            ]
      funInit winConfig gameMap [player] () initScore input gameCycle (Timer 16) bmpList
    
    createPlayer :: PongObject
    createPlayer = 
      let barPi = Basic (Polyg [(-25,-6),(25,-6),(25,6),(-25,6)] 1.0 1.0 1.0 Unfilled)
      in object "player" barPi False (125,30) (0,0) ()             
    
    moveBarToRight :: Modifiers -> Position -> IOGame GameAttribute () () () ()
    moveBarToRight _ _ = do
      obj <- findObject "player" "playerGroup"
      (pX,pY) <- getObjectPosition obj
      (sX,_)  <- getObjectSize obj
      if (pX + (sX/2) + movespeed <= w)
       then (setObjectPosition ((pX + movespeed),pY) obj)
       else (setObjectPosition ((w - (sX/2)),pY) obj)
    
    moveBarToLeft :: Modifiers -> Position -> IOGame GameAttribute () () () ()
    moveBarToLeft _ _ = do
      obj <- findObject "player" "playerGroup"
      (pX,pY) <- getObjectPosition obj
      (sX,_)  <- getObjectSize obj
      if (pX - (sX/2) - movespeed >= 0)
       then (setObjectPosition ((pX - movespeed),pY) obj)
       else (setObjectPosition (sX/2,pY) obj)
    
    gameCycle :: PongAction ()
    gameCycle = do
        showFPS TimesRoman24 (30,0) 1.0 0.0 0.0