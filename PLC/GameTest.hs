module Main where

    import Graphics.UI.Fungen
    import Graphics.Rendering.OpenGL (GLdouble)

    type PlayerCharacter = GameObject ()
    type PlayerAction a = IOGame GameAttribute () () () a
    data GameAttribute = Score Int

    --Player Settings
    moveSpeed = 5
    jumpVelocity = 15
    jumpPressed = False

    --World Settings
    gravityScale = 10
    frameTime = 16

    --UI Settings
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
            (SpecialKey KeyRight,  StillDown, movePlayerRight)
            ,(SpecialKey KeyLeft,  StillDown, movePlayerLeft)
            ,(SpecialKey KeyUp,    Press, playerJump)
            ,(Char 'q',            Press,     \_ _ -> funExit)
            ]
      funInit winConfig gameMap [player] () initScore input gameCycle (Timer frameTime) bmpList

    createPlayer :: PlayerCharacter
    createPlayer = 
        let playerBounds = [(-6,-6),(6,-6),(6,6),(-6,6)]
            playerPoly   = Basic (Polyg playerBounds 1.0 0.0 0.0 Filled)
        in object "player" playerPoly False (w/2, h) (0,0) ()
        
    movePlayerRight :: Modifiers -> Position -> IOGame GameAttribute () () () ()
    movePlayerRight _ _ = do
     obj     <- findObject "player" "playerGroup"
     (pX,pY) <- getObjectPosition obj
     (sX,_)  <- getObjectSize obj
     if (pX + (sX/2) + 5 <= w)
      then (setObjectPosition ((pX + moveSpeed),pY) obj)
      else (setObjectPosition ((w - (sX/2)),pY) obj)

    movePlayerLeft :: Modifiers -> Position -> IOGame GameAttribute () () () ()
    movePlayerLeft _ _ = do
      obj <- findObject "player" "playerGroup"
      (pX,pY) <- getObjectPosition obj
      (sX,_)  <- getObjectSize obj
      if (pX - (sX/2) - moveSpeed >= 0)
       then (setObjectPosition ((pX - 5),pY) obj)
       else (setObjectPosition (sX/2,pY) obj)

    movePlayerUp :: Modifiers -> Position -> IOGame GameAttribute () () () ()
    movePlayerUp _ _ = do
      obj <- findObject "player" "playerGroup"
      (pX,pY) <- getObjectPosition obj
      (_,sY)  <- getObjectSize obj
      if (pY + (sY/2) + moveSpeed <= h)
       then (setObjectPosition (pX, (pY+jumpVelocity) ) obj)
       else (setObjectPosition (pX, (h - (sY/2)) ) obj)

    playerJump :: Modifiers -> Position -> IOGame GameAttribute () () () ()
    playerJump _ _ = do
       player <- findObject "player" "playerGroup"
       (vX, vY) <- getObjectSpeed player
       if(vY <= 0 && not jumpPressed)
        then (setObjectSpeed(vX, jumpVelocity) player)
        else (setObjectSpeed(vX, vY-(gravityScale/16)) player)

    gameCycle :: PlayerAction ()
    gameCycle = do 
      player <- findObject "player" "playerGroup"
      (vX,vY) <- getObjectSpeed player
      (pX,pY) <- getObjectPosition player
      (_,sY)  <- getObjectSize player
      col4 <- objectBottomMapCollision player
      when(col4) (do 
        setObjectPosition (pX, sY/2) player
        setObjectSpeed (vX, 0) player)
      showFPS TimesRoman24 (30,0) 1.0 0.0 0.0
      if(not jumpPressed)
       then (setObjectSpeed ((0.0, -0.5+vY)) player)
       else (setObjectSpeed (vX,vY) player)