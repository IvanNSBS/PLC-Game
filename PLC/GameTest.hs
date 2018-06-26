module Main where

    import Graphics.UI.Fungen
    import Graphics.Rendering.OpenGL (GLdouble)

    type PlayerCharacter = GameObject ()
    type PlayerBullet = GameObject ()
    type PlayerAction a = IOGame GameAttribute () () () a
    
    
    --data GameAttribute = Score Int
    --Ammo TravelDirection JumpPressed
    data GameAttribute = GA Int Double Bool

    --Player Settings
    moveSpeed = 5
    jumpVelocity = 15
    pSqSize = 12
    bulletSpeed = 15
    jumpPressed = False
    maxAmmo = 999

    --World Settings
    gravityScale = 10
    frameTime = 16

    --UI Settings
    width = 840
    height = 600
    w = fromIntegral width :: GLdouble
    h = fromIntegral height :: GLdouble

    main :: IO ()
    main = do
      let winConfig = ((100,80),(width,height),"A brief example!")
          bmpList = [("tex.bmp", Nothing)]
          gameMap = textureMap 0 30 30 w h
          player  = objectGroup "playerGroup"  [createPlayer]
          bullet  = objectGroup "bulletGroup"  createBullets
          initAmmo = GA 999 1.0 False
          input = [
            (SpecialKey KeyRight,  StillDown, movePlayerRight)
            ,(SpecialKey KeyLeft,  StillDown, movePlayerLeft)
            ,(SpecialKey KeyUp,    Press, playerJump)
            ,(Char 'c',            Press, spawnBullet)
            ,(Char 'q',            Press,     \_ _ -> funExit)
            ]
      funInit winConfig gameMap [player,bullet] () initAmmo input gameCycle (Timer frameTime) bmpList

    createPlayer :: PlayerCharacter
    createPlayer = 
        let playerBounds = [(-pSqSize,-pSqSize),(pSqSize,-pSqSize),(pSqSize,pSqSize),(-pSqSize,pSqSize)]
            playerPoly   = Basic (Polyg playerBounds 1.0 0.0 0.0 Filled)
        in object "player" playerPoly False (w/2, h) (0,0) ()

    createBullets :: [PlayerBullet]
    createBullets =
      let bulletPic = Basic (Circle 3.0 1.0 1.0 0 Filled) 
      in (createAsleepBullets 1 maxAmmo bulletPic)

    createAsleepBullets :: Int -> Int -> ObjectPicture -> [PlayerBullet]
    createAsleepBullets tMin tMax pic
     | (tMin > tMax) = []
     | otherwise = (object ("bullet" ++ (show tMin)) pic True (0,0) (bulletSpeed,0) ()):(createAsleepBullets (tMin + 1) tMax pic)

    spawnBullet :: Modifiers -> Position -> PlayerAction ()
    spawnBullet _ _ = do
      (GA a t b) <- getGameAttribute
      if(a > 0)
        then( do
        bullet <- findObject ("bullet" ++ (show a)) "bulletGroup"
        player <- findObject "player" "playerGroup"
        (pX, pY) <- getObjectPosition player
        (sX, sY) <- getObjectSize player
        setObjectAsleep False bullet
        setObjectPosition (pX+(sX*t), pY) bullet
        setObjectSpeed (bulletSpeed*t,0) bullet
        setGameAttribute(GA (a-1) t b)
        )
        else return()
        
    movePlayerRight :: Modifiers -> Position -> IOGame GameAttribute () () () ()
    movePlayerRight _ _ = do
     obj     <- findObject "player" "playerGroup"
     (pX,pY) <- getObjectPosition obj
     (sX,_)  <- getObjectSize obj
     (GA a t b) <- getGameAttribute
     setGameAttribute(GA a 1.0 b)
     if (pX + (sX/2) + 5 <= w)
      then (setObjectPosition ((pX + moveSpeed),pY) obj)
      else (setObjectPosition ((w - (sX/2)),pY) obj)

    movePlayerLeft :: Modifiers -> Position -> IOGame GameAttribute () () () ()
    movePlayerLeft _ _ = do
      obj <- findObject "player" "playerGroup"
      (pX,pY) <- getObjectPosition obj
      (sX,_)  <- getObjectSize obj
      (GA a t b) <- getGameAttribute
      setGameAttribute(GA a (-1.0) b)
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
       (GA a t b) <- getGameAttribute
       if(not b)
        then (do 
          setObjectSpeed(vX, jumpVelocity) player
          setGameAttribute(GA a t True)
          )
        else return()

    gameCycle :: PlayerAction ()
    gameCycle = do 
      player <- findObject "player" "playerGroup"
      (vX,vY) <- getObjectSpeed player
      (pX,pY) <- getObjectPosition player
      (_,sY)  <- getObjectSize player
      (GA a t b) <- getGameAttribute
      col4 <- objectBottomMapCollision player
      when(col4) (do 
        setObjectPosition (pX, sY/2) player
        setObjectSpeed (vX, 0) player
        if(b)
         then(setGameAttribute(GA a t False))
         else(return())
        )
      when(not col4)((setObjectSpeed ((0.0, -0.5+vY)) player))
      showFPS TimesRoman24 (w-24, h-28) 1.0 0.0 0.0
      printOnScreen ("Ammo Remaining: " ++ show a) TimesRoman24 (0,0) 1.0 1.0 1.0

      