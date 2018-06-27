module Main where

    import Graphics.UI.Fungen
    import Graphics.Rendering.OpenGL (GLdouble)

    --Player
    type PlayerCharacter = GameObject ()
    type PlayerBullet = GameObject ()
    type PlayerAction a = IOGame GameAttribute () () () a

    --SpaceInvader
    type SpaceInvader = GameObject ()
    type InvaderAction a = IOGame GameAttribute () () () a
  
    --Ammo TravelDirection PressingUp JumpPressed
    data GameAttribute = GA Int Double Bool Bool

    --Player Settings
    moveSpeed = 5
    jumpVelocity = 15
    pSqSize = 12
    bulletSpeed = 15
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
          invaders= objectGroup "invaderGroup" [createInvaders]
          initAmmo = GA maxAmmo 1.0 False False
          input = [
            (SpecialKey KeyRight,  StillDown, movePlayerRight)
            ,(SpecialKey KeyLeft,  StillDown, movePlayerLeft)
            ,(SpecialKey KeyUp,    Press, toggleUpPressed)
            ,(SpecialKey KeyUp,    Release, toggleUpPressed)
            ,(Char ' ',            Press, playerJump)
            ,(Char 'c',            Press, spawnBullet)
            ,(Char 'q',            Press,     \_ _ -> funExit)
            ]
      funInit winConfig gameMap [player,bullet,invaders] () initAmmo input gameCycle (Timer frameTime) bmpList

    --Cria o player. Neste momento o player é apenas um quadrado vermelho
    createPlayer :: PlayerCharacter
    createPlayer = 
        let playerBounds = [(-pSqSize,-pSqSize),(pSqSize,-pSqSize),(pSqSize,pSqSize),(-pSqSize,pSqSize)] -- 'area' do quadrado
            playerPoly   = Basic (Polyg playerBounds 1.0 0.0 0.0 Filled) -- gera a forma do player
        in object "player" playerPoly False (w/2, pSqSize) (0,0) () -- inicializa o player com esta forma gerada

    --Cria as munições do player. Retorna uma lista com todas as balas disponíveis para o jogador
    createBullets :: [PlayerBullet]
    createBullets =
      let bulletPic = Basic (Circle 3.0 1.0 1.0 0 Filled) -- A forma da bala é um pequeno ciruclo
      in (createAsleepBullets 1 maxAmmo bulletPic)

    --método auxiliar que cria a lista das munições.
    --As balas são inicializadas com o booleano True. Isso fará com que as balas inicializadas estejam dormmindo 
    --E não apareçam no jogo sem que o player atire.
    createAsleepBullets :: Int -> Int -> ObjectPicture -> [PlayerBullet]
    createAsleepBullets tMin tMax pic
     | (tMin > tMax) = []
     | otherwise = (object ("bullet" ++ (show tMin)) pic True (0,0) (bulletSpeed,0) ()):(createAsleepBullets (tMin + 1) tMax pic)--Cria a bala e dá a ela o nome correspondente (bullet + index, ex: bullet1 )

    --Dispara as balas
    spawnBullet :: Modifiers -> Position -> PlayerAction ()
    spawnBullet _ _ = do
      (GA a t pUp b) <- getGameAttribute
      if(a > 0)
        then( do
        bullet <- findObject ("bullet" ++ (show a)) "bulletGroup" --Encontra a última bala da lista
        player <- findObject "player" "playerGroup" --Encontra o player
        (pX, pY) <- getObjectPosition player
        (sX, sY) <- getObjectSize player --Coleta dados do jogador
        setObjectAsleep False bullet --Spawna efetivamente a bala no mapa
        if(pUp)
          then(do 
              setObjectPosition (pX, pY) bullet --Passa os parametros necessários como posição e velocidade.
              setObjectSpeed (0,bulletSpeed) bullet)
          else( do
               setObjectPosition (pX+(2*t), pY) bullet --Passa os parametros necessários como posição e velocidade.
               setObjectSpeed (bulletSpeed*t,0) bullet)
        setGameAttribute(GA (a-1) t pUp b)-- Diminui a quantidade de balas disponiveis
        )
        else return()
        
    --Move o jogador para a direita.
    --Atualiza a travelDirection para 1. travelDirection serve para dar a velocidade necessária 
    --para a bala ir para o caminho correto
    movePlayerRight :: Modifiers -> Position -> IOGame GameAttribute () () () ()
    movePlayerRight _ _ = do
     obj     <- findObject "player" "playerGroup"
     (pX,pY) <- getObjectPosition obj
     (sX,_)  <- getObjectSize obj
     (GA a t pUp b) <- getGameAttribute
     setGameAttribute(GA a 1.0 pUp b)--atualiza a travel direction para -1
     if (pX + (sX/2) + 5 <= w)
      then (setObjectPosition ((pX + moveSpeed),pY) obj)
      else (setObjectPosition ((w - (sX/2)),pY) obj)

    --Move o jogador para a direita.
    --Atualiza a travelDirection para -1. travelDirection serve para dar a velocidade necessária 
    --para a bala ir para o caminho correto
    movePlayerLeft :: Modifiers -> Position -> IOGame GameAttribute () () () ()
    movePlayerLeft _ _ = do
      obj <- findObject "player" "playerGroup"
      (pX,pY) <- getObjectPosition obj
      (sX,_)  <- getObjectSize obj
      (GA a t pUp b) <- getGameAttribute
      setGameAttribute(GA a (-1.0) pUp b)--atualiza a travel direction para -1
      if (pX - (sX/2) - moveSpeed >= 0)
       then (setObjectPosition ((pX - 5),pY) obj)
       else (setObjectPosition (sX/2,pY) obj)

    toggleUpPressed :: Modifiers -> Position -> IOGame GameAttribute () () () ()
    toggleUpPressed _ _ = do
      (GA a t pUp b) <- getGameAttribute
      if(pUp)
        then ( setGameAttribute(GA a t False b) )
        else ( setGameAttribute(GA a t True b) )
    
      
    --Dá velocidade vertical ao jogador caso ele nao esteja pulando
    playerJump :: Modifiers -> Position -> IOGame GameAttribute () () () ()
    playerJump _ _ = do
       player <- findObject "player" "playerGroup"
       (vX, vY) <- getObjectSpeed player
       (GA a t pUp b) <- getGameAttribute
       if(not b)
        then (do 
          setObjectSpeed(vX, jumpVelocity) player
          setGameAttribute(GA a t pUp True)--Booleano diz que o jogador está atualmente pulando
          )
        else return()



    ---Space Invaders
    createInvaders :: SpaceInvader
    createInvaders = 
        let invaderBounds = [(-pSqSize,-pSqSize),(pSqSize,-pSqSize),(pSqSize,pSqSize),(-pSqSize,pSqSize)] -- 'area' do quadrado
            invaderPoly   = Basic (Polyg invaderBounds 1.0 0.0 0.0 Filled) -- gera a forma do player
        in object "invader" invaderPoly False (w/2, (h/3)-(pSqSize*3)) (5,0) () -- inicializa o player com esta forma gerada

    moveInvaders :: InvaderAction ()
    moveInvaders = do
       invader <- findObject "invader" "invaderGroup"
       (pX,pY) <- getObjectPosition invader
       if(pX == (w/2 + 100) )
        then reverseXSpeed invader
        else if (pX == w/2 - 100)
          then reverseXSpeed invader
        else return()



    gameCycle :: PlayerAction ()
    gameCycle = do 
      --Gets
      player <- findObject "player" "playerGroup"
      invader <- findObject "invader" "invaderGroup"
      bullets <- getObjectsFromGroup "bulletGroup"
      (vX,vY) <- getObjectSpeed player
      (pX,pY) <- getObjectPosition player
      (_,sY)  <- getObjectSize player
      (GA a t pUp b) <- getGameAttribute
      moveInvaders
      col4 <- objectBottomMapCollision player
      col5 <- objectListObjectCollision bullets invader
      col6 <- objectBottomMapCollision invader
      --Collisions
      when(col4) (do 
        setObjectPosition (pX, sY/2) player
        setObjectSpeed (vX, 0) player
        if(b)
         then(setGameAttribute(GA a t pUp False))--Caso o jogador pouse no chão, significa que o pulo acabou. Atualizar o booleano
         else(return())
        )
      when(not col4)( (setObjectSpeed ( (0.0, vY-(gravityScale/16) ) ) player) )--Caso não esteja pisando no chão, simular gravidade.
      when(col5) (setObjectAsleep True invader)
      when(col6) (funExit)
      showFPS TimesRoman24 (w-24, h-28) 1.0 0.0 0.0
      printOnScreen ("Ammo Remaining: " ++ show a) TimesRoman24 (0,0) 1.0 1.0 1.0

      