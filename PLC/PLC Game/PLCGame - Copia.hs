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
    rows = 3
    columns = 5
    
    --Enemies Left Ammo TravelDirection PressingUp JumpPressed
    data GameAttribute = GA Int Int Double Bool Bool

    --Level
    data GameState = LevelStart Int | Level Int | GameOver
    data TileAttribute = NoTileAttribute
    type GameTile = Tile TileAttribute
    type GameMap = TileMatrix TileAttribute

    --Player Settings
    moveSpeed = 5
    jumpVelocity = 15
    pSqSize = 12
    bulletSpeed = 15
    maxAmmo = 200

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
      let winConfig = ((100,80),(width,height),"PLC Project")

          bmpList  = [("tex.bmp", Nothing)]

          gameMap  = textureMap 0 30 30 w h

          groups = [(objectGroup "playerGroup"   [createPlayer]),
                    (objectGroup "bulletGroup"    createBullets),
                    (objectGroup "invaderGroup"   initInvaders )]

          initAmmo = GA (rows * ((columns*2)-1) ) maxAmmo 1.0 False False

          input = [
            (SpecialKey KeyRight,  StillDown, movePlayerRight)
            ,(SpecialKey KeyLeft,  StillDown, movePlayerLeft)
            ,(SpecialKey KeyUp,    Press, toggleUpPressed)
            ,(SpecialKey KeyUp,    Release, toggleUpPressed)
            ,(Char ' ',            Press, playerJump)
            ,(Char 'c',            Press, spawnBullet)
            ,(Char 'q',            Press,     \_ _ -> funExit)
            ]
            
      funInit winConfig gameMap groups () initAmmo input gameCycle (Timer frameTime) bmpList

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
    spawnBullet _ _= do
      (GA e a t pUp b) <- getGameAttribute
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
        setGameAttribute(GA e (a-1) t pUp b)-- Diminui a quantidade de balas disponiveis
        )
        else return()

        
    checkBulletCollision :: PlayerBullet -> [SpaceInvader] -> PlayerAction ()
    checkBulletCollision bullet (x:xs) = do
      col <- objectsCollision bullet x
      if(col)
        then( do
              setObjectAsleep True x
              setObjectAsleep True bullet
              (GA e a t pUp b) <- getGameAttribute
              setGameAttribute(GA (e-1) a t pUp b) )
        else if(not(length xs == 0))
          then (checkBulletCollision bullet xs)
          else return()

    --Precisa de threads
    checkAllBulletsCollision :: [PlayerBullet] -> PlayerAction ()
    checkAllBulletsCollision (b:bs)
     | (length bs == 0) = do
                          invaders <- getObjectsFromGroup "invaderGroup"
                          checkBulletCollision b invaders
                          return()
     | otherwise = do 
                   invaders <- getObjectsFromGroup "invaderGroup"
                   checkBulletCollision b invaders
                   checkAllBulletsCollision bs  

    --Move o jogador para a direita.
    --Atualiza a travelDirection para 1. travelDirection serve para dar a velocidade necessária 
    --para a bala ir para o caminho correto
    movePlayerRight :: Modifiers -> Position -> IOGame GameAttribute () () () ()
    movePlayerRight _ _ = do
     obj     <- findObject "player" "playerGroup"
     (pX,pY) <- getObjectPosition obj
     (sX,_)  <- getObjectSize obj
     (GA e a t pUp b) <- getGameAttribute
     setGameAttribute(GA e a 1.0 pUp b)--atualiza a travel direction para -1
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
      (GA e a t pUp b) <- getGameAttribute
      setGameAttribute(GA e a (-1.0) pUp b)--atualiza a travel direction para -1
      if (pX - (sX/2) - moveSpeed >= 0)
       then (setObjectPosition ((pX - 5),pY) obj)
       else (setObjectPosition (sX/2,pY) obj)

    toggleUpPressed :: Modifiers -> Position -> IOGame GameAttribute () () () ()
    toggleUpPressed _ _ = do
      (GA e a t pUp b) <- getGameAttribute
      if(pUp)
        then ( setGameAttribute(GA e a t False b) )
        else ( setGameAttribute(GA e a t True b) )
    
      
    --Dá velocidade vertical ao jogador caso ele nao esteja pulando
    playerJump :: Modifiers -> Position -> IOGame GameAttribute () () () ()
    playerJump _ _ = do
       player <- findObject "player" "playerGroup"
       (vX, vY) <- getObjectSpeed player
       (GA e a t pUp b) <- getGameAttribute
       if(not b)
        then (do 
          setObjectSpeed(vX, jumpVelocity) player
          setGameAttribute(GA e a t pUp True)--Booleano diz que o jogador está atualmente pulando
          )
        else return()



    ---Space Invaders
    createInvaderAt :: (GLdouble, GLdouble) -> String -> SpaceInvader
    createInvaderAt (pX, pY) name =
        let invaderBounds = [(-pSqSize,-pSqSize),(pSqSize,-pSqSize),(pSqSize,pSqSize),(-pSqSize,pSqSize)] -- 'area' do quadrado
            invaderPoly   = Basic (Polyg invaderBounds 1.0 0.5 0.0 Filled) -- gera a forma do player
        in object name invaderPoly False (pX, pY) (0,0) () -- inicializa o player com esta forma gerada

    createInvaders :: Int-> Int -> [SpaceInvader]
    createInvaders row column
      | (row >= rows) = []
      | (column >= columns) = createInvaders (row+1) (-4)
      | otherwise = do 
        let offset = pSqSize*2 + 5
        let x = (w/2)+(offset * (fromIntegral column))
        let y = (h - 100)+((fromIntegral row) * offset)
        let name = "invader" ++ (show row) ++ (show column)
        (createInvaderAt (x, y) (name):(createInvaders (row) (column+1)) )   

    initInvaders :: [SpaceInvader]
    initInvaders = (createInvaders 0 (-4) )

    reverseInvadersSpeed :: [SpaceInvader] -> InvaderAction ()
    reverseInvadersSpeed (x:xs)
      | (length xs == 0) = do 
         reverseXSpeed x
         return()
      | otherwise = do 
         reverseXSpeed x
         reverseInvadersSpeed (xs)
    
    --Precisa de thread.     
    moveInvaders :: InvaderAction ()
    moveInvaders = do
       invader <- findObject "invader00" "invaderGroup"
       allInvaders <- getObjectsFromGroup "invaderGroup"
       (pX,pY) <- getObjectPosition invader
       if(pX >= (w/2 + 100) )
        then reverseInvadersSpeed allInvaders
        else if (pX <= w/2 - 100)
          then reverseInvadersSpeed allInvaders
        else return()


    -- Game Cycle    
    gameCycle :: PlayerAction ()
    gameCycle = do 
      --Gets
      player <- findObject "player" "playerGroup"
      bullets <- getObjectsFromGroup "bulletGroup"

      (vX,vY) <- getObjectSpeed player
      (pX,pY) <- getObjectPosition player
      (_,sY)  <- getObjectSize player

      (GA e a t pUp b) <- getGameAttribute

      --Collisions
      checkAllBulletsCollision bullets

      col4 <- objectBottomMapCollision player
      when(col4) (do 
        setObjectPosition (pX, sY/2) player
        setObjectSpeed (vX, 0) player
        if(b)
         then(setGameAttribute(GA e a t pUp False))--Caso o jogador pouse no chão, significa que o pulo acabou. Atualizar o booleano
         else(return())
        )
      when(not col4)( (setObjectSpeed ( (0.0, vY-(gravityScale/16) ) ) player) )--Caso não esteja pisando no chão, simular gravidade.

      --UI
      showFPS TimesRoman24 (w-24, h-28) 1.0 0.0 0.0
      printOnScreen ("Ammo Remaining: " ++ show a) TimesRoman24 (0,0) 1.0 1.0 1.0
      printOnScreen ("Enemies Remaining: " ++ show e) TimesRoman24 (0,h-25) 1.0 1.0 1.0

{-
Falta : 
Verificar colisao das balas com o uso de threads para eficiencia
Verificar colisao dos invaders com o final do mapa para game over
Os 3 níveis
Threads para mover os invaders de um lado para o outro
Os níveis a mais terão tipos de inimigos a mais
Os inimigos serão spawnados com threadss
-}      