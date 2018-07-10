module Main where

    import Graphics.UI.Fungen
    import Graphics.Rendering.OpenGL (GLdouble)
    import Control.Concurrent
    import Control.Concurrent.MVar
    import System.Random

    
    --Player
    type PlayerCharacter = GameObject ()
    type PlayerBullet = GameObject ()
    type PlayerAction a = IOGame GameAttribute () GameState TileAttribute a
    
    --SpaceInvader
    type SpaceInvader = GameObject ()
    type InvaderAction a = IOGame GameAttribute () GameState TileAttribute a
    rows = 4
    columns = 5
    invaderYSpeed = (-0.5)
    invaderXSpeed = 4
    enemy1Amount = 64
    enemy1Delay = 1.0
    enemy1Speed = -6.0
    enemy2Amount = 72
    enemy2Delay = 0.8
    enemy2Speed = 4.0
    
    --EnemiesLeft AmmoLeft TravelDirection JumpPressed T1EnemiesLeft T2EnemiesLeft
    data GameAttribute = GA Int Int Double Bool Int Int

    --Level
    data GameState = Level Int
    data TileAttribute = NoTileAttribute
    type GameTile = Tile TileAttribute
    type PLCGameMap = TileMatrix TileAttribute

    --Player Settings
    moveSpeed = 10
    jumpVelocity = 20
    pSqSize = 12
    bulletSpeed = 15
    maxAmmo = 100

    --World Settings
    gravityScale = 10
    frameTime = 16

    --UI Settings
    width = 780
    height = 600
    w = fromIntegral width :: GLdouble
    h = fromIntegral height :: GLdouble

    --Tile settings
    tileSize :: GLdouble
    tileSize = 32.0
    -- position of the paths in the list:
    border1, border2, border3, free1, free2, free3 :: Int
    border1 = 1
    border2 = 2
    border3 = 3
    free1   = 4
    free2   = 5
    free3   = 6

    main :: IO ()
    main = do
      let winConfig = ((100,80),(width,height),"PLC Project")

          bmpList  = [("tex.bmp",             Nothing),
                      ("border1.bmp",         Nothing),
                      ("border2.bmp",         Nothing),
                      ("border3.bmp",         Nothing),
                      ("free1.bmp",           Nothing),
                      ("free2.bmp",           Nothing),
                      ("free3.bmp",           Nothing)]

          gameMap  = multiMap [(tileMap map1 tileSize tileSize),
                               (tileMap map2 tileSize tileSize),
                               (tileMap map3 tileSize tileSize)] 0

          groups = [(objectGroup "playerGroup"      [createPlayer] ),
                    (objectGroup "bulletGroup"       createBullets ),
                    (objectGroup "invaderGroup"       initInvaders ),
                    (objectGroup "enemy1Group"     createEnemiesT1 ),
                    (objectGroup "enemy2Group"     createEnemiesT2 ),
                    (objectGroup "helperGroup"      [createHelper] )]

          initAttributes = GA (rows * ((columns*2)-1) ) maxAmmo 1.0 False enemy1Amount enemy2Amount

          input = [
            (SpecialKey KeyRight,   StillDown, movePlayerRight)
            ,(SpecialKey KeyLeft,    StillDown, movePlayerLeft)
            ,(SpecialKey KeyUp,              Press, playerJump)
            ,(SpecialKey KeyDown,            Press, playerFall)
            ,(Char 'z',                     Press, spawnBullet)
            ,(Char 'x',                     Press, spawnBullet)
            ,(Char 'q',                 Press, \_ _ -> funExit)
            ]
      
      sem1 <- newMVar False
      sem2 <- newMVar False
      threadKiller <- newMVar 0
      funInit winConfig gameMap groups (Level 1) initAttributes input (gameCycle sem1 sem2 threadKiller) (Timer frameTime) bmpList

    --Player Functions
    
    
    --Cria o player. Neste momento o player é apenas um quadrado vermelho
    createPlayer :: PlayerCharacter
    createPlayer = 
        let playerBounds = [(-pSqSize,-pSqSize),(pSqSize,-pSqSize),(pSqSize,pSqSize),(-pSqSize,pSqSize)] -- 'area' do quadrado
            playerPoly   = Basic (Polyg playerBounds 1.0 0.0 0.0 Filled) -- gera a forma do player
        in object "player" playerPoly False (w/2, pSqSize*10) (0,0) () -- inicializa o player com esta forma gerada

    --Player Collision

    --Detecta colisao entre o player e o chao e simula gravidade caso o player nao esteja no chao
    --também reseta a capacidade de pular quando atinge o chao
    playerDefaultCol :: PlayerAction ()
    playerDefaultCol = do
      player <- findObject "player" "playerGroup"
      (pX, pY) <- getObjectPosition player
      tile <- getTileFromWindowPosition (pX, pY - pSqSize)
      (vX, vY) <- getObjectSpeed player
      (GA e a t b et1 et2) <- getGameAttribute
      when(getTileBlocked tile) (do 
        setObjectPosition (pX, tileSize) player
        setObjectSpeed (vX, 0) player
        if(b)
         then(setGameAttribute(GA e a t False et1 et2))--Caso o jogador pouse no chão, significa que o pulo acabou. Atualizar o booleano
         else(return())
        )
      when(not (getTileBlocked tile))( (setObjectSpeed ( (0.0, vY-(gravityScale/10) ) ) player) )--Caso não esteja pisando no chão, simular gravidade. 
     
    --Detecta colisao com os inimigos e o player
    --caso um inimigo atinja o player, voltar para o nivel 1 e resetar tudo  
    playerEnemyCollision :: MVar Int -> PlayerAction() 
    playerEnemyCollision threadKiller = do
      player <- findObject "player" "playerGroup"
      
      enemies <- getObjectsFromGroup "enemy1Group"
      col1 <- objectListObjectCollision enemies player
      
      enemies <- getObjectsFromGroup "enemy2Group"
      col2 <- objectListObjectCollision enemies player
      when(col1 || col2) (do 
                killThreads threadKiller
                setGameState(Level 1)
                setNewLevel 1)

    checkPlayerCollisions :: MVar Int -> PlayerAction ()
    checkPlayerCollisions threadKiller = do
      playerEnemyCollision threadKiller
      playerDefaultCol


    --Player IO

    --Move o jogador para a direita.
    --Atualiza a travelDirection para 1. travelDirection serve para dar a velocidade necessária 
    --para a bala ir para o caminho correto
    movePlayerRight :: Modifiers -> Position -> IOGame GameAttribute () GameState TileAttribute ()
    movePlayerRight _ _ = do
     obj     <- findObject "player" "playerGroup"
     (pX,pY) <- getObjectPosition obj
     (sX,_)  <- getObjectSize obj
     (GA e a t b et1 et2) <- getGameAttribute
     setGameAttribute(GA e a 1.0 b et1 et2)--atualiza a travel direction para 1
     if (pX + (sX/2) + moveSpeed <= w)
      then (setObjectPosition ((pX + moveSpeed),pY) obj)
      else (setObjectPosition ((w - (sX/2)),pY) obj)

    --Move o jogador para a direita.
    --Atualiza a travelDirection para -1. travelDirection serve para dar a velocidade necessária 
    --para a bala ir para o caminho correto
    movePlayerLeft :: Modifiers -> Position -> IOGame GameAttribute () GameState TileAttribute ()
    movePlayerLeft _ _ = do
      obj <- findObject "player" "playerGroup"
      (pX,pY) <- getObjectPosition obj
      (sX,_)  <- getObjectSize obj
      (GA e a t b et1 et2) <- getGameAttribute
      setGameAttribute(GA e a (-1.0) b et1 et2)--atualiza a travel direction para -1
      if (pX - (sX/2) - moveSpeed >= 0)
       then (setObjectPosition ((pX - moveSpeed),pY) obj)
       else (setObjectPosition (sX/2,pY) obj)
    
    --Dá velocidade vertical ao jogador caso ele nao esteja pulando
    playerJump :: Modifiers -> Position -> IOGame GameAttribute () GameState TileAttribute ()
    playerJump _ _ = do
       player <- findObject "player" "playerGroup"
       (vX, vY) <- getObjectSpeed player
       (GA e a t b et1 et2) <- getGameAttribute
       if(not b)
        then (do 
          setObjectSpeed(vX, jumpVelocity) player
          setGameAttribute(GA e a t True et1 et2)--Booleano diz que o jogador está atualmente pulando
          )
        else return()

    --Faz o jogador cair rapidamente
    playerFall :: Modifiers -> Position -> IOGame GameAttribute () GameState TileAttribute ()
    playerFall _ _ = do
       player <- findObject "player" "playerGroup"
       (vX, vY) <- getObjectSpeed player
       (GA e a t b et1 et2) <- getGameAttribute
       if(b)
        then (do 
          setObjectSpeed(vX, -jumpVelocity) player
          )
        else return()

    --Player Ammo

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
      (GA e a t b et1 et2) <- getGameAttribute
      bullet <- findObject ("bullet" ++ (show a)) "bulletGroup" --Encontra a última bala da lista
      player <- findObject "player" "playerGroup" --Encontra o player
      (pX, pY) <- getObjectPosition player
      (sX, sY) <- getObjectSize player --Coleta dados do jogador
      setObjectAsleep False bullet --Spawna efetivamente a bala no mapa
      setObjectPosition (pX, pY) bullet --Passa os parametros necessários como posição e velocidade.
      setObjectSpeed (0,bulletSpeed) bullet
      setGameAttribute(GA e (a-1) t b et1 et2)

    checkBulletCollision :: PlayerBullet -> [SpaceInvader] -> PlayerAction ()
    checkBulletCollision bullet (x:xs) = do
      col <- objectsCollision bullet x
      if(col)
        then( do
              setObjectAsleep True x
              setObjectAsleep True bullet
              (GA e a t b et1 et2) <- getGameAttribute
              setGameAttribute(GA (e-1) a t b et1 et2) )
        else if(not(length xs == 0))
          then (checkBulletCollision bullet xs)
          else return()

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

    disableAllBullets :: [PlayerBullet] -> PlayerAction ()
    disableAllBullets [] = return ()
    disableAllBullets (x:xs) = do
      setObjectAsleep True x
      disableAllBullets xs         
                   
                   



    ---Space Invaders
    createInvaderAt :: (GLdouble, GLdouble) -> String -> SpaceInvader
    createInvaderAt (pX, pY) name =
        let invaderBounds = [(-pSqSize,-pSqSize),(pSqSize,-pSqSize),(pSqSize,pSqSize),(-pSqSize,pSqSize)] -- 'area' do quadrado
            invaderPoly   = Basic (Polyg invaderBounds 1.0 0.5 0.0 Filled) -- gera a forma do player
        in object name invaderPoly False (pX, pY) (invaderXSpeed, invaderYSpeed) () -- inicializa o player com esta forma gerada

    --Invader Helper
    createHelper :: SpaceInvader
    createHelper =
        let invaderBounds = [(-pSqSize,-pSqSize),(pSqSize,-pSqSize),(pSqSize,pSqSize),(-pSqSize,pSqSize)] -- 'area' do quadrado
            invaderPoly   = Basic (Polyg invaderBounds 1.0 1.0 1.0 Filled) -- gera a forma do player
        in object "helper" invaderPoly False (w/2, h+50) (invaderXSpeed, 0) () -- inicializa o player com esta forma gerada

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
    
    moveInvaders :: InvaderAction ()
    moveInvaders = do
       invader <- findObject "helper" "helperGroup"
       allInvaders <- getObjectsFromGroup "invaderGroup"
       (pX,pY) <- getObjectPosition invader
       if(pX == (w/2 + 100) )
        then (do 
               reverseInvadersSpeed allInvaders
               reverseXSpeed invader)
        else if (pX == w/2 - 100)
          then (do 
                 reverseInvadersSpeed allInvaders
                 reverseXSpeed invader)
        else return()

    resetInvadersPos :: [SpaceInvader] -> Int-> Int -> InvaderAction ()
    resetInvadersPos [] _ _ = return ()
    resetInvadersPos (i:xs) row column  
      | (row >= rows) = return()
      | (column >= columns) = resetInvadersPos (i:xs) (row+1) (-4)
      | otherwise = do 
        let offset = pSqSize*2 + 5
        let x = (w/2)+(offset * (fromIntegral column))
        let y = (h - 100)+((fromIntegral row) * offset)
        setObjectAsleep False i
        setObjectPosition (x,y) i
        resetInvadersPos xs (row) (column + 1) 
        
    resetHelperPos :: InvaderAction ()
    resetHelperPos = do
      helper <- findObject "helper" "helperGroup"
      setObjectPosition (w/2, h+50) helper 
        
    checkInvadersMapCol :: [SpaceInvader] -> InvaderAction ()
    checkInvadersMapCol [] = return()
    checkInvadersMapCol (x:xs) = do
      invaderPos <- getObjectPosition x
      tile <- getTileFromWindowPosition invaderPos
      if(getTileBlocked tile)
        then (do 
               setGameState (Level 1)
               setNewLevel 1)
        else checkInvadersMapCol xs

    checkInvadersBottomMapCol :: InvaderAction ()
    checkInvadersBottomMapCol = do
      invaders <- getObjectsFromGroup "invaderGroup"
      checkInvadersMapCol invaders


    --Enemies

    killThreads :: MVar Int -> PlayerAction()
    killThreads threadKiller = do
        state <- getGameState
        case state of
            Level n -> case n of
                       1 -> return()
                       2 -> liftIOtoIOGame $ modifyMVar_ threadKiller (\x -> return(x+2))
                       3 -> liftIOtoIOGame $ modifyMVar_ threadKiller (\x -> return(x+4))
    
    delayedStartup :: IO() -> IO()
    delayedStartup f = do
        threadDelay 500000
        f
    
    wakePeriodically :: MVar Bool -> MVar Int -> Double -> IO()
    wakePeriodically sem threadKiller avg = do
        modifyMVar_ sem (\b -> return(True))
        gen <- getStdGen
        (rand, newgen) <- return(randomR (-avg*2/3,avg/4) gen)
        setStdGen newgen
        threadDelay ((round (avg + rand) * 1000000))
        i <- readMVar threadKiller
        if(0<i)
        then (do
            modifyMVar_ threadKiller (\x -> return(x-1))
            modifyMVar_ sem (\b -> return(False))
            return()
            )
        else (do
            wakePeriodically sem threadKiller avg
            )

    
    --Enemy1
    createEnemyT1 :: String -> SpaceInvader
    createEnemyT1 name =
        let invaderBounds = [(-pSqSize,-pSqSize*1.5),(pSqSize*2.5,-pSqSize*1.5),(pSqSize*2.5,pSqSize),(-pSqSize,pSqSize)]
            invaderPoly   = Basic (Polyg invaderBounds 0.0 0.5 1.0 Filled)
        in object name invaderPoly True (w+10, (pSqSize+30)) (enemy1Speed,0) ()

    createAsleepEnemies1 :: Int -> [SpaceInvader]
    createAsleepEnemies1 amount
     | (amount >= enemy1Amount) = []
     | otherwise = do 
        let name = "enemy1" ++ show(amount)
        (createEnemyT1 (name)):createAsleepEnemies1(amount+1)    

    createEnemiesT1 :: [SpaceInvader]
    createEnemiesT1 = createAsleepEnemies1 0    

    spawnT1Enemy :: MVar Bool -> InvaderAction ()
    spawnT1Enemy sem = do
      (GA e a t b et1 et2) <- getGameAttribute
      b <- liftIOtoIOGame(readMVar sem)
      when(b && 0 < et1) (do 
        enemy <- findObject ("enemy1" ++ show(et1 - 1)) "enemy1Group"
        setObjectAsleep False enemy
        setGameAttribute (GA e a t b (et1 - 1) et2)
        liftIOtoIOGame (modifyMVar_ sem (\b -> return (False)))
        )

    resetT1Enemies :: [SpaceInvader] -> InvaderAction ()
    resetT1Enemies [] = return ()
    resetT1Enemies (x:xs) = do
      setObjectPosition ((w+10, (pSqSize+30))) x  
      setObjectAsleep True x
      resetT1Enemies xs
        
    

    --Enemy2
    createEnemyT2 :: String -> SpaceInvader
    createEnemyT2 name =
        let invaderBounds = [(-pSqSize,-pSqSize),(pSqSize*2,-pSqSize),(pSqSize*2,pSqSize*3.5),(-pSqSize,pSqSize*3.5)]
            invaderPoly   = Basic (Polyg invaderBounds 1.0 0.0 1.0 Filled)
        in object name invaderPoly True (-10, (pSqSize+30)) (enemy2Speed,0) ()

    createAsleepEnemies2 :: Int -> [SpaceInvader]
    createAsleepEnemies2 amount
     | (amount >= enemy2Amount) = []
     | otherwise = do 
        let name = "enemy2" ++ show(amount)
        (createEnemyT2 (name)):createAsleepEnemies2(amount+1)    

    createEnemiesT2 :: [SpaceInvader]
    createEnemiesT2 = createAsleepEnemies2 0   
    
    spawnT2Enemy :: MVar Bool -> InvaderAction ()
    spawnT2Enemy sem = do
      (GA e a t b et1 et2) <- getGameAttribute
      b <- liftIOtoIOGame(readMVar sem)
      when(b && 0 < et2) (do 
        enemy <- findObject ("enemy2" ++ show(et2 - 1)) "enemy2Group"
        setObjectAsleep False enemy
        setGameAttribute (GA e a t b et1 (et2 - 1))
        liftIOtoIOGame (modifyMVar_ sem (\b -> return (False)))
        )

    resetT2Enemies :: [SpaceInvader] -> InvaderAction ()
    resetT2Enemies [] = return ()
    resetT2Enemies (x:xs) = do
      setObjectPosition (-10, (pSqSize+30)) x  
      setObjectAsleep True x
      resetT2Enemies xs


    --Level
    setNewLevel :: Int -> PlayerAction ()
    setNewLevel n = do
      if(n >= 1 && n <= 3)
        then(do
             resetLevelAttributes

             invaders <- getObjectsFromGroup "invaderGroup"
             resetInvadersPos invaders 0 (-4)

             t1Enemies <- getObjectsFromGroup "enemy1Group"
             resetT1Enemies t1Enemies

             t2Enemies <- getObjectsFromGroup "enemy2Group"
             resetT2Enemies t2Enemies

             bullets <- getObjectsFromGroup "bulletGroup"
             disableAllBullets bullets

             resetHelperPos

             setCurrentMapIndex (n-1))
             
        else return()     

    resetLevelAttributes :: PlayerAction()
    resetLevelAttributes = do
       (GA e a t b et1 et2) <- getGameAttribute
       setGameAttribute(GA 36 maxAmmo t b enemy1Amount enemy2Amount)

    -- Game Cycle    
    gameCycle :: MVar Bool -> MVar Bool -> MVar Int -> PlayerAction ()
    gameCycle sem1 sem2 threadKiller = do 
      --Gets
      bullets <- getObjectsFromGroup "bulletGroup"
      gState <- getGameState
      (GA e a t b et1 et2) <- getGameAttribute

      --Actions
      spawnT1Enemy sem1
      spawnT2Enemy sem2
      moveInvaders
      
      --Collisions
      checkAllBulletsCollision bullets
      checkPlayerCollisions threadKiller
      checkInvadersBottomMapCol

      --Advance Level
      if(e <= 0)
        then(do         
          case gState of
            Level n -> case n of
                       1 -> (do 
                            setGameState (Level 2)
                            setNewLevel 2
                            liftIOtoIOGame (forkIO (wakePeriodically sem1 threadKiller enemy1Delay))
                            liftIOtoIOGame (forkIO (wakePeriodically sem2 threadKiller enemy1Delay))
                            return())
                       2 -> (do 
                            setGameState (Level 3)
                            setNewLevel 3
                            liftIOtoIOGame (forkIO (wakePeriodically sem1 threadKiller enemy1Delay))
                            liftIOtoIOGame (forkIO (wakePeriodically sem2 threadKiller enemy1Delay))
                            return())
                       3 -> (do 
                            killThreads threadKiller
                            e1 <- getObjectsFromGroup "enemy1Group"
                            e2 <- getObjectsFromGroup "enemy2Group"
                            resetT1Enemies e1
                            resetT2Enemies e2))
        else return()

      --UI
      showFPS TimesRoman24 (w-24, h-28) 1.0 0.0 0.0
      printOnScreen ("Ammo Remaining: " ++ show a) TimesRoman24 (0,0) 1.0 1.0 1.0
      printOnScreen ("Enemies Remaining: " ++ show e) TimesRoman24 (0,h-25) 1.0 1.0 1.0
      



    b,f,b2,g2,b3,g3 :: GameTile
    b = (border1, True,  0.0, NoTileAttribute)
    f = (free1,   False, 0.0, NoTileAttribute)
    b2 = (border2, True,  0.0, NoTileAttribute)
    g2 = (free2, False,  0.0, NoTileAttribute)
    b3 = (border3, True,  0.0, NoTileAttribute)
    g3 = (free3, False,  0.0, NoTileAttribute)
      

    map1 :: PLCGameMap
    map1 = [[f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
            [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
            [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
            [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
            [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
            [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
            [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
            [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
            [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
            [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
            [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
            [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
            [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
            [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
            [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
            [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
            [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
            [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
            [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
            [b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b]]

    map2 :: PLCGameMap
    map2 = [[g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
            [g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
            [g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
            [g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
            [g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
            [g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
            [g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
            [g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
            [g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
            [g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
            [g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
            [g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
            [g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
            [g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
            [g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
            [g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
            [g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
            [g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
            [g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2,g2],
            [b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2]]

    map3 :: PLCGameMap
    map3 = [[g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
            [g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
            [g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
            [g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
            [g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
            [g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
            [g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
            [g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
            [g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
            [g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
            [g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
            [g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
            [g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
            [g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
            [g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
            [g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
            [g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
            [g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
            [g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3,g3],
            [b3,b3,b3,b3,b3,b3,b3,b3,b3,b3,b3,b3,b3,b3,b3,b3,b3,b3,b3,b3,b3,b3,b3,b3,b3,b3]]      