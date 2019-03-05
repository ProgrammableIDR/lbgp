{-# LANGUAGE RecordWildCards #-}
module Redistributor where
import Control.Concurrent
import qualified System.IO.Streams as Streams
import Control.Monad(void)

import BGPReader(pathReadRib)
import BGPRib
import BGPlib
import Route
import Global
import Config
import ZServ

redistribute :: Global -> IO ()
redistribute global@Global{..} = do
    insertTestRoutes global (configTestRoutePath config) (configTestRouteCount config)
    if not (configEnableDataPlane config )
    then putStrLn "configEnableDataPlane not set, not starting zserv API"
    else do threadId <- myThreadId
            putStrLn $ "Thread " ++ show threadId ++ " starting redistributor"
            ( zStreamIn, zStreamOut ) <- getZServerStreamUnix "/var/run/quagga/zserv.api"
            zservRegister zStreamOut _ZEBRA_ROUTE_BGP
            if configEnableRedistribution config
            then void $ forkIO (zservReader global (localPeer gd) ( zStreamIn, zStreamOut ))
            else putStrLn "configEnableRedistribution not enabled - not staring Zserv listener"

            let routeInstall (route, nextHop) = do
                    putStrLn $ "install " ++ show route ++ " via " ++ show nextHop
                    addRoute zStreamOut (toAddrRange $ toPrefix route) nextHop
                routeDelete route = do
                    putStrLn $ "delete " ++ show route
                    delRoute zStreamOut (toAddrRange $ toPrefix route)

            ribUpdateListener (routeInstall,routeDelete) global ( localPeer gd ) 1


ribUpdateListener (routeInstall,routeDelete) global@Global{..} peer timeout = do
    updates <- msgTimeout timeout (pullAllUpdates peer rib)
    if null updates then
        yield -- null op - could check if exit from thread is needed...
    else do 
        putStrLn $ show (length updates) ++ " updates for " ++ show peer
        putStr "Ready to install routes"
        let (update,withdraw) = foldl disc ([],[]) updates
            disc (u,w) (pfxs,0) = (u,w++pfxs) -- withdraw has 0 for the route index
            disc (u,w) (pfxs,ri) = ((pfxs,ri):u,w) -- alternate case is an update, not withdraw , the routeIndex is preserved for the route lookup
        routes <- routesFromAdjRibEntrys rib update
        mapM_ routeDelete withdraw
        mapM_ routeInstall routes

    -- rinse and repeat...

    ribUpdateListener (routeInstall,routeDelete) global peer timeout


zservReader global@Global{..} peer ( zStreamIn, zStreamOut ) = do
    zservRequestRouterId zStreamOut
    zservRequestInterface zStreamOut
    zservRequestRedistributeAll zStreamOut
    loop zStreamIn
    where
    loop stream = do
        msg <- Streams.read stream
        maybe (putStrLn "end of messages")
              ( \zMsg -> do 
                              -- print zMsg
                              maybe (putStrLn "--")
                                    -- (\s -> putStrLn $ "local route:" ++ show s)
                                    (\(pfx,maybeNH) -> maybe (do putStrLn $ "delete route: " ++ show pfx
                                                                 delRouteRib rib peer pfx )
                                                             (\nh -> do putStrLn $ "add route: " ++ show pfx ++ " via " ++ show nh
                                                                        addRouteRib rib peer pfx nh)
                                                             maybeNH

                                    )
                                    ( getZRoute zMsg )
                              -- let route = getZRoute zMsg
                              -- print route
                              loop stream )
              msg


insertTestRoutes _ "" _ = putStrLn "no test route data specified"
insertTestRoutes Global{..} path count = do
    putStrLn $ "test route set requested: " ++ path
    updates <- pathReadRib path
    let count' = if count == 0 then length updates else count
    putStrLn $ "inserting " ++ show count' ++ " routes"
    let updates' = concatMap (\((_,pas),pfxs) -> makeUpdate pfxs [] pas) updates
        updates'' = if 0 == count then updates' else take count updates'
    mapM (ribUpdater rib ( localPeer gd )) updates''
    putStrLn "done"
