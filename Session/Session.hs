{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
module Session.Session ( module Session.Session
                       , module Session.Poll ) where

import Data.Maybe
import Control.Concurrent
import Control.Monad (forever)
import qualified Network.Socket as NS
import System.IO
import System.Exit(die)
import Data.IP
import qualified Data.Map.Strict as Data.Map
import System.IO.Error
import GHC.IO.Exception(ioe_description)
import Foreign.C.Error
import Session.Poll(fdWaitOnQEmpty,waitOnQEmpty)


type App = ((NS.Socket,NS.SockAddr) -> IO ())
type RaceCheck = ((IPv4,IPv4) -> IO Bool)
type RaceCheckUnblock = ((IPv4,IPv4) -> IO ())
data State = State { port :: NS.PortNumber
                   , raceCheckBlock :: RaceCheck
                   , raceCheckNonBlock :: RaceCheck
                   , raceCheckUnblock :: RaceCheckUnblock
                   , defaultApp :: App
                   , peers :: [(IPv4,IPv4)]
                   , listenAddress :: IPv4
                   }

logger :: String -> IO ()
logger = putStrLn
-- logger _ = return ()

debug :: String -> IO ()
debug _ = return ()
--debug = putStrLn

seconds :: Int
seconds = 1000000

respawnDelay :: Int
respawnDelay = 10 * seconds

--session :: NS.PortNumber -> App -> [IPv4] -> Bool -> IO ()
--session port defaultApp peers enableInbound = do
session :: NS.PortNumber -> App -> IPv4 -> [(IPv4,IPv4)] -> Bool -> IO ()
session port defaultApp localIPv4 peers enableInbound = do
    state <- mkState port defaultApp peers
    mapM_ ( forkIO . run state ) peers
    if enableInbound then
        listener state
    else do
        putStrLn "session: inbound connections not enabled"
        forever (threadDelay $ 10^12)
    where

    mkState port defaultApp peers = do
        mapMVar <- newMVar Data.Map.empty
        let raceCheckNonBlock = raceCheck False mapMVar
            raceCheckBlock = raceCheck True mapMVar
            raceCheckUnblock = raceCheckUnblocker mapMVar
            listenAddress = localIPv4
        return State {..}

    -- RACE CHECK
    -- before calling the application perform a race check
    -- if there is a race then don't call the application
    -- optionally, block waiting for the other session to complete
    -- the race check has three entry points - blocking and non block request, and unblock.
    -- the race check uses a map stored in MVar, and another MVar to support the blocking request
    -- the blocked request returns an error condition even when unblocked, to prevent an
    -- overeager talker from sharing the limelight too easily

    --raceCheckUnblocker :: MVar (Data.Map.Map (IPv4,IPv4) (MVar ())) -> IPv4 -> IO ()
    raceCheckUnblocker mapMVar address = do
        map <- readMVar mapMVar
        let Just peerMVar = Data.Map.lookup address map
        putMVar peerMVar ()

    --raceCheck :: Bool -> MVar (Data.Map.Map (IPv4,IPv4) (MVar ())) -> IPv4 -> IO Bool
    raceCheck blocking mapMVar address = do
    -- get the specific MVar out of the Map
    -- if it doesn't exist then insert it and take it
    -- this is non-blocking so if it does exist but is empty then just exit 
        -- threadId <- myThreadId
        map <- takeMVar mapMVar
        let maybePeerMVar = Data.Map.lookup address map
        maybe (do peerMVar <- newEmptyMVar :: IO (MVar ())
                  putMVar mapMVar (Data.Map.insert address peerMVar map)
                  return True )
              (\peerMVar -> do
                  putMVar mapMVar map
                  maybeFree <- tryTakeMVar peerMVar
                  if isJust maybeFree
                  then return True
                  else if not blocking
                  then return False else
                      do
                      _ <- readMVar peerMVar
                      return False
              )
              maybePeerMVar

listener :: State -> IO ()
listener state@State{..} = do
    eSock <- tryIOError ( bindSock' port (toHostAddress listenAddress) )
    either
        ( \e -> do Errno errno <- getErrno
                   if | errno == 13 -> die "permission error binding port (are you su?)"
                      | errno `elem` [98] -> do hPutStrLn stderr "waiting to bind port"
                                                threadDelay (10 * seconds)
                                                listener state
                      | otherwise -> error $ errReport' errno e )
        ( \(listeningSocket,_) -> forever ( do s <- NS.accept listeningSocket
                                               forkIO $ listenClient s ))
        eSock
    where
    
    bindSock' port ip = do
        sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
        NS.setSocketOption sock NS.ReuseAddr 1
        NS.setSocketOption sock NS.NoDelay 1
        NS.bind sock (NS.SockAddrInet port ip)
        NS.listen sock 100
        return ( sock , addr )

    
    listenClient (sock, NS.SockAddrInet _ remoteHostAddress) = do
            ( NS.SockAddrInet _ localHostAddress ) <- NS.getSocketName sock
            let addressPair = ( fromHostAddress localHostAddress ,fromHostAddress remoteHostAddress)
            logger $ "listener - connect request (src/dst): " ++ show addressPair
            unblocked <- raceCheckNonBlock addressPair
            if unblocked then do
                wrap state defaultApp sock 
                raceCheckUnblock addressPair
            else do
                logger "listener - connect reject due to race"
                NS.close sock
    
wrap :: State -> ((NS.Socket, NS.SockAddr) -> IO a) -> NS.Socket -> IO ()
wrap State{..} app sock = do
    peerAddress <- NS.getPeerName sock
    let ip = fromPeerAddress peerAddress
        fromPeerAddress (NS.SockAddrInet _ ip) = fromHostAddress ip
    catchIOError
        ( do logger $ "connected to : " ++ show ip
             _ <- app ( sock , peerAddress )
             NS.close sock
             logger $ "app terminated for : " ++ show ip )
        (\e -> do Errno errno <- getErrno
                  logger $ "Exception in session with " ++ show ip ++ " - " ++ errReport errno e )

run :: State -> (IPv4,IPv4) -> IO ()
run state@State{..} (src,dst) = do
    debug $ "run: " ++ show (src,dst) ++ " start"
    unblocked <- raceCheckBlock (src,dst)
    debug $ "run: " ++ show (src,dst) ++ " checked"
    if unblocked then
         do
            debug $ "run: " ++ show (src,dst) ++ " unblocked"
            sock <- connectTo port (src,dst)
            debug $ "run: " ++ show (src,dst) ++ " connected"
            maybe ( return () )
                  (wrap state defaultApp)
                  sock
            raceCheckUnblock (src,dst)
    else logger $ "run blocked for " ++ show (src,dst)
    threadDelay respawnDelay
    run state (src,dst)
    where
    connectTo port (src,dst) =
        catchIOError
        ( do sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
             NS.setSocketOption sock NS.NoDelay 1
             NS.bind sock (NS.SockAddrInet NS.defaultPort $ toHostAddress src)
             NS.connect sock $ NS.SockAddrInet port $ toHostAddress dst
             return $ Just sock )

        (\e -> do
            Errno errno <- getErrno
            -- most errors are timeouts or connection rejections from unattended ports
            -- a better way to report would be handy - repeated console messages are not useful!
            logger $ "Exception connecting to " ++ show dst ++ " - " ++ errReport errno e
            return Nothing )

errReport errno e | errno `elem` [2,32,107,115] = ioe_description e ++ " (" ++ show errno ++ ")"
                  | otherwise = errReport' errno e

errReport' errno e = unlines
    [ "*** UNKNOWN exception, please record this"
    -- , ioeGetErrorString e
    , "error " ++ ioeGetErrorString e
    , "errno " ++ show errno
    , "description " ++ ioe_description e
    ]
