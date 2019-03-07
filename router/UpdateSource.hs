{-# LANGUAGE OverloadedStrings #-}
module UpdateSource where
import Data.IP
import Data.Word
import Control.Concurrent
import BGPlib
import BGPRib

type UpdateSource = IO [ParsedUpdate]
initSource :: PeerData -> AddrRange IPv4 -> Int -> IO UpdateSource
initSource peer startPrefix tableSize = do
    mv <- newMVar ((0,1) :: (Int,Word32))
    print $ addrRangePair startPrefix
    let (ip,l) = addrRangePair startPrefix
        increment = 2 ^ (32 - l)
        f mv = do
             (n,as) <- takeMVar mv
             putMVar mv $ if n == tableSize then (0,as+1) else (n + 1,as)
             let ip4  = fromHostAddress . byteSwap32
                 ip4' = byteSwap32 . toHostAddress
                 prefix = fromAddrRange $ makeAddrRange ( ip4 ( (ip4' ip) + fromIntegral n * fromIntegral increment)) l
                 aspath = if isExternal peer then [as] ++ [myAS $ globalData peer] else [as]
                 localPrefOrMED = if isExternal peer then PathAttributeLocalPref 0 else PathAttributeMultiExitDisc 0
                 nextHop = PathAttributeNextHop (localIPv4 peer)
                 update = makeUpdate [prefix]
                                     []
                                     [ PathAttributeOrigin _BGP_ORIGIN_IGP , PathAttributeASPath $ ASPath4 [ASSequence aspath] , nextHop , localPrefOrMED ]
             return update
    return (f mv)
