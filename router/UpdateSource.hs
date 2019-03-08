{-# LANGUAGE OverloadedStrings #-}
module UpdateSource where
import Data.IP
import Data.Word
import Control.Concurrent
import Data.Bits
import Control.Monad

import BGPlib
import BGPRib hiding ( group )

type UpdateSource = IO [ParsedUpdate]
{-
initSource :: PeerData -> AddrRange IPv4 -> Int -> Int -> Int -> Int -> IO UpdateSource
initSource peer startPrefix tableSize groupSize burstSize repeatCount = do
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
             return $ buildUpdate peer as [prefix]
    return (f mv)

--buildPrefixes initialAddress prefixLength groupSize burstSize =
--    map (\ip -> fromAddrRange $ makeAddrRange ( ip4 ( (ip4' ip) + fromIntegral n * fromIntegral increment)) prefixLength ips
--    where
--    ips = [ initialAddress + j * increment | j <- 0 .. burstSize

buildUpdate peer as prefixes = let
    aspath = if isExternal peer then [as] ++ [myAS $ globalData peer] else [as]
    localPrefOrMED = if isExternal peer then PathAttributeLocalPref 0 else PathAttributeMultiExitDisc 0
    nextHop = PathAttributeNextHop (localIPv4 peer)
    in
    makeUpdate prefixes
               []
               [ PathAttributeOrigin _BGP_ORIGIN_IGP , PathAttributeASPath $ ASPath4 [ASSequence aspath] , nextHop , localPrefOrMED ]
-}



--initSource :: AddrRange IPv4 -> Word32 -> Word32 -> IO (IO (Word32, Word32, [ParsedUpdate]))
initSource :: PeerData -> AddrRange IPv4 -> Word32 -> Word32 -> Word32 -> IO UpdateSource
initSource peer startPrefix tableSize groupSize burstSize = do
    mv <- newMVar  0 -- (0 :: Word32)
    print $ addrRangePair startPrefix
    let f mv = do
             n <- takeMVar mv
             putMVar mv $ n + 1
             return $ concatMap (updates peer startPrefix tableSize groupSize) (map (n * burstSize +) [0..burstSize-1])
    return (f mv)


prefixes :: AddrRange IPv4 -> Word32 -> Word32 -> Word32 -> [AddrRange IPv4]
prefixes startPrefix tableSize groupSize n = group startPrefix groupSize $ n `mod` tableSize

group startPrefix groupSize index = map ip4 $ seeds (ip4' ip) groupSize index
    where
    (ip,prefixLength) = addrRangePair startPrefix
    ip4  =  (flip makeAddrRange) prefixLength . fromHostAddress . byteSwap32 . (flip shiftL) (32-prefixLength)
    ip4' = (flip shiftR) (32-prefixLength) . byteSwap32 . toHostAddress
    seeds base groupSize index = map (base + index * groupSize +) [0..groupSize-1]

main = do
    s <- initSource dummyPeerData "192.168.0.0/24" 8 2 4
    replicateM 10 s >>= print


updates :: PeerData -> AddrRange IPv4 -> Word32 -> Word32 -> Word32 -> [ParsedUpdate]
updates peer startPrefix tableSize groupSize n = buildUpdate peer 
                                                                (n `mod` tableSize)
                                                                (n `div` tableSize)
                                                                ( group startPrefix groupSize $ n `mod` tableSize)
    where

--buildUpdate :: PeerData -> Word32 -> [Prefix] -> [ParsedUpdate]
    buildUpdate :: PeerData -> Word32 -> Word32 -> [AddrRange IPv4] -> [ParsedUpdate]
    buildUpdate peer as1 as2 prefixes = let
        aspath = if isExternal peer then [as1,as2] ++ [myAS $ globalData peer] else [as1,as2]
        localPrefOrMED = if isExternal peer then PathAttributeLocalPref 0 else PathAttributeMultiExitDisc 0
        nextHop = PathAttributeNextHop (localIPv4 peer)
        in
        makeUpdate ( map fromAddrRange prefixes )
                   []
                   [ PathAttributeOrigin _BGP_ORIGIN_IGP , PathAttributeASPath $ ASPath4 [ASSequence aspath] , nextHop , localPrefOrMED ]
