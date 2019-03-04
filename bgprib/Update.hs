{-# LANGUAGE RecordWildCards #-}
module Update(encodeUpdates,processUpdate,getUpdate,ungetUpdate,ParsedUpdate(..),makeUpdate,makeUpdateSimple,igpUpdate,originateWithdraw,originateUpdate,myHash) where
import qualified Data.ByteString.Lazy as L
import Data.Int
import Data.Binary
import Data.Either

import Common
import BGPlib
import FarmHash(hash64)

-- 'hash' will become 'routeId' when it is inserted into the RouteData record....
myHash :: L.ByteString -> Int
myHash = fromIntegral . hash64 . L.toStrict

data ParsedUpdate = ParsedUpdate { puPathAttributes :: [PathAttribute], nlri :: [Prefix], withdrawn :: [Prefix], hash :: Int } deriving Show

parseUpdate a n w = (decodedAttributes,decodedNlri,decodedWithdrawn)
    where
        decodedAttributes = decodeOrFail a :: Either (L.ByteString, Int64, String) (L.ByteString, Int64, [PathAttribute])
        decodedNlri = decodeOrFail n :: Either (L.ByteString, Int64, String) (L.ByteString, Int64, [Prefix])
        decodedWithdrawn = decodeOrFail w :: Either (L.ByteString, Int64, String) (L.ByteString, Int64, [Prefix])

parseSuccess (a,n,w) = isRight a && isRight n && isRight w
parseErrorMesgs (a,n,w) = concat [getMsgA a,getMsgP n,getMsgP w]
    where getMsgP (Right _) = ""
          getMsgP (Left(_,_,s)) = s
          getMsgA (Right _) = ""
          getMsgA (Left(_,_,s)) = s
validResult (a,n,w) = (f a,f n, f w) where f (Right (_, _, x)) = x

diagoseResult (a',n',w') (a,n,w) = diagnose "attributes" a' a ++
                                   diagnose "NLRI" n' n ++
                                   diagnose "withdrawn" w' w where
    diagnose _ (Right _) _ = ""
    diagnose t (Left (_,n,_)) x = "Error parsing " ++ t ++ " at position " ++ show n ++ "\n" ++ toHex' x

encodeUpdates :: [ParsedUpdate] -> [BGPMessage]
encodeUpdates = map ungetUpdate

-- TODO rename getUpdate/ungetUpdate encodeUpdate/decodeUpdate
ungetUpdate :: ParsedUpdate -> BGPMessage
ungetUpdate ParsedUpdate{..} = BGPUpdate { withdrawn = encode withdrawn , attributes = encode puPathAttributes , nlri = encode nlri } 

getUpdate :: BGPMessage -> ParsedUpdate
getUpdate BGPUpdate{..} = ParsedUpdate { puPathAttributes = a , nlri = n , withdrawn = w,
                                        hash = myHash attributes  }
                               where (a,n,w) = validResult $ parseUpdate attributes nlri withdrawn

-- TODO clean up the mess here around error handling.....
processUpdate :: BGPMessage -> Maybe ParsedUpdate
processUpdate ( BGPUpdate w a n ) = 
    let parsedResult = parseUpdate a n w
        (puPathAttributes,nlri,withdrawn) = validResult parsedResult
        hash = myHash a
    in
    if parseSuccess parsedResult then Just (ParsedUpdate puPathAttributes nlri withdrawn hash)
    -- for 'production' use this should be 'Nothing', in which case the session will be dropped....
    -- a better solution (**TODO**) would be to change the retrun type to 'either' and log the text later....
    -- else Nothing
    else error $
        "parsing failed: " ++
        parseErrorMesgs parsedResult ++
        diagoseResult parsedResult (a,n,w)
{- informative error message is:
        "parsing failed: "
        parseErrorMesgs parsedResult
        diagoseResult parsedResult (a,n,w)
-}

originateWithdraw prefixes = ParsedUpdate []  [] prefixes 0

originateUpdate :: Word8 -> [ASSegment Word32] -> IPv4 -> [Prefix] -> ParsedUpdate
originateUpdate origin path nextHop prefixes = ParsedUpdate attributes prefixes [] hash where
    attributes = [PathAttributeOrigin origin, PathAttributeASPath (ASPath4 path), PathAttributeNextHop nextHop]
    hash = myHash $ encode attributes

makeUpdateSimple :: [PathAttribute] -> [Prefix] -> [Prefix] -> ParsedUpdate
makeUpdateSimple p n w  = head $ makeUpdate n w p

makeUpdate :: [Prefix] -> [Prefix] -> [PathAttribute] -> [ParsedUpdate]
makeUpdate = makeSegmentedUpdate
makeUpdate' nlri withdrawn attributes = ParsedUpdate attributes nlri withdrawn ( myHash $ encode attributes)

makeSegmentedUpdate :: [Prefix] -> [Prefix] -> [PathAttribute] -> [ParsedUpdate]
makeSegmentedUpdate nlri withdrawn attributes = result where
                                                    withdrawnRoutesLength = L.length (encode withdrawn)
                                                    pathAttributesLength = L.length (encode attributes)
                                                    nlriLength = L.length (encode nlri)
                                                    nonPrefixLength = 16 + 2 + 1 + 2 + 2 + pathAttributesLength
                                                    nominalLength = nonPrefixLength + withdrawnRoutesLength + nlriLength
                                                    availablePrefixSpace = fromIntegral (4096 - nonPrefixLength) :: Int64
                                                    chunkedNlri = chunkPrefixes availablePrefixSpace nlri
                                                    chunkedWithdrawn = chunkPrefixes availablePrefixSpace withdrawn
                                                    updates = map (\pfxs -> makeUpdate' pfxs [] attributes) (tail chunkedNlri)
                                                    withdraws = map (\pfxs -> makeUpdate' [] pfxs attributes) (tail chunkedWithdrawn)
                                                    result = if availablePrefixSpace >= L.length (encode (head chunkedNlri))
                                                                                        + L.length (encode (head chunkedWithdrawn))
                                                             then [makeUpdate' (head chunkedNlri) (head chunkedWithdrawn) attributes] ++ withdraws ++ updates
                                                             else [makeUpdate' [] (head chunkedWithdrawn) attributes,
                                                                   makeUpdate' (head chunkedNlri) [] attributes] ++ withdraws ++ updates


igpUpdate = originateUpdate _BGP_ORIGIN_IGP []
