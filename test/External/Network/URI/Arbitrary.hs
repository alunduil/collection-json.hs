{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : External.Network.URI.Arbitrary
Description : Arbitrary Instances for Network.URI
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Arbitrary instances for "Network.URI".
-}
module External.Network.URI.Arbitrary () where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (replicateM)
import Data.List (intercalate)
import Network.URI (URI (..), URIAuth (..))
import Test.QuickCheck (Arbitrary (arbitrary, shrink), choose, elements, Gen, listOf, listOf1, oneof, suchThat)

instance Arbitrary URI where
  arbitrary =
    do uriScheme    <- scheme
       uriAuthority <- arbitrary :: Gen (Maybe URIAuth)
       uriPath      <- path (null uriScheme) $ maybe True emptyAuthority uriAuthority
       uriQuery     <- oneof [query, return ""]
       uriFragment  <- oneof [fragment, return ""]

       return URI {..}
    where emptyAuthority URIAuth{..} = all null [uriUserInfo, uriRegName, uriPort]

  shrink URI{..} = [ URI uriScheme' uriAuthority' uriPath' uriQuery' uriFragment' | (uriScheme', uriAuthority', uriPath', uriQuery', uriFragment') <- shrink (uriScheme, uriAuthority, uriPath, uriQuery, uriFragment) ]

instance Arbitrary URIAuth where
  arbitrary = URIAuth <$> userinfo
                      <*> host `suchThat` (not . null)
                      <*> port

  shrink URIAuth{..} = [ URIAuth uriUserInfo' uriRegName' uriPort' | (uriUserInfo', uriRegName', uriPort') <- shrink (uriUserInfo, uriRegName, uriPort) ]

-- * RFC 3986 Generators
--
--   Some generators are handled by the 'Arbitrary' instances above, and others
--   are folded into symbols that are preceeded or followed by identifying
--   tokens.

scheme :: Gen String
scheme =
  do a <- alpha
     r <- listOf $ oneof [alpha, digit, elements ['+', '-', '.']]
     return $ a : (r ++ ":")

userinfo :: Gen String
userinfo =
  do u <- concat <$> userinfo'
     if null u
        then return ""
        else return $ u ++ "@"
  where userinfo' = listOf $ oneof [ replicateM 1 $ oneof [unreserved, subDelims, return ':']
                                   , percentEncoded
                                   ]

host :: Gen String
host = oneof [ ipLiteral
             , ipv4Address
             , regName
             ]

port :: Gen String
port =
  do p <- listOf digit
     if null p
        then return ""
        else return $ ':':p

ipLiteral :: Gen String
ipLiteral =
  do x <- oneof [ ipv6Address
                --, ipvFuture
                ]
     return $ "[" ++ x ++ "]"

{- TODO Check that "Network.URI" implements this correctly.
ipvFuture :: Gen String
ipvFuture =
  do h <- hexdig
     o <- oneof [ unreserved, subDelims, return ':' ]
     return ['v', h, '.', o]
-}

ipv6Address :: Gen String
ipv6Address = concat <$> oneof [ sequence                        [b 6, ls32]
                               , sequence           [return "::", b 5, ls32]
                               , sequence      [h16, return "::", b 4, ls32]
                               , sequence [b 1, h16, return "::", b 3, ls32]
                               , sequence [b 2, h16, return "::", b 2, ls32]
                               , sequence [b 3, h16, return "::", b 1, ls32]
                               , sequence [b 4, h16, return "::",      ls32]
                               , sequence [b 5, h16, return "::",       h16]
                               , sequence [b 6, h16, return "::"]
                               ]
  where b n = fmap concat $ replicateM n $ fmap (++ ":") h16 :: Gen String

h16 :: Gen String
h16 = replicateM 4 hexdig

ls32 :: Gen String
ls32 = oneof [ intercalate ":" <$> replicateM 2 h16
             , ipv4Address
             ]

ipv4Address :: Gen String
ipv4Address = intercalate "." <$> replicateM 4 decOctet

decOctet :: Gen String
decOctet = (show :: Int -> String) <$> choose (0, 255)

regName :: Gen String
regName = fmap concat $ listOf $ oneof [ replicateM 1 unreserved
                                       , percentEncoded
                                       , replicateM 1 subDelims
                                       ]

path :: Bool -> Bool -> Gen String
path emptyScheme emptyURIAuth = if emptyURIAuth
                                   then oneof [ pathAbsolute
                                              , if emptyScheme then pathNoScheme else pathRootless
                                              , return ""
                                              ]
                                   else pathAbEmpty

pathAbEmpty :: Gen String
pathAbEmpty = concat <$> listOf ((('/':) . concat) <$> listOf pchar)

pathAbsolute :: Gen String
pathAbsolute = ('/':) <$> oneof [return "", pathRootless]

pathNoScheme :: Gen String
pathNoScheme = concat <$> sequence [segment1nc, pathAbEmpty]

pathRootless :: Gen String
pathRootless = concat <$> sequence [ concat <$> listOf1 pchar
                                   , pathAbEmpty
                                   ]

segment1nc :: Gen String
segment1nc = oneof [ replicateM 1 unreserved
                   , percentEncoded
                   , replicateM 1 subDelims
                   , replicateM 1 $ return '@'
                   ] 

pchar :: Gen String
pchar = oneof [ replicateM 1 unreserved
              , percentEncoded
              , replicateM 1 subDelims
              , replicateM 1 $ return ':'
              , replicateM 1 $ return '@'
              ]

query :: Gen String
query = fmap (('?':) . concat) $ listOf $ oneof [ pchar
                                                , return "/"
                                                , return "?"
                                                ]

fragment :: Gen String
fragment = fmap (('#':) . concat) $ listOf $ oneof [ pchar
                                                   , return "/"
                                                   , return "?"
                                                   ]

percentEncoded :: Gen String
percentEncoded = ('%':) <$> replicateM 2 hexdig

unreserved :: Gen Char
unreserved = oneof [ alpha, digit, elements ['-', '.', '_', '~']]

subDelims :: Gen Char
subDelims = elements ['!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=']

-- * RFC 2234 Generators

alpha :: Gen Char
alpha = elements $ ['a'..'z'] ++ ['A'..'Z']

digit :: Gen Char
digit = elements ['0'..'9']

hexdig :: Gen Char
hexdig = oneof [digit, elements ['A'..'F']]
