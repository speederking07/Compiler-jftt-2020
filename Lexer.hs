{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
{-# LINE 1 "Lexer.x" #-}

module Lexer (lexer, Token(..)) where

#if __GLASGOW_HASKELL__ >= 603
#include "ghcconfig.h"
#elif defined(__GLASGOW_HASKELL__)
#include "config.h"
#endif
#if __GLASGOW_HASKELL__ >= 503
import Data.Array
import Data.Array.Base (unsafeAt)
#else
import Array
#endif
{-# LINE 1 "templates/wrappers.hs" #-}
{-# LINE 1 "templates/wrappers.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 9 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














































{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc551d_0/ghc_2.h" #-}














































































































































































{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "templates/wrappers.hs" #-}
-- -----------------------------------------------------------------------------
-- Alex wrapper code.
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.






import Data.Word (Word8)
{-# LINE 28 "templates/wrappers.hs" #-}

import Data.Char (ord)
import qualified Data.Bits

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]



type Byte = Word8

-- -----------------------------------------------------------------------------
-- The input type

{-# LINE 79 "templates/wrappers.hs" #-}

{-# LINE 102 "templates/wrappers.hs" #-}

{-# LINE 120 "templates/wrappers.hs" #-}

{-# LINE 138 "templates/wrappers.hs" #-}

-- -----------------------------------------------------------------------------
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.

{-# LINE 161 "templates/wrappers.hs" #-}

-- -----------------------------------------------------------------------------
-- Default monad

{-# LINE 274 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Monad (with ByteString input)

{-# LINE 379 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Basic wrapper


type AlexInput = (Char,[Byte],String)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (c,_,_) = c

-- alexScanTokens :: String -> [token]
alexScanTokens str = go ('\n',[],str)
  where go inp__@(_,_bs,s) =
          case alexScan inp__ 0 of
                AlexEOF -> []
                AlexError _ -> error "lexical error"
                AlexSkip  inp__' _ln     -> go inp__'
                AlexToken inp__' len act -> act (take len s) : go inp__'

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (c,(b:bs),s) = Just (b,(c,bs,s))
alexGetByte (_,[],[])    = Nothing
alexGetByte (_,[],(c:s)) = case utf8Encode c of
                             (b:bs) -> Just (b, (c, bs, s))
                             [] -> Nothing



-- -----------------------------------------------------------------------------
-- Basic wrapper, ByteString version

{-# LINE 425 "templates/wrappers.hs" #-}

{-# LINE 440 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Posn wrapper

-- Adds text positions to the basic model.

{-# LINE 457 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Posn wrapper, ByteString version

{-# LINE 473 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- GScan wrapper

-- For compatibility with previous versions of Alex, and because we can.

alex_tab_size :: Int
alex_tab_size = 8
alex_base :: Array Int Int
alex_base = listArray (0 :: Int, 102)
  [ -8
  , -66
  , -65
  , -64
  , -63
  , -62
  , -69
  , -61
  , -70
  , -59
  , -49
  , -80
  , 28
  , -58
  , -55
  , -60
  , -48
  , -54
  , -52
  , -57
  , -53
  , -10
  , -45
  , -50
  , -42
  , -17
  , -41
  , -12
  , -14
  , 156
  , -6
  , 284
  , -3
  , 0
  , 397
  , -11
  , -4
  , -1
  , -7
  , 0
  , 462
  , -5
  , -18
  , 718
  , 654
  , 0
  , -9
  , -2
  , 2
  , 4
  , 8
  , 3
  , 20
  , 33
  , 34
  , 41
  , 36
  , 48
  , 44
  , 53
  , 56
  , 47
  , 0
  , 0
  , 57
  , 0
  , 0
  , 0
  , 0
  , 0
  , -15
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 65
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 67
  , 68
  , 0
  , 0
  , 815
  , 0
  , 83
  , 0
  , 0
  ]

alex_table :: Array Int Int
alex_table = listArray (0 :: Int, 1070)
  [ 0
  , 102
  , 102
  , 62
  , 67
  , 69
  , 32
  , 71
  , 75
  , 77
  , 80
  , 14
  , 93
  , 101
  , 58
  , 61
  , 70
  , 56
  , 76
  , 55
  , 51
  , 13
  , 6
  , 2
  , 102
  , 10
  , 53
  , 54
  , 16
  , 91
  , 19
  , 49
  , 81
  , 82
  , 89
  , 87
  , 84
  , 88
  , 11
  , 90
  , 99
  , 100
  , 100
  , 100
  , 100
  , 100
  , 100
  , 100
  , 100
  , 100
  , 85
  , 83
  , 94
  , 92
  , 95
  , 52
  , 48
  , 21
  , 30
  , 46
  , 4
  , 50
  , 18
  , 23
  , 38
  , 59
  , 17
  , 42
  , 25
  , 22
  , 26
  , 24
  , 41
  , 47
  , 27
  , 9
  , 7
  , 35
  , 5
  , 37
  , 3
  , 28
  , 8
  , 12
  , 15
  , 1
  , 60
  , 98
  , 79
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 78
  , 74
  , 73
  , 68
  , -1
  , 72
  , 101
  , 66
  , 65
  , 64
  , 63
  , 86
  , 20
  , 96
  , 97
  , 57
  , 100
  , 100
  , 100
  , 100
  , 100
  , 100
  , 100
  , 100
  , 100
  , 100
  , 0
  , 0
  , 0
  , 36
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 43
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 44
  , 29
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 40
  , 31
  , 33
  , 33
  , 33
  , 34
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 43
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 29
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 39
  , 40
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 44
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 98
  , 0
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 98
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  ]

alex_check :: Array Int Int
alex_check = listArray (0 :: Int, 1070)
  [ -1
  , 9
  , 10
  , 69
  , 69
  , 69
  , 69
  , 69
  , 77
  , 79
  , 69
  , 72
  , 61
  , 93
  , 69
  , 73
  , 79
  , 65
  , 79
  , 73
  , 65
  , 71
  , 79
  , 83
  , 32
  , 33
  , 79
  , 79
  , 69
  , 37
  , 82
  , 73
  , 40
  , 41
  , 42
  , 43
  , 44
  , 45
  , 10
  , 47
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 58
  , 59
  , 60
  , 61
  , 62
  , 65
  , 73
  , 69
  , 66
  , 73
  , 68
  , 69
  , 70
  , 69
  , 67
  , 73
  , 84
  , 78
  , 72
  , 76
  , 80
  , 72
  , 87
  , 78
  , 82
  , 84
  , 84
  , 85
  , 76
  , 87
  , 76
  , 82
  , 84
  , 91
  , 76
  , 82
  , 78
  , 95
  , 68
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , 82
  , 82
  , 76
  , 70
  , 91
  , 84
  , 93
  , 78
  , 70
  , 68
  , 78
  , 61
  , 70
  , 61
  , 61
  , 73
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , -1
  , 87
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 128
  , 129
  , 130
  , 131
  , 132
  , 133
  , 134
  , 135
  , 136
  , 137
  , 138
  , 139
  , 140
  , 141
  , 142
  , 143
  , 144
  , 145
  , 146
  , 147
  , 148
  , 149
  , 150
  , 151
  , 152
  , 153
  , 154
  , 155
  , 156
  , 157
  , 158
  , 159
  , 160
  , 161
  , 162
  , 163
  , 164
  , 165
  , 166
  , 167
  , 168
  , 169
  , 170
  , 171
  , 172
  , 173
  , 174
  , 175
  , 176
  , 177
  , 178
  , 179
  , 180
  , 181
  , 182
  , 183
  , 184
  , 185
  , 186
  , 187
  , 188
  , 189
  , 190
  , 191
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  , 128
  , 129
  , 130
  , 131
  , 132
  , 133
  , 134
  , 135
  , 136
  , 137
  , 138
  , 139
  , 140
  , 141
  , 142
  , 143
  , 144
  , 145
  , 146
  , 147
  , 148
  , 149
  , 150
  , 151
  , 152
  , 153
  , 154
  , 155
  , 156
  , 157
  , 158
  , 159
  , 160
  , 161
  , 162
  , 163
  , 164
  , 165
  , 166
  , 167
  , 168
  , 169
  , 170
  , 171
  , 172
  , 173
  , 174
  , 175
  , 176
  , 177
  , 178
  , 179
  , 180
  , 181
  , 182
  , 183
  , 184
  , 185
  , 186
  , 187
  , 188
  , 189
  , 190
  , 191
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  , 128
  , 129
  , 130
  , 131
  , 132
  , 133
  , 134
  , 135
  , 136
  , 137
  , 138
  , 139
  , 140
  , 141
  , 142
  , 143
  , 144
  , 145
  , 146
  , 147
  , 148
  , 149
  , 150
  , 151
  , 152
  , 153
  , 154
  , 155
  , 156
  , 157
  , 158
  , 159
  , 160
  , 161
  , 162
  , 163
  , 164
  , 165
  , 166
  , 167
  , 168
  , 169
  , 170
  , 171
  , 172
  , 173
  , 174
  , 175
  , 176
  , 177
  , 178
  , 179
  , 180
  , 181
  , 182
  , 183
  , 184
  , 185
  , 186
  , 187
  , 188
  , 189
  , 190
  , 191
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  , 143
  , 144
  , 145
  , 146
  , 147
  , 148
  , 149
  , 150
  , 151
  , 152
  , 153
  , 154
  , 155
  , 156
  , 157
  , 158
  , 159
  , 160
  , 161
  , 162
  , 163
  , 164
  , 165
  , 166
  , 167
  , 168
  , 169
  , 170
  , 171
  , 172
  , 173
  , 174
  , 175
  , 176
  , 177
  , 178
  , 179
  , 180
  , 181
  , 182
  , 183
  , 184
  , 185
  , 186
  , 187
  , 188
  , 189
  , 190
  , 191
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  , 191
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  , 0
  , 1
  , 2
  , 3
  , 4
  , 5
  , 6
  , 7
  , 8
  , 9
  , 10
  , 11
  , 12
  , 13
  , 14
  , 15
  , 16
  , 17
  , 18
  , 19
  , 20
  , 21
  , 22
  , 23
  , 24
  , 25
  , 26
  , 27
  , 28
  , 29
  , 30
  , 31
  , 32
  , 33
  , 34
  , 35
  , 36
  , 37
  , 38
  , 39
  , 40
  , 41
  , 42
  , 43
  , 44
  , 45
  , 46
  , 47
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 58
  , 59
  , 60
  , 61
  , 62
  , 63
  , 64
  , 65
  , 66
  , 67
  , 68
  , 69
  , 70
  , 71
  , 72
  , 73
  , 74
  , 75
  , 76
  , 77
  , 78
  , 79
  , 80
  , 81
  , 82
  , 83
  , 84
  , 85
  , 86
  , 87
  , 88
  , 89
  , 90
  , 91
  , 92
  , 93
  , 94
  , 95
  , 96
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , 123
  , 124
  , 125
  , 126
  , 127
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  , 95
  , -1
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  ]

alex_deflt :: Array Int Int
alex_deflt = listArray (0 :: Int, 102)
  [ -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 12
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 39
  , 39
  , -1
  , -1
  , -1
  , -1
  , 45
  , 45
  , -1
  , -1
  , 12
  , 12
  , 12
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  ]

alex_accept = listArray (0 :: Int, 102)
  [ AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAcc 38
  , AlexAcc 37
  , AlexAcc 36
  , AlexAcc 35
  , AlexAcc 34
  , AlexAcc 33
  , AlexAcc 32
  , AlexAcc 31
  , AlexAcc 30
  , AlexAcc 29
  , AlexAcc 28
  , AlexAcc 27
  , AlexAcc 26
  , AlexAcc 25
  , AlexAcc 24
  , AlexAcc 23
  , AlexAcc 22
  , AlexAcc 21
  , AlexAcc 20
  , AlexAcc 19
  , AlexAcc 18
  , AlexAcc 17
  , AlexAcc 16
  , AlexAcc 15
  , AlexAcc 14
  , AlexAcc 13
  , AlexAcc 12
  , AlexAcc 11
  , AlexAcc 10
  , AlexAcc 9
  , AlexAcc 8
  , AlexAcc 7
  , AlexAcc 6
  , AlexAcc 5
  , AlexAcc 4
  , AlexAcc 3
  , AlexAcc 2
  , AlexAcc 1
  , AlexAcc 0
  , AlexAccSkip
  , AlexAccSkip
  ]

alex_actions = array (0 :: Int, 39)
  [ (38,alex_action_0)
  , (37,alex_action_1)
  , (36,alex_action_2)
  , (35,alex_action_3)
  , (34,alex_action_4)
  , (33,alex_action_5)
  , (32,alex_action_6)
  , (31,alex_action_7)
  , (30,alex_action_8)
  , (29,alex_action_9)
  , (28,alex_action_10)
  , (27,alex_action_11)
  , (26,alex_action_12)
  , (25,alex_action_13)
  , (24,alex_action_14)
  , (23,alex_action_15)
  , (22,alex_action_16)
  , (21,alex_action_17)
  , (20,alex_action_18)
  , (19,alex_action_19)
  , (18,alex_action_20)
  , (17,alex_action_21)
  , (16,alex_action_22)
  , (15,alex_action_23)
  , (14,alex_action_24)
  , (13,alex_action_25)
  , (12,alex_action_26)
  , (11,alex_action_27)
  , (10,alex_action_28)
  , (9,alex_action_29)
  , (8,alex_action_30)
  , (7,alex_action_31)
  , (6,alex_action_32)
  , (5,alex_action_33)
  , (4,alex_action_34)
  , (3,alex_action_35)
  , (2,alex_action_36)
  , (1,alex_action_37)
  , (0,alex_action_37)
  ]

{-# LINE 48 "Lexer.x" #-}
    
lexer :: String -> [Token]
lexer = alexScanTokens

data Token
    = TDeclare
    | TBegin
    | TEnd
    | TIf
    | TThen
    | TElse
    | TEndif
    | TWhile
    | TDo
    | TEndwhile
    | TRepeat
    | TUntil
    | TFor
    | TFrom
    | TTo
    | TDownto
    | TEndfor
    | TRead
    | TWrite
    | TOB
    | TCB
    | TS
    | TCollon
    | TC
    | TAssign
    | TPlus
    | TMinus
    | TMul
    | TDiv
    | TMod
    | TEq
    | TDiff
    | TLess
    | TMore
    | TLessEq
    | TMoreEq
    | TId String
    | TNum Integer
    deriving (Eq,Show)

alex_action_0 = return TDeclare
alex_action_1 = return TBegin
alex_action_2 = return TEnd
alex_action_3 = return TIf
alex_action_4 = return TThen
alex_action_5 = return TElse
alex_action_6 = return TEndif
alex_action_7 = return TWhile
alex_action_8 = return TDo
alex_action_9 = return TEndwhile
alex_action_10 = return TRepeat
alex_action_11 = return TUntil
alex_action_12 = return TFor
alex_action_13 = return TFrom
alex_action_14 = return TTo
alex_action_15 = return TDownto
alex_action_16 = return TEndfor
alex_action_17 = return TRead
alex_action_18 = return TWrite
alex_action_19 = return TOB
alex_action_20 = return TCB
alex_action_21 = return TS
alex_action_22 = return TC
alex_action_23 = return TCollon
alex_action_24 = return TAssign
alex_action_25 = return TPlus
alex_action_26 = return TMinus
alex_action_27 = return TMul
alex_action_28 = return TDiv
alex_action_29 = return TMod
alex_action_30 = return TEq
alex_action_31 = return TDiff
alex_action_32 = return TLess
alex_action_33 = return TMore
alex_action_34 = return TLessEq
alex_action_35 = return TMoreEq
alex_action_36 =  \s -> TId s
alex_action_37 =  \s -> TNum (read s)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc8a9b_0/ghc_2.h" #-}














































































































































































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- -----------------------------------------------------------------------------
-- ALEX TEMPLATE
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

-- -----------------------------------------------------------------------------
-- INTERNALS and main scanner engine

{-# LINE 21 "templates/GenericTemplate.hs" #-}

{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 72 "templates/GenericTemplate.hs" #-}
alexIndexInt16OffAddr arr off = arr ! off


{-# LINE 93 "templates/GenericTemplate.hs" #-}
alexIndexInt32OffAddr arr off = arr ! off


{-# LINE 105 "templates/GenericTemplate.hs" #-}
quickIndex arr i = arr ! i


-- -----------------------------------------------------------------------------
-- Main lexing routines

data AlexReturn a
  = AlexEOF
  | AlexError  !AlexInput
  | AlexSkip   !AlexInput !Int
  | AlexToken  !AlexInput !Int a

-- alexScan :: AlexInput -> StartCode -> AlexReturn a
alexScan input__ (sc)
  = alexScanUser undefined input__ (sc)

alexScanUser user__ input__ (sc)
  = case alex_scan_tkn user__ input__ (0) input__ sc AlexNone of
  (AlexNone, input__') ->
    case alexGetByte input__ of
      Nothing ->



                                   AlexEOF
      Just _ ->



                                   AlexError input__'

  (AlexLastSkip input__'' len, _) ->



    AlexSkip input__'' len

  (AlexLastAcc k input__''' len, _) ->



    AlexToken input__''' len (alex_actions ! k)


-- Push the input through the DFA, remembering the most recent accepting
-- state it encountered.

alex_scan_tkn user__ orig_input len input__ s last_acc =
  input__ `seq` -- strict in the input
  let
  new_acc = (check_accs (alex_accept `quickIndex` (s)))
  in
  new_acc `seq`
  case alexGetByte input__ of
     Nothing -> (new_acc, input__)
     Just (c, new_input) ->



      case fromIntegral c of { (ord_c) ->
        let
                base   = alexIndexInt32OffAddr alex_base s
                offset = (base + ord_c)
                check  = alexIndexInt16OffAddr alex_check offset

                new_s = if (offset >= (0)) && (check == ord_c)
                          then alexIndexInt16OffAddr alex_table offset
                          else alexIndexInt16OffAddr alex_deflt s
        in
        case new_s of
            (-1) -> (new_acc, input__)
                -- on an error, we want to keep the input *before* the
                -- character that failed, not after.
            _ -> alex_scan_tkn user__ orig_input (if c < 0x80 || c >= 0xC0 then (len + (1)) else len)
                                                -- note that the length is increased ONLY if this is the 1st byte in a char encoding)
                        new_input new_s new_acc
      }
  where
        check_accs (AlexAccNone) = last_acc
        check_accs (AlexAcc a  ) = AlexLastAcc a input__ (len)
        check_accs (AlexAccSkip) = AlexLastSkip  input__ (len)

        check_accs (AlexAccPred a predx rest)
           | predx user__ orig_input (len) input__
           = AlexLastAcc a input__ (len)
           | otherwise
           = check_accs rest
        check_accs (AlexAccSkipPred predx rest)
           | predx user__ orig_input (len) input__
           = AlexLastSkip input__ (len)
           | otherwise
           = check_accs rest


data AlexLastAcc
  = AlexNone
  | AlexLastAcc !Int !AlexInput !Int
  | AlexLastSkip     !AlexInput !Int

data AlexAcc user
  = AlexAccNone
  | AlexAcc Int
  | AlexAccSkip

  | AlexAccPred Int (AlexAccPred user) (AlexAcc user)
  | AlexAccSkipPred (AlexAccPred user) (AlexAcc user)

type AlexAccPred user = user -> AlexInput -> Int -> AlexInput -> Bool

-- -----------------------------------------------------------------------------
-- Predicates on a rule

alexAndPred p1 p2 user__ in1 len in2
  = p1 user__ in1 len in2 && p2 user__ in1 len in2

--alexPrevCharIsPred :: Char -> AlexAccPred _
alexPrevCharIs c _ input__ _ _ = c == alexInputPrevChar input__

alexPrevCharMatches f _ input__ _ _ = f (alexInputPrevChar input__)

--alexPrevCharIsOneOfPred :: Array Char Bool -> AlexAccPred _
alexPrevCharIsOneOf arr _ input__ _ _ = arr ! alexInputPrevChar input__

--alexRightContext :: Int -> AlexAccPred _
alexRightContext (sc) user__ _ _ input__ =
     case alex_scan_tkn user__ input__ (0) input__ sc AlexNone of
          (AlexNone, _) -> False
          _ -> True
        -- TODO: there's no need to find the longest
        -- match when checking the right context, just
        -- the first match will do.
