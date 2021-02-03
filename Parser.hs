{-# OPTIONS_GHC -w #-}
module Parser(parse, Declaration(..), Command(..), Expression(..), Condition(..), Value(..), Identifier(..)) where
import Lexer
import Translator
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.8

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,176) ([24576,0,0,32768,0,0,0,1,0,0,0,0,0,37136,194,0,512,0,0,4352,3113,0,0,0,8,0,0,1,24576,0,0,32768,1,0,0,42052,48,0,16,0,0,64,0,0,384,0,0,4096,0,0,0,0,17,8192,0,0,0,1,0,0,42052,48,0,0,1024,0,0,0,0,0,0,0,0,0,1,0,1024,0,0,1024,0,0,512,0,0,0,0,126,512,0,0,96,0,0,384,0,0,0,0,0,0,0,0,0,0,16,0,0,31744,0,0,128,0,0,512,0,17472,778,0,384,0,0,1536,0,0,6144,0,0,24576,0,0,32768,1,0,0,6,0,0,37136,194,0,96,0,0,384,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,512,0,8,0,0,0,0,0,0,384,0,0,0,1,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,32768,1,0,0,6,0,0,24,0,0,96,0,0,384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,17472,778,0,0,0,0,0,0,0,0,0,0,24576,0,0,32768,1,0,0,0,128,0,0,4096,0,32768,0,0,0,2,0,0,2,0,0,0,0,16384,2628,3,0,10513,12,0,4,0,0,0,0,0,0,128,0,0,512,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","program","declarations","commands","command","expression","condition","value","identifier","NUM","ID","\"DECLARE\"","\"BEGIN\"","\"END\"","\"IF\"","\"THEN\"","\"ELSE\"","\"ENDIF\"","\"WHILE\"","\"DO\"","\"ENDWHILE\"","\"REPEAT\"","\"UNTIL\"","\"FOR\"","\"FROM\"","\"TO\"","\"DOWNTO\"","\"ENDFOR\"","\"READ\"","\"WRITE\"","'('","')'","';'","':'","','","\":=\"","'+'","'-'","'*'","'/'","\"mod\"","'='","\"!=\"","'<'","'>'","\"<=\"","\">=\"","%eof"]
        bit_start = st * 50
        bit_end = (st + 1) * 50
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..49]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (14) = happyShift action_2
action_0 (15) = happyShift action_4
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (14) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (13) = happyShift action_16
action_2 (5) = happyGoto action_15
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (50) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (13) = happyShift action_8
action_4 (17) = happyShift action_9
action_4 (21) = happyShift action_10
action_4 (24) = happyShift action_11
action_4 (26) = happyShift action_12
action_4 (31) = happyShift action_13
action_4 (32) = happyShift action_14
action_4 (6) = happyGoto action_5
action_4 (7) = happyGoto action_6
action_4 (11) = happyGoto action_7
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (16) = happyShift action_32
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (13) = happyShift action_8
action_6 (17) = happyShift action_9
action_6 (21) = happyShift action_10
action_6 (24) = happyShift action_11
action_6 (26) = happyShift action_12
action_6 (31) = happyShift action_13
action_6 (32) = happyShift action_14
action_6 (6) = happyGoto action_31
action_6 (7) = happyGoto action_6
action_6 (11) = happyGoto action_7
action_6 _ = happyReduce_8

action_7 (38) = happyShift action_30
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (33) = happyShift action_29
action_8 _ = happyReduce_32

action_9 (12) = happyShift action_22
action_9 (13) = happyShift action_8
action_9 (9) = happyGoto action_28
action_9 (10) = happyGoto action_27
action_9 (11) = happyGoto action_21
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (12) = happyShift action_22
action_10 (13) = happyShift action_8
action_10 (9) = happyGoto action_26
action_10 (10) = happyGoto action_27
action_10 (11) = happyGoto action_21
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (13) = happyShift action_8
action_11 (17) = happyShift action_9
action_11 (21) = happyShift action_10
action_11 (24) = happyShift action_11
action_11 (26) = happyShift action_12
action_11 (31) = happyShift action_13
action_11 (32) = happyShift action_14
action_11 (6) = happyGoto action_25
action_11 (7) = happyGoto action_6
action_11 (11) = happyGoto action_7
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (13) = happyShift action_24
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (13) = happyShift action_8
action_13 (11) = happyGoto action_23
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (12) = happyShift action_22
action_14 (13) = happyShift action_8
action_14 (10) = happyGoto action_20
action_14 (11) = happyGoto action_21
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (15) = happyShift action_19
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (33) = happyShift action_17
action_16 (37) = happyShift action_18
action_16 _ = happyReduce_5

action_17 (12) = happyShift action_51
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (13) = happyShift action_16
action_18 (5) = happyGoto action_50
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (13) = happyShift action_8
action_19 (17) = happyShift action_9
action_19 (21) = happyShift action_10
action_19 (24) = happyShift action_11
action_19 (26) = happyShift action_12
action_19 (31) = happyShift action_13
action_19 (32) = happyShift action_14
action_19 (6) = happyGoto action_49
action_19 (7) = happyGoto action_6
action_19 (11) = happyGoto action_7
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (35) = happyShift action_48
action_20 _ = happyFail (happyExpListPerState 20)

action_21 _ = happyReduce_31

action_22 _ = happyReduce_30

action_23 (35) = happyShift action_47
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (27) = happyShift action_46
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (25) = happyShift action_45
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (22) = happyShift action_44
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (44) = happyShift action_38
action_27 (45) = happyShift action_39
action_27 (46) = happyShift action_40
action_27 (47) = happyShift action_41
action_27 (48) = happyShift action_42
action_27 (49) = happyShift action_43
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (18) = happyShift action_37
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (12) = happyShift action_35
action_29 (13) = happyShift action_36
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (12) = happyShift action_22
action_30 (13) = happyShift action_8
action_30 (8) = happyGoto action_33
action_30 (10) = happyGoto action_34
action_30 (11) = happyGoto action_21
action_30 _ = happyFail (happyExpListPerState 30)

action_31 _ = happyReduce_7

action_32 _ = happyReduce_2

action_33 (35) = happyShift action_71
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (39) = happyShift action_66
action_34 (40) = happyShift action_67
action_34 (41) = happyShift action_68
action_34 (42) = happyShift action_69
action_34 (43) = happyShift action_70
action_34 _ = happyReduce_18

action_35 (34) = happyShift action_65
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (34) = happyShift action_64
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (13) = happyShift action_8
action_37 (17) = happyShift action_9
action_37 (21) = happyShift action_10
action_37 (24) = happyShift action_11
action_37 (26) = happyShift action_12
action_37 (31) = happyShift action_13
action_37 (32) = happyShift action_14
action_37 (6) = happyGoto action_63
action_37 (7) = happyGoto action_6
action_37 (11) = happyGoto action_7
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (12) = happyShift action_22
action_38 (13) = happyShift action_8
action_38 (10) = happyGoto action_62
action_38 (11) = happyGoto action_21
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (12) = happyShift action_22
action_39 (13) = happyShift action_8
action_39 (10) = happyGoto action_61
action_39 (11) = happyGoto action_21
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (12) = happyShift action_22
action_40 (13) = happyShift action_8
action_40 (10) = happyGoto action_60
action_40 (11) = happyGoto action_21
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (12) = happyShift action_22
action_41 (13) = happyShift action_8
action_41 (10) = happyGoto action_59
action_41 (11) = happyGoto action_21
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (12) = happyShift action_22
action_42 (13) = happyShift action_8
action_42 (10) = happyGoto action_58
action_42 (11) = happyGoto action_21
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (12) = happyShift action_22
action_43 (13) = happyShift action_8
action_43 (10) = happyGoto action_57
action_43 (11) = happyGoto action_21
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (13) = happyShift action_8
action_44 (17) = happyShift action_9
action_44 (21) = happyShift action_10
action_44 (24) = happyShift action_11
action_44 (26) = happyShift action_12
action_44 (31) = happyShift action_13
action_44 (32) = happyShift action_14
action_44 (6) = happyGoto action_56
action_44 (7) = happyGoto action_6
action_44 (11) = happyGoto action_7
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (12) = happyShift action_22
action_45 (13) = happyShift action_8
action_45 (9) = happyGoto action_55
action_45 (10) = happyGoto action_27
action_45 (11) = happyGoto action_21
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (12) = happyShift action_22
action_46 (13) = happyShift action_8
action_46 (10) = happyGoto action_54
action_46 (11) = happyGoto action_21
action_46 _ = happyFail (happyExpListPerState 46)

action_47 _ = happyReduce_16

action_48 _ = happyReduce_17

action_49 (16) = happyShift action_53
action_49 _ = happyFail (happyExpListPerState 49)

action_50 _ = happyReduce_3

action_51 (36) = happyShift action_52
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (12) = happyShift action_83
action_52 _ = happyFail (happyExpListPerState 52)

action_53 _ = happyReduce_1

action_54 (28) = happyShift action_81
action_54 (29) = happyShift action_82
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (35) = happyShift action_80
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (23) = happyShift action_79
action_56 _ = happyFail (happyExpListPerState 56)

action_57 _ = happyReduce_29

action_58 _ = happyReduce_28

action_59 _ = happyReduce_27

action_60 _ = happyReduce_26

action_61 _ = happyReduce_25

action_62 _ = happyReduce_24

action_63 (19) = happyShift action_77
action_63 (20) = happyShift action_78
action_63 _ = happyFail (happyExpListPerState 63)

action_64 _ = happyReduce_34

action_65 _ = happyReduce_33

action_66 (12) = happyShift action_22
action_66 (13) = happyShift action_8
action_66 (10) = happyGoto action_76
action_66 (11) = happyGoto action_21
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (12) = happyShift action_22
action_67 (13) = happyShift action_8
action_67 (10) = happyGoto action_75
action_67 (11) = happyGoto action_21
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (12) = happyShift action_22
action_68 (13) = happyShift action_8
action_68 (10) = happyGoto action_74
action_68 (11) = happyGoto action_21
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (12) = happyShift action_22
action_69 (13) = happyShift action_8
action_69 (10) = happyGoto action_73
action_69 (11) = happyGoto action_21
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (12) = happyShift action_22
action_70 (13) = happyShift action_8
action_70 (10) = happyGoto action_72
action_70 (11) = happyGoto action_21
action_70 _ = happyFail (happyExpListPerState 70)

action_71 _ = happyReduce_9

action_72 _ = happyReduce_23

action_73 _ = happyReduce_22

action_74 _ = happyReduce_21

action_75 _ = happyReduce_20

action_76 _ = happyReduce_19

action_77 (13) = happyShift action_8
action_77 (17) = happyShift action_9
action_77 (21) = happyShift action_10
action_77 (24) = happyShift action_11
action_77 (26) = happyShift action_12
action_77 (31) = happyShift action_13
action_77 (32) = happyShift action_14
action_77 (6) = happyGoto action_87
action_77 (7) = happyGoto action_6
action_77 (11) = happyGoto action_7
action_77 _ = happyFail (happyExpListPerState 77)

action_78 _ = happyReduce_11

action_79 _ = happyReduce_12

action_80 _ = happyReduce_13

action_81 (12) = happyShift action_22
action_81 (13) = happyShift action_8
action_81 (10) = happyGoto action_86
action_81 (11) = happyGoto action_21
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (12) = happyShift action_22
action_82 (13) = happyShift action_8
action_82 (10) = happyGoto action_85
action_82 (11) = happyGoto action_21
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (34) = happyShift action_84
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (37) = happyShift action_91
action_84 _ = happyReduce_6

action_85 (22) = happyShift action_90
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (22) = happyShift action_89
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (20) = happyShift action_88
action_87 _ = happyFail (happyExpListPerState 87)

action_88 _ = happyReduce_10

action_89 (13) = happyShift action_8
action_89 (17) = happyShift action_9
action_89 (21) = happyShift action_10
action_89 (24) = happyShift action_11
action_89 (26) = happyShift action_12
action_89 (31) = happyShift action_13
action_89 (32) = happyShift action_14
action_89 (6) = happyGoto action_94
action_89 (7) = happyGoto action_6
action_89 (11) = happyGoto action_7
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (13) = happyShift action_8
action_90 (17) = happyShift action_9
action_90 (21) = happyShift action_10
action_90 (24) = happyShift action_11
action_90 (26) = happyShift action_12
action_90 (31) = happyShift action_13
action_90 (32) = happyShift action_14
action_90 (6) = happyGoto action_93
action_90 (7) = happyGoto action_6
action_90 (11) = happyGoto action_7
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (13) = happyShift action_16
action_91 (5) = happyGoto action_92
action_91 _ = happyFail (happyExpListPerState 91)

action_92 _ = happyReduce_4

action_93 (30) = happyShift action_96
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (30) = happyShift action_95
action_94 _ = happyFail (happyExpListPerState 94)

action_95 _ = happyReduce_14

action_96 _ = happyReduce_15

happyReduce_1 = happyReduce 5 4 happyReduction_1
happyReduction_1 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_3  4 happyReduction_2
happyReduction_2 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (([], happy_var_2)
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  5 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_3)
	_
	(HappyTerminal (TId happy_var_1))
	 =  HappyAbsSyn5
		 (SingleDeclaration happy_var_1 : happy_var_3
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happyReduce 8 5 happyReduction_4
happyReduction_4 ((HappyAbsSyn5  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TNum happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TNum happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TId happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (ArrayDeclaration happy_var_1 happy_var_3 happy_var_5 : happy_var_8
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_1  5 happyReduction_5
happyReduction_5 (HappyTerminal (TId happy_var_1))
	 =  HappyAbsSyn5
		 ([SingleDeclaration happy_var_1]
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happyReduce 6 5 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyTerminal (TNum happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TNum happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TId happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 ([ArrayDeclaration happy_var_1 happy_var_3 happy_var_5]
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_2  6 happyReduction_7
happyReduction_7 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1:happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  6 happyReduction_8
happyReduction_8 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happyReduce 4 7 happyReduction_9
happyReduction_9 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Assign happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_10 = happyReduce 7 7 happyReduction_10
happyReduction_10 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (IfElse happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 5 7 happyReduction_11
happyReduction_11 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (IfElse happy_var_2 happy_var_4 []
	) `HappyStk` happyRest

happyReduce_12 = happyReduce 5 7 happyReduction_12
happyReduction_12 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (While happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_13 = happyReduce 5 7 happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Until happy_var_4 happy_var_2
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 9 7 happyReduction_14
happyReduction_14 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (ForUp happy_var_2 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 9 7 happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (ForDown happy_var_2 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest

happyReduce_16 = happySpecReduce_3  7 happyReduction_16
happyReduction_16 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (Read happy_var_2
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  7 happyReduction_17
happyReduction_17 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (Write happy_var_2
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  8 happyReduction_18
happyReduction_18 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn8
		 (SingleValue happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  8 happyReduction_19
happyReduction_19 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn8
		 (Add happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  8 happyReduction_20
happyReduction_20 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn8
		 (Sub happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  8 happyReduction_21
happyReduction_21 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn8
		 (Mul happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  8 happyReduction_22
happyReduction_22 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn8
		 (Div happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  8 happyReduction_23
happyReduction_23 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn8
		 (Mod happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  9 happyReduction_24
happyReduction_24 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (Cond happy_var_1 Translator.EQ happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  9 happyReduction_25
happyReduction_25 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (Cond happy_var_1 Translator.NE happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  9 happyReduction_26
happyReduction_26 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (Cond happy_var_1 Translator.LT happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  9 happyReduction_27
happyReduction_27 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (Cond happy_var_1 Translator.GT happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  9 happyReduction_28
happyReduction_28 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (Cond happy_var_1 Translator.LE happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  9 happyReduction_29
happyReduction_29 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (Cond happy_var_1 Translator.GE happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  10 happyReduction_30
happyReduction_30 (HappyTerminal (TNum happy_var_1))
	 =  HappyAbsSyn10
		 (Const happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  10 happyReduction_31
happyReduction_31 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (Var happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  11 happyReduction_32
happyReduction_32 (HappyTerminal (TId happy_var_1))
	 =  HappyAbsSyn11
		 (SingleVar happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happyReduce 4 11 happyReduction_33
happyReduction_33 (_ `HappyStk`
	(HappyTerminal (TNum happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TId happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (ArrayConst happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_34 = happyReduce 4 11 happyReduction_34
happyReduction_34 (_ `HappyStk`
	(HappyTerminal (TId happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TId happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (ArrayVar happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 50 50 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TNum happy_dollar_dollar -> cont 12;
	TId happy_dollar_dollar -> cont 13;
	TDeclare -> cont 14;
	TBegin -> cont 15;
	TEnd -> cont 16;
	TIf -> cont 17;
	TThen -> cont 18;
	TElse -> cont 19;
	TEndif -> cont 20;
	TWhile -> cont 21;
	TDo -> cont 22;
	TEndwhile -> cont 23;
	TRepeat -> cont 24;
	TUntil -> cont 25;
	TFor -> cont 26;
	TFrom -> cont 27;
	TTo -> cont 28;
	TDownto -> cont 29;
	TEndfor -> cont 30;
	TRead -> cont 31;
	TWrite -> cont 32;
	TOB -> cont 33;
	TCB -> cont 34;
	TS -> cont 35;
	TCollon -> cont 36;
	TC -> cont 37;
	TAssign -> cont 38;
	TPlus -> cont 39;
	TMinus -> cont 40;
	TMul -> cont 41;
	TDiv -> cont 42;
	TMod -> cont 43;
	TEq -> cont 44;
	TDiff -> cont 45;
	TLess -> cont 46;
	TMore -> cont 47;
	TLessEq -> cont 48;
	TMoreEq -> cont 49;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 50 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Either String a -> (a -> Either String b) -> Either String b
happyThen = (>>=)
happyReturn :: () => a -> Either String a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Either String a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> Either String a
happyError' = (\(tokens, _) -> parserError tokens)
parse tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parserError t = Left $ "Syntax error at "++show t

data Declaration = SingleDeclaration String
    | ArrayDeclaration String Integer Integer deriving (Show, Eq) 

data Command = Assign Identifier Expression
    | IfElse Condition [Command] [Command]
    | If Condition [Command]
    | While Condition [Command]
    | Until Condition [Command]
    | ForUp String Value Value [Command]
    | ForDown String Value Value [Command]
    | Read Identifier
    | Write Value
    deriving (Eq) 

data Expression = SingleValue Value
    | Add Value Value
    | Sub Value Value
    | Mul Value Value
    | Div Value Value
    | Mod Value Value
    deriving (Eq)

data Condition = Cond Value Comparator Value deriving (Eq) 

data Value = Const Integer
    | Var Identifier
    deriving (Eq, Ord)

data Identifier = SingleVar String
    | ArrayConst String Integer
    | ArrayVar String String deriving (Eq, Ord)
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
{-# LINE 1 "/tmp/ghc8814_0/ghc_2.h" #-}




























































































































































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 137 "templates/GenericTemplate.hs" #-}

{-# LINE 147 "templates/GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 267 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 333 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
