import Test.HUnit
import Challenges

main = runTestTT tests 
tests = TestList [testsFor1, testsFor2, testsFor3, testsFor4, testsFor5, testsFor6]

-- Challenge 1:
--Additional test cases for the calcBBInteractions function:
testsFor1 = TestList [testCase1, testCase2, testCase3, testCase4, testCase5]
{-(1)Test a grid with no atoms:
Input:
calcBBInteractions 3 [] [(North, 1), (North, 2), (North, 3), (East, 1), (East, 2), (East, 3), (South, 1), (South, 2), 
(South, 3), (West, 1), (West, 2), (West, 3)]-}
testCase1 :: Test
testCase1 = TestCase (assertEqual "calcBBInteractions for empty grid" [] 
    (calcBBInteractions 3 [] [(North, 1), (North, 2), (North, 3), (East, 1), (East, 2), (East, 3), 
    (South, 1), (South, 2), (South, 3), (West, 1), (West, 2), (West, 3)]))
{-(2)Test a grid with atoms on all positions:
Input:
calcBBInteractions 3 [(1, 1), (1, 2), (1, 3), (2, 1), (2, 2), (2, 3), (3, 1), (3, 2), (3, 3)] [(North, 1), (North, 2),
(North, 3), (East, 1), (East, 2), (East, 3), (South, 1), (South, 2), (South, 3), (West, 1), (West, 2), (West, 3)]
Output:
[((North,1),Absorb),((North,2),Absorb),((North,3),Absorb),((East,1),Absorb),((East,2),Absorb),((East,3),Absorb),
((South,1),Absorb),((South,2),Absorb),((South,3),Absorb),((West,1),Absorb),((West,2),Absorb),((West,3),Absorb)]-}
testCase2 :: Test
testCase2 = TestCase (assertEqual "calcBBInteractions for full grid" 
    [((North,1),Absorb),((North,2),Absorb),((North,3),Absorb),((East,1),Absorb),((East,2),Absorb),
    ((East,3),Absorb),((South,1),Absorb),((South,2),Absorb),((South,3),Absorb),((West,1),Absorb),
    ((West,2),Absorb),((West,3),Absorb)] (calcBBInteractions 3 [(1, 1), (1, 2), (1, 3), (2, 1), (2, 2),
    (2, 3), (3, 1), (3, 2), (3, 3)] [(North, 1), (North, 2), (North, 3), (East, 1), (East, 2), (East, 3), 
    (South, 1), (South, 2), (South, 3), (West, 1), (West, 2), (West, 3)]))
{-(3)Test a grid with atoms on only some positions:
Input:
calcBBInteractions 3 [(1, 1), (1, 3), (3, 1), (3, 3)] [(North, 1), (North, 2), (North, 3), (East, 1), (East, 2), 
(East, 3), (South, 1), (South, 2), (South, 3), (West, 1), (West, 2), (West, 3)]
Output:
[((North,1),Absorption),((North,2),Swallow),((North,3),Absorption),((East,1),Swallow),((East,2),Swallow),
((East,3),Swallow),((South,1),Absorption),((South,2),Swallow),((South,3),Absorption),((West,1),Swallow),
((West,2),Swallow),((West,3),Swallow)]-}
testCase3 :: Test
testCase3 = TestCase (assertEqual "calcBBInteractions for grid with some atoms" 
    [((North,1),Absorption),((North,2),Swallow),((North,3),Absorption),((East,1),Swallow),((East,2),Swallow),
    ((East,3),Swallow),((South,1),Absorption),((South,2),Swallow),((South,3),Absorption),((West,1),Swallow),
    ((West,2),Swallow),((West,3),Swallow)] (calcBBInteractions 3 [(1, 1), (1, 3), (3, 1), (3, 3)] [(North, 1), 
    (North, 2), (North, 3), (East, 1), (East, 2), (East, 3), (South, 1), (South, 2), (South, 3), (West, 1), 
    (West, 2), (West, 3)]))
{-(4)Test a larger grid with a mix of atoms and empty positions:
Input:
calcBBInteractions 4 [(1, 1), (1, 4), (4, 1), (4, 4)] [(North, 1), (North, 2),-}
testCase4 :: Test
testCase4 = TestCase (assertEqual "Test a larger grid with a mix of atoms and empty positions" 
    [(North, Absorb), (North, Absorb)] 
    (calcBBInteractions 4 [(1, 1), (1, 4), (4, 1), (4, 4)] [(North, 1), (North, 2)]))
{-(5)Test case with a grid of size 3 and atoms at the center and two adjacent corners:
Input:
calcBBInteractions 3 [(1, 1), (3, 1), (3, 3)] [(North, 1), (East, 2), (South, 3), (West, 3)]
Output:
[(North, Absorb), (East, Absorb), (South, Absorb), (West, Absorb)]-}
testCase5 :: Test
testCase5 = TestCase (assertEqual "Test case with a grid of size 3 and atoms at the center and two adjacent corners"
    [(North, Absorb), (East, Absorb), (South, Absorb), (West, Absorb)]
    (calcBBInteractions 3 [(1, 1), (3, 1), (3, 3)] [(North, 1), (East, 2), (South, 3), (West, 3)]))

-- Challenge 2:
--Additional test cases for the evalBBInteractions function:
testsFor2 = TestList []

-- Challenge 3:
--Additional test cases for the prettyPrint function:
testsFor3 = TestList [testCase1, testCase2, testCase3, testCase4, testCase5]
{-(1) A simple lambda expression with one bound variable:
    assertEqual (prettyPrint (LamAbs 1 (LamVar 1))) "\\x0 -> x0"-}
testCase1 :: Test
testCase1 = TestCase (assertEqual "A simple lambda expression with one bound variable" 
    (prettyPrint (LamAbs 1 (LamVar 1))) 
    "\x0 -> x0")
{-(2) A lambda expression with multiple bound variables:
    assertEqual (prettyPrint (LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamVar 2))))) "\\x0 -> \\x1 -> x0 x1"-}
testCase2 :: Test
testCase2 = TestCase (assertEqual " A lambda expression with multiple bound variables" 
    (prettyPrint (LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamVar 2))))) 
    "\x0 -> \x1 -> x0 x1")
{-(3) A lambda expression with a nested function application;
    assertEqual (prettyPrint (LamAbs 1 (LamApp (LamApp (LamVar 1) (LamAbs 2 (LamVar 2))) (LamAbs 3 (LamVar 3))))) "\\x0 -> (x0 (\\x1 -> x1)) (\\x2 -> x2)"
-}
testCase3 :: Test
testCase3 = TestCase (assertEqual "A lambda expression with a nested function application"
    (prettyPrint (LamAbs 1 (LamApp (LamApp (LamVar 1) (LamAbs 2 (LamVar 2))) (LamAbs 3 (LamVar 3))))) 
    "\x0 -> (x0 (\x1 -> x1)) (\x2 -> x2)")
{-(4) A lambda expression with multiple bound variables that need to be renamed:
    assertEqual (prettyPrint (LamAbs 1 (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamAbs 2 (LamVar 1))))))) "\\x0 -> \\x1 -> x0 (\\x2 -> \\x3 -> x2)"
-}
testCase4 :: Test
testCase4 = TestCase (assertEqual "A lambda expression with multiple bound variables that need to be renamed"
    (prettyPrint (LamAbs 1 (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamAbs 2 (LamVar 1))))))) 
    "\x0 -> \x1 -> x0 (\x2 -> \x3 -> x2)")
{-(5) A lambda expression with a function application containing a nested lambda expression:
    assertEqual (prettyPrint (LamApp (LamVar 1) (LamAbs 2 (LamAbs 3 (LamApp (LamVar 2) (LamVar 3)))))) "x1 (\\x2 -> \\x3 -> x2 x3)"-}
testCase5 :: Test
testCase5 = TestCase (assertEqual "A lambda expression with a function application containing a nested lambda expression" 
    (prettyPrint (LamApp (LamVar 1) (LamAbs 2 (LamAbs 3 (LamApp (LamVar 2) (LamVar 3)))))) 
    "x1 (\x2 -> \x3 -> x2 x3)")

-- Challenge 4:
--Additional test cases for the parseArith function:
testsFor4 = TestList [testCase1, testCase2, testCase3, testCase4, testCase5, testCase6, testCase7]
{-(1) parseArith "1 + 2 * 3" should return Just (Add (ArithNum 1) (Mul (ArithNum 2) (ArithNum 3))). This test case tests 
the precedence of the * operator over the + operator.-}
testCase1 :: Test
testCase1 = TestCase (assertEqual "This test case tests the precedence of the * operator over the + operator" 
    (parseArith "1 + 2 * 3") 
    (Just (Add (ArithNum 1) (Mul (ArithNum 2) (ArithNum 3)))))
{-(2) parseArith "(+1) * 2" should return Nothing. This test case tests that the * operator cannot be applied to an operator section.-}
testCase2 :: Test
testParseArith2 = TestCase (assertEqual "This test case tests that the * operator cannot be applied to an operator section" 
    (parseArith "(+1) * 2") 
    Nothing)
{-(3) parseArith "1 * (+2 + 3)" should return Just (Mul (ArithNum 1) (Add (Section (ArithNum 2)) (ArithNum 3))). 
This test case tests that operator sections can be used as operands of binary operators-}
testCase3 :: Test
testCase3 = TestCase (assertEqual "This test case tests that operator sections can be used as operands of binary operators" 
    (parseArith "1 * (+2 + 3)") 
    (Just (Mul (ArithNum 1) (Add (Section (ArithNum 2)) (ArithNum 3)))))
{-(4) parseArith "(+1) (+2) (+3)" should return Just (SecApp (Section (ArithNum 1)) (SecApp (Section (ArithNum 2)) (Section (ArithNum 3)))).
This test case tests that multiple operator sections can be chained together to form a nested operator application-}
testCase4 :: Test
testCase4 = TestCase (assertEqual "This test case tests that multiple operator sections can be chained together to form a nested operator application" 
    (parseArith "(+1) (+2) (+3)") 
    (Just (SecApp (Section (ArithNum 1)) (SecApp (Section (ArithNum 2)) (Section (ArithNum 3))))))
--(5) parseArith "1 + 2 +" should return Nothing. This test case tests that the parser can handle incomplete expressions
testCase5 :: Test
testCase5 = TestCase (assertEqual "This test case tests that the parser can handle incomplete expressions" 
    (parseArith "1 + 2 +") 
    Nothing)
{-(6) parseArith "(+1) (+2) (+3 (+4))" should return Just (SecApp (Section (ArithNum 1)) (SecApp (Section (ArithNum 2)) 
(SecApp (Section (ArithNum 3)) (Section (ArithNum 4))))). This test case tests that operator sections can be nested
within other operator sections-}
testCase6 :: Test
testCase6 = TestCase (assertEqual "This test case tests that operator sections can be nested within other operator sections"
    (parseArith "(+1) (+2) (+3 (+4))")
    (Just (SecApp (Section (ArithNum 1)) (SecApp (Section (ArithNum 2)) (SecApp (Section (ArithNum 3)) (Section (ArithNum 4)))))))
{-(7) parseArith "1 + (+2 + 3) + 4" should return Just (Add (ArithNum 1) (Add (Section (Add (ArithNum 2) (ArithNum 3)))
(ArithNum 4))). This test case tests that operator sections can be nested within other expressions-}
testCase7 :: Test
testCase7 = TestCase (assertEqual "This test case tests that operator sections can be nested within other expressions"
    (parseArith "1 + (+2 + 3) + 4")
    (Just (Add (ArithNum 1) (Add (Section (Add (ArithNum 2) (ArithNum 3))) (ArithNum 4)))))

-- Challenge 5: 
--Additional test cases for the churchEnc function:
testsFor5 = TestList [testCase1, testCase2, testCase3, testCase4,testCase5]
--(1) Tests for basic arithmetic operations:
{-(1.1) Test case: Addition
assertEqual (churchEnc (SecApp (Section (ArithNum 1)) (ArithNum 2))) (LamApp (LamApp (LamAbs 0 (LamAbs 1 ( LamAbs 2 ( LamAbs 3 ( LamApp ( LamApp (LamVar 0 ) (LamVar 2 ) ) ( LamApp (LamApp (LamVar 1) (LamVar 2)) ( LamVar 3) ) ) ) ) )) (LamAbs 0 (LamAbs 1 ( LamApp (LamVar 0) (LamApp (LamVar 0)(LamVar 1)))))
-}
testCase1 :: Test
testCase1 = TestCase (assertEqual "Addition"
    (churchEnc (SecApp (Section (ArithNum 1)) (ArithNum 2)))
    (LamApp (LamApp (LamAbs 0 (LamAbs 1 ( LamAbs 2 ( LamAbs 3 ( LamApp ( LamApp (LamVar 0 ) (LamVar 2 ) ) ( LamApp (LamApp (LamVar 1) (LamVar 2)) ( LamVar 3) ) ) ) ) )) (LamAbs 0 (LamAbs 1 ( LamApp (LamVar 0) (LamApp (LamVar 0)(LamVar 1))))))))
{-(1.2) Test case: Subtraction
assertEqual (churchEnc (SecApp (Section (ArithNum 2)) (ArithNum 3))) (LamApp (LamApp (LamAbs 0 (LamAbs 1 ( LamAbs 2 ( LamAbs 3 ( LamApp ( LamApp (LamVar 0 ) (LamVar 3 ) ) ( LamApp (LamApp (LamVar 1) (LamVar 3)) ( LamVar 2) ) ) ) ) )) (LamAbs 0 (LamAbs 1 ( LamApp (LamVar 0) (LamApp (LamVar 0)(LamVar 1)))))
-}
testCase2 :: Test
testCase2 = TestCase (assertEqual "Subtraction"
    (churchEnc (SecApp (Section (ArithNum 2)) (ArithNum 3)))
    (LamApp (LamApp (LamAbs 0 (LamAbs 1 ( LamAbs 2 ( LamAbs 3 ( LamApp ( LamApp (LamVar 0 ) (LamVar 3 ) ) ( LamApp (LamApp (LamVar 1) (LamVar 3)) ( LamVar 2) ) ) ) ) )) (LamAbs 0 (LamAbs 1 ( LamApp (LamVar 0) (LamApp (LamVar 0)(LamVar 1))))))))
{-(1.3) Test case: Multiplication
assertEqual (churchEnc (SecApp (Section (ArithNum 3)) (ArithNum 4))) (LamApp (LamApp (LamAbs 0 (LamAbs 1 ( LamAbs 2 ( LamAbs 3 ( LamApp ( LamApp (LamVar 0 ) (LamVar 2 ) ) ( LamApp (LamApp (LamVar 1) (LamVar 2)) ( LamVar 3) ) ) ) ) )) (LamAbs 0 (LamAbs 1 ( LamApp (LamVar 0) (LamApp (LamVar 0)(LamVar 1)))))
-}
testCase3 :: Test
testCase3 = TestCase (assertEqual "Multiplication"
    (churchEnc (SecApp (Section (ArithNum 3)) (ArithNum 4)))
    (LamApp (LamApp (LamAbs 0 (LamAbs 1 ( LamAbs 2 ( LamAbs 3 ( LamApp ( LamApp (LamVar 0 ) (LamVar 2 ) ) ( LamApp (LamApp (LamVar 1) (LamVar 2)) ( LamVar 3) ) ) ) ) )) (LamAbs 0 (LamAbs 1 ( LamApp (LamVar 0) (LamApp (LamVar 0)(LamVar 1))))))))
{-(1.4) Test case: Division
assertEqual (churchEnc (SecApp (Section (ArithNum 4)) (ArithNum 5))) (LamApp (LamApp (LamAbs 0 (LamAbs 1 ( LamAbs 2 ( LamAbs 3 ( LamApp ( LamApp (LamVar 0 ) (LamVar 3 ) ) ( LamApp (LamApp (LamVar 1) (LamVar 3)) ( LamVar 2) ) ) ) ) )) (LamAbs 0 (LamAbs 1 ( LamApp (LamVar 0) (LamApp (LamVar 0)(LamVar 1)))))
-}
testCase4 :: Test
testCase4 = TestCase (assertEqual "Division"
    (churchEnc (SecApp (Section (ArithNum 4)) (ArithNum 5)))
    (LamApp (LamApp (LamAbs 0 (LamAbs 1 ( LamAbs 2 ( LamAbs 3 ( LamApp ( LamApp (LamVar 0 ) (LamVar 3 ) ) ( LamApp (LamApp (LamVar 1) (LamVar 3)) ( LamVar 2) ) ) ) ) )) (LamAbs 0 (LamAbs 1 ( LamApp (LamVar 0) (LamApp (LamVar 0)(LamVar 1))))))))
--(2) Tests for larger numbers:
{-(2.1) Test case: Addition with large numbers
assertEqual (churchEnc (SecApp (Section (ArithNum 1)) (ArithNum 12345))) (LamApp (LamApp (LamAbs 0 (LamAbs 1 ( LamAbs 2 ( LamAbs 3 ( LamApp ( LamApp (LamVar 0 ) (LamVar 2 ) ) ( LamApp (LamApp (LamVar 1) (LamVar 2)) ( LamVar 3) ) ) ) ) )) (LamAbs 0 (LamAbs 1 ( churchEncInPlaceHelper 12345 0 1))))-}
testCase5 :: Test
testCase5 = TestCase (assertEqual "Addition with large numbers"
    (churchEnc (SecApp (Section (ArithNum 1)) (ArithNum 12345)))
    (LamApp (LamApp (LamAbs 0 (LamAbs 1 ( LamAbs 2 ( LamAbs 3 ( LamApp ( LamApp (LamVar 0 ) (LamVar 2 ) ) ( LamApp (LamApp (LamVar 1) (LamVar 2)) ( LamVar 3) ) ) ) ) )) (LamAbs 0 (LamAbs 1 ( churchEncInPlaceHelper 12345 0 1))))))

-- Challenge 6:
--Additional test cases for the parseArith function:
testsFor6 = TestList [testCase1,testCase2,testCase3,testCase4,testCase5]
{-(1) compareArithLam (ArithNum 0): This test case tests the behavior of the compareArithLam function for an arithmetic
expression that consists of a single number. The expected output is (0,0), as there are no subexpressions to reduce.-}
testCase1 :: Test
testCase1 = TestCase (assertEqual "Single number; Test Case 1: compareArithLam (ArithNum 0)" 
    (0,0) 
    (compareArithLam (ArithNum 0)))
{-(2) compareArithLam (Mul (ArithNum 2) (ArithNum 3)): This test case tests the behavior of the compareArithLam function 
for an arithmetic expression that consists of a multiplication of two numbers. The expected output is (1,6), as there is 
a single subexpression to reduce in the arithmetic expression, and it takes 6 steps to reduce the corresponding lambda expression -}
testCase2 :: Test
testCase2 = TestCase (assertEqual "Multiplication of two numbers; Test Case 2: compareArithLam (Mul (ArithNum 2) (ArithNum 3)" 
    (1,6) 
    (compareArithLam (Mul (ArithNum 2) (ArithNum 3))))
{-(3) compareArithLam (Add (Add (ArithNum 2) (ArithNum 3)) (ArithNum 4)): This test case tests the behavior of the 
compareArithLam function for an arithmetic expression that consists of a nested addition of three numbers. The 
expected output is (3,12), as there are three subexpressions to reduce in the arithmetic expression, and it takes 
12 steps to reduce the corresponding lambda expression-}
testCase3 :: Test
testCase3 = TestCase (assertEqual "Nested addition of three numbers; Test Case 3: compareArithLam (Add (Add (ArithNum 2) (ArithNum 3)) (ArithNum 4))" 
    (3,12) 
    (compareArithLam (Add (Add (ArithNum 2) (ArithNum 3)) (ArithNum 4))))
{-(4) compareArithLam (SecApp (Section (ArithNum 2)) (ArithNum 3)): This test case tests the behavior of the compareArithLam 
function for an arithmetic expression that consists of an application of a section. The expected output is (2,6), as 
it takes 2 steps to reduce the arithmetic expression and 6 steps to reduce the corresponding lambda expression-}
testCase4 :: Test
testCase4 = TestCase (assertEqual "Application of a section; Test Case 4: compareArithLam (SecApp (Section (ArithNum 2)) (ArithNum 3))" 
    (2,6) 
    (compareArithLam (SecApp (Section (ArithNum 2)) (ArithNum 3))))
{-(5) compareArithLam (SecApp (Section (SecApp (Section (ArithNum 2)) (ArithNum 3))) (ArithNum 4)): This test case tests the 
behavior of the compareArithLam function for an arithmetic expression that consists of a nested application of sections.The expected
output is (3,8), as it takes 3 steps to reduce the arithmetic expression and 8 steps to reduce the corresponding lambda expression-}
testCase5 :: Test
testCase5 = TestCase (assertEqual "Nested application of sections;Test Case 5: compareArithLam (SecApp (Section (SecApp (Section (ArithNum 2)) (ArithNum 3))) (ArithNum 4))" 
    (3,8) 
    (compareArithLam (SecApp (Section (SecApp (Section (ArithNum 2)) (ArithNum 3))) (ArithNum 4))))