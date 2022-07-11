import Data.Matrix


strassenRecursive :: Matrix Int -> Matrix Int -> Int -> Matrix Int
strassenRecursive a b 2 = fromList 2 2 [m1 + m4 - m5 + m7, m3 + m5, m2 + m4, m1 - m2 + m3 + m6]
    where m1 = (getElem 1 1 a + getElem 2 2 a) * (getElem 1 1 b + getElem 2 2 b)
          m2 = (getElem 2 1 a + getElem 2 2 a) * getElem 1 1 b
          m3 = getElem 1 1 a * (getElem 1 2 b - getElem 2 2 b)
          m4 = getElem 2 2 a * (getElem 2 1 b - getElem 1 1 b)
          m5 = (getElem 1 1 a + getElem 1 2 a) * getElem 2 2 b
          m6 = (getElem 2 1 a - getElem 1 1 a) * (getElem 1 1 b + getElem 1 2 b)
          m7 = (getElem 1 2 a - getElem 2 2 a) * (getElem 2 1 b + getElem 2 2 b)
strassenRecursive a b n = joinBlocks(m1 + m4 - m5 + m7, m3 + m5, m2 + m4, m1 - m2 + m3 + m6)
    where (a11, a12, a21, a22) = splitBlocks (div n 2) (div n 2) a
          (b11, b12, b21, b22) = splitBlocks (div n 2) (div n 2) b
          m1 = strassenRecursive (a11 + a22) (b11 + b22) (div n 2)
          m2 = strassenRecursive (a21 + a22) b11 (div n 2)
          m3 = strassenRecursive a11 (b12 - b22) (div n 2)
          m4 = strassenRecursive a22 (b21 - b11) (div n 2)
          m5 = strassenRecursive (a11 + a12) b22 (div n 2)
          m6 = strassenRecursive (a21 - a11) (b11 + b12) (div n 2)
          m7 = strassenRecursive (a12 - a22) (b21 + b22) (div n 2)


strassenMultiply :: Matrix Int -> Matrix Int -> Matrix Int
strassenMultiply a b = strassenRecursive a b (nrows a)


main :: IO()
main = do

    let a = fromList 4 4 [2, 1, -1, 1, 13, 1, 3, 4, 2, 1, 2, 3, 67, 8, 5, 4]
    let b = fromList 4 4 [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]

    print( strassenMultiply a b )
    print( a * b )
