module Matrix (
    matMult,
    matMult',
    genMatMult
    ) where

{- 
    We complete our introduction to Haskell arrays with the familiar example of matrix multiplication, 
    taking advantage of overloading to define a fairly general function. Since only multiplication and 
    addition on the element type of the matrices is involved, we get a function that multiplies matrices 
    of any numeric type unless we try hard not to. Additionally, if we are careful to apply only (!) 
    and the operations of Ix to indices, we get genericity over index types, and in fact, the four row 
    and column index types need not all be the same. For simplicity, however, we require that the left 
    column indices and right row indices be of the same type, and moreover, that the bounds be equal: 
-}

matMult         :: (Ix a, Ix b, Ix c, Num d) =>
                   Array (a,b) d -> Array (b,c) d -> Array (a,c) d
matMult x y     =  array resultBounds
                         [((i,j), sum [x!(i,k) * y!(k,j) | k <- range (lj,uj)])
                                       | i <- range (li,ui),
                                         j <- range (lj',uj') ]
        where ((li,lj),(ui,uj))         =  bounds x
              ((li',lj'),(ui',uj'))     =  bounds y
              resultBounds
                | (lj,uj)==(li',ui')    =  ((li,lj'),(ui,uj'))
                | otherwise             = error "matMult: incompatible bounds"

{- 
    As an aside, we can also define matMult using accumArray, 
    resulting in a presentation that more closely resembles the 
    usual formulation in an imperative language: 
-}

matMult'         :: (Ix a, Ix b, Ix c, Num d) =>
                   Array (a,b) d -> Array (b,c) d -> Array (a,c) d
matMult' x y     =  accumArray (+) 0 resultBounds
                              [((i,j), x!(i,k) * y!(k,j))
                                      | i <- range (li,ui),
                                        j <- range (lj',uj'),
                                        k <- range (lj,uj)  ]
        where ((li,lj),(ui,uj))         =  bounds x
              ((li',lj'),(ui',uj'))     =  bounds y
              resultBounds
                | (lj,uj)==(li',ui')    =  ((li,lj'),(ui,uj'))
                | otherwise             = error "matMult: incompatible bounds"

{- We can generalize further by making the function higher-order, simply replacing sum and (*) by functional parameters: -}

genMatMult      :: (Ix a, Ix b, Ix c) =>
                   ([f] -> g) -> (d -> e -> f) ->
                   Array (a,b) d -> Array (b,c) e -> Array (a,c) g
genMatMult sum' star x y  =
      array resultBounds
            [((i,j), sum' [x!(i,k) `star` y!(k,j) | k <- range (lj,uj)])
                                 | i <- range (li,ui),
                                   j <- range (lj',uj') ]
        where ((li,lj),(ui,uj))         =  bounds x
              ((li',lj'),(ui',uj'))     =  bounds y
              resultBounds
                | (lj,uj)==(li',ui')    =  ((li,lj'),(ui,uj'))
                | otherwise             = error "matMult: incompatible bounds"

{- APL fans will recognize the usefulness of functions like the following: -}

genMatMult maximum (-)
genMatMult and (==)

{- 
    With the first of these, the arguments are numeric matrices, and the (i,j)-th 
    element of the result is the maximum difference between corresponding elements 
    of the i-th row and j-th column of the inputs. In the second case, the arguments 
    are matrices of any equality type, and the result is a Boolean matrix in which 
    element (i,j) is True if and only if the i-th row of the first argument and 
    j-th column of the second are equal as vectors. 

    Notice that the element types of genMatMult need not be the same, but merely 
    appropriate for the function parameter star. We could generalize still further 
    by dropping the requirement that the first column index and second row index 
    types be the same; clearly, two matrices could be considered conformable as 
    long as the lengths of the columns of the first and the rows of the second are 
    equal. The reader may wish to derive this still more general version. (Hint: 
    Use the index operation to determine the lengths.) 
-}
