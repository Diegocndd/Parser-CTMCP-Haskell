data Arvore = Nulo | No String Arvore Arvore | TriNo String Arvore Arvore Arvore deriving (Show)

stat :: [String] -> (Arvore, [String])
comp :: [String] -> (Arvore, [String])
expr :: [String] -> (Arvore, [String])
term :: [String] -> (Arvore, [String])
fact :: [String] -> (Arvore, [String])
cop :: String -> Bool
eop :: String -> Bool
top :: String -> Bool
getFstTreeTuple :: (Arvore, [String]) -> Arvore
getSndTreeTuple :: (Arvore, [String]) -> [String]
splitList :: [String] -> Int -> [String]
nthelement :: [String] -> Integer -> String

nthelement (x:xs) 0 = x
nthelement (x:xs) n = nthelement xs (n-1)

splitList (x:xs) 0 = (x:xs)
splitList (x:xs) 1 = xs
splitList (x:xs) n = splitList xs (n-1)

isNumber :: String -> Bool

isNumber x =
    case (reads x) :: [(Double, String)] of
      [(_, "")] -> True
      _         -> False

getFstTreeTuple (x,xs) = x
getSndTreeTuple (x,xs) = xs

cop y = (y=="<") || (y==">") || (y=="!=") || (y=="=<") || (y==">=") || (y=="==")

top y = (y=="*") || (y=="/")

eop y = (y=="+") || (y=="-")

comp s1 = let (tree', strings') = expr s1 in
                          case strings' of
                            (t:s3) -> if (cop t)
                                      then ((No t tree' (getFstTreeTuple (comp s3))), (getSndTreeTuple (comp s3)))
                                      else (tree', (t:s3))
                            _  -> (tree', strings')

expr s1 = let (tree', strings') = term s1 in
                          case strings' of
                            (t:s3) -> if (eop t)
                                      then ((No t tree' (getFstTreeTuple (expr s3))), (getSndTreeTuple (expr s3)))
                                      else (tree', (t:s3))
                            _  -> (tree', strings')
                  
term s1 = let (tree', strings') = fact s1 in
                          case strings' of
                            (t:s3) -> if (top t)
                                      then ((No t tree' (getFstTreeTuple (term s3))), (getSndTreeTuple (term s3)))
                                      else (tree', (t:s3))
                            _  -> (tree', strings')
  
fact (t:s2) = ((No t Nulo Nulo), s2)

stat (x:xs) | (x == "while") =
                  let (c, (l:ls)) = comp xs in
                    let (x, sn) = stat ls in
                      ((No "while" c x), sn)
            | (x == "if") =
                  let (c, (l:ls)) = comp xs in
                    let (x1, (y:ys)) = stat ls in
                      let (x2, n) = stat ys in
                        ((TriNo "if" c x1 x2), n)
            | (x == "read") =
                  let ident = (nthelement xs 0) in
                    if ((isNumber ident) == False)
                    then ((No "read" (No ident Nulo Nulo) Nulo), [])
                    else error "Parse error"
            | (x == "write") =
                  let (e, sn) = expr xs in
                    (No "write" e Nulo, sn)
            | (isNumber x == False) =
                let s3 = (splitList xs 1) in
                  let (e, sn) = expr s3 in
                    ((No "assign" (No x Nulo Nulo) e), sn)
            | otherwise = error "Parse error"