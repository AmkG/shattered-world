
module Game.ShatteredWorld.Parse.Token(Token(..), strToTokens) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator
import Data.List
import Data.Char

data Token = TIndent
           | TDedent
           | TNext -- next item / next line or ';'
           | TKeyword String -- keyword
           | TOp String -- operator
           | TString String -- string
           | TNumber Integer -- 
           deriving(Show, Read, Eq, Ord)

strToTokens :: SourceName -> String -> Either ParseError [Token]
strToTokens src = runParser doc [] src . removeEmptyLine where

  removeEmptyLine :: String -> String
  removeEmptyLine = unlines . filter (inv $ all isSpace) . lines
    where inv f x = case f x of True -> False ; False -> True

  doc :: GenParser Char [Integer] [Token]
  doc = do ini <- perhaps anyDent
           body <- many $ try $ do l <- line
                                   return l
           final <- try nonnewlineline
           eof
           let body' = concat body ++ final
           case ini of
             Just a   -> return $ a ++ body'
             Nothing  -> return $ body'

  perhaps p =     (do t <- try p
                      return $ Just t)
              <|> (return Nothing)

  anyDent = do dent <- many $ char ' '
               levels <- getState
               let nowlevel = genericLength dent
               loop levels nowlevel []
     where loop []      nowlevel rv = do setState [nowlevel]
                                         return rv
           loop (l:ls)  nowlevel rv
             | l < nowlevel         = do setState $ nowlevel:l:ls
                                         return $ rv ++ [TIndent]
             | l == nowlevel        = return $ rv ++ [TNext]
             | l > nowlevel         = do setState $ ls
                                         loop ls nowlevel $ [TDedent] ++ rv

  -- a line without a terminating newline
  nonnewlineline = do contents <- many (do rv <- nonspacetoken
                                           many nonnewlinewhitespace
                                           return rv)
                      try $ many nonnewlinewhitespace
                      return contents

  line = do contents <- try $ do contents <- nonnewlineline
                                 newline
                                 dent <- anyDent
                                 return $ contents ++ dent
            return contents

  nonspacetoken = keyword <|> string <|> integer <|> operator
    where keyword = do c <- letter
                       s <- many alphaNum
                       return $ TKeyword $ c:s
                    <?> "keyword"
          string = do char '\"'
                      s <- stringContent
                      return $ TString s
                   <?> "string"
            where stringContent = manyTill stringChar (try $ char '\"')
                  stringChar =     do char '\\'
                                      -- TODO: recognize codes like \n etc.
                                      -- for simplicity just get the character
                                      anyChar
                               <|> anyChar
          integer = do cs <- many1 digit
                       return $ TNumber $ read cs
          operator =     many1 (satisfy isSymbol <|> satisfy isPunctuation)
                     >>= return . TOp
  newline = char '\n' <?> "end of line"
  nonnewlinewhitespace = char ' ' <|> char '\t' <|> char '\r' <?> "whitespace"

