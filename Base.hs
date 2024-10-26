{-# LANGUAGE OverloadedStrings #-}

module Base where
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text.IO as TIOStrict
import Data.Maybe (listToMaybe)
import System.Random (randomRIO)
import System.IO (openFile, hClose, appendFile, IOMode(ReadMode))
import Control.Monad.IO.Class (liftIO)
import Web.Scotty


---------- FUNCIONAMENTO DO JOGO
-- Auxiliar
verificaInt :: Int -> Bool
verificaInt i = if(i == 1) then False else True

-- Compara uma letra de chute com a palavra real
letraPalavra :: Char -> String -> Int -> Int -> Int
letraPalavra letra palavra pos posChute
    | length palavra <= pos = 0
    | (palavra !! pos) == letra && pos == posChute = 2
    | elem letra palavra = 1
    | otherwise = 0

-- Compara uma palavra chute com a resposta 
comparaPalavra :: String -> String -> [Int]
comparaPalavra pChute pReal = zipWith (\pos c1 -> letraPalavra c1 pReal pos pos) [0..] pChute

-- verifica se o vetor resultante da comparacao esta inteiramente correto ou nao
verificaVitoria :: [Int] -> Bool
verificaVitoria l = if(length(filter verificaInt l) == 0) then True else False


---------- MANIPULAÇÃO DE ARQUIVO
-- Inserir uma palavra em um arquivo
inserirPalavra :: FilePath -> String -> IO ()
inserirPalavra filePath palavra = do
    appendFile filePath (palavra ++ "\n")

-- Sobrescrever uma palavra em um arquivo
escreverPalavra :: FilePath -> String -> IO ()
escreverPalavra filePath palavra = do
    writeFile filePath (palavra ++ "\n")

-- Ler primeira palavra de um arquivo
lerPalavra :: FilePath -> IO T.Text
lerPalavra filePath = do
    handle <- openFile filePath ReadMode 
    conteudo <- TIOStrict.hGetContents handle  
    hClose handle  
    let palavras = T.words (T.fromStrict conteudo)  
    return $ if null palavras                       
             then T.empty                           
             else case listToMaybe palavras of
                    Just p  -> p
                    Nothing -> T.empty                   

-- Ler lista de palavra de um arquivo
lerPalavras :: FilePath -> IO [T.Text]
lerPalavras filePath = do
    handle <- openFile filePath ReadMode  
    conteudo <- TIOStrict.hGetContents handle  
    hClose handle  
    let palavras = T.words (T.fromStrict conteudo)  
    return $ if null palavras                       
             then [T.empty]                         
             else palavras                          

-- Função que retorna uma palavra aleatória como Text
fetchRandomWord :: FilePath -> IO T.Text
fetchRandomWord filePath = do
    contents <- TIO.readFile filePath
    let words = T.lines contents
    if null words
        then return "Nenhuma palavra disponível."
        else do
            index <- randomRIO (0, length words - 1)
            return (words !! index)

-- Função que sorteia uma palavra e a escreve em um arquivo
sorteiaPalavra :: ActionM ()
sorteiaPalavra = do
    palavra <- liftIO $ fetchRandomWord "data/palavras.txt"  
    let palavraString = T.unpack palavra 

    liftIO $ escreverPalavra "data/sorteada.txt" palavraString

------- TESTE RENDER

-- Função que converte a estrutura do jogo em uma string HTML
renderJogoHtml :: [[(Char, Int)]] -> T.Text
renderJogoHtml jogo = T.concat [
    "<div class=\"jogo\">",
    T.concat [renderLinha linha | linha <- jogo],
    "</div>"
    ]

-- Função que renderiza uma linha do jogo
renderLinha :: [(Char, Int)] -> T.Text
renderLinha linha = 
    "<div class=\"palavra\">" <> T.concat [renderCelula (letra, cor) | (letra, cor) <- linha] <> "</div>"

-- Função que renderiza uma célula do jogo
renderCelula :: (Char, Int) -> T.Text
renderCelula (letra, cor) =
    let corClasse = case cor of
                      0 -> "gray"
                      1 -> "yellow"
                      2 -> "green"
    in "<div class=\"letter-box " <> T.pack corClasse <> "\">" <> T.pack [letra] <> "</div>"

