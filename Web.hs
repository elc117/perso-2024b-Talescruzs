{-# LANGUAGE OverloadedStrings #-}

module Web where
import Base 
import Web.Scotty
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (listToMaybe)


main :: IO ()
main = scotty 3000 $ do
    get "/" $ do
        palavra <- liftIO $ lerPalavras "data/sorteada.txt"

        let palavraString = case listToMaybe palavra of
                        Just p  -> T.unpack p
                        Nothing -> "" 

        if palavraString == "" then do
            sorteada <- liftIO $ fetchRandomWord "data/palavras.txt"  -- Chama a função e obtém o nome
            let sorteadaString = T.unpack sorteada
            liftIO $ escreverPalavra "data/sorteada.txt" sorteadaString
            
            redirect "/"
            
        else do
            atual <- liftIO $ lerPalavra "data/sorteada.txt" 
            chutes <- liftIO $ lerPalavras "data/chutes.txt"

            let atualString = T.unpack atual
            let chutesString = map T.unpack chutes

            let comparacoes = map (\x -> comparaPalavra x atualString) chutesString
            let final = map (\(comp, chute) -> zip chute comp) (zip comparacoes chutesString)

            htmlContent <- liftIO $ TIO.readFile "static/index.html" 
            let jogoHtml = renderJogoHtml final

            let htmlWithUser = T.replace "{{jogo}}" (jogoHtml) htmlContent
            html htmlWithUser

    post "/chute" $ do
        palavra <- param "palavra"
        palavraSorteada <- liftIO $ lerPalavra "data/sorteada.txt"

        let palavraString = T.unpack palavra        
        let palavraSorteadaString = T.unpack palavraSorteada

        liftIO $ inserirPalavra "data/chutes.txt" palavraString
        chutes <- liftIO $ lerPalavras "data/chutes.txt"

        if palavraString == palavraSorteadaString then do
            liftIO $ escreverPalavra "data/sorteada.txt" ""
            liftIO $ escreverPalavra "data/chutes.txt" ""
            redirect "/venceu"

        else if((length chutes) >= 5) then do
            liftIO $ escreverPalavra "data/sorteada.txt" ""
            liftIO $ escreverPalavra "data/chutes.txt" ""
            redirect "/perdeu"

        else do
            redirect "/"

    get "/venceu" $ do
        htmlContent <- liftIO $ TIO.readFile "static/vitoria.html"
        html htmlContent

    get "/perdeu" $ do
        htmlContent <- liftIO $ TIO.readFile "static/derrota.html"
        html htmlContent