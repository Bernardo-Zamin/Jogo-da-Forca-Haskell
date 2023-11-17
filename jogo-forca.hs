{-# HLINT ignore "Use when" #-}
{-# HLINT ignore "Use void" #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- usado para ignorar erro

import Control.Monad (when)
import Data.Char (isAlpha, toLower)
import Data.List ((\\))
import System.Cmd (system)
import System.IO
import System.Process (callCommand)
import System.Random (randomRIO)

data GameState = GameState
  { palavra :: String,
    letrasDigitadas :: [Char],
    tentativasInc :: Int,
    primeiraLetra :: Bool,
    ganhouLetraExtra :: Bool
  }

main :: IO ()
main = do
  putStrLn "|-----------------------------|"
  putStrLn "| Bem-vindo ao Jogo da Forca! |"
  putStrLn "|-----------------------------|\n"
  jogarJogo
  putStrLn "Obrigado por jogar!"

jogarJogo :: IO ()
jogarJogo = do
  listaPalavras <- leituraArq "palavras.txt"

  randomIndex <- randomRIO (0, length listaPalavras - 1)
  let initialpalavra = listaPalavras !! randomIndex

  let initialState = GameState initialpalavra [] 0 True False

  loopJogo initialState

loopJogo :: GameState -> IO ()
loopJogo gameState = do
  limparTela
  gameState' <- checaNovaPalavra gameState
  displayJogo gameState'

  if acabouJogo gameState'
    then fimJogo gameState'
    else do
      guess <- getTenta
      if guess `elem` letrasDigitadas gameState'
        then do
          putStrLn "Você já digitou essa letra. Tente outra."
          putStrLn "Pressione Enter para continuar..."
          _ <- getLine
          loopJogo gameState'
        else do
          let newGameState = atualizaEstado gameState' guess
          when (ganhouLetraExtra newGameState) $ limparTela >> putStrLn "Parabéns! Você acertou a primeira letra e ganhou uma letra extra!\nEsta é a nossa regra secreta do Jogo da Forca!\n"
          loopJogo newGameState

checaNovaPalavra :: GameState -> IO GameState
checaNovaPalavra gameState =
  if all (`elem` letrasDigitadas gameState) (palavra gameState)
    then do
      listaPalavras <- leituraArq "palavras.txt"
      randomIndex <- randomRIO (0, length listaPalavras - 1)
      let novaPalavra = listaPalavras !! randomIndex
      return gameState {palavra = novaPalavra, letrasDigitadas = [], tentativasInc = 0, primeiraLetra = True, ganhouLetraExtra = False}
    else return gameState

displayJogo :: GameState -> IO ()
displayJogo gameState = do
  putStrLn $ "Palavra: " ++ getPedaco gameState
  putStrLn $ "\nLetras erradas: " ++ show (tentativasInc gameState)
  putStrLn $ desenhaBoneco (tentativasInc gameState)

acabouJogo :: GameState -> Bool
acabouJogo gameState =
  palavraEscolhida || limiteMax
  where
    palavraEscolhida = all (`elem` letrasDigitadas gameState) (palavra gameState)
    limiteMax = tentativasInc gameState == tentativasMax

fimJogo :: GameState -> IO ()
fimJogo gameState = do
  if all (`elem` letrasDigitadas gameState) (palavra gameState)
    then putStrLn "Parabéns! Você venceu!"
    else putStrLn $ "Você perdeu. A palavra era: " ++ palavra gameState

  putStrLn "Deseja jogar novamente? (s/n)"
  jogarDnv <- getLine
  if null jogarDnv
    then fimJogo gameState
    else
      if map toLower jogarDnv == "s"
        then jogarJogo
        else return ()

getTenta :: IO Char
getTenta = do
  putStrLn "Digite uma letra: "
  guess <- getLine
  if length guess == 1 && isAlpha (head guess)
    then return $ toLower (head guess)
    else do
      putStrLn "Entrada invalida. Por favor, digite apenas uma unica letra."
      getTenta

atualizaEstado :: GameState -> Char -> GameState
atualizaEstado gameState guess
  | guess `elem` letrasDigitadas gameState = gameState
  | guess `elem` palavra gameState =
      if primeiraLetra gameState
        then
          let newLetrasDigitadas = guess : take 1 (palavra gameState \\ (guess : letrasDigitadas gameState)) ++ letrasDigitadas gameState
           in gameState {letrasDigitadas = newLetrasDigitadas, primeiraLetra = False, ganhouLetraExtra = True}
        else gameState {letrasDigitadas = guess : letrasDigitadas gameState, primeiraLetra = False}
  | otherwise = gameState {letrasDigitadas = guess : letrasDigitadas gameState, tentativasInc = tentativasInc gameState + 1, primeiraLetra = False}

leituraArq :: FilePath -> IO [String]
leituraArq path = do
  content <- readFile path
  return $ lines content

getPedaco :: GameState -> String
getPedaco gameState = [if c `elem` letrasDigitadas gameState || not (isAlpha c) then c else '_' | c <- palavra gameState]

tentativasMax :: Int
tentativasMax = 7

desenhaBoneco :: Int -> String
desenhaBoneco n
  | n == 0 = ""
  | n == 1 = "  _______\n |      |\n |\n |\n |\n |\n_|______\n"
  | n == 2 = "  _______\n |      |\n |      0\n |\n |\n |\n_|______\n"
  | n == 3 = "  _______\n |      |\n |      0\n |      |\n |\n |\n_|______\n"
  | n == 4 = "  _______\n |      |\n |      0\n |     /|\n |\n |\n_|______\n"
  | n == 5 = "  _______\n |      |\n |      0\n |     /|\\\n |\n |\n_|______\n"
  | n == 6 = "  _______\n |      |\n |      0\n |     /|\\\n |     /\n |\n_|______\n"
  | n == 7 = "  _______\n |      |\n |      0\n |     /|\\\n |     / \\\n |\n_|______\n"
  | otherwise = "Erro: Numero de tentativas invalido"

limparTela :: IO ()
limparTela = callCommand "clear"