{-# HLINT ignore "Use when" #-}
{-# HLINT ignore "Use void" #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Redundant bracket" #-}

-- usado para ignorar erro

import Control.Monad (unless, when)
import Data.Char (isAlpha, toLower)
import Data.List (nub, (\\))
import System.Cmd (system)
import System.IO
import System.Process (callCommand)
import System.Random (randomRIO)

data GameState = GameState
  { palavra :: String,
    palavraAnterior :: String,
    letrasDigitadas :: [Char],
    tentativasInc :: Int,
    primeiraLetra :: Bool,
    ganhouLetraExtra :: Bool,
    mensagem :: String
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

  let initialState = GameState initialpalavra "" [] 0 True False ""
  loopJogo initialState

loopJogo :: GameState -> IO ()
loopJogo gameState = do
  unless (null (mensagem gameState)) $ putStrLn (mensagem gameState)
  let gameState' = gameState {mensagem = ""}
  gameState'' <- checaNovaPalavra gameState'
  displayJogo gameState''

  if acabouJogo gameState'
    then fimJogo gameState'
    else do
      guess <- getTenta
      if guess `elem` letrasDigitadas gameState'
        then do
          limparTela
          putStrLn "Você já digitou essa letra. Tente outra."
          putStrLn "Pressione Enter para continuar..."
          _ <- getLine
          loopJogo gameState'
        else do
          newGameState <- atualizaEstado gameState' guess
          -- Não limpe a tela se a mensagem do 30% estiver sendo exibida ou se a regra secreta da primeira letra estiver em vigor
          unless ((not . null $ mensagem newGameState) || (primeiraLetra newGameState && ganhouLetraExtra newGameState)) limparTela
          when (ganhouLetraExtra newGameState) $ putStrLn "Parabéns! Você acertou uma letra na primeira tentativa e ganhou uma letra extra!\nEsta é a nossa regra secreta do Jogo da Forca!\n"
          loopJogo newGameState

atualizaEstado :: GameState -> Char -> IO GameState
atualizaEstado gameState guess = do
  let novoEstado =
        if guess `elem` letrasDigitadas gameState
          then gameState
          else
            if guess `elem` palavra gameState
              then gameState {letrasDigitadas = guess : letrasDigitadas gameState, primeiraLetra = False, ganhouLetraExtra = primeiraLetra gameState}
              else gameState {letrasDigitadas = guess : letrasDigitadas gameState, tentativasInc = tentativasInc gameState + 1, primeiraLetra = False}
  if umaLetraRestante novoEstado
    then do
      chance <- chanceAleatoria
      if chance
        then do
          novasPalavras <- leituraArq "palavras.txt"
          novaPalavra <- encontraNovaPalavra novasPalavras (letrasDigitadas novoEstado) novoEstado
          let palavraAnterior = palavra novoEstado
          let letrasNaNovaPalavra = filter (`elem` novaPalavra) (letrasDigitadas novoEstado)
          let mensagem = "A palavra foi alterada por uma chance de 30%. A palavra anterior era: " ++ palavraAnterior
          return novoEstado {palavra = novaPalavra, palavraAnterior = palavraAnterior, letrasDigitadas = letrasNaNovaPalavra, mensagem = mensagem}
        else return novoEstado
    else return novoEstado {letrasDigitadas = guess : letrasDigitadas novoEstado, primeiraLetra = False}

checaNovaPalavra :: GameState -> IO GameState
checaNovaPalavra gameState =
  if all (`elem` letrasDigitadas gameState) (palavra gameState)
    then
      if acabouJogo gameState
        then return gameState
        else do
          listaPalavras <- leituraArq "palavras.txt"
          randomIndex <- randomRIO (0, length listaPalavras - 1)
          let novaPalavra = listaPalavras !! randomIndex
          putStrLn "\nUma nova palavra foi selecionada!\n"
          return gameState {palavra = novaPalavra, letrasDigitadas = [], tentativasInc = 0, primeiraLetra = True, ganhouLetraExtra = False}
    else return gameState

displayJogo :: GameState -> IO ()
displayJogo gameState = do
  unless (null (mensagem gameState)) $ putStrLn (mensagem gameState)
  putStrLn $ "Palavra: " ++ getPedaco gameState
  putStrLn $ "\nLetras erradas: " ++ show (tentativasInc gameState)
  putStrLn $ desenhaBoneco (tentativasInc gameState)

acabouJogo :: GameState -> Bool
acabouJogo gameState =
  palavraEscolhida || limiteMax
  where
    palavraEscolhida = all (`elem` letrasDigitadas gameState) (palavra gameState)
    limiteMax = tentativasInc gameState == tentativasMax

umaLetraRestante :: GameState -> Bool
umaLetraRestante gameState = length (filter (`notElem` letrasDigitadas gameState) (nub (filter isAlpha (palavra gameState)))) == 1

chanceAleatoria :: IO Bool
chanceAleatoria = do
  randNum <- randomRIO (0 :: Double, 1 :: Double)
  return (randNum <= 0.3) -- 30% de chance de mudar a palavra

encontraNovaPalavra :: [String] -> [Char] -> GameState -> IO String
encontraNovaPalavra palavras letras gameState = do
  let letrasCorretas = filter (`elem` (palavra gameState)) letras
  let possiveisPalavras = filter (\palavra -> all (`elem` palavra) letrasCorretas) palavras
  if null possiveisPalavras
    then return (palavra gameState)
    else do
      index <- randomRIO (0, length possiveisPalavras - 1)
      return (possiveisPalavras !! index)

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

leituraArq :: FilePath -> IO [String]
leituraArq path = do
  content <- readFile path
  return $ lines content

getPedaco :: GameState -> String
getPedaco gameState = [if c `elem` letrasDigitadas gameState || not (isAlpha c) then c else '_' | c <- palavra gameState]

tentativasMax :: Int
tentativasMax = 6

desenhaBoneco :: Int -> String
desenhaBoneco n
  | n == 0 = ""
  | n == 1 = "  _______\n |      |\n |      0\n |\n |\n |\n_|______\n"
  | n == 2 = "  _______\n |      |\n |      0\n |      |\n |\n |\n_|______\n"
  | n == 3 = "  _______\n |      |\n |      0\n |     /|\n |\n |\n_|______\n"
  | n == 4 = "  _______\n |      |\n |      0\n |     /|\\\n |\n |\n_|______\n"
  | n == 5 = "  _______\n |      |\n |      0\n |     /|\\\n |     /\n |\n_|______\n"
  | n == 6 = "  _______\n |      |\n |      0\n |     /|\\\n |     / \\\n |\n_|______\n"
  | otherwise = "Erro: Numero de tentativas invalido"

limparTela :: IO ()
-- cls para windows
limparTela = callCommand "clear"