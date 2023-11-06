import Text.Printf
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

data Item = Item { nome :: String, preco :: Double } deriving Show

main :: IO ()
main = do
    putStrLn "Bem-vindo à aplicação de construção de prédio!"
    startApp []

startApp :: [Item] -> IO ()
startApp items = do
    putStrLn "\nMenu Principal:"
    putStrLn "1. Adicionar item à lista"
    putStrLn "2. Listar todos os itens disponíveis"
    putStrLn "3. Sair da Aplicação"
    putStrLn "\nEscolha uma opção (1/2/3): "
    choice <- getLine
    case choice of
        "1" -> do
            newItem <- addItem
            startApp (newItem : items)
        "2" -> do
            listItems items
            startApp items
        "3" -> putStrLn "\nSaindo da aplicação."
        _ -> do
            putStrLn "Opção inválida. Tente novamente."
            startApp items

addItem :: IO Item
addItem = do
    putStrLn "\nDigite o nome do item: "
    nomeItem <- getLine
    putStrLn "\nDigite o preço do item (se houver centavos, digite com .e): "
    precoItemStr <- getLine
    let precoItem = fromMaybe 0.0 (readMaybe precoItemStr)
    let item = Item nomeItem precoItem
    putStrLn "\nItem adicionado à lista."
    return item

listItems :: [Item] -> IO ()
listItems items = do
    putStrLn "\nItens disponíveis:"
    mapM_ (\(Item nome preco) -> printf "%s - Preço: R$%.2f\n" nome preco) items