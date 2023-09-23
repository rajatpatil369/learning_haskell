-- main = putStr "Hello, world!\n"

greetuser :: String -> IO ()
greetuser greeting = do {
    putStrLn "Hii! What's your name?";
    putStr ">>> ";
    name <- getLine;
    putStrLn ("Hi " ++ name ++ "! " ++ greeting);
}

main = do
    greetuser "Welcome!"
    greetuser "Welcome!"
