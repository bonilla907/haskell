main = do
    putStrLn("")
    putStrLn("")
    putStrLn("|------------------Menu------------------|")
    putStrLn("1.- Serie Fibonacci")    
    putStrLn("2.- Presentar numeros del 1 al 10") 
    putStrLn("3.- Factorial") 
    putStrLn("4.- Desaparece numeros") 
    putStrLn("5.- Palindromos") 
    putStrLn("6.- Menu calculadora") 
    putStrLn("7.- Bye!!") 
    putStr("Ingresa una opcion: ") 
    opc <- getLine
    menu (read opc)

menu opc = do
    case opc of 
        1 -> fibonacci
        2 -> presentarnumeros 1
        3 -> numfact
        4 -> desaparece
        5 -> palindromos 
        6 -> calculadora
        7 -> print("Terminado")
        _ -> print("Opcion Invalida")


fibonacci = do
    putStr("Ingresa la posicion que deseas: ")
    pos <- getLine
    
    putStr("El resultado es: ")
    print(fib (read pos))
    main

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

presentarnumeros n = do
    if n <= 10
        then do
            print n
            presentarnumeros(n+1)
            
    else do
        main

        
numfact = do
    putStr("Escribe un numero: ")
    fac <- getLine
    let n = read fac :: Int
    let factorial = product[1..n]
    
    putStrLn("El factorial de "++fac++" es: "++show(factorial))
    main


desaparece = do
    arr [0,1,2,3,4,5,6,7,8,9,10]

arr x =
    if null x
        then
            main
        else do
            print(x)
            arr(init x)


palindromos = do
    putStr("Introduce el palindromo: ")
    frase <- getLine
    palindro (frase)
    
palindro frase = do
     if frase == reverse frase
        then do
            putStrLn("Es palindromo")
            main
        else do
            putStrLn("No es palindromo")
            main

calculadora = do
    putStrLn("1.- Suma")
    putStrLn("2.- Resta")
    putStrLn("3.- Multiplicacion")
    putStrLn("4.- Division")
    putStrLn("5.- Salir")
    putStrLn("Selecciona una opcion")
    n <- getLine
    caso (read n)

caso n = do
    case n of
        1 -> suma
        2 -> resta
        3 -> multi
        4 -> division
        5 -> print("Salir")
        _ -> print("Opcion no valida")
        
suma = do
    putStrLn("Ingresa primer numero")
    a <- getLine
    let x = read a::Int
    putStrLn("Ingresa segundo numero")
    b <- getLine
    let y = read b::Int
    let rsuma = x+y
    putStrLn("El resultado de la suma es: "++show (rsuma))
    main

resta = do
    putStrLn("Ingresa primer numero")
    a <- getLine
    let x = read a::Int
    putStrLn("Ingresa segundo numero")
    b <- getLine
    let y = read b::Int
    let rresta = x-y
    putStrLn("El resultado de la resta es: "++show(rresta))
    main
    
multi = do
    putStrLn("Ingresa primer numero")
    a <- getLine
    let x = read a::Int
    putStrLn("Ingresa segundo numero")
    b <- getLine
    let y = read b::Int
    let rmulti = x*y
    putStrLn("El resultado de la multiplicacion es: "++show(rmulti))
    main
    
division = do
    putStrLn("Ingresa primer numero")
    a <- getLine
    let c = read a::Float
    putStrLn("Ingresa segundo numero")
    b <- getLine
    let d = read b::Float
    let rdivision = c/d
    putStrLn("El resultado de la division es: "++show(rdivision))
    main
