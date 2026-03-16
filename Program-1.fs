// Палехов Виктор 6 вариант БАС-2024-1, 4 Лаба
open System

type 't btree =
    | Node of 't * 't btree * 't btree
    | Nil

let rec insert direction value =
    match direction with
    | Nil -> Node(value, Nil, Nil)
    | Node(z, L, R) -> 
        if value < z then 
            Node(z, insert L value, R)
        elif value > z then
            Node(z, L, insert R value)
        else
            Node(z, L, R)

let rec checkDigit ()=  
   let c = Console.ReadLine()
   match Int32.TryParse(c) with
    |(true, c) -> 
        if c < 0 then
            printfn "Введи положительное число!"
            checkDigit()
        else c
    |(false, _) ->
        printfn "Error: ВВЕДИТЕ ЦЕЛОЕ ЧИСЛО!"
        checkDigit()

let createStringWord countTree minDig maxDig =
    let r = Random()
    
    List.init countTree (fun _ -> 
        let len = r.Next(minDig, maxDig)
        String.init len (fun _ -> 
            char(r.Next(97, 123)).ToString()))        //ASCII: 97 - "a", 123 - "z"

let printTree tree =
    let rec printIndent t level =
        match t with
        | Nil -> ()
        | Node(v, l, r) ->
            printIndent r (level + 1)
            printfn "%s%s" (String.replicate level "  ") v
            printIndent l (level + 1)
    printIndent tree 0

let shiftWord str = 
    str |> List.map (fun s -> 
        s |> String.map (fun c -> 
            if c = 'z' then 
                'a'
            else
                char(int c + 1)))

[<EntryPoint>]
let main args = 
    printfn "Бинарное дерево"
    printfn "Введите кол-во элементов в дереве: "
    let count = checkDigit()

    printfn "Введите минимальную длину слова: "
    let minLen = checkDigit()

    printfn "Введите максимальную длину слова: "
    let maxLen = checkDigit()

    let stringWord = createStringWord count minLen maxLen
    printfn "Исходные строки: %A" stringWord

    let tree = stringWord |> List.fold insert Nil
    printfn "\nИсходное дерево:"
    printTree tree

    let stringWordShift = shiftWord stringWord
    printfn "Сдвиг символов: %A" stringWordShift

    let treeShift = stringWordShift |> List.fold insert Nil
    printfn "\nИсходное дерево:"
    printTree treeShift

    0
