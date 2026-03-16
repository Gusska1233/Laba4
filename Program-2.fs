// Палехов Виктор 6 вариант БАС-2024-1, 4 Лаба, 2 задание
open System

type 't btree =
    Node of 't * 't btree * 't btree
    | Nil

let rec insert direction x =
    match direction with
    | Nil -> Node(x, Nil, Nil)
    | Node(z, L, R) -> 
        if x < z then Node(z, insert L x, R)
        else Node(z, L, insert R x)

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

let createDigit countTree =
    let r = new Random()
    List.init countTree (fun _ -> 
        r.Next(1, 15))

let printTree tree =
    let rec printIndent t level =
        match t with
        | Nil -> ()
        | Node(v, l, r) ->
            printIndent r (level + 1)
            printfn "%s%A" (String.replicate level "  ") v
            printIndent l (level + 1)
    printIndent tree 0

let isLeaf = function
    | Nil -> false  // Пустое дерево - не лист
    | Node(_, Nil, Nil) -> true  // Есть значение, но нет потомков
    | _ -> false

let nodesWithTwoListya tree =
    let rec collect tree acc =
        match tree with
        | Nil -> acc
        | Node(value, left, right) ->
            let newAcc = 
                if isLeaf left && isLeaf right then 
                    value :: acc
                else acc
            collect left (collect right newAcc)
    collect tree [] |> List.rev

[<EntryPoint>]
let main args = 
    printfn "Бинарное дерево"
    printfn "Введите кол-во элементов в дереве: "
    let count = checkDigit()

    let digits = createDigit count
    printfn "Сгенерированные числа: %A" digits

    let tree = digits |> List.fold insert Nil
    printfn "Исходное дерево:"
    printTree tree

    let listUzel = nodesWithTwoListya tree
    printfn "Список узлов с 2 листами: %A" listUzel

    0
