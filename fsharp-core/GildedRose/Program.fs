module GildedRose
open System.Collections.Generic

type Item = { Name: string; SellIn: int; Quality: int }

type ClassifiedItem =
    | Regular of Item
    | BetterWithAge of Item
    | ExactDate of Item
    | Legendary of Item
    | Conjured of Item
    
// this hardcoded classification would come from the datastore
let classifyByName item =
    match item.Name with
    | "Aged Brie" -> BetterWithAge item
    | "Sulfuras, Hand of Ragnaros" -> Legendary item
    | "Backstage passes to a TAFKAL80ETC concert" -> ExactDate item
    | "Conjured Mana Cake" -> Conjured item
    | _ -> Regular item

let reduceQuality reduceBy item =
    { item with Quality = max 0 (item.Quality - reduceBy) }
let increaseQuality increaseBy item =
    { item with Quality = min 50 (item.Quality + increaseBy) }
let reduceSellIn item =
    { item with SellIn = item.SellIn - 1 }    
let isExpired item =
    item.SellIn <= 0

let processItem item =
    
    let processRegularItem item =
        let qualityLoss = if isExpired item then 2 else 1
        item |> reduceQuality qualityLoss |> reduceSellIn

    let processBetterWithAgeItem item =
        let qualityGain = if isExpired item then 2 else 1
        item |> increaseQuality qualityGain |> reduceSellIn

    let processExactDateItem item =
        let daysLeft n = item.SellIn <= n
        match true with
            | _ when isExpired item -> { item with Quality = 0 }
            | _ when daysLeft 5 -> item |> increaseQuality 3
            | _ when daysLeft 10 -> item |> increaseQuality 2
            | _ -> item |> increaseQuality 1
        |> reduceSellIn

    let processLegendaryItem item = item

    let processConjuredItem item =
        let loss = if isExpired item then 4 else 2
        item |> reduceQuality loss |> reduceSellIn
        
    match item with
    | Regular item -> processRegularItem item
    | BetterWithAge item -> processBetterWithAgeItem item
    | ExactDate item -> processExactDateItem item
    | Legendary item -> processLegendaryItem item
    | Conjured item -> processConjuredItem item

let updateItem item =
    item |> classifyByName |> processItem

let updateDay = List.map updateItem

let updatePeriod days initialItems =
    [1 .. days]
    |> List.scan (fun items _ -> updateDay items) initialItems


module Program =
    [<EntryPoint>]   
          
    let main argv =
        printfn "OMGHAI!"
        let Items = new List<Item>()
        Items.Add({Name = "+5 Dexterity Vest"; SellIn = 10; Quality = 20})
        Items.Add({Name = "Aged Brie"; SellIn = 2; Quality = 0})
        Items.Add({Name = "Elixir of the Mongoose"; SellIn = 5; Quality = 7})
        Items.Add({Name = "Sulfuras, Hand of Ragnaros"; SellIn = 0; Quality = 80})
        Items.Add({Name = "Sulfuras, Hand of Ragnaros"; SellIn = -1; Quality = 80})
        Items.Add({Name = "Backstage passes to a TAFKAL80ETC concert"; SellIn = 15; Quality = 20})
        Items.Add({Name = "Backstage passes to a TAFKAL80ETC concert"; SellIn = 10; Quality = 49})
        Items.Add({Name = "Backstage passes to a TAFKAL80ETC concert"; SellIn = 5; Quality = 49})
        Items.Add({Name = "Conjured Mana Cake"; SellIn = 3; Quality = 6})
                
        updatePeriod 30 (Items |> List.ofSeq)
            |> List.iteri (fun day items ->
                printfn "-------- day %d --------" day
                printfn "name, sellIn, quality"
                items
                |> List.iter (fun item ->
                    printfn "%s, %d, %d" item.Name item.SellIn item.Quality)
                printfn "")
        0 