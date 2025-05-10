module GildedRose

open System.Collections.Generic

type Item = { Name: string; SellIn: int; Quality: int }
type Regular = RegularItem of Item
type BetterWithAge = BetterWithAgeItem of Item
type ExactDate = ExactDateItem of Item
type Legendary = LegendaryItem of Item
type Conjured = ConjuredItem of Item

type ClassifiedItem =
    | Regular of Regular
    | BetterWithAge of BetterWithAge
    | ExactDate of ExactDate
    | Legendary of Legendary
    | Conjured of Conjured
    
let classifyByName item =
    match item.Name with
    | "Aged Brie" -> BetterWithAge (BetterWithAgeItem item)
    | "Sulfuras, Hand of Ragnaros" -> Legendary (LegendaryItem item)
    | "Backstage passes to a TAFKAL80ETC concert" -> ExactDate (ExactDateItem item)
    | "Conjured Mana Cake" -> Conjured (ConjuredItem item)
    | _ -> Regular (RegularItem item)

let pastDue item =
    item.SellIn < 0

let reduceQuality reduceBy item =
    { item with Quality = max 0 (item.Quality - reduceBy)}
let increaseQuality increaseBy item =
    { item with Quality = min 50 (item.Quality + increaseBy) }
    
let reduceSellin item =
    { item with SellIn = item.SellIn - 1 }       
   
let processRegularItem (RegularItem item) =
    match item with
    | item when item.SellIn <= 0 -> item |> reduceQuality 2
    | _ -> item |> reduceQuality 1
    |> reduceSellin
    
let processBetterWithAgeItem (BetterWithAgeItem item) =
    match item with
    | item when item.SellIn <= 0 -> item |> increaseQuality 2
    | _ -> item |> increaseQuality 1    
    |> reduceSellin
    
let processExactDateItem (ExactDateItem item) =
    match item with
    | item when item.SellIn <= 0 -> {item with Quality = 0}    
    | item when item.SellIn <= 5 -> item |> increaseQuality 3
    | item when item.SellIn <= 10 -> item |> increaseQuality 2    
    | item -> item |> increaseQuality 1
    |> reduceSellin
    
let processLegendaryItem (LegendaryItem item) =
    item
    
let processConjuredItem (ConjuredItem item) =
    item
    |> reduceQuality 2
    |> reduceSellin
 
let processItem item =
    match item with
    | Regular item ->
        processRegularItem item
    | BetterWithAge item ->
        processBetterWithAgeItem item        
    | ExactDate item ->
        processExactDateItem item        
    | Legendary item ->
        processLegendaryItem item
    | Conjured item ->
        processConjuredItem item    

let updateDay (items : Item list) =
    items
    |> List.map classifyByName
    |> List.map processItem
    
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
                
        updatePeriod 30 (Items |> Seq.toList)
            |> List.iteri (fun day items ->
                printfn "-------- day %d --------" day
                printfn "name, sellIn, quality"
                items
                |> List.iter (fun item ->
                    printfn "%s, %d, %d" item.Name item.SellIn item.Quality)
                printfn "")
        0 