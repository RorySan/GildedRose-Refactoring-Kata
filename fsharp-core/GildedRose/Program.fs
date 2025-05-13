namespace GildedRose
open FileOperations

open System.Collections.Generic

module Program =
    open Update
    [<EntryPoint>]         
    let main argv =
        let rules =
            match tryReadFile "ItemDb.txt" with
            | Ok lines ->
                lines
                |> Array.choose parseLine
                |> List.ofArray
            | Error msg ->                
                printfn $"%s{msg}"
                []
                
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
                
        updatePeriod 30 rules (Items |> List.ofSeq)
            |> List.iteri (fun day items ->
                printfn "-------- day %d --------" day
                printfn "name, sellIn, quality"
                items
                |> List.iter (fun item ->
                    printfn "%s, %d, %d" item.Name item.SellIn item.Quality)
                printfn "")
        0