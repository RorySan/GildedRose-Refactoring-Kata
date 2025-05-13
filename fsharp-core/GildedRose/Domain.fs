namespace GildedRose

open System.IO

type Item =
    { Name: string
      SellIn: int
      Quality: int }

[<AutoOpen>]
module ItemOperations =
    let reduceQuality reduceBy item =
        { item with
            Quality = max 0 (item.Quality - reduceBy) }

    let increaseQuality increaseBy item =
        { item with
            Quality = min 50 (item.Quality + increaseBy) }

    let reduceSellIn item = { item with SellIn = item.SellIn - 1 }

    let isExpired item = item.SellIn <= 0


module ItemTypes =
    [<Literal>]
    let Regular = "Regular"
    [<Literal>]
    let BetterWithAge = "BetterWithAge"
    [<Literal>]
    let CountdownSpecial = "CountdownSpecial"
    [<Literal>]
    let Legendary = "Legendary"
    [<Literal>]
    let Conjured = "Conjured"


module Behaviors =
    // todo reducesellin happens always. extract it to the general function and leave this
    // to deal exclusively with quality. This might not be the Behaviors module but the qualityadjustment

    let adjustQualityRegular item =
        let qualityLoss = if isExpired item then 2 else 1
        item |> reduceQuality qualityLoss
    
    let updateRegular item =
        item |> adjustQualityRegular |> reduceSellIn

    let adjustQualityBetterWithAge item =
        let qualityGain = if isExpired item then 2 else 1
        item |> increaseQuality qualityGain
    
    let updateBetterWithAge item =
        item |> adjustQualityBetterWithAge |> reduceSellIn

    let adjustQualityCountdownSpecial item =
        let daysLeft n = item.SellIn <= n

        match true with
        | _ when isExpired item -> { item with Quality = 0 }
        | _ when daysLeft 5 -> item |> increaseQuality 3
        | _ when daysLeft 10 -> item |> increaseQuality 2
        | _ -> item |> increaseQuality 1
        
    let updateCountdownSpecial item =
        item |> adjustQualityCountdownSpecial |> reduceSellIn
        
    let updateLegendary item = item

    let adjustQualityConjured item =
        let applyRegularAdjustmentTwice item =
            item |> adjustQualityRegular
            |> adjustQualityRegular
        
        item
        |> applyRegularAdjustmentTwice
        
    let updateConjured item =
        item |> adjustQualityConjured
        |> reduceSellIn
    
    // todo we can improve this with an [<ItemBehavior>] attribute that autoregisters for better scaling
    let public behaviorMap =
        [ (ItemTypes.Regular, updateRegular)
          (ItemTypes.BetterWithAge, updateBetterWithAge)
          (ItemTypes.CountdownSpecial, updateCountdownSpecial)
          (ItemTypes.Legendary, updateLegendary)
          (ItemTypes.Conjured, updateConjured) ]
        |> Map.ofList    
        
module Update =
    open Behaviors
    let getItemType (itemMap: Map<string, string>) item =
        itemMap.TryFind item.Name
        |> Option.defaultValue ItemTypes.Regular
    
    let private getItemBehavior itemType =
        match Map.tryFind itemType behaviorMap with
        | Some behavior -> behavior
        | None -> adjustQualityRegular // default behavior

    let updateItem (name, itemType) item =
        if item.Name = name then
            getItemBehavior itemType item
        else
            item

    let updateItemWithRules rules item =
        match rules |> List.tryFind (fun (name, _) -> name = item.Name) with
        | Some (_, itemType) -> getItemBehavior itemType item
        | None -> item    

    let updateDay rules = List.map (updateItemWithRules rules)

    let updatePeriod days rules initialItems =
        [ 1..days ] |> List.scan (fun items _ -> updateDay rules items) initialItems


module FileOperations =
    let tryReadFile path =
        try
            File.ReadAllLines(path) |> Ok
        with ex ->
            Error($"Failed to read file: {ex.Message}")
    
    let parseLine (line: string) =
        match line.Split("->") with
        | [| name; itemType |] -> Some(name.Trim(), itemType.Trim())
        | _ -> None
