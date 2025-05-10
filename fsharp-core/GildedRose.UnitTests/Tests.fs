module GildedRose.UnitTests

open GildedRose
open Xunit
open Swensen.Unquote


let item =
    {Name = "foo"; SellIn = 30; Quality = 30}


[<Fact>]
let ``Regular item sellIn reduces on update`` () =     
    let item = Regular (RegularItem {item with SellIn = 30})
    let updatedItem = processItem item    
    test <@ 29 = updatedItem.SellIn @>
    
[<Fact>]
let ``Regular item quality reduces on update`` () =
    let item = Regular (RegularItem {item with Quality = 10})
    let updatedItem = processItem item    
    test <@ 9 = updatedItem.Quality @>
    
[<Fact>]
let ``Regular item quality does not go below 0`` () =
    let item = Regular (RegularItem {item with Quality = 0})
    let updatedItem = processItem item    
    test <@ 0 = updatedItem.Quality @>
    
[<Fact>]
let ``Regular item quality reduces twice as fast when sellIn is due`` () =
    let item = Regular(RegularItem {item with SellIn = 0; Quality = 10})
    let updatedItem = processItem item
    test <@ 8 = updatedItem.Quality @>
    
[<Fact>]
let ``Item quality cannot be over 50`` () =
    let item = BetterWithAge (BetterWithAgeItem {item with Quality = 50})
    let updatedItem = processItem item
    test <@ 50 = updatedItem.Quality @>
    
[<Fact>]
let ``Better with age item quality increases on update`` () =    
    let item = BetterWithAge (BetterWithAgeItem {item with Quality = 10}) 
    let updatedItem = processItem item    
    test <@ 11 = updatedItem.Quality @>
    
[<Fact>]
let ``Better with age item quality increases twice as fast after sellin`` () =    
    let item = BetterWithAge (BetterWithAgeItem {Name = "Item"; Quality = 10; SellIn = 0})
    let updatedItem = processItem item    
    test <@ 12 = updatedItem.Quality @>
    
[<Fact>]
let ``ExactDate item quality increases on update`` () =
    let item = ExactDate (ExactDateItem {item with Quality = 10})
    let updatedItem = processItem item
    test <@ 11 = updatedItem.Quality @>
    
[<Fact>]
let ``ExactDate item quality increases twice as fast 10 days from the date`` () =
    let item = ExactDate (ExactDateItem  {Name = "foo"; SellIn = 10; Quality = 30})
    let updatedItem = processItem item
    test <@ 32 = updatedItem.Quality @>

[<Fact>]
let ``ExactDate item quality increases thrice as fast 5 days from the date`` () =
    let item = ExactDate (ExactDateItem {Name = "foo"; SellIn = 5; Quality = 30})
    let updatedItem = processItem item
    test <@ 33 = updatedItem.Quality @>

[<Fact>]
let ``ExactDate item quality drops to 0 after the date`` () =
    let item = ExactDate (ExactDateItem {Name = "foo"; SellIn = 0; Quality = 30})
    let updatedItem = processItem item
    test <@ 0 = updatedItem.Quality @>
    
[<Fact>]
let ``ExactDate item sellin reduces on update`` () =
    let item = ExactDate (ExactDateItem {item with SellIn = 10})
    let updatedItem = processItem item   
    test <@ 9 = updatedItem.SellIn @>

[<Fact>]
let ``Conjured items quality reduces twice as fast`` () =
    let item = Conjured (ConjuredItem {item with Quality = 10})
    let updatedItem = processItem item
    test <@ 8 = updatedItem.Quality @>
    
[<Fact>]
let ``Legendary items quality doesn't change`` () =
    let item = Legendary (LegendaryItem {item with Quality = 80})
    let updatedItem = processItem item
    test <@ 80 = updatedItem.Quality @>
    
    
