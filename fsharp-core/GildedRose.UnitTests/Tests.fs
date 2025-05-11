module GildedRose.UnitTests

open GildedRose
open Xunit
open Swensen.Unquote


let baseItem =
    {Name = "foo"; SellIn = 30; Quality = 30}


[<Fact>]
let ``Regular item sellIn reduces on update`` () =     
    let item = Regular {baseItem with SellIn = 30}
    let updatedItem = processItem item    
    test <@ 29 = updatedItem.SellIn @>
    
[<Fact>]
let ``Regular item quality reduces on update`` () =
    let item = Regular {baseItem with Quality = 10}
    let updatedItem = processItem item    
    test <@ 9 = updatedItem.Quality @>
    
[<Fact>]
let ``Regular item quality does not go below 0`` () =
    let item = Regular {baseItem with Quality = 0}
    let updatedItem = processItem item    
    test <@ 0 = updatedItem.Quality @>
    
[<Fact>]
let ``Regular item quality reduces twice as fast when sellIn is due`` () =
    let item = Regular {baseItem with SellIn = 0; Quality = 10}
    let updatedItem = processItem item
    test <@ 8 = updatedItem.Quality @>
    
[<Fact>]
let ``Item quality cannot be over 50`` () =
    let item = BetterWithAge {baseItem with Quality = 50}
    let updatedItem = processItem item
    test <@ 50 = updatedItem.Quality @>
    
[<Fact>]
let ``Better with age item quality increases on update`` () =    
    let item = BetterWithAge {baseItem with Quality = 10} 
    let updatedItem = processItem item    
    test <@ 11 = updatedItem.Quality @>
    
[<Fact>]
let ``Better with age item quality increases twice as fast after sellin`` () =    
    let item = BetterWithAge {baseItem with Quality = 10; SellIn = 0}
    let updatedItem = processItem item    
    test <@ 12 = updatedItem.Quality @>
    
[<Fact>]
let ``ExactDate item quality increases 1 on update`` () =
    let item = ExactDate {baseItem with Quality = 10}
    let updatedItem = processItem item
    test <@ 11 = updatedItem.Quality @>
    
[<Fact>]
let ``ExactDate item quality increases 2 when 10 days from the date`` () =
    let item = ExactDate {baseItem with Quality = 30}
    let updatedItem = processItem item
    test <@ 32 = updatedItem.Quality @>

[<Fact>]
let ``ExactDate item quality increases 3 when 5 days from the date`` () =
    let item = ExactDate {baseItem with SellIn = 5; Quality = 30}
    let updatedItem = processItem item
    test <@ 33 = updatedItem.Quality @>

[<Fact>]
let ``ExactDate item quality drops to 0 after the date`` () =
    let item = ExactDate {baseItem with SellIn = 0; Quality = 30}
    let updatedItem = processItem item
    test <@ 0 = updatedItem.Quality @>
    
[<Fact>]
let ``ExactDate item sellin reduces on update`` () =
    let item = ExactDate {baseItem with SellIn = 10}
    let updatedItem = processItem item   
    test <@ 9 = updatedItem.SellIn @>

[<Fact>]
let ``Conjured items quality reduces 2 per update`` () =
    let item = Conjured {baseItem with Quality = 10}
    let updatedItem = processItem item
    test <@ 8 = updatedItem.Quality @>
    
[<Fact>]
let ``Legendary items quality doesn't change`` () =
    let item = Legendary {baseItem with Quality = 80}
    let updatedItem = processItem item
    test <@ 80 = updatedItem.Quality @>
    
[<Fact>]
let ``Legendary items SellIn doesn't change`` () =
    let item = Legendary {baseItem with SellIn = 10}
    let updatedItem = processItem item
    test <@ 10 = updatedItem.SellIn @>
    
    
    
