module GildedRose.UnitTests

open GildedRose
open Xunit
open Swensen.Unquote

[<Fact>]
let ``Regular item sellIn reduces on update`` () =     
    let items = [{Name = "foo"; SellIn = 0; Quality = 0}]
    let updatedItem = updateQuality items |> Seq.head
    
    test <@ -1 = updatedItem.SellIn @>
    
[<Fact>]
let ``Regular item quality reduces on update`` () =
    let items = [{Name = "foo"; SellIn = 0; Quality = 10}]
    let updatedItem = updateQuality items |> Seq.head    
    test <@ 9 = updatedItem.Quality @>