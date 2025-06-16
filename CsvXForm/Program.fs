// This code reads a vertical CSV file, transforms it into a structured format, and displays it in a horizontal table format.

module Models =

    type LineTax =
        { Jurisdiction: string
          Amount: decimal }

    type Item =
        { SKU: string
          Quantity: int
          Price: decimal
          LineSubTotal: decimal
          LineTaxes: LineTax list
          LineTaxTotal: decimal
          LineTotal: decimal }

    type Customer =
        { Id: string
          Name: string
          Street: string
          State: string
          City: string
          Zip: string }

    type DeliveryAddress =
        { Name: string
          Street: string
          State: string
          City: string
          Zip: string }

    type Order =
        { OrderID: string
          OrderDate: System.DateOnly
          Customer: Customer
          DeliveryAddress: DeliveryAddress
          Items: Item list
          TotalDue: decimal
          InitialDue: decimal
          Terms: string
          Notes: string }

module CsvXForm =
    open Models
    open FSharp.Data
    open System
    open System.Text.RegularExpressions

    // Load CSV file dynamically
    let loadCsv (filePath: string) =
        CsvFile.Load(filePath, hasHeaders = true)

    let readVerticalCsv (filePath: string) : Order list =
        let csv = loadCsv filePath
        let headers = csv.Headers |> Option.defaultValue [||]
        let orderCount = headers.Length - 1 // Exclude "Field" column
        let rows = csv.Rows |> Seq.toList

        // Initialize orders
        let orders =
            Array.init orderCount (fun _ ->
                { OrderID = ""
                  OrderDate = DateOnly.MinValue
                  Customer =
                    { Id = ""
                      Name = ""
                      Street = ""
                      State = ""
                      City = ""
                      Zip = "" }
                  DeliveryAddress =
                    { Name = ""
                      Street = ""
                      State = ""
                      City = ""
                      Zip = "" }
                  Items = []
                  TotalDue = 0M
                  InitialDue = 0M
                  Terms = ""
                  Notes = "" })

        // Parse rows
        rows
        |> List.iter (fun row ->
            let rowValues = row.Columns
            let fieldName = rowValues.[0] // The "Field" column

            for i in 0 .. orderCount - 1 do
                if i + 1 < rowValues.Length then
                    let value = rowValues.[i + 1]

                    if not (String.IsNullOrEmpty value) then
                        let order = orders.[i]

                        match fieldName with
                        | "OrderID" -> orders.[i] <- { order with OrderID = value }
                        | "OrderDate" ->
                            orders.[i] <-
                                { order with
                                    OrderDate = DateOnly.Parse(value) }
                        | "Customer.Id" ->
                            orders.[i] <-
                                { order with
                                    Customer = { order.Customer with Id = value } }
                        | "Customer.Name" ->
                            orders.[i] <-
                                { order with
                                    Customer = { order.Customer with Name = value } }
                        | "Customer.Street" ->
                            orders.[i] <-
                                { order with
                                    Customer = { order.Customer with Street = value } }
                        | "Customer.State" ->
                            orders.[i] <-
                                { order with
                                    Customer = { order.Customer with State = value } }
                        | "Customer.City" ->
                            orders.[i] <-
                                { order with
                                    Customer = { order.Customer with City = value } }
                        | "Customer.Zip" ->
                            orders.[i] <-
                                { order with
                                    Customer = { order.Customer with Zip = value } }
                        | "DeliveryAddress.Name" ->
                            orders.[i] <-
                                { order with
                                    DeliveryAddress =
                                        { order.DeliveryAddress with
                                            Name = value } }
                        | "DeliveryAddress.Address" ->
                            orders.[i] <-
                                { order with
                                    DeliveryAddress =
                                        { order.DeliveryAddress with
                                            Street = value } }
                        | "DeliveryAddress.State" ->
                            orders.[i] <-
                                { order with
                                    DeliveryAddress =
                                        { order.DeliveryAddress with
                                            State = value } }
                        | "DeliveryAddress.City" ->
                            orders.[i] <-
                                { order with
                                    DeliveryAddress =
                                        { order.DeliveryAddress with
                                            City = value } }
                        | "DeliveryAddress.Zip" ->
                            orders.[i] <-
                                { order with
                                    DeliveryAddress =
                                        { order.DeliveryAddress with
                                            Zip = value } }
                        | "TotalDue" -> orders.[i] <- { order with TotalDue = decimal value }
                        | "InitialDue" ->
                            orders.[i] <-
                                { order with
                                    InitialDue = decimal value }
                        | "Terms" -> orders.[i] <- { order with Terms = value }
                        | "Notes" -> orders.[i] <- { order with Notes = value }
                        | field when Regex.IsMatch(field, @"Item\[\d+\]\.") ->
                            let itemMatch = Regex.Match(field, @"Item\[(\d+)\]\.(.+)")
                            let itemIndex = int itemMatch.Groups.[1].Value
                            let itemField = itemMatch.Groups.[2].Value

                            // Ensure item exists
                            while orders.[i].Items.Length <= itemIndex do
                                orders.[i] <-
                                    { order with
                                        Items =
                                            orders.[i].Items
                                            @ [ { SKU = ""
                                                  Quantity = 0
                                                  Price = 0M
                                                  LineSubTotal = 0M
                                                  LineTaxes = []
                                                  LineTaxTotal = 0M
                                                  LineTotal = 0M } ] }

                            let item = orders.[i].Items.[itemIndex]

                            match itemField with
                            | "SKU" ->
                                orders.[i] <-
                                    { order with
                                        Items =
                                            orders.[i].Items
                                            |> List.mapi (fun j it ->
                                                if j = itemIndex then { it with SKU = value } else it) }
                            | "Quantity" ->
                                orders.[i] <-
                                    { order with
                                        Items =
                                            orders.[i].Items
                                            |> List.mapi (fun j it ->
                                                if j = itemIndex then
                                                    { it with Quantity = int value }
                                                else
                                                    it) }
                            | "Price" ->
                                orders.[i] <-
                                    { order with
                                        Items =
                                            orders.[i].Items
                                            |> List.mapi (fun j it ->
                                                if j = itemIndex then
                                                    { it with Price = decimal value }
                                                else
                                                    it) }
                            | "LineSubTotal" ->
                                orders.[i] <-
                                    { order with
                                        Items =
                                            orders.[i].Items
                                            |> List.mapi (fun j it ->
                                                if j = itemIndex then
                                                    { it with LineSubTotal = decimal value }
                                                else
                                                    it) }
                            | "LineTaxTotal" ->
                                orders.[i] <-
                                    { order with
                                        Items =
                                            orders.[i].Items
                                            |> List.mapi (fun j it ->
                                                if j = itemIndex then
                                                    { it with LineTaxTotal = decimal value }
                                                else
                                                    it) }
                            | "LineTotal" ->
                                orders.[i] <-
                                    { order with
                                        Items =
                                            orders.[i].Items
                                            |> List.mapi (fun j it ->
                                                if j = itemIndex then
                                                    { it with LineTotal = decimal value }
                                                else
                                                    it) }
                            | field when Regex.IsMatch(field, @"LineTax\[\d+\]\.") ->
                                let taxMatch = Regex.Match(field, @"LineTax\[(\d+)\]\.(.+)")
                                let taxIndex = int taxMatch.Groups.[1].Value
                                let taxField = taxMatch.Groups.[2].Value

                                // Ensure tax exists
                                let updatedItem =
                                    if item.LineTaxes.Length <= taxIndex then
                                        { item with
                                            LineTaxes =
                                                item.LineTaxes
                                                @ List.replicate
                                                    (taxIndex - item.LineTaxes.Length + 1)
                                                    { Jurisdiction = ""; Amount = 0M } }
                                    else
                                        item

                                let updatedTax =
                                    match taxField with
                                    | "Jurisdiction" ->
                                        { updatedItem.LineTaxes.[taxIndex] with
                                            Jurisdiction = value }
                                    | "Amount" ->
                                        { updatedItem.LineTaxes.[taxIndex] with
                                            Amount = decimal value }
                                    | _ -> updatedItem.LineTaxes.[taxIndex]

                                orders.[i] <-
                                    { order with
                                        Items =
                                            orders.[i].Items
                                            |> List.mapi (fun j it ->
                                                if j = itemIndex then
                                                    { updatedItem with
                                                        LineTaxes =
                                                            updatedItem.LineTaxes
                                                            |> List.mapi (fun k t ->
                                                                if k = taxIndex then updatedTax else t) }
                                                else
                                                    it) }
                            | _ -> ()
                        | _ -> ())

        orders |> Array.toList


    let displayHorizontalTable (orders: Order list) =
        // Collect all unique field names
        let fieldNames =
            orders
            |> List.collect (fun order ->
                [ yield "OrderID"
                  yield "OrderDate"
                  yield "Customer.Id"
                  yield "Customer.Name"
                  yield "Customer.Street"
                  yield "Customer.State"
                  yield "Customer.City"
                  yield "Customer.Zip"
                  yield "DeliveryAddress.Name"
                  yield "DeliveryAddress.Street"
                  yield "DeliveryAddress.State"
                  yield "DeliveryAddress.City"
                  yield "DeliveryAddress.Zip"
                  for i in 0 .. order.Items.Length - 1 do
                      yield sprintf "Item[%d].SKU" i
                      yield sprintf "Item[%d].Quantity" i
                      yield sprintf "Item[%d].Price" i
                      yield sprintf "Item[%d].LineSubTotal" i

                      for j in 0 .. order.Items.[i].LineTaxes.Length - 1 do
                          yield sprintf "Item[%d].LineTax[%d].Jurisdiction" i j
                          yield sprintf "Item[%d].LineTax[%d].Amount" i j

                      yield sprintf "Item[%d].LineTaxTotal" i
                      yield sprintf "Item[%d].LineTotal" i
                  yield "TotalDue"
                  yield "InitialDue"
                  yield "Terms"
                  yield "Notes" ])
            |> List.distinct
            |> List.sort

        // Print header
        printf "Field\t"
        orders |> List.iteri (fun i _ -> printf "Order%d\t" (i + 1))
        printfn ""

        // Print rows
        for field in fieldNames do
            printf "%s\t" field

            for order in orders do
                let value =
                    match field with
                    | "OrderID" -> order.OrderID
                    | "OrderDate" -> order.OrderDate.ToString()
                    | "Customer.Id" -> order.Customer.Id
                    | "Customer.Name" -> order.Customer.Name
                    | "Customer.Street" -> order.Customer.Street
                    | "Customer.State" -> order.Customer.State
                    | "Customer.City" -> order.Customer.City
                    | "Customer.Zip" -> order.Customer.Zip
                    | "DeliveryAddress.Name" -> order.DeliveryAddress.Name
                    | "DeliveryAddress.Street" -> order.DeliveryAddress.Street
                    | "DeliveryAddress.State" -> order.DeliveryAddress.State
                    | "DeliveryAddress.City" -> order.DeliveryAddress.City
                    | "DeliveryAddress.Zip" -> order.DeliveryAddress.Zip
                    | "TotalDue" -> order.TotalDue.ToString("F2")
                    | "InitialDue" -> order.InitialDue.ToString("F2")
                    | "Terms" -> order.Terms
                    | "Notes" -> order.Notes
                    | field when Regex.IsMatch(field, @"Item\[\d+\]\.") ->
                        let itemMatch = Regex.Match(field, @"Item\[(\d+)\]\.(.+)")
                        let itemIndex = int itemMatch.Groups.[1].Value

                        if itemIndex < order.Items.Length then
                            let item = order.Items.[itemIndex]

                            match itemMatch.Groups.[2].Value with
                            | "SKU" -> item.SKU
                            | "Quantity" -> item.Quantity.ToString()
                            | "Price" -> item.Price.ToString("F2")
                            | "LineSubTotal" -> item.LineSubTotal.ToString("F2")
                            | "LineTaxTotal" -> item.LineTaxTotal.ToString("F2")
                            | "LineTotal" -> item.LineTotal.ToString("F2")
                            | field when Regex.IsMatch(field, @"LineTax\[\d+\]\.") ->
                                let taxMatch = Regex.Match(field, @"LineTax\[(\d+)\]\.(.+)")
                                let taxIndex = int taxMatch.Groups.[1].Value

                                if taxIndex < item.LineTaxes.Length then
                                    let tax = item.LineTaxes.[taxIndex]

                                    match taxMatch.Groups.[2].Value with
                                    | "Jurisdiction" -> tax.Jurisdiction
                                    | "Amount" -> tax.Amount.ToString("F2")
                                    | _ -> ""
                                else
                                    ""
                            | _ -> ""
                        else
                            ""
                    | _ -> ""

                printf "%s\t" value

            printfn ""


open CsvXForm

[<EntryPoint>]
let main argv =
    if argv.Length <> 1 then
        printfn "Usage: CsvXForm <path_to_csv_file>"
        1
    else
        let filePath = argv.[0]

        try
            let orders = readVerticalCsv filePath
            displayHorizontalTable orders
            0
        with ex ->
            printfn "Error: %s" ex.Message
            1
