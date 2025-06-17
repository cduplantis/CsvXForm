// This code reads a horizontal CSV file where each line is a separate order,
// transforms it into a structured format, and displays it.

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
    open System.Collections.Generic

    // Load CSV file dynamically
    let loadCsv (filePath: string) =
        CsvFile.Load(filePath, hasHeaders = true)

    let readHorizontalCsv (filePath: string) : Order list =
        let csv = loadCsv filePath
        let headers = csv.Headers |> Option.defaultValue [||]
        let rows = csv.Rows |> Seq.toList

        // Process each row as a complete order
        let orders =
            rows
            |> List.map (fun row ->
                // Create a dictionary for easy field lookup
                let fields = Dictionary<string, string>()

                for i = 0 to headers.Length - 1 do
                    if i < row.Columns.Length then
                        fields.[headers.[i]] <- row.Columns.[i]
                    else
                        fields.[headers.[i]] <- ""

                // Helper function to safely get field value
                let getField (key: string) =
                    match fields.TryGetValue(key) with
                    | true, value -> value
                    | _ -> ""

                // Helper function to parse decimal safely
                let parseDecimal (s: string) =
                    match Decimal.TryParse(s) with
                    | true, value -> value
                    | _ -> 0M

                // Helper function to parse integer safely
                let parseInt (s: string) =
                    match Int32.TryParse(s) with
                    | true, value -> value
                    | _ -> 0

                // Helper function to parse DateTime safely
                let parseDateOnly (s: string) =
                    match DateOnly.TryParse(s) with
                    | true, value -> value
                    | _ -> DateOnly.MinValue

                // Extract items from the row
                let items =
                    let mutable i = 0
                    let mutable items = []
                    let mutable continueProcessing = true

                    while continueProcessing do
                        let skuKey = sprintf "Item[%d].SKU" i

                        if fields.ContainsKey(skuKey) && not (String.IsNullOrWhiteSpace(fields.[skuKey])) then
                            // Process taxes for this item
                            let taxes =
                                let mutable j = 0
                                let mutable taxes = []
                                let mutable continueTaxProcessing = true

                                while continueTaxProcessing do
                                    let jurisdictionKey = sprintf "Item[%d].LineTax[%d].Jurisdiction" i j
                                    let amountKey = sprintf "Item[%d].LineTax[%d].Amount" i j

                                    if
                                        fields.ContainsKey(jurisdictionKey)
                                        && not (String.IsNullOrWhiteSpace(fields.[jurisdictionKey]))
                                    then
                                        taxes <-
                                            taxes
                                            @ [ { Jurisdiction = getField jurisdictionKey
                                                  Amount = parseDecimal (getField amountKey) } ]

                                        j <- j + 1
                                    else
                                        continueTaxProcessing <- false

                                taxes

                            // Create the item
                            items <-
                                items
                                @ [ { SKU = getField (sprintf "Item[%d].SKU" i)
                                      Quantity = parseInt (getField (sprintf "Item[%d].Quantity" i))
                                      Price = parseDecimal (getField (sprintf "Item[%d].Price" i))
                                      LineSubTotal = parseDecimal (getField (sprintf "Item[%d].LineSubTotal" i))
                                      LineTaxes = taxes
                                      LineTaxTotal = parseDecimal (getField (sprintf "Item[%d].LineTaxTotal" i))
                                      LineTotal = parseDecimal (getField (sprintf "Item[%d].LineTotal" i)) } ]

                            i <- i + 1
                        else
                            continueProcessing <- false

                    items

                // Create and return the order
                { OrderID = getField "OrderID"
                  OrderDate = parseDateOnly (getField "OrderDate")
                  Customer =
                    { Id = getField "Customer.Id"
                      Name = getField "Customer.Name"
                      Street = getField "Customer.Street"
                      State = getField "Customer.State"
                      City = getField "Customer.City"
                      Zip = getField "Customer.Zip" }
                  DeliveryAddress =
                    { Name = getField "DeliveryAddress.Name"
                      Street = getField "DeliveryAddress.Street"
                      State = getField "DeliveryAddress.State"
                      City = getField "DeliveryAddress.City"
                      Zip = getField "DeliveryAddress.Zip" }
                  Items = items
                  TotalDue = parseDecimal (getField "TotalDue")
                  InitialDue = parseDecimal (getField "InitialDue")
                  Terms = getField "Terms"
                  Notes = getField "Notes" })

        orders

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
            let orders = readHorizontalCsv filePath
            displayHorizontalTable orders
            0
        with ex ->
            printfn "Error: %s" ex.Message
            1
