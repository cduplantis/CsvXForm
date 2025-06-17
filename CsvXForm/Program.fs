// This code reads a CSV file, transforms it into a structured format, and displays it.

namespace CsvXForm

module CsvProcessor =
    open Models
    open FSharp.Data
    open System
    open System.Text.RegularExpressions
    open System.Collections.Generic

    // Load CSV file dynamically
    let loadCsv (filePath: string) =
        CsvFile.Load(filePath, hasHeaders = true)


    let readHorizontalCsv (filePath: string) : Result<Order list, string> =
        try
            let csv = loadCsv filePath
            let headers = csv.Headers |> Option.defaultValue [||]

            if headers.Length = 0 then
                failwith "CSV file has no headers."

            // Print headers for debugging
            printfn "Found headers: %A" headers

            let rows = csv.Rows |> Seq.toList
            printfn "Found %d rows" rows.Length

            // Process each row as an order
            let orderResults =
                rows
                |> List.mapi (fun rowIndex row ->
                    // Create a dictionary for field lookup
                    let fields = Dictionary<string, string>()

                    for i = 0 to headers.Length - 1 do
                        if i < row.Columns.Length then
                            fields.[headers.[i]] <- row.Columns.[i]

                    printfn "Row %d - Processing with %d fields" rowIndex fields.Count

                    // Debug print some key fields
                    [ "OrderID"; "Customer.Id"; "Customer.Name" ]
                    |> List.iter (fun key ->
                        match getField fields key with
                        | Some value -> printfn "  %s: %s" key value
                        | None -> printfn "  %s: <missing>" key)

                    // Create the order from fields
                    let orderResult = Order.fromDictionary fields

                    // Log the result
                    match orderResult with
                    | Ok order ->
                        printfn
                            "  Successfully created order with ID: %s, containing %d items"
                            (OrderId.value order.OrderID)
                            order.Items.Length
                    | Error e -> printfn "  Failed to create order: %s" e

                    orderResult)

            // Count successes and failures
            let validOrders =
                orderResults
                |> List.choose (function
                    | Ok order -> Some order
                    | Error _ -> None)

            let errorCount = orderResults.Length - validOrders.Length
            printfn "Parsed %d valid orders (with %d errors)" validOrders.Length errorCount

            // If we have any valid orders, return them, otherwise return an error
            if validOrders.Length > 0 then
                Ok validOrders
            else
                let errors =
                    orderResults
                    |> List.choose (function
                        | Ok _ -> None
                        | Error e -> Some e)
                    |> String.concat "; "

                Error $"Failed to parse any valid orders: {errors}"

        with ex ->
            Error $"Error reading CSV: {ex.Message}\n{ex.StackTrace}"

    let readVerticalCsv (filePath: string) : Result<Order list, string> =
        try
            let csv = loadCsv filePath
            let rows = csv.Rows |> Seq.toList

            // Determine how many orders we have by looking at the header row
            let headers = csv.Headers |> Option.defaultValue [||]

            if headers.Length <= 1 then
                Error "CSV file doesn't contain order columns"
            else
                let orderCount = headers.Length - 1 // Subtract 1 for the "Field" column
                printfn "Found %d orders in vertical CSV" orderCount

                // Convert the rows into a dictionary mapping field names to values for each order
                let orderDictionaries =
                    [ for orderIndex in 1..orderCount do
                          let orderDict = Dictionary<string, string>()

                          // Fill the dictionary with values from each row
                          for row in rows do
                              if row.Columns.Length > orderIndex then
                                  let fieldName = row.Columns.[0] // Field name is in first column
                                  let value = row.Columns.[orderIndex] // Order value is in column by index
                                  orderDict.[fieldName] <- value

                          yield orderDict ]

                // Process each order dictionary
                let orderResults =
                    orderDictionaries
                    |> List.mapi (fun i dict ->
                        printfn "Processing order %d with %d fields" (i + 1) dict.Count

                        // Debug print some key fields
                        [ "OrderID"; "Customer.Id"; "Customer.Name" ]
                        |> List.iter (fun key ->
                            match getField dict key with
                            | Some value -> printfn "  %s: %s" key value
                            | None -> printfn "  %s: <missing>" key)

                        Order.fromDictionary dict)

                // Process results
                let validOrders =
                    orderResults
                    |> List.choose (function
                        | Ok order -> Some order
                        | Error _ -> None)

                let errorCount = orderResults.Length - validOrders.Length
                printfn "Parsed %d valid orders (with %d errors)" validOrders.Length errorCount

                if validOrders.Length > 0 then
                    Ok validOrders
                else
                    let errors =
                        orderResults
                        |> List.choose (function
                            | Ok _ -> None
                            | Error e -> Some e)
                        |> String.concat "; "

                    Error $"Failed to parse any valid orders: {errors}"

        with ex ->
            Error $"Error reading CSV: {ex.Message}\n{ex.StackTrace}"

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
                    | "OrderID" -> order.OrderID |> OrderId.value
                    | "OrderDate" -> order.OrderDate.ToString()
                    | "Customer.Id" -> order.Customer.Id |> CustomerId.value
                    | "Customer.Name" -> order.Customer.Name |> NonEmptyString.value
                    | "Customer.Address" -> order.Customer.Address |> NonEmptyString.value
                    | "Customer.State" -> order.Customer.State |> UsState.value
                    | "Customer.City" -> order.Customer.City |> NonEmptyString.value
                    | "Customer.Zip" -> order.Customer.Zip |> ZipCode.value
                    | "DeliveryAddress.Name" -> order.DeliveryAddress.Name |> Option.defaultValue ""
                    | "DeliveryAddress.Address" -> order.DeliveryAddress.Address |> NonEmptyString.value
                    | "DeliveryAddress.State" -> order.DeliveryAddress.State |> UsState.value
                    | "DeliveryAddress.City" -> order.DeliveryAddress.City |> NonEmptyString.value
                    | "DeliveryAddress.Zip" -> order.DeliveryAddress.Zip |> ZipCode.value
                    | "TotalDue" -> order.TotalDue |> NonNegativeDecimal.value |> sprintf "%.2f"
                    | "InitialDue" -> order.InitialDue |> NonNegativeDecimal.value |> sprintf "%.2f"
                    | "Terms" ->
                        order.Terms
                        |> function
                            | Net days -> sprintf "Net %d" (PositiveInt.value days)
                            | DueOnReceipt -> "Due On Receipt"
                            | Custom note -> NonEmptyString.value note
                    | "Notes" -> order.Notes |> Option.defaultValue ""
                    | field when Regex.IsMatch(field, @"Item\[\d+\]\.") ->
                        let itemMatch = Regex.Match(field, @"Item\[(\d+)\]\.(.+)")
                        let itemIndex = int itemMatch.Groups.[1].Value

                        if itemIndex < order.Items.Length then
                            let item = order.Items.[itemIndex]

                            match itemMatch.Groups.[2].Value with
                            | "SKU" -> item.SKU |> Sku.value
                            | "Quantity" -> item.Quantity |> PositiveInt.value |> string
                            | "Price" -> item.Price |> PositiveDecimal.value |> sprintf "%.2f"
                            | "LineSubTotal" -> item.LineSubTotal |> PositiveDecimal.value |> sprintf "%.2f"
                            | "LineTaxTotal" -> item.LineTaxTotal |> NonNegativeDecimal.value |> sprintf "%.2f"
                            | "LineTotal" -> item.LineTotal |> PositiveDecimal.value |> sprintf "%.2f"
                            | field when Regex.IsMatch(field, @"LineTax\[\d+\]\.") ->
                                let taxMatch = Regex.Match(field, @"LineTax\[(\d+)\]\.(.+)")
                                let taxIndex = int taxMatch.Groups.[1].Value

                                if taxIndex < item.LineTaxes.Length then
                                    let tax = item.LineTaxes.[taxIndex]

                                    match taxMatch.Groups.[2].Value with
                                    | "Jurisdiction" -> tax.Jurisdiction |> NonEmptyString.value
                                    | "Amount" -> tax.Amount |> NonNegativeDecimal.value |> sprintf "%.2f"
                                    | _ -> ""
                                else
                                    ""
                            | _ -> ""
                        else
                            ""
                    | _ -> ""

                printf "%s\t" value

            printfn ""


module Program =
    open CsvProcessor
    open Models

    [<EntryPoint>]
    let main argv =
        if argv.Length < 1 || argv.Length > 2 then
            printfn "Usage: CsvXForm <path_to_csv_file> [format]"
            printfn "  format: horizontal (default) or vertical"
            1
        else
            let filePath = argv.[0]

            let format =
                if argv.Length > 1 then
                    argv.[1].ToLowerInvariant()
                else
                    "horizontal"

            try
                let result =
                    match format with
                    | "vertical" -> readVerticalCsv filePath
                    | _ -> readHorizontalCsv filePath // Default to horizontal

                match result with
                | Ok orders ->
                    displayHorizontalTable orders
                    printfn "CSV processed successfully."
                    0
                | Error error ->
                    printfn "Error: %s" error
                    1
            with ex ->
                printfn "Error: %s" ex.Message
                1
