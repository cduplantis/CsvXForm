module Models

open System
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Collections.Immutable
open System.Globalization
open System.Linq

// Constrained string type
type NonEmptyString = private NonEmptyString of string

module NonEmptyString =
    let create (s: string) =
        if String.IsNullOrWhiteSpace s then
            Error "String cannot be empty"
        else
            Ok(NonEmptyString s)

    let value (NonEmptyString s) = s


// Constrained positive int type
type PositiveInt = private PositiveInt of int

module PositiveInt =
    let create (i: int) =
        if i <= 0 then
            Error "Value must be positive"
        else
            Ok(PositiveInt i)

    let value (PositiveInt i) = i

// Define your custom types to work with units of measure
type PositiveDecimal = private PositiveDecimal of decimal

module PositiveDecimal =
    let create (i: decimal) =
        if i < 1M then
            Error "Value must be positive"
        else
            Ok(PositiveDecimal i)

    let value (PositiveDecimal i) = i



type NonNegativeDecimal = NonNegativeDecimal of decimal

module NonNegativeDecimal =
    let create (i: decimal) =
        if i <= 0.0M then
            Error "Value must be not negative"
        else
            Ok(NonNegativeDecimal i)

    let value (NonNegativeDecimal i) = i

// Specific types for IDs and other strings
type OrderId = private OrderId of NonEmptyString

module OrderId =
    let create s =
        NonEmptyString.create s |> Result.map OrderId

    let value (OrderId nes) = NonEmptyString.value nes

type CustomerId = private CustomerId of NonEmptyString

module CustomerId =
    let create s =
        NonEmptyString.create s |> Result.map CustomerId

    let value (CustomerId nes) = NonEmptyString.value nes

type Sku = private Sku of NonEmptyString

module Sku =
    let create s =
        NonEmptyString.create s |> Result.map Sku

    let value (Sku nes) = NonEmptyString.value nes

// Zip code (US format: 5 digits)
type ZipCode = private ZipCode of string

module ZipCode =
    let create (s: string) =
        let pattern = @"^\d{5}$"

        if String.IsNullOrEmpty s || not (Regex.IsMatch(s, pattern)) then
            Error "Invalid ZIP code: must be 5 digits"
        else
            Ok(ZipCode s)

    let value (ZipCode s) = s

// State (US two-letter code)
type UsState = private UsState of string

module UsState =
    let create (s: string) =
        let pattern = @"^[A-Z]{2}$"

        if String.IsNullOrEmpty s || not (Regex.IsMatch(s, pattern)) then
            Error "Invalid state: must be 2 uppercase letters"
        else
            Ok(UsState s)

    let value (UsState s) = s

type LineTax =
    { Jurisdiction: NonEmptyString
      Amount: NonNegativeDecimal }

module LineTax =
    open CsvXForm

    // Pure function to create a LineTax from explicit parameters
    let create (jurisdiction: string) (amount: decimal) : Result<LineTax, string> =
        validated {
            let! validJurisdiction = NonEmptyString.create jurisdiction
            let! validAmount = NonNegativeDecimal.create amount

            return
                { Jurisdiction = validJurisdiction
                  Amount = validAmount }
        }

    // Function to extract LineTax data from a dictionary
    let fromDictionary (fields: Dictionary<string, string>) (itemIndex: int) (taxIndex: int) : Result<LineTax, string> =
        let jurisdictionKey = sprintf "Item[%d].LineTax[%d].Jurisdiction" itemIndex taxIndex
        let amountKey = sprintf "Item[%d].LineTax[%d].Amount" itemIndex taxIndex

        validated {
            let! jurisdiction =
                match getField fields jurisdictionKey with
                | Some v -> Ok v
                | None -> Error $"Tax jurisdiction for item {itemIndex}, tax {taxIndex} is required"

            let! amount =
                match getField fields amountKey with
                | Some v ->
                    match Decimal.TryParse(v) with
                    | true, value -> Ok value
                    | _ -> Error $"Invalid tax amount for item {itemIndex}, tax {taxIndex}: {v}"
                | None -> Error $"Tax amount for item {itemIndex}, tax {taxIndex} is required"

            return! create jurisdiction amount
        }



type Customer =
    { Id: CustomerId
      Name: NonEmptyString
      Street: NonEmptyString
      State: UsState
      City: NonEmptyString
      Zip: ZipCode }

module Customer =
    open CsvXForm

    // Pure function to create a Customer from explicit parameters
    let create
        (id: string)
        (name: string)
        (street: string)
        (state: string)
        (city: string)
        (zip: string)
        : Result<Customer, string> =
        validated {
            let! validId = CustomerId.create id
            let! validName = NonEmptyString.create name
            let! validStreet = NonEmptyString.create street
            let! validState = UsState.create state
            let! validCity = NonEmptyString.create city
            let! validZip = ZipCode.create zip

            return
                { Id = validId
                  Name = validName
                  Street = validStreet
                  State = validState
                  City = validCity
                  Zip = validZip }
        }

    // Function to extract Customer data from a dictionary
    let fromDictionary (fields: Dictionary<string, string>) : Result<Customer, string> =
        validated {
            let! id =
                match getField fields "Customer.Id" with
                | Some v -> Ok v
                | None -> Error "Customer ID is required"

            let! name =
                match getField fields "Customer.Name" with
                | Some v -> Ok v
                | None -> Error "Customer name is required"

            let! street =
                match getField fields "Customer.Street" with
                | Some v -> Ok v
                | None -> Error "Customer street is required"

            let! state =
                match getField fields "Customer.State" with
                | Some v -> Ok v
                | None -> Error "Customer state is required"

            let! city =
                match getField fields "Customer.City" with
                | Some v -> Ok v
                | None -> Error "Customer city is required"

            let! zip =
                match getField fields "Customer.Zip" with
                | Some v -> Ok v
                | None -> Error "Customer zip is required"

            return! create id name street state city zip
        }

type DeliveryAddress =
    { Name: string option // Optional, as delivery may not have a specific name
      Street: NonEmptyString
      State: UsState
      City: NonEmptyString
      Zip: ZipCode }

// DeliveryAddress module updates
module DeliveryAddress =
    open CsvXForm

    // Pure function to create a DeliveryAddress from explicit parameters
    let create
        (name: string option)
        (street: string)
        (state: string)
        (city: string)
        (zip: string)
        : Result<DeliveryAddress, string> =
        validated {
            let! validStreet = NonEmptyString.create street
            let! validState = UsState.create state
            let! validCity = NonEmptyString.create city
            let! validZip = ZipCode.create zip

            return
                { Name = name
                  Street = validStreet
                  State = validState
                  City = validCity
                  Zip = validZip }
        }

    // Function to extract DeliveryAddress data from a dictionary
    let fromDictionary (fields: Dictionary<string, string>) : Result<DeliveryAddress, string> =
        validated {
            let! street =
                match getField fields "DeliveryAddress.Street" with
                | Some v -> Ok v
                | None -> Error "Delivery street is required"

            let! state =
                match getField fields "DeliveryAddress.State" with
                | Some v -> Ok v
                | None -> Error "Delivery state is required"

            let! city =
                match getField fields "DeliveryAddress.City" with
                | Some v -> Ok v
                | None -> Error "Delivery city is required"

            let! zip =
                match getField fields "DeliveryAddress.Zip" with
                | Some v -> Ok v
                | None -> Error "Delivery zip is required"

            let name = getField fields "DeliveryAddress.Name"

            return! create name street state city zip
        }

type Item =
    { SKU: Sku
      Quantity: PositiveInt
      Price: PositiveDecimal
      LineSubTotal: PositiveDecimal
      LineTaxes: LineTax list
      LineTaxTotal: NonNegativeDecimal
      LineTotal: PositiveDecimal }

// Item module updates
module Item =
    open CsvXForm

    // Pure function to create an Item from explicit parameters
    let create
        (sku: string)
        (quantity: int)
        (price: decimal)
        (lineSubTotal: decimal)
        (lineTaxes: LineTax list)
        (lineTaxTotal: decimal)
        (lineTotal: decimal)
        : Result<Item, string> =
        validated {
            let! validSku = Sku.create sku
            let! validQuantity = PositiveInt.create quantity
            let! validPrice = PositiveDecimal.create price
            let! validLineSubTotal = PositiveDecimal.create lineSubTotal
            let! validLineTaxTotal = NonNegativeDecimal.create lineTaxTotal
            let! validLineTotal = PositiveDecimal.create lineTotal

            return
                { SKU = validSku
                  Quantity = validQuantity
                  Price = validPrice
                  LineSubTotal = validLineSubTotal
                  LineTaxes = lineTaxes
                  LineTaxTotal = validLineTaxTotal
                  LineTotal = validLineTotal }
        }

    // Function to extract Item data from a dictionary
    let fromDictionary (itemIndex: int) (fields: Dictionary<string, string>) : Result<Item, string> =
        validated {
            let skuKey = sprintf "Item[%d].SKU" itemIndex
            let quantityKey = sprintf "Item[%d].Quantity" itemIndex
            let priceKey = sprintf "Item[%d].Price" itemIndex
            let subTotalKey = sprintf "Item[%d].LineSubTotal" itemIndex
            let taxTotalKey = sprintf "Item[%d].LineTaxTotal" itemIndex
            let totalKey = sprintf "Item[%d].LineTotal" itemIndex

            let! sku =
                match getField fields skuKey with
                | Some v -> Ok v
                | None -> Error $"SKU for item {itemIndex} is required"

            let! quantity =
                match getField fields quantityKey with
                | Some v ->
                    match Int32.TryParse(v) with
                    | true, value -> Ok value
                    | _ -> Error $"Invalid quantity for item {itemIndex}: {v}"
                | None -> Error $"Quantity for item {itemIndex} is required"

            let! price =
                match getField fields priceKey with
                | Some v ->
                    match Decimal.TryParse(v) with
                    | true, value -> Ok value
                    | _ -> Error $"Invalid price for item {itemIndex}: {v}"
                | None -> Error $"Price for item {itemIndex} is required"

            let! subTotal =
                match getField fields subTotalKey with
                | Some v ->
                    match Decimal.TryParse(v) with
                    | true, value -> Ok value
                    | _ -> Error $"Invalid subtotal for item {itemIndex}: {v}"
                | None -> Error $"Subtotal for item {itemIndex} is required"

            let! taxTotal =
                match getField fields taxTotalKey with
                | Some v ->
                    match Decimal.TryParse(v) with
                    | true, value -> Ok value
                    | _ -> Error $"Invalid tax total for item {itemIndex}: {v}"
                | None -> Error $"Tax total for item {itemIndex} is required"

            let! total =
                match getField fields totalKey with
                | Some v ->
                    match Decimal.TryParse(v) with
                    | true, value -> Ok value
                    | _ -> Error $"Invalid total for item {itemIndex}: {v}"
                | None -> Error $"Total for item {itemIndex} is required"

            // Find and parse all taxes for this item
            let rec collectTaxes acc taxIndex =
                let taxKey = sprintf "Item[%d].LineTax[%d].Jurisdiction" itemIndex taxIndex

                match getField fields taxKey with
                | Some _ ->
                    match LineTax.fromDictionary fields itemIndex taxIndex with
                    | Ok tax -> collectTaxes (acc @ [ tax ]) (taxIndex + 1)
                    | Error _ -> collectTaxes acc (taxIndex + 1) // Skip invalid taxes
                | None -> acc

            let taxes = collectTaxes [] 0

            return! create sku quantity price subTotal taxes taxTotal total
        }

type Terms =
    | Net of PositiveInt // e.g., Net 30
    | DueOnReceipt
    | Custom of NonEmptyString

type Order =
    { OrderID: OrderId
      OrderDate: DateOnly
      Customer: Customer
      DeliveryAddress: DeliveryAddress
      Items: Item list
      TotalDue: NonNegativeDecimal
      InitialDue: NonNegativeDecimal
      Terms: Terms
      Notes: string option }

// Order module updates
module Order =
    open CsvXForm

    // Pure function to create an Order from explicit parameters
    let create
        (orderId: string)
        (orderDate: DateOnly)
        (customer: Customer)
        (deliveryAddress: DeliveryAddress)
        (items: Item list)
        (totalDue: decimal)
        (initialDue: decimal)
        (terms: Terms)
        (notes: string option)
        : Result<Order, string> =
        validated {
            let! validOrderId = OrderId.create orderId
            let! validTotalDue = NonNegativeDecimal.create totalDue
            let! validInitialDue = NonNegativeDecimal.create initialDue

            return
                { OrderID = validOrderId
                  OrderDate = orderDate
                  Customer = customer
                  DeliveryAddress = deliveryAddress
                  Items = items
                  TotalDue = validTotalDue
                  InitialDue = validInitialDue
                  Terms = terms
                  Notes = notes }
        }

    // Helper function to parse terms from string
    let parseTerms (termsStr: string option) : Terms =
        match termsStr with
        | Some v ->
            match v with
            | "DueOnReceipt" -> DueOnReceipt
            | t when t.StartsWith("Net") ->
                let days = t.Replace("Net", "").Trim() |> int

                match PositiveInt.create days with
                | Ok d -> Net d
                | Error _ -> DueOnReceipt
            | other ->
                match NonEmptyString.create other with
                | Ok s -> Custom s
                | Error _ -> DueOnReceipt
        | None -> DueOnReceipt

    // Function to extract Order data from a dictionary
    let fromDictionary (fields: Dictionary<string, string>) : Result<Order, string> =
        validated {
            let! orderId =
                match getField fields "OrderID" with
                | Some v -> Ok v
                | None -> Error "Order ID is required"

            let! orderDate =
                match getField fields "OrderDate" with
                | Some v ->
                    match DateOnly.TryParse(v) with
                    | true, dt -> Ok dt
                    | _ -> Error "Invalid order date/time"
                | None -> Error "Order date/time is required"

            let! customer = Customer.fromDictionary fields
            let! deliveryAddress = DeliveryAddress.fromDictionary fields

            let! totalDue =
                match getField fields "TotalDue" with
                | Some v ->
                    match Decimal.TryParse(v) with
                    | true, value -> Ok value
                    | _ -> Error "Invalid total due"
                | None -> Error "Total due is required"

            let! initialDue =
                match getField fields "InitialDue" with
                | Some v ->
                    match Decimal.TryParse(v) with
                    | true, value -> Ok value
                    | _ -> Error "Invalid initial due"
                | None -> Error "Initial due is required"

            let terms = parseTerms (getField fields "Terms")
            let notes = getField fields "Notes"

            // Find and parse all items for this order
            let rec collectItems acc itemIndex =
                let skuKey = sprintf "Item[%d].SKU" itemIndex

                match getField fields skuKey with
                | Some _ ->
                    match Item.fromDictionary itemIndex fields with
                    | Ok item -> collectItems (acc @ [ item ]) (itemIndex + 1)
                    | Error _ -> collectItems acc (itemIndex + 1) // Skip invalid items
                | None -> acc

            let items = collectItems [] 0

            return! create orderId orderDate customer deliveryAddress items totalDue initialDue terms notes
        }
