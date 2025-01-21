(** @author Arizona Hanson *)

(** [fail] always fails, returning None *)
let fail source = None

(** [return] trivially produces [value] without consuming from the [source] *)
let return value source = Some (value, source)

(** [bind] combines [parse] with [take] sequentially, passing the value 
  produced by [parse] as an argument to [take], which returns a new parser *)
let bind parse take source =
  match parse source with
  | None -> None
  | Some (sink, link) -> link |> take sink

let ( >>= ) = bind
let ( let@ ) = bind

(** [choose] produces a value from the first parser in [parsers] to succeed,
  if none succeed, [choose] fails *)
let rec choose parsers source =
  match parsers with
  | [] -> None
  | parse :: more -> (
      match parse source with
      | Some result -> Some result
      | None -> source |> choose more
  )

let ( <|> ) p1 p2 = choose [p1; p2]

(** [nil] trivially produces [[]] without consuming from the [source] *)
let nil source = source |> return []

(** [no] produces [[]] if [parse] fails, and fails if [parse] succeeds *)
let no parse source =
  match parse source with
  | Some result -> None
  | None -> source |> nil

(* -- derivitives (no mention of [source] due to partial application) -- *)

(** [maybe] always succeeds, producing the value from [parse], if [parse]
  fails, [maybe] produces [[]] *)
let maybe parse = parse <|> nil

(** [many] produces a list of zero or more values, one for every successful
  [parse]. produces [[]] if [parse] does not succeed at least once *)
let rec many parse = maybe @@ some parse

(** [some] produces a list of one or more values, one for every successful
  [parse]. [parse] must succeed at least once *)
and some parse =
  let@ value = parse in
  let@ next = many parse in
  return @@ value :: next

let rec foldr fn init parsers =
  match parsers with
  | [] -> return init
  | parse :: more -> (
    let@ value = parse in
    let@ next = foldr fn init more in
    return @@ fn value next
  )

let rec foldl fn init parsers =
  match parsers with
  | [] -> zero
  | parse :: more -> (
    let@ value = parse in
    let result = fn init value in
    match more with
    | [] -> return result
    | _ :: _ -> foldl fn result more
  )

(** [each] produces a sequence of values, one for each parser in [parsers].
  [each] fails upon the failure of any parser in [parsers] *)
let each parsers =
  foldr List.cons [] parsers

let ( <~> ) p1 p2 = each [p1; p2]