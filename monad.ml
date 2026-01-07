(** @author Arizona Hanson 
  just, bind/let*, any/either/<|>, never,
  maybe, many, some, foldr, foldl, all/both/<&>
*)

(** [just] trivially produce [value] without consuming [source] *)
let just value source =
  Some (value, source)

(** [bind] combines [parse] with [combo] sequentially, passing the value 
  produced by [parse] as an argument to [combo], which returns a new parser.
  short-circuits if [parse] fails by returning None *)
let bind parse combo source =
  match parse source with
  | None -> None (* fail *)
  | Some (value, tail) -> tail |> combo value
let ( let* ) = bind

(** [any] produces a value from the first parser in [parsers] to succeed,
  if none succeed, [any] fails *)
let rec any parsers source =
  match parsers with
  | [] -> None
  | parse :: more -> (
      match parse source with
      | Some result -> Some result
      | None -> source |> any more
  )

(** binary [any] (either) *)
let either p q = any [p; q]
let ( <|> ) = either

(** [never] fails if [parse] succeeds, otherwise produces [[]] *)
let never parse source =
  match parse source with
  | Some result -> None
  | None -> source |> just []

(* -- derivitives (no mention of [source] due to partial application) -- *)

(** [maybe] always succeeds, producing the value from [parse], if [parse]
  fails, [maybe] produces [[]] *)
let maybe parse =
  parse <|> just []

(** [many] produces a list of zero or more values, one for every successful
  [parse]. produces [[]] if [parse] does not succeed at least once.
  mutually-recursive with [some] *)
let rec many parse =
  maybe @@ some parse

(** [some] produces a list of one or more values, one for every successful
  [parse]. [parse] must succeed at least once. mutually-recursive with [many] *)
and some parse =
  let* value = parse in
  let* next = many parse in
  just @@ value :: next

(** [foldr] applies [binop] to the value from the first parser in [parsers],
and the value from applying [binop] to the rest of the parsers recursively,
ending with [init] and accumulating from right to left *)
let rec foldr binop init parsers =
  match parsers with
  | [] -> just init (* terminator *)
  | parse :: more -> (
    let* value = parse in
    let* next = foldr binop init more in
    just @@ binop value next
  )

(** [foldl] applies [binop] to the value from the first parser in [parsers],
and the value from applying [binop] to the rest of the parsers recursively,
starting with [init] and accumulating from left to right *)
let rec foldl binop init parsers =
  match parsers with
  | [] -> just init (* no-op *)
  | parse :: more -> (
    let* value = parse in
    let result = binop init value in
    match more with
    | [] -> just result (* terminator *)
    | _ :: _ -> foldl binop result more
  )

(** [all] produces a sequence of values, one for every parser in [parsers].
  [all] fails if any parser in [parsers] fails *)
let all parsers =
  foldr List.cons [] parsers

(** binary [all] (both) *)
let both p q = all [p; q]
let ( <&> ) = both