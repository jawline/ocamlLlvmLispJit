open Type

type t =
  | Int of int
  | Float of float
  | Bool of bool
  | Unit
[@@deriving show]

let value_to_type = function
  | Int _ -> IntType
  | Float _ -> FloatType
  | Bool _ -> BoolType
  | Unit -> UnitType
;;
