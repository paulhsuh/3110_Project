type t 

type weapon 

type property

type location = int 

type cost = int 

type property_name = string 

type corner_name = string 

type weapon_name = string 

type food_name = string

exception UnknownProperty of property_name

exception UnknownCorner of corner_name

(*Returns a list of properties owned by player Player.t *)
(* val player_properties: Player.t -> t -> property list *)



