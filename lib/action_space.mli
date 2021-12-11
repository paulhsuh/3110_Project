type t
(**The abstract type representing an action space, a space on the game
   board that may give a player money, take money, take health, or
   transport the player to a new board location. *)

val take_money : t -> int
(**[take_money a] is the amount of money this action space takes from a
   player who lands on it.*)

val give_money : t -> int
(**[give_money a] is the amount of money this action space gives to a
   player who lands on it.*)

val take_health : t -> int
(**[take_health a] is the amount of health this action space takes from
   a player who lands on it.*)

val new_board_location : t -> int
(**[new_board_location a] is the new board location a player should be
   moved to after landing upon this action space.*)

val board_order : t -> int
(**[board_order a] gives the location of this [a] on the game board.*)

val get_action_space_from_json : Yojson.Basic.t -> t
(**[get_action_space_from_json js] is the action space that [js]
   represents.*)

val get_action_spaces_from_json : Yojson.Basic.t -> t list
(**[get_action_space_from_json js] is a list of action space that [js]
   represents.*)