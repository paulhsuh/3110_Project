type t
(** List of popups; list of buttons; list of players; list of property
    cards. Use command.mli to check for user input; and update state as *)

val from_json : Yojson.Basic.t -> t