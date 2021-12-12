open Yojson.Basic.Util

type fac =
  | Stripes
  | Solids
  | Unassigned

type t = {
  x_coord : int;
  y_coord : int;
  small_image : string;
  medium_image : string;
  large_image : string;
  player_number : int;
  weapon : Weapon.t option;
  money : int;
  health : int;
  board_location : int;
  properties : Property.t list;
  faction : fac;
}

let x_coord player = player.x_coord

let y_coord player = player.y_coord

let small_image player = player.small_image

let large_image player = player.large_image

let medium_image player = player.medium_image

let health player = player.health

let update_health player amt = { player with health = amt }

let money player = player.money

let update_money player amt = { player with money = amt }

let location player = player.board_location

let move_board player loc = { player with board_location = loc }

let move_coord player x y = { player with x_coord = x; y_coord = y }

let properties player = player.properties

let obtain_property player property =
  { player with properties = property :: player.properties }

let has_weapon player =
  match player.weapon with
  | None -> false
  | Some _ -> true

let weapon_damage player =
  match player.weapon with
  | None -> failwith "Player has no weapon"
  | Some wpn -> Weapon.damage wpn

let obtain_weapon player wpn = { player with weapon = wpn }

let faction player = player.faction

let get_player_from_json json =
  let player_number = json |> member "player_number" |> to_int in
  ( player_number,
    {
      x_coord = json |> member "x_coord" |> to_int;
      y_coord = json |> member "y_coord" |> to_int;
      small_image = json |> member "small_image" |> to_string;
      medium_image = json |> member "medium_image" |> to_string;
      large_image = json |> member "large_image" |> to_string;
      money = json |> member "money" |> to_int;
      health = json |> member "health" |> to_int;
      weapon = None;
      board_location = 0;
      properties = [];
      faction = Unassigned;
      player_number;
    } )

let get_players_from_json json =
  json |> to_list |> List.map get_player_from_json
