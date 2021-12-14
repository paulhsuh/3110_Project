open Yojson.Basic.Util
open Maps

type button_map = Button.t SM.t

type subscreen_map = Subscreen.t SM.t

type player_map = Player.t IM.t

type property_map = Property.t IM.t

type food_stack_map = Food_stack.t IM.t

type weapon_stack_map = Weapon_stack.t IM.t

type action_space_map = Action_space.t IM.t

type t = {
  buttons : button_map;
  players : player_map;
  active_players : int list;
  properties : property_map;
  food_stacks : food_stack_map;
  weapon_stacks : weapon_stack_map;
  action_spaces : action_space_map;
  subscreens : subscreen_map;
  team_info : subscreen_map;
  info_cards : Subscreen.t;
  background_image : string;
  background_xcoord : int;
  background_ycoord : int;
  gameboard_image : string;
  gameboard_xcoord : int;
  gameboard_ycoord : int;
  dice : Die.t list;
  order_list : (int * (int * int)) list;
  curr_player_index : int;
  curr_player_roll : bool;
}

(* Returns the order list or the board_location to (x, y) coords. *)
let get_order_list_from_json el =
  ( el |> member "order" |> to_int,
    (el |> member "x_coord" |> to_int, el |> member "y_coord" |> to_int)
  )

let get_game_screen_from_json (json : Yojson.Basic.t) : t =
  let gs_json = json |> member "game_screen" in
  let btns =
    gs_json |> member "buttons" |> Button.get_buttons_from_json
  in
  let plyrs =
    gs_json |> member "players" |> Player.get_players_from_json
  in
  let props =
    gs_json |> member "properties" |> Property.get_properties_from_json
  in
  let ti =
    gs_json |> member "team_info" |> Subscreen.get_subscreens_from_json
  in
  let bi =
    gs_json
    |> member "game_screen_background"
    |> member "image_name" |> to_string
  in
  let bi_x_coord =
    gs_json
    |> member "game_screen_background"
    |> member "x_coord" |> to_int
  in
  let bi_y_coord =
    gs_json
    |> member "game_screen_background"
    |> member "y_coord" |> to_int
  in
  let gi =
    gs_json |> member "gameboard" |> member "image_name" |> to_string
  in
  let gi_x_coord =
    gs_json |> member "gameboard" |> member "x_coord" |> to_int
  in
  let gi_y_coord =
    gs_json |> member "gameboard" |> member "y_coord" |> to_int
  in
  let _, ic =
    gs_json |> member "info_cards" |> Subscreen.get_subscreen_from_json
  in
  let pops =
    gs_json |> member "subscreens" |> Subscreen.get_subscreens_from_json
  in
  let foods =
    gs_json |> member "foods" |> member "food_types"
    |> Food.get_foods_from_json
  in
  let f_stacks =
    gs_json |> member "foods" |> member "food_stacks"
    |> Food_stack.get_food_stacks_from_json foods
  in
  let weapons =
    gs_json |> member "weapons" |> member "weapon_types"
    |> Weapon.get_weapons_from_json
  in
  let w_stacks =
    gs_json |> member "weapons" |> member "weapon_stacks"
    |> Weapon_stack.get_weapon_stacks_from_json weapons
  in
  let dice = gs_json |> member "dice" |> Die.get_dice_from_json in
  let actions =
    gs_json |> member "action_spaces"
    |> Action_space.get_action_spaces_from_json
  in
  let or_lst =
    gs_json
    |> member "board_order_coords"
    |> to_list
    |> List.map get_order_list_from_json
  in
  {
    buttons = SM.of_lst btns SM.empty;
    players = IM.of_lst plyrs IM.empty;
    properties = IM.of_lst props IM.empty;
    subscreens = SM.of_lst pops SM.empty;
    team_info = Subscreen.activates (SM.of_lst ti SM.empty);
    background_image = bi;
    background_xcoord = bi_x_coord;
    background_ycoord = bi_y_coord;
    gameboard_image = gi;
    gameboard_xcoord = gi_x_coord;
    gameboard_ycoord = gi_y_coord;
    info_cards = Subscreen.activate ic;
    food_stacks = IM.of_lst f_stacks IM.empty;
    weapon_stacks = IM.of_lst w_stacks IM.empty;
    dice;
    action_spaces = IM.of_lst actions IM.empty;
    order_list = or_lst;
    active_players = [];
    curr_player_index = 0;
    curr_player_roll = false;
  }

let buttons gs = gs.buttons

let players gs = gs.players

let properties gs = gs.properties

let food_stacks gs = gs.food_stacks

let weapon_stacks gs = gs.weapon_stacks

let action_spaces gs = gs.action_spaces

let subscreens gs = gs.subscreens

let team_info gs = gs.team_info

let info_cards gs = gs.info_cards

let background_image gs = gs.background_image

let background_xcoord gs = gs.background_xcoord

let background_ycoord gs = gs.background_ycoord

let gameboard_image gs = gs.gameboard_image

let gameboard_xcoord gs = gs.gameboard_xcoord

let gameboard_ycoord gs = gs.gameboard_ycoord

let dice gs = gs.dice

let curr_player gs = gs.curr_player_index

(*there's some complicated stuff going on here. to put it simply, all
  players module are either given a player number or they were not
  selected, meaning they are deactivated.*)
let initialize gs chars =
  let initialized_players =
    IM.mapi
      (fun k p ->
        if List.exists (fun c -> k = c) chars then Player.activate p
        else p)
      gs.players
  in
  {
    gs with
    players = initialized_players;
    active_players = List.rev chars;
  }

type response =
  | EndGame
  | NewGS of t

(*BaseGS with the regular game screen buttons, the dice buttons, and the
  property buttons. ActiveSubscreenGS with the buttons inside the
  subscreen*)
type screen_buttons =
  | BaseGS of Button.t SM.t * Button.t list * Button.t IM.t
  | ActiveSubscreenGS of Button.t SM.t

let get_dice_buttons gs = List.map (fun d -> Die.button d) gs.dice

let get_property_buttons gs =
  IM.mapi (fun _ p -> Property.button p) gs.properties

let get_buttons gs =
  let active_subscreen =
    SM.fold
      (fun _ s init -> if Subscreen.active s then Some s else init)
      gs.subscreens None
  in
  let dice_buttons = get_dice_buttons gs in
  let property_buttons = get_property_buttons gs in
  match active_subscreen with
  | None -> BaseGS (gs.buttons, dice_buttons, property_buttons)
  | Some s -> ActiveSubscreenGS (Subscreen.buttons s)

(*currently only gets dice buttons*)
let check_dice_button_clicked_new buttons (x, y) =
  List.exists (fun b -> Button.is_clicked b (x, y)) buttons

let check_dice_button_clicked buttons (x, y) =
  List.find_opt (fun b -> Button.is_clicked b (x, y)) buttons

let check_imap_button_clicked properties (x, y) =
  IM.fold
    (fun n b init ->
      if Button.is_clicked b (x, y) then Some n else init)
    properties None

let check_smap_button_clicked map (x, y) =
  SM.fold
    (fun n b init ->
      if Button.is_clicked b (x, y) then Some n else init)
    map None

let update_board_loc pl dice_val =
  let loc_old = Player.location pl in
  (loc_old + dice_val) mod 40

let get_xy_for_board_loc loc gs =
  match List.assoc_opt loc gs.order_list with
  | None -> failwith "board order and respective coords dont exist"
  | Some v -> v

let next_turn_popup gs =
  let s = SM.find Constants.new_turn gs.subscreens in
  let activated_s = Subscreen.activate s in
  let d_image_map = Subscreen.images s in
  let char_player_image =
    SM.find Constants.new_turn_dynamic d_image_map
  in
  let wiped = Dynamic_image.clear_images char_player_image in
  let new_char_player_image =
    Dynamic_image.add_image wiped
      (List.nth gs.active_players gs.curr_player_index)
  in
  let new_d_image_map =
    SM.add Constants.new_turn_dynamic new_char_player_image d_image_map
  in
  let new_subscreen =
    Subscreen.replace_images activated_s new_d_image_map
  in
  let new_screens =
    SM.add Constants.new_turn new_subscreen gs.subscreens
  in
  NewGS { gs with subscreens = new_screens }

(* let update_info_property_card gs loc = let card_info_screen = SM.find
   Constants.info_cards gs.subscreens in let image_map =
   Subscreen.images card_info_screen in let info_card_image = SM.find
   Constants.info_card_cornerimg image_map in let wiped =
   Dynamic_image.clear_images info_card_image in let new_info_image =
   Dynamic_image.add_image wiped loc in let new_d_image_map = SM.add
   Constants.info_card_cornerimg new_info_image image_map in let
   new_subscreen = Subscreen.replace_images card_info_screen
   new_d_image_map in SM.add Constants.info_cards new_subscreen
   gs.subscreens *)

let update_info_property_card gs loc =
  let info_card_subscreen = gs.info_cards in
  let d_imgs = Subscreen.images info_card_subscreen in
  let curr_img = SM.find Constants.info_cards d_imgs in
  let wiped = Dynamic_image.clear_images curr_img in
  let new_img_todraw = Dynamic_image.add_image wiped loc in
  let new_d_image_map =
    SM.add Constants.info_cards_screen new_img_todraw d_imgs
  in
  Subscreen.replace_images info_card_subscreen new_d_image_map

let respond_to_property_roll gs board_loc =
  let property = IM.find board_loc gs.properties in
  match Property.is_acquirable property with
  | true ->
      let buy_property_screen =
        SM.find Constants.buy_property_screen gs.subscreens
      in
      let activated_buy_property_screen =
        Subscreen.activate buy_property_screen
      in
      let d_image_map =
        Subscreen.images activated_buy_property_screen
      in
      let property_image =
        SM.find Constants.buy_property_dynamic d_image_map
      in
      let wiped = Dynamic_image.clear_images property_image in
      let new_property_image =
        Dynamic_image.add_image wiped board_loc
      in
      let new_d_image_map =
        SM.add Constants.buy_property_dynamic new_property_image
          d_image_map
      in
      let new_subscreen =
        Subscreen.replace_images activated_buy_property_screen
          new_d_image_map
      in
      let new_subscreens =
        SM.add Constants.buy_property_screen new_subscreen gs.subscreens
      in
      new_subscreens
  | false -> gs.subscreens

let respond_to_buy_button gs =
  let plyr_id = List.nth gs.active_players gs.curr_player_index in
  let curr_player = IM.find plyr_id gs.players in
  let curr_location = Player.location curr_player in
  let curr_property = IM.find curr_location gs.properties in
  let purchase_cost = Property.initial_purchase curr_property in
  let updated_money_plyr =
    match Player.money curr_player - purchase_cost with
    | x when x >= 0 -> Player.update_money curr_player x
    | _ ->
        failwith
          "Player can't buy if they don't have enough money to do so. \
           Maybe need some way of dimming a button? or a screen?"
  in
  let acquired_prop = Property.acquire curr_property in
  let new_props = IM.add curr_location acquired_prop gs.properties in
  let new_plyr =
    Player.obtain_property updated_money_plyr acquired_prop
  in
  let new_plyrs = IM.add plyr_id new_plyr gs.players in
  let curr_subscreen =
    SM.find Constants.buy_property_screen gs.subscreens
  in
  let deactivated_screen = Subscreen.deactivate curr_subscreen in
  let new_screens =
    SM.add Constants.buy_property_screen deactivated_screen
      gs.subscreens
  in
  NewGS
    {
      gs with
      subscreens = new_screens;
      players = new_plyrs;
      properties = new_props;
    }

let respond_to_forfeit_button gs =
  let curr_subscreen =
    SM.find Constants.buy_property_screen gs.subscreens
  in
  let deactivated_screen = Subscreen.deactivate curr_subscreen in
  let new_screens =
    SM.add Constants.buy_property_screen deactivated_screen
      gs.subscreens
  in
  NewGS { gs with subscreens = new_screens }

let respond_to_dice_click gs =
  if gs.curr_player_roll then NewGS gs
  else
    let pl_num = List.nth gs.active_players gs.curr_player_index in
    match gs.dice with
    | [ h; t ] -> (
        let first_roll = Die.roll_die h in
        let new_first_die = Die.new_image h first_roll in
        let second_roll = Die.roll_die t in
        let new_second_die = Die.new_image t second_roll in
        let dice_val = first_roll + second_roll in
        match IM.find_opt pl_num gs.players with
        | None -> failwith "doesnt have current player"
        | Some v ->
            let new_board_loc = update_board_loc v dice_val in
            let new_x, new_y = get_xy_for_board_loc new_board_loc gs in
            let v_new =
              Player.move_board new_board_loc v
              |> Player.move_coord
                   (new_x + Constants.player_offset)
                   (new_y + Constants.player_offset)
            in
            let pl_map = IM.add pl_num v_new gs.players in
            let info_map =
              match IM.find_opt new_board_loc gs.properties with
              | None -> update_info_property_card gs 1
              | Some _ -> update_info_property_card gs new_board_loc
            in
            let subscreens =
              match IM.find_opt new_board_loc gs.properties with
              | None -> gs.subscreens
              | Some _ -> respond_to_property_roll gs new_board_loc
            in
            NewGS
              {
                gs with
                dice = [ new_first_die; new_second_die ];
                players = pl_map;
                info_cards = info_map;
                curr_player_roll = true;
                subscreens;
              }
        (* NewGS { gs with dice = [ new_first_die; new_second_die ]} *))
    | _ -> failwith "precondition violation"

let rec determine_factions gs active_plyrs index accum =
  match active_plyrs with
  | [] -> accum
  | h :: t ->
      let plyr = IM.find h gs.players in
      let assigned_plyr =
        Player.assign_faction plyr
          (if index mod 2 = 0 then Stripes else Solids)
      in
      determine_factions gs t (index + 1) (IM.add h assigned_plyr accum)

let rec add_image_team_selection
    gs
    active_plyrs
    solid_index
    stripes_index
    accum
    player_map =
  match active_plyrs with
  | [] -> accum
  | h :: t -> (
      let player = IM.find h player_map in
      let new_index, constant =
        match Player.faction player with
        | Solids -> (solid_index, Constants.solids_selection_slot)
        | Stripes -> (stripes_index, Constants.stripes_selection_slot)
        | _ -> failwith "impossible"
      in
      let slot_img =
        SM.find (constant ^ string_of_int new_index) accum
      in
      let new_slot_img = Dynamic_image.add_image slot_img h in
      match Player.faction player with
      | Solids ->
          add_image_team_selection gs t (solid_index + 1) stripes_index
            (SM.add
               (constant ^ string_of_int new_index)
               new_slot_img accum)
            player_map
      | Stripes ->
          add_image_team_selection gs t solid_index (stripes_index + 1)
            (SM.add
               (constant ^ string_of_int new_index)
               new_slot_img accum)
            player_map
      | _ -> failwith "impossible")

let team_selection_popup gs =
  let new_player_map =
    determine_factions gs gs.active_players 0 IM.empty
  in
  let team_selection_screen =
    SM.find Constants.team_selection_screen gs.subscreens
  in
  let activated_team_selection =
    Subscreen.activate team_selection_screen
  in
  let d_image_map = Subscreen.images team_selection_screen in
  let new_d_map =
    add_image_team_selection gs gs.active_players 1 1 d_image_map
      new_player_map
  in
  let new_subscreen =
    Subscreen.replace_images activated_team_selection new_d_map
  in
  let new_subscreens =
    SM.add Constants.team_selection_screen new_subscreen gs.subscreens
  in
  NewGS
    { gs with subscreens = new_subscreens; players = new_player_map }

let respond_to_property_button gs property_num =
  let property =
    IM.fold
      (fun _ p init ->
        if property_num = Property.board_order p then Some p else init)
      gs.properties None
  in
  match property with
  | None -> failwith "impossible"
  | Some _ ->
      let property_action_screen =
        SM.find Constants.property_action_screen gs.subscreens
      in
      let activated_property_action =
        Subscreen.activate property_action_screen
      in
      let d_image_map = Subscreen.images property_action_screen in
      let info_card_image =
        SM.find Constants.property_action_dynamic_image d_image_map
      in
      let wiped = Dynamic_image.clear_images info_card_image in
      let new_info_image = Dynamic_image.add_image wiped property_num in
      let new_d_image_map =
        SM.add Constants.property_action_dynamic_image new_info_image
          d_image_map
      in
      let new_subscreen =
        Subscreen.replace_images activated_property_action
          new_d_image_map
      in
      let new_subscreens =
        SM.add Constants.property_action_screen new_subscreen
          gs.subscreens
      in
      NewGS { gs with subscreens = new_subscreens }

let response_to_cancel_button gs =
  let subscreen =
    SM.find Constants.property_action_screen gs.subscreens
  in
  let deactivated_subscreen = Subscreen.deactivate subscreen in
  let new_screens =
    SM.add Constants.property_action_screen deactivated_subscreen
      gs.subscreens
  in
  NewGS { gs with subscreens = new_screens }

(* respond_to_dice_click gs 1 is moving player 1 for now. Yet to
   implement multi player movement *)
let respond_to_click gs (x, y) =
  let buttons = get_dice_buttons gs in
  let clicked_button = check_dice_button_clicked buttons (x, y) in
  match clicked_button with
  | None -> NewGS gs (*check if this is false*)
  | Some _ -> respond_to_dice_click gs

let base_click_response gs b_name =
  match b_name with
  | s when s = Constants.exit_game_button -> EndGame
  | s when s = Constants.end_turn_button ->
      let next_pl_ind =
        (gs.curr_player_index + 1) mod List.length gs.active_players
      in
      NewGS
        {
          gs with
          curr_player_index = next_pl_ind;
          curr_player_roll = false;
        }
  | _ -> failwith "not yet implemented"

let new_respond_to_click gs (x, y) =
  let button_response = get_buttons gs in
  match button_response with
  | BaseGS (base_buttons, dice_buttons, property_buttons) -> (
      let dice_clicked =
        check_dice_button_clicked_new dice_buttons (x, y)
      in
      let base_clicked =
        check_smap_button_clicked base_buttons (x, y)
      in
      let property_clicked =
        check_imap_button_clicked property_buttons (x, y)
      in
      match (dice_clicked, base_clicked, property_clicked) with
      | false, None, None -> NewGS gs (*no button was clicked*)
      | true, None, None ->
          respond_to_dice_click gs
          (*dice button was clicked, currently just moving player 1*)
      | false, Some b_name, None -> base_click_response gs b_name
      | false, None, Some board_loc ->
          respond_to_property_button gs board_loc
      | _, _, _ ->
          failwith
            "Impossible, either one button was clicked or no buttons \
             were clicked.")
  | ActiveSubscreenGS button_map -> (
      let button_clicked =
        check_smap_button_clicked button_map (x, y)
      in
      match button_clicked with
      | None -> NewGS gs
      | Some btn_name -> (
          match btn_name with
          | s when s = Constants.property_action_cancel_button ->
              response_to_cancel_button gs
          | s when s = Constants.buy_button -> respond_to_buy_button gs
          | s when s = Constants.forfeit_button ->
              respond_to_forfeit_button gs
          | _ -> failwith "TODO "))
(*these pattern matches here will be identical to the ones in home
  screen, a bunch of different button names*)
