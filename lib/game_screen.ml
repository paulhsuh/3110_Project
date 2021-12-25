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
  | ClosingGS of t * t
  | AnimatePlayerGS of t * int * int

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
  { gs with subscreens = new_screens }

let update_info_cards gs loc player =
  let info_card_screen = gs.info_cards in
  let d_imgs = Subscreen.images info_card_screen in
  let property_image = SM.find Constants.property_info_card d_imgs in
  let wiped_property = Dynamic_image.clear_images property_image in
  let new_loc =
    match IM.find_opt loc gs.properties with
    | None -> 0
    | Some _ -> loc
  in
  let new_property_image =
    Dynamic_image.add_image wiped_property new_loc
  in
  let new_d_image_map =
    SM.add Constants.property_info_card new_property_image d_imgs
  in

  let food = Player.food player in
  let food_health =
    match food with
    | None -> 0
    | Some fd -> Food.health fd
  in
  let food_image = SM.find Constants.food_info_card new_d_image_map in
  let wiped_food = Dynamic_image.clear_images food_image in
  let new_food_image = Dynamic_image.add_image wiped_food food_health in
  let new_d_image_map' =
    SM.add Constants.food_info_card new_food_image new_d_image_map
  in
  let weapon = Player.weapon player in
  let weapon_dmg =
    match weapon with
    | None -> 0
    | Some wn -> Weapon.damage wn
  in
  let weapon_image =
    SM.find Constants.weapon_info_card new_d_image_map'
  in
  let wiped_weapon = Dynamic_image.clear_images weapon_image in
  let new_weapon_image =
    Dynamic_image.add_image wiped_weapon weapon_dmg
  in
  let updated_d_img_map =
    SM.add Constants.weapon_info_card new_weapon_image new_d_image_map'
  in
  Subscreen.replace_images info_card_screen updated_d_img_map

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
      { gs with subscreens = new_subscreens }
  | false -> gs

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

let respond_to_food_roll gs fs =
  let food = Food_stack.generate_food fs in
  let food_subscreen =
    SM.find Constants.food_pick_up_screen gs.subscreens
  in
  let activated_food_pu = Subscreen.activate food_subscreen in
  let d_image_map = Subscreen.images activated_food_pu in
  let food_image = SM.find Constants.food_pick_up_dynamic d_image_map in
  let wiped = Dynamic_image.clear_images food_image in
  let new_food_image =
    match Food.name food with
    | s when s = Constants.burger -> Dynamic_image.add_image wiped 15
    | s when s = Constants.pizza -> Dynamic_image.add_image wiped 10
    | s when s = Constants.instant_ramen ->
        Dynamic_image.add_image wiped 5
    | s when s = Constants.steak -> Dynamic_image.add_image wiped 20
    | _ -> failwith "food impossible"
  in
  let new_d_image_map =
    SM.add Constants.food_pick_up_dynamic new_food_image d_image_map
  in
  let subscreen_open =
    Subscreen.replace_images activated_food_pu new_d_image_map
  in
  let subscreen_closed =
    Subscreen.replace_images food_subscreen new_d_image_map
  in
  let subscreens_open =
    SM.add Constants.food_pick_up_screen subscreen_open gs.subscreens
  in
  let subscreens_close =
    SM.add Constants.food_pick_up_screen subscreen_closed gs.subscreens
  in

  let plyr_id = List.nth gs.active_players gs.curr_player_index in
  let curr_player = IM.find plyr_id gs.players in
  let player_new =
    if Player.health curr_player < 100 then
      Player.update_health curr_player (Food.health food)
    else Player.obtain_food curr_player (Some food)
  in
  let new_plyrs = IM.add plyr_id player_new gs.players in

  ( { gs with subscreens = subscreens_open; players = new_plyrs },
    { gs with subscreens = subscreens_close; players = new_plyrs } )

let respond_to_weapon_roll gs ws =
  let weapon = Weapon_stack.generate_weapon ws in
  let weapon_subscreen =
    SM.find Constants.weapon_pick_up_screen gs.subscreens
  in
  let activated_weapon_pu = Subscreen.activate weapon_subscreen in
  let d_image_map = Subscreen.images activated_weapon_pu in
  let weapon_image =
    SM.find Constants.weapon_pick_up_dynamic d_image_map
  in
  let wiped = Dynamic_image.clear_images weapon_image in
  let new_weapon_image =
    match Weapon.name weapon with
    | s when s = Constants.fork -> Dynamic_image.add_image wiped 5
    | s when s = Constants.shaving_razor ->
        Dynamic_image.add_image wiped 10
    | s when s = Constants.baseball_bat ->
        Dynamic_image.add_image wiped 15
    | s when s = Constants.pistol -> Dynamic_image.add_image wiped 40
    | _ -> failwith "food impossible"
  in
  let new_d_image_map =
    SM.add Constants.weapon_pick_up_dynamic new_weapon_image d_image_map
  in
  let subscreen_open =
    Subscreen.replace_images activated_weapon_pu new_d_image_map
  in
  let subscreen_closed =
    Subscreen.replace_images weapon_subscreen new_d_image_map
  in
  let subscreens_open =
    SM.add Constants.weapon_pick_up_screen subscreen_open gs.subscreens
  in
  let subscreens_close =
    SM.add Constants.weapon_pick_up_screen subscreen_closed
      gs.subscreens
  in
  let plyr_id = List.nth gs.active_players gs.curr_player_index in
  let curr_player = IM.find plyr_id gs.players in
  let new_player = Player.obtain_weapon curr_player (Some weapon) in
  let new_plyrs = IM.add plyr_id new_player gs.players in

  ( { gs with subscreens = subscreens_open; players = new_plyrs },
    { gs with subscreens = subscreens_close; players = new_plyrs } )

let respond_to_action_space_roll gs ac =
  let subscreen, constant =
    match Action_space.name ac with
    | s when s = Constants.bribe_money ->
        ( SM.find_opt Constants.bribe_money_screen gs.subscreens,
          Constants.bribe_money_screen )
    | s when s = Constants.prison_fight ->
        ( SM.find_opt Constants.prison_fight_screen gs.subscreens,
          Constants.prison_fight_screen )
    | s when s = Constants.warden's_favor ->
        ( SM.find_opt Constants.wardens_favor_screen gs.subscreens,
          Constants.wardens_favor_screen )
    | s when s = Constants.go_to_wardens ->
        ( SM.find_opt Constants.go_to_warden_screen gs.subscreens,
          Constants.go_to_warden_screen )
    | _ -> (None, "")
  in
  let subscreens_new =
    match subscreen with
    | None -> gs.subscreens
    | Some sub -> (
        let activated_subscreen = Subscreen.activate sub in
        match constant with
        | "" -> gs.subscreens
        | _ -> SM.add constant activated_subscreen gs.subscreens)
  in

  let plyr_id = List.nth gs.active_players gs.curr_player_index in
  let curr_player = IM.find plyr_id gs.players in
  let money_change = Action_space.money ac in
  let health_change = Action_space.take_health ac in
  let new_player = Player.update_health curr_player health_change in
  let new_player' =
    Player.update_money new_player
      (Player.money new_player + money_change)
  in
  let new_loc = Action_space.new_board_location ac in
  let player_new = Player.move_board new_loc new_player' in
  let next_x, next_y = List.assoc new_loc gs.order_list in
  let player_new' = Player.move_coord next_x next_y player_new in
  let new_plyrs = IM.add plyr_id player_new' gs.players in
  ( { gs with subscreens = subscreens_new; players = new_plyrs },
    { gs with players = new_plyrs } )

let respond_to_roll gs board_loc =
  let food = IM.find_opt board_loc gs.food_stacks in
  let weapon = IM.find_opt board_loc gs.weapon_stacks in
  let action_space = IM.find_opt board_loc gs.action_spaces in
  match (food, weapon, action_space) with
  | None, None, None -> NewGS (respond_to_property_roll gs board_loc)
  | Some fs, None, None ->
      let opened, closed = respond_to_food_roll gs fs in
      ClosingGS (opened, closed)
  | None, Some ws, None ->
      let opened, closed = respond_to_weapon_roll gs ws in
      ClosingGS (opened, closed)
  | None, None, Some ac ->
      let opened, closed = respond_to_action_space_roll gs ac in
      ClosingGS (opened, closed)
  | _, _, _ -> failwith "not possible"

type direction =
  | Left of int * int
  | Right of int * int
  | Up of int * int
  | Down of int * int

type animation_response =
  | InProgress of t
  | Finished of t

let move_player gs pl_num target_loc =
  let plyr = IM.find pl_num gs.players in
  let curr_board_order = Player.location plyr in
  let next_board_order = (curr_board_order + 1) mod 40 in
  let next_x, next_y = List.assoc next_board_order gs.order_list in
  let curr_x, curr_y = (Player.x_coord plyr, Player.y_coord plyr) in
  print_endline ("Target loc:" ^ string_of_int target_loc);
  print_endline ("Player board order:" ^ string_of_int curr_board_order);
  print_endline ("Next board order:" ^ string_of_int next_board_order);

  let next_step =
    if curr_board_order >= Constants.corner_4 then
      Down (0, -Constants.animation_speed)
    else if curr_board_order >= Constants.corner_3 then
      Right (Constants.animation_speed, 0)
    else if curr_board_order >= Constants.corner_2 then
      Up (0, Constants.animation_speed)
    else Left (-Constants.animation_speed, 0)
  in
  let stepped_x, stepped_y =
    match next_step with
    | Left (x_step, y_step) ->
        let step_x = x_step + curr_x in
        let step_y = y_step + curr_y in
        if step_x < next_x then (next_x, step_y) else (step_x, step_y)
    | Right (x_step, y_step) ->
        let step_x = x_step + curr_x in
        let step_y = y_step + curr_y in
        if step_x > next_x then (next_x, step_y) else (step_x, step_y)
    | Up (x_step, y_step) ->
        let step_x = x_step + curr_x in
        let step_y = y_step + curr_y in
        if step_y > next_y then (step_x, next_y) else (step_x, step_y)
    | Down (x_step, y_step) ->
        let step_x = x_step + curr_x in
        let step_y = y_step + curr_y in
        if step_y < next_y then (step_x, next_y) else (step_x, step_y)
  in
  print_endline ("Stepped_x:" ^ string_of_int stepped_x);
  print_endline ("Stepped_y:" ^ string_of_int stepped_y);

  let moved_player = Player.move_coord stepped_x stepped_y plyr in
  match stepped_x = next_x && stepped_y = next_y with
  | true ->
      let new_player =
        Player.move_board next_board_order moved_player
      in
      let new_players = IM.add pl_num new_player gs.players in
      let new_gs = { gs with players = new_players } in
      if next_board_order = target_loc then (
        print_string "finished";
        Finished new_gs)
      else InProgress new_gs
  | false ->
      let new_players = IM.add pl_num moved_player gs.players in
      let new_gs = { gs with players = new_players } in
      InProgress new_gs

let new_respond_to_dice_click gs =
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
        let new_gs =
          { gs with dice = [ new_first_die; new_second_die ] }
        in
        match IM.find_opt pl_num gs.players with
        | None -> failwith "doesnt have current player"
        | Some v ->
            let new_board_loc = update_board_loc v dice_val in
            AnimatePlayerGS (new_gs, pl_num, new_board_loc))
    | _ -> failwith "precondition violation"

let update_on_roll gs pl_num board_loc =
  let plyr = IM.find pl_num gs.players in
  (* let gs_open, gs_closed = respond_to_roll gs board_loc in *)
  let info_map =
    match IM.find_opt board_loc gs.properties with
    | None -> update_info_cards gs 0 plyr
    | Some _ -> update_info_cards gs board_loc plyr
  in
  match respond_to_roll gs board_loc with
  | NewGS gs' ->
      NewGS { gs' with info_cards = info_map; curr_player_roll = true }
  | ClosingGS (opened, closed) ->
      ClosingGS
        ( { opened with info_cards = info_map; curr_player_roll = true },
          { closed with info_cards = info_map; curr_player_roll = true }
        )
  | _ -> failwith "not possible"
(* ClosingGS ( { gs_open with info_cards = info_map; curr_player_roll =
   true }, { gs_closed with info_cards = info_map; curr_player_roll =
   true } ) *)

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

let assign_players_faction gs =
  let new_p_map = determine_factions gs gs.active_players 0 IM.empty in
  { gs with players = new_p_map }

let activate_team_selection gs =
  let team_selection_screen =
    SM.find Constants.team_selection_screen gs.subscreens
  in
  let activated_team_selection =
    Subscreen.activate team_selection_screen
  in
  let d_image_map = Subscreen.images team_selection_screen in
  let new_d_map =
    add_image_team_selection gs gs.active_players 1 1 d_image_map
      gs.players
  in
  let new_subscreen =
    Subscreen.replace_images activated_team_selection new_d_map
  in
  let new_subscreens =
    SM.add Constants.team_selection_screen new_subscreen gs.subscreens
  in
  { gs with subscreens = new_subscreens }

let rec get_properties_in_set properties set acc =
  match properties with
  | [] -> acc
  | h :: t ->
      if Property.set h = set then get_properties_in_set t set (acc + 1)
      else get_properties_in_set t set acc

let check_property_set gs property =
  let plyr_id = List.nth gs.active_players gs.curr_player_index in
  let curr_player = IM.find plyr_id gs.players in
  let player_properties = Player.properties curr_player in
  let property_set = Property.set property in
  let properties_of_set =
    if property_set = Constants.mail then 2 else 3
  in
  let properties_number =
    get_properties_in_set player_properties property_set 0
  in
  if properties_of_set = properties_number then true else false

let respond_to_property_button gs property_num =
  let property =
    IM.fold
      (fun _ p init ->
        if property_num = Property.board_order p then Some p else init)
      gs.properties None
  in
  match property with
  | None -> failwith "impossible"
  | Some p ->
      let property_action_screen =
        SM.find Constants.property_action_screen gs.subscreens
      in
      let activated_property_action =
        Subscreen.activate property_action_screen
      in
      let btn_map = Subscreen.buttons activated_property_action in
      let has_set = check_property_set gs p in
      let new_btns =
        print_string (string_of_bool has_set);
        if has_set then SM.map (fun v -> Button.undim v) btn_map
        else SM.map (fun v -> Button.dim v) btn_map
      in
      let cancel_button =
        SM.find Constants.property_action_cancel_button btn_map
      in
      let activate_cancel = Button.undim cancel_button in
      let new_buttons =
        SM.add Constants.property_action_cancel_button activate_cancel
          new_btns
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
      let new_subscreen' =
        Subscreen.replace_buttons new_subscreen new_buttons
      in
      let new_subscreens =
        SM.add Constants.property_action_screen new_subscreen'
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

let base_click_response gs b_name =
  match b_name with
  | s when s = Constants.exit_game_button -> EndGame
  | s when s = Constants.end_turn_button ->
      let next_pl_ind =
        (gs.curr_player_index + 1) mod List.length gs.active_players
      in
      let plyr_id = List.nth gs.active_players next_pl_ind in
      let player = IM.find plyr_id gs.players in
      let player_loc = Player.location player in
      let subscreen = update_info_cards gs player_loc player in
      NewGS
        {
          gs with
          curr_player_index = next_pl_ind;
          curr_player_roll = false;
          info_cards = subscreen;
        }
  | _ -> failwith "not yet implemented"

type img =
  | HP
  | Money
  | Background

let image_constructor slot_number img =
  match img with
  | HP -> "slot_hp_" ^ slot_number
  | Money -> "slot_money_" ^ slot_number
  | Background -> "slot_" ^ slot_number

let rec update_info_for_team players faction accum index gs =
  match players with
  | [] ->
      print_string "empty";
      accum
  | h :: t -> (
      let player = IM.find h gs.players in
      match Player.faction player = faction with
      | true ->
          let d_images = Subscreen.images accum in
          let hp_img_name =
            image_constructor (string_of_int index) HP
          in
          let money_img_name =
            image_constructor (string_of_int index) Money
          in
          let background_img_name =
            image_constructor (string_of_int index) Background
          in
          let plyr_health = Player.health player in
          let plyr_money = Player.money player in
          let hp_img =
            Dynamic_image.clear_images (SM.find hp_img_name d_images)
          in
          let money_img =
            Dynamic_image.clear_images (SM.find money_img_name d_images)
          in
          let background_img =
            Dynamic_image.clear_images
              (SM.find background_img_name d_images)
          in
          let new_hp_image =
            Dynamic_image.add_images hp_img plyr_health
          in
          let new_money_image =
            Dynamic_image.add_images money_img plyr_money
          in
          let new_background_image =
            Dynamic_image.add_image background_img h
          in
          let d_images' = SM.add hp_img_name new_hp_image d_images in
          let d_images'' =
            SM.add money_img_name new_money_image d_images'
          in
          let d_images''' =
            SM.add background_img_name new_background_image d_images''
          in
          update_info_for_team t faction
            (Subscreen.replace_images accum d_images''')
            (index + 1) gs
      | false ->
          print_string "false";
          update_info_for_team t faction accum index gs)

let initialize_team_info gs =
  let solids_info = SM.find Constants.solids_info_screen gs.team_info in
  let stripes_info =
    SM.find Constants.stripes_info_screen gs.team_info
  in
  let solid_subscreen =
    update_info_for_team gs.active_players Solids solids_info 1 gs
  in
  let stripes_subscreen =
    update_info_for_team gs.active_players Stripes stripes_info 1 gs
  in
  let team_info_subscreen' =
    SM.add Constants.solids_info_screen solid_subscreen gs.team_info
  in
  let team_info_subscreen'' =
    SM.add Constants.stripes_info_screen stripes_subscreen
      team_info_subscreen'
  in
  { gs with team_info = team_info_subscreen'' }

let update_card_info gs pl_num board_loc =
  let plyr = IM.find pl_num gs.players in
  let info_map =
    match IM.find_opt board_loc gs.properties with
    | None -> update_info_cards gs 0 plyr
    | Some _ -> update_info_cards gs board_loc plyr
  in
  { gs with info_cards = info_map }

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
          new_respond_to_dice_click gs
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
