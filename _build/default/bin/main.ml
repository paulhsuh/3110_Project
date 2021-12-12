open Game

let file = Yojson.Basic.from_file "data/standard.json"

(*[base_hs] is the static home screen that will not be changed, used for
  redrawing purposes.*)
let base_hs = Home_screen.get_home_screen_from_json file

let hs = ref (Home_screen.get_home_screen_from_json file)

let gs = ref (Game_screen.get_game_screen_from_json file)

(**[redraw_hs_and_sleep _] draws the base home screen and then sleeps
   the system for [0.5] seconds. *)
let redraw_hs_and_sleep () =
  Gui.draw_home_screen base_hs;
  Unix.sleepf 0.5

(**[update_home_screen _] checks for a mouse click and updates home
   screen based upon that mouse click. If the mouse click caused a
   change on the home screen, the home screen is redrawn.*)
let rec update_home_screen _ =
  let coords = Gui.mouse_click () in
  match Home_screen.respond_to_click !hs coords with
  | NoButtonClicked -> update_home_screen ()
  | NewHS (new_hs, sleep) ->
      let _ = if sleep then redraw_hs_and_sleep () else () in
      Gui.draw_home_screen new_hs;
      hs := new_hs;
      update_home_screen ()
  | ProceedToGS -> Gui.draw_game_screen !gs

(**[run_game _] initializes a new empty Gui window, draws the home
   screen, and continually updates the home screen.*)
let run_game _ =
  Gui.initialize_window !hs;
  Gui.draw_home_screen !hs;
  update_home_screen ();
  Gui.press_button 'c'

let _ = run_game ()
