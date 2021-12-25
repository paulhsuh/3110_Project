open Yojson.Basic.Util

type control =
  | Base
  | Set
  | ControlOne
  | ControlTwo
  | ControlThree
  | ControlFour
  | TakeOver

type rent = {
  base : int;
  set : int;
  control_one : int;
  control_two : int;
  control_three : int;
  control_four : int;
  take_over : int;
}

type cost = {
  initial_purchase : int;
  build_control : int;
  remove_control : int;
  withdraw : int;
}

type status =
  | Owned
  | Unowned

type t = {
  name : string;
  button : Button.t;
  board_order : int;
  set : string;
  rent : rent;
  control : control;
  cost : cost;
  status : status;
  image_name : string;
}

let image_name p = p.image_name

let get_control p = p.control

let set p = p.set

let acquire p = { p with status = Owned }

let initial_purchase p = p.cost.initial_purchase

let is_acquirable p =
  match p.status with
  | Owned -> false
  | Unowned -> true

let upgrade_to_set p =
  {
    p with
    control =
      (match p.control with
      | Base -> Set
      | _ -> failwith "Only a Base property can be made into a Set.");
  }

let degrade_from_set p =
  {
    p with
    control =
      (match p.control with
      | Set -> Base
      | _ -> failwith "Only a Set property can turned back into a Base.");
  }

let upgrade p =
  {
    p with
    control =
      (match p.control with
      | Base -> failwith "Base property is not upgradable."
      | Set -> ControlOne
      | ControlOne -> ControlTwo
      | ControlTwo -> ControlThree
      | ControlThree -> ControlFour
      | ControlFour -> TakeOver
      | TakeOver -> failwith "Takeover property is not upgradable.");
  }

let upgrade_cost p = p.cost.build_control

let is_upgradable p =
  match p.control with
  | Base
  | TakeOver ->
      false
  | _ -> true

let degrade p =
  {
    p with
    control =
      (match p.control with
      | Base -> failwith "Base property is not degradable."
      | Set -> failwith "Set property is not degradable"
      | ControlOne -> Set
      | ControlTwo -> ControlOne
      | ControlThree -> ControlTwo
      | ControlFour -> ControlThree
      | TakeOver -> ControlFour);
  }

let degrade_refund p = p.cost.remove_control

let is_degradable p =
  match p.control with
  | Set
  | Base ->
      false
  | _ -> true

let get_rent p =
  match p.control with
  | Base -> p.rent.base
  | Set -> p.rent.set
  | ControlOne -> p.rent.control_one
  | ControlTwo -> p.rent.control_two
  | ControlThree -> p.rent.control_three
  | ControlFour -> p.rent.control_four
  | TakeOver -> p.rent.take_over

let withdraw p = { p with status = Unowned }

let withdraw_refund p = p.cost.withdraw

let board_order p = p.board_order

let button p = p.button

let get_cost_from_json json =
  {
    initial_purchase = json |> member "purchase_cost" |> to_int;
    build_control = json |> member "upgrade_cost" |> to_int;
    remove_control = json |> member "degrade_refund" |> to_int;
    withdraw = json |> member "mortgage" |> to_int;
  }

let get_rent_from_json json =
  {
    base = json |> member "base" |> to_int;
    set = json |> member "set" |> to_int;
    control_one = json |> member "control_1" |> to_int;
    control_two = json |> member "control_2" |> to_int;
    control_three = json |> member "control_3" |> to_int;
    control_four = json |> member "control_4" |> to_int;
    take_over = json |> member "take_over" |> to_int;
  }

let get_property_from_json json =
  let _, button =
    json |> member "button" |> Button.get_button_from_json
  in
  let board_order = json |> member "board_order" |> to_int in
  ( board_order,
    {
      name = json |> member "property_name" |> to_string;
      button;
      board_order;
      set = json |> member "set" |> to_string;
      rent = json |> member "fee" |> get_rent_from_json;
      control = Base;
      cost = json |> member "cost" |> get_cost_from_json;
      status = Unowned;
      image_name = json |> member "image_name" |> to_string;
    } )

let get_properties_from_json json =
  json |> to_list |> List.map get_property_from_json
