open Yojson.Basic.Util

type t = {
  name : string;
  images : string list;
  image_path : string;
  x_coord : int;
  y_coord : int;
  width : int;
  height : int;
}

let name d = d.name

let x_coord d = d.x_coord

let y_coord d = d.y_coord

let width d = d.width

let height d = d.height

let images d = d.images

let clear_images d = { d with images = [] }

let add_image d x =
  {
    d with
    images = (d.image_path ^ string_of_int x ^ ".png") :: d.images;
  }

let get_image_from_json json =
  {
    name = json |> member "name" |> to_string;
    images = json |> member "images" |> to_list |> List.map to_string;
    image_path = json |> member "image_path" |> to_string;
    x_coord = json |> member "x_coord" |> to_int;
    y_coord = json |> member "y_coord" |> to_int;
    width = json |> member "width" |> to_int;
    height = json |> member "height" |> to_int;
  }

let get_images_from_json json =
  json |> to_list |> List.map get_image_from_json