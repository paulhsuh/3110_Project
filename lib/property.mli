type control =
  | Base
  | Set
  | ControlOne
  | ControlTwo
  | ControlThree
  | ControlFour
  | TakeOver  (**The varying control levels that a property can have.*)

type t
(**The abstract data type representing a property.*)

val image_name : t -> string
(**[image_name property] is the image name of the property [property].*)

val get_control : t -> control
(**[get_control property] is the control level of the [property].*)

val acquire : t -> t
(**[acquire property] gives the [property] an owner. *)

val initial_purchase : t -> int

val set : t -> string

val is_acquirable : t -> bool
(**[is_acquirable property] is [true] if the [property] has an owner and
   [false] otherwise.*)

val upgrade_to_set : t -> t
(**[upgrade_to_set property] upgrades the control level of the
   [property] from [Base] to [Set]. Requires: [property] was acquired by
   an owner, [not (is_acquirable property)]. [property] has control
   level [Base].*)

val degrade_from_set : t -> t
(**[degrade_from_set property] degrades the [property] so it is no
   longer part of a complete set. Requires: [property] was acquired by
   an owner, [not (is_acquirable property)]. [property] has control
   level [Set].*)

val upgrade : t -> t
(**[upgrade property] upgrades the [property] to the next control level.
   Requires: [is_upgradable property]. *)

val upgrade_cost : t -> int
(**[upgrade_cost property] is the cost to upgrade to the next control
   level. Requires: [is_upgradable property].*)

val is_upgradable : t -> bool
(**[is_upgradable property] is whether [property] has a control level
   that is one of [Set], [ControlOne], [ControlTwo], [ControlThree],
   [ControlFour].*)

val degrade : t -> t
(**[degrade property] degrades the [property] down to a lower control
   level. Requires: [is_degradable property].*)

val degrade_refund : t -> int
(**[degrade_refund] is the money refunded when a property is degraded to
   the previous control level. Requires: [is_degradable property].*)

val is_degradable : t -> bool
(**[degradable property] is whether the is whether [property] has a
   control level that is one of [ControlOne], [ControlTwo],
   [ControlThree], [ControlFour], [TakeOver].*)

val get_rent : t -> int
(**[get_rent property] gives the current rent of the property [p].
   Requires: [property] was acquired by an owner,
   [not (is_acquirable property)].*)

val withdraw : t -> t
(** [withdraw property] removes the owner from the property. Requires:
    [property] was acquired by an owner. [property] is at control level
    of [Set] or [Base].*)

val withdraw_refund : t -> int
(**[withdraw_refund property] is the amount paid when the owner
   relinquishes ownership of a property. Requires: [property] was
   acquired by an owner. [property] is at control level of [Set] or
   [Base]. *)

val board_order : t -> int
(**[board_order property] is the location that this property has on the
   board.*)

val button : t -> Button.t
(**[button property] is the button associated with this property on the
   board.*)

val get_property_from_json : Yojson.Basic.t -> int * t
(**[get_property_from_json js] is the property that [js] represents
   along with its identifier, the board location.*)

val get_properties_from_json : Yojson.Basic.t -> (int * t) list
(**[get_properties_from_json js] are the properties that [js] represents
   along with their identifiers, the board location.*)
