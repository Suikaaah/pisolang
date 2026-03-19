type base =
  | BaseUnit
  | BaseIdent of int
  | BaseVar of int
  | BaseProd of base List2.t
  | BaseApp of base List1.t * int
[@@deriving show]

type iso =
  | IsoBiArrow of base * base
  | IsoArrow of iso * iso
  | IsoVar of int
  | IsoInv of iso
[@@deriving show]

type 'a subst = 'a * int

val is_fv_base : int -> base -> bool
val is_fv_iso : int -> iso -> bool
val invert : iso -> iso
val fv_base : base -> Util.IntSet.t
val fv_iso : iso -> Util.IntSet.t
val subst_base : base subst -> base -> base
val subst_iso : iso subst -> iso -> iso
val subst_base_iso : base subst -> iso -> iso
val subst_base_bulk : base subst list -> base -> base
val subst_iso_bulk : iso subst list -> base subst list -> iso -> iso
val pp_base_remap : string Util.IntMap.t -> Format.formatter -> base -> unit
val pp_iso_remap : string Util.IntMap.t -> Format.formatter -> iso -> unit
val push_inv : iso -> iso
