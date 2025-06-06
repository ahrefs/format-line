let a = f ~x:y

let a = f ~x y

let ( * ) = Float.mul

let ( *. ) = Int.mul

let ( / ) = Float.div

let ( - ) = Float.div

let a = -3

let a = ~-3

let a = !myref

let ( let* ) = Result.bind

let ( and+ ) = Cmdliner.Term.Arg.( and+ )

let ( .%[] ) = Bytes.get

let a = match gadt with A -> "A" | _ -> .

let a = `A

let a =
  let%lwt a = Lwt.return 1 in
  a

let f : type a. ?locale:_ -> a kind -> unit -> a t = f

let f : type a. locale:_ -> a kind -> unit -> a t = f

let f : type a b c d. locale:_ -> a kind -> unit -> a t = f

let a = Typ.alias [%type: < .. > ] t

type t = [> `A of [ | t]]

let a = func ~arg:"my string"

let a = func ~arg:{|my string|}

let l = [%ext {|aaaa|}]

[@@@attr "my attr"]

let a = f [@@attr "my attr"]

let a =
  begin [@attr "my attr"]
    f
  end

let f ?(optionnal = default) = g optionnal

module type S = sig
  val f : arg:t -> unit

  type 'a t = 'a list
end

let a = obj#meth

let f ?arg1 ?arg2 = g arg1 arg2

let v = (f ~a : 'a t)

let v = (f ~a:b : 'a t)

let a = match x with #F.t as e -> f e

let a = []

let a = [ a; b ]