module type AlphabetType = {
  type t
  let size: int
  let ord: t => int
}

module type S = {
  type sym
  type t
  let create: int => t
  let append: (t, sym) => unit
  let skip: (t, sym) => unit
  let compare: (t, t) => bool
}

module Make = (Sym: AlphabetType): (S with type sym := Sym.t) => {
  type t = {
    u: ref<int>,
    d: ref<int>,
    p: int,
  }

  let pow = (a, n, m) => {
    let rec aux = (n, acc) =>
      switch n {
      | 0 => acc
      | _ => aux(n - 1, mod(acc * a, m))
      }

    aux(n, 1)
  }

  let create = p => {
    u: ref(0),
    d: ref(0),
    p: p,
  }

  let append = ({u, d, p}, c) => {
    u := mod(u.contents * Sym.size + Sym.ord(c), p)
    d := d.contents + 1
  }

  let skip = ({u, d, p}, c) => {
    let mod = (a, b) => mod(mod(a, b) + b, b)
    u := mod(u.contents - Sym.ord(c) * pow(Sym.size, d.contents - 1, p), p)
    d := d.contents - 1
  }

  let compare = (a, b) => a == b
}
