type binding<'a, 'b> = {
  key: 'a,
  value: 'b,
}

type slot<'a, 'b> =
  | Empty
  | Deleted
  | Filled(binding<'a, 'b>)

type t<'a, 'b> = {
  pre_hash: 'a => int,
  hash: (int, int, int) => int,
  table: ref<array<slot<'a, 'b>>>,
  num_bindings: ref<int>,
  load: float,
}

let create = (~init_size=1, ~pre_hash, ~hash, ()) => {
  pre_hash: pre_hash,
  hash: hash,
  table: ref(Array.make(init_size, Empty)),
  num_bindings: ref(0),
  load: 0.25,
}

exception Key_not_found
exception Inconsistent_state

let length = map => map.num_bindings.contents

let find_index = (map, key) => {
  let {hash, pre_hash, table} = map
  let table = table.contents

  let num_slots = Array.length(table)
  let hash = hash(num_slots, pre_hash(key))

  let rec search = iter => {
    if iter == num_slots {
      raise(Key_not_found)
    }

    let index = hash(iter)
    let slot = table[index]
    switch slot {
    | Empty => raise(Key_not_found)
    | Deleted => search(iter + 1)
    | Filled(binding) if binding.key == key => index
    | _ => search(iter + 1)
    }
  }

  search(0)
}

let find = (map, key) => {
  let index = find_index(map, key)
  let slot = map.table.contents[index]
  switch slot {
  | Empty | Deleted => raise(Inconsistent_state)
  | Filled(binding) => binding.value
  }
}

let iter = (f, map) => {
  let f = x =>
    switch x {
    | Empty => ()
    | Deleted => ()
    | Filled(binding) => f(binding.key, binding.value)
    }

  Array.iter(f, map.table.contents)
}

let expected_num_slots = map => {
  let {table, num_bindings} = map
  let num_slots = Array.length(table.contents)
  let load = float_of_int(num_bindings.contents) /. float_of_int(num_slots)
  switch load {
  | l if l > map.load *. 2. => num_slots * 2
  | l if l < map.load /. 2. && num_slots > 1 => num_slots / 2
  | _ => num_slots
  }
}

let insert = ({hash, pre_hash, table, num_bindings}) => {
  let table = table.contents
  let num_slots = Array.length(table)
  let hash = hash(num_slots)

  (key, value) => {
    let hash = hash(pre_hash(key))

    let rec aux = iter => {
      if iter == num_slots {
        raise(Key_not_found)
      }

      let index = hash(iter)
      let slot = table[index]
      switch slot {
      | Empty | Deleted =>
        table[index] = Filled({key: key, value: value})
        num_bindings := num_bindings.contents + 1
      | Filled(binding) if binding.key == key => table[index] = Filled({key: key, value: value})
      | _ => aux(iter + 1)
      }
    }

    aux(0)
  }
}

let rehash = (map, expected_num_slots) => {
  let {pre_hash, hash} = map
  let temp_map = create(~pre_hash, ~hash, ~init_size=expected_num_slots, ())

  let populate = insert(temp_map)
  iter(populate, map)

  map.table := temp_map.table.contents
}

let maybe_rehash = map => {
  let num_slots = Array.length(map.table.contents)
  let expected_num_slots = expected_num_slots(map)
  if num_slots != expected_num_slots {
    rehash(map, expected_num_slots)
  }
}

let add = (map, key, value) => {
  insert(map, key, value)
  maybe_rehash(map)
}

let remove = (map, key) => {
  let index = find_index(map, key)
  map.table.contents[index] = Deleted
  map.num_bindings := map.num_bindings.contents - 1
  maybe_rehash(map)
}
