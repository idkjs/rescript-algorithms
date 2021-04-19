type t<'a> = {
  array: ref<array<option<'a>>>,
  size: ref<int>,
}

exception InvalidArgument(string)
exception InconsistentState(string)
exception ArrayIsEmpty

let create = () => {
  array: ref([]),
  size: ref(0),
}

let length = dynamic_array => dynamic_array.size.contents

let contents = x =>
  switch x {
  | None => raise(InconsistentState("expected a value"))
  | Some(value) => value
  }

let execute_if_index_is_valid = (dynamic_array, index, fn) => {
  let {size, array} = dynamic_array
  let size = size.contents
  let array = array.contents
  if index < 0 || index >= size {
    raise(InvalidArgument("index out of bounds"))
  } else {
    fn(array)
  }
}

let get = (dynamic_array, index) => {
  let fn = array => array[index] |> contents
  execute_if_index_is_valid(dynamic_array, index, fn)
}

let set = (dynamic_array, index, value) => {
  let fn = array => array[index] = Some(value)
  execute_if_index_is_valid(dynamic_array, index, fn)
}

let push = (dynamic_array, value) => {
  let {size, array} = dynamic_array
  let available_space = Array.length(array.contents)

  if size.contents == available_space {
    array := Array.append(array.contents, Array.make(available_space + 1, None))
  }

  array.contents[size.contents] = Some(value)
  size := size.contents + 1
}

let pop = dynamic_array => {
  let {size, array} = dynamic_array
  let available_space = Array.length(array.contents)

  if size.contents == 0 {
    raise(ArrayIsEmpty)
  } else {
    let index = size.contents - 1
    let value = array.contents[index]
    array.contents[index] = None

    if size.contents < available_space / 4 {
      array := Array.sub(array.contents, 0, available_space / 2)
    }

    size := size.contents - 1
    contents(value)
  }
}

let swap = (dynamic_array, a, b) => {
  let a' = get(dynamic_array, a)
  let b' = get(dynamic_array, b)
  set(dynamic_array, a, b')
  set(dynamic_array, b, a')
}

let inspect = dynamic_array => Js.Array.toString(dynamic_array.array.contents)
