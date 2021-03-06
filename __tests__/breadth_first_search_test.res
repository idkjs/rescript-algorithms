open Jest
open Expect

describe("Breadth First Search", () => {
  open BreadthFirstSearch

  test("single node", () => {
    let adj_list = list{{id: "A", neighbours: list{}}}

    let expected_level = Some(0)
    let expected_parent = None

    let {level, parent} = search(adj_list, "A")
    let level = Hashtbl.find(level, "A")
    let parent = Hashtbl.find(parent, "A")

    expect((level, parent)) |> toEqual((expected_level, expected_parent))
  })

  test("simple cycle", () => {
    let adj_list = list{
      {id: "A", neighbours: list{"B", "D"}},
      {id: "B", neighbours: list{"A", "C"}},
      {id: "C", neighbours: list{"B", "D"}},
      {id: "D", neighbours: list{"A", "C"}},
    }

    let expected_levels = (Some(0), Some(1), Some(2), Some(1))
    let expected_parents = (None, Some("A"), Some("B"), Some("A"))

    let {level, parent} = search(adj_list, "A")

    let levels = (
      Hashtbl.find(level, "A"),
      Hashtbl.find(level, "B"),
      Hashtbl.find(level, "C"),
      Hashtbl.find(level, "D"),
    )

    let parents = (
      Hashtbl.find(parent, "A"),
      Hashtbl.find(parent, "B"),
      Hashtbl.find(parent, "C"),
      Hashtbl.find(parent, "D"),
    )

    expect((levels, parents)) |> toEqual((expected_levels, expected_parents))
  })

  test("complete graph", () => {
    let adj_list = list{
      {id: "A", neighbours: list{"B", "C", "D"}},
      {id: "B", neighbours: list{"A", "C", "D"}},
      {id: "C", neighbours: list{"A", "B", "D"}},
      {id: "D", neighbours: list{"A", "B", "C"}},
    }

    let expected_levels = (Some(0), Some(1), Some(1), Some(1))
    let expected_parents = (None, Some("A"), Some("A"), Some("A"))

    let {level, parent} = search(adj_list, "A")

    let levels = (
      Hashtbl.find(level, "A"),
      Hashtbl.find(level, "B"),
      Hashtbl.find(level, "C"),
      Hashtbl.find(level, "D"),
    )

    let parents = (
      Hashtbl.find(parent, "A"),
      Hashtbl.find(parent, "B"),
      Hashtbl.find(parent, "C"),
      Hashtbl.find(parent, "D"),
    )

    expect((levels, parents)) |> toEqual((expected_levels, expected_parents))
  })

  test("forest", () => {
    let adj_list = list{
      {id: "A", neighbours: list{"B"}},
      {id: "B", neighbours: list{"A"}},
      {id: "C", neighbours: list{"D"}},
      {id: "D", neighbours: list{"C"}},
    }

    let expected_levels = (Some(0), Some(1), None, None)
    let expected_parents = (None, Some("A"), None, None)

    let {level, parent} = search(adj_list, "A")

    let levels = (
      Hashtbl.find(level, "A"),
      Hashtbl.find(level, "B"),
      Hashtbl.find(level, "C"),
      Hashtbl.find(level, "D"),
    )

    let parents = (
      Hashtbl.find(parent, "A"),
      Hashtbl.find(parent, "B"),
      Hashtbl.find(parent, "C"),
      Hashtbl.find(parent, "D"),
    )

    expect((levels, parents)) |> toEqual((expected_levels, expected_parents))
  })
})
