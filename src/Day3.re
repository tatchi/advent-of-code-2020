let (/) = Fp.join;

let input =
  Fp.absoluteExn(Sys.getcwd())
  / Fp.relativeExn("src/day3.txt")
  |> Fs.readTextExn
  |> List.map(Str.split(Str.regexp("")))
  |> List.map(Array.of_list)
  |> Array.of_list;

module Area = {
  type slot =
    | Square
    | Tree;

  type t = {
    x: int,
    y: int,
    area: array(array(slot)),
  };
  type move = {
    x: int,
    y: int,
  };

  let slot_of_string = s =>
    switch (s) {
    | "#" => Tree
    | "." => Square
    | _ => failwith("Unexpected character:" ++ s)
    };

  let create = input => {
    let area = input |> Array.map(Array.map(slot_of_string));
    {x: 0, y: 0, area};
  };

  let move = ({x, y}, position: t) => {
    let newX = position.x + x;
    let newY = (position.y + y) mod Array.length(position.area[0]);
    if (newX >= Array.length(position.area)) {
      None;
    } else {
      Some((
        position.area[newX][newY],
        {x: newX, y: newY, area: position.area},
      ));
    };
  };
};

let area = Area.create(input);

let getNbTree = (area, move) => {
  let rec loop = (area, nbTree) => {
    switch (Area.move(move, area)) {
    | None => nbTree
    | Some((slot, newArea)) =>
      switch (slot) {
      | Tree => loop(newArea, nbTree + 1)
      | Square => loop(newArea, nbTree)
      }
    };
  };
  loop(area, 0);
};

print_endline(
  "Part1: " ++ (getNbTree(area, {x: 1, y: 3}) |> string_of_int),
);

let res =
  Area.[
    {x: 1, y: 1},
    {x: 1, y: 3},
    {x: 1, y: 5},
    {x: 1, y: 7},
    {x: 2, y: 1},
  ]
  |> List.map(getNbTree(area))
  |> List.fold_left((acc, n) => acc * n, 1);

print_endline("Part2: " ++ string_of_int(res));
