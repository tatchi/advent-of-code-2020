type seat = {
  row: int,
  col: int,
};

type rowDirection =
  | Front
  | Back;
type columnDirection =
  | Right
  | Left;

type partitioning = {
  rowDirections: list(rowDirection),
  columnDirections: list(columnDirection),
};

let rowDirection_to_string = rd =>
  switch (rd) {
  | Front => "F"
  | Back => "B"
  };
let columnDirection_to_string = cd =>
  switch (cd) {
  | Right => "R"
  | Left => "L"
  };

let toDirection = s => {
  let rowDirections =
    String.sub(s, 0, 7)
    |> Str.split(Str.regexp(""))
    |> List.map(s =>
         switch (s) {
         | "F" => Front
         | "B" => Back
         | _ => failwith("Unkownm row direction: " ++ s)
         }
       );
  let columnDirections =
    String.sub(s, 7, 3)
    |> Str.split(Str.regexp(""))
    |> List.map(s =>
         switch (s) {
         | "R" => Right
         | "L" => Left
         | _ => failwith("Unkownm col direction: " ++ s)
         }
       );
  {rowDirections, columnDirections};
};

let getSeatRow = rds => {
  let rec loop = (rds, (min, max)) => {
    switch (rds) {
    | [] => failwith("row directions should not be empty")
    | [rd] =>
      switch (rd) {
      | Front => min
      | Back => max
      }
    | [rd, ...rest] =>
      switch (rd) {
      | Front => loop(rest, (min, (max - min) / 2 + min))
      | Back => loop(rest, ((max - min) / 2 + min + 1, max))
      }
    };
  };
  loop(rds, (0, 127));
};
let getSeatCol = cds => {
  let rec loop = (cds, (min, max)) => {
    switch (cds) {
    | [] => failwith("col directions should not be empty")
    | [cd] =>
      switch (cd) {
      | Left => min
      | Right => max
      }
    | [rd, ...rest] =>
      switch (rd) {
      | Left => loop(rest, (min, (max - min) / 2 + min))
      | Right => loop(rest, ((max - min) / 2 + min + 1, max))
      }
    };
  };
  loop(cds, (0, 7));
};

let (/) = Fp.join;

let getSeatId = ({rowDirections, columnDirections}) => {
  let row = getSeatRow(rowDirections);
  let col = getSeatCol(columnDirections);
  row * 8 + col;
};

let input =
  Fp.absoluteExn(Sys.getcwd())
  / Fp.relativeExn("src/day5.txt")
  |> Fs.readTextExn
  |> List.map(toDirection);

let getPart1 = input =>
  input
  |> List.map(getSeatId)
  |> List.fold_left((acc, id) => id > acc ? id : acc, 0);

print_endline("Part1: " ++ string_of_int(getPart1(input)));

let getPart2 = input => {
  let seatIds = input |> List.map(getSeatId);

  let hashtbl = Hashtbl.create(List.length(seatIds));

  List.iter(id => Hashtbl.add(hashtbl, id, ()), seatIds);

  let rec findId = cursor => {
    switch (
      Hashtbl.mem(hashtbl, cursor - 1),
      Hashtbl.mem(hashtbl, cursor),
      Hashtbl.mem(hashtbl, cursor + 1),
    ) {
    | (_, true, false) => findId(cursor + 1)
    | (_, true, true) => findId(cursor + 2)
    | (_, false, false) => findId(cursor + 1)
    | (false, false, _) => findId(cursor + 1)
    | (true, false, true) => cursor
    };
  };

  findId(1);
};

print_endline("Part2: " ++ string_of_int(getPart2(input)));
