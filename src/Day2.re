let (/) = Fp.join;

type inputRowPart1 = {
  minOccurence: int,
  maxOccurence: int,
  character: char,
  password: string,
};

let toInputRowPart1 = row => {
  let splitted = String.split_on_char(' ', row);

  let occurences = String.split_on_char('-', List.nth(splitted, 0));

  {
    minOccurence: List.nth(occurences, 0) |> int_of_string,
    maxOccurence: List.nth(occurences, 1) |> int_of_string,
    character: List.nth(splitted, 1).[0],
    password: List.nth(splitted, 2),
  };
};

let input =
  Fp.absoluteExn(Sys.getcwd())
  / Fp.relativeExn("src/day2.txt")
  |> Fs.readTextExn;

let getValidPasswordsPart1 = input => {
  let isValid = row => {
    let nbOccurence =
      (String.split_on_char(row.character, row.password) |> List.length) - 1;
    nbOccurence >= row.minOccurence && nbOccurence <= row.maxOccurence;
  };
  let rec loop = (input, validPwds) => {
    switch (input) {
    | [] => validPwds
    | [row, ...rest] =>
      let validPwds =
        isValid(row) ? [row.password, ...validPwds] : validPwds;
      loop(rest, validPwds);
    };
  };
  loop(input |> List.map(toInputRowPart1), []);
};

print_endline(
  "[Part1]: "
  ++ (getValidPasswordsPart1(input) |> List.length |> string_of_int),
);

type inputRowPart2 = {
  positions: list(int),
  character: char,
  password: string,
};

let toInputRowPart2 = row => {
  let splitted = String.split_on_char(' ', row);

  let positions = String.split_on_char('-', List.nth(splitted, 0));

  let firstPosition = (List.nth(positions, 0) |> int_of_string) - 1;
  let secondPosition = (List.nth(positions, 1) |> int_of_string) - 1;

  {
    positions: [firstPosition, secondPosition],
    character: List.nth(splitted, 1).[0],
    password: List.nth(splitted, 2),
  };
};

let getValidPasswordsPart2 = input => {
  let isValid = row => {
    row.positions
    |> List.map(String.get(row.password))
    |> List.filter(Char.equal(row.character))
    |> List.length == 1;
  };
  let rec loop = (input, validPwds) => {
    switch (input) {
    | [] => validPwds
    | [row, ...rest] =>
      if (isValid(row)) {
        loop(rest, [row.password, ...validPwds]);
      } else {
        loop(rest, validPwds);
      }
    };
  };

  loop(input |> List.map(toInputRowPart2), []);
};

print_endline(
  "[Part2]: "
  ++ (getValidPasswordsPart2(input) |> List.length |> string_of_int),
);
