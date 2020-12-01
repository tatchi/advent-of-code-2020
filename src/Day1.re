let (/) = Fp.join;

let input =
  Fp.absoluteExn(Sys.getcwd())
  / Fp.relativeExn("src/day1.txt")
  |> Fs.readTextExn
  |> List.map(int_of_string);

let getTwoNumbers = (input, sum) => {
  let hashtbl = Hashtbl.create(List.length(input));
  let rec loop = numbers => {
    switch (numbers) {
    | [] => None
    | [n, ...rest] =>
      let numberToFind = sum - n;

      if (Hashtbl.mem(hashtbl, numberToFind)) {
        Some((n, numberToFind));
      } else {
        Hashtbl.add(hashtbl, n, ());
        loop(rest);
      };
    };
  };
  loop(input);
};

let firstResult = getTwoNumbers(input, 2020);

switch (firstResult) {
| None => print_endline("[First]: No result!")
| Some((n1, n2)) => print_endline("[First]: " ++ string_of_int(n1 * n2))
};

let getThreeNumbers = (input, sum) => {
  let rec loop = input => {
    switch (input) {
    | [] => None
    | [n, ...rest] =>
      let sumToFind = getTwoNumbers(rest, sum - n);
      switch (sumToFind) {
      | None => loop(rest)
      | Some((n2, n3)) => Some((n, n2, n3))
      };
    };
  };
  loop(input);
};

let secondResult = getThreeNumbers(input, 2020);

switch (secondResult) {
| None => print_endline("[Second]: No result!")
| Some((n1, n2, n3)) =>
  print_endline("[Second]: " ++ string_of_int(n1 * n2 * n3))
};
