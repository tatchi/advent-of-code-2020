let (/) = Fp.join;

type hgt =
  | Cm(int)
  | In(int);

type field =
  | Ecl(string)
  | Pid(int)
  | Eyr(int)
  | Hcl(string)
  | Byr(int)
  | Iyr(int)
  | Cid(string)
  | Hgt(hgt);

let parse_byr = s => {
  Option.bind(int_of_string_opt(s), n =>
    n >= 1920 && n <= 2002 ? Some(Byr(n)) : None
  );
};
let parse_iyr = s => {
  Option.bind(int_of_string_opt(s), n =>
    n >= 2010 && n <= 2020 ? Some(Iyr(n)) : None
  );
};

let parse_eyr = s => {
  Option.bind(int_of_string_opt(s), n =>
    n >= 2020 && n <= 2030 ? Some(Eyr(n)) : None
  );
};

let parse_hgt = s =>
  try({
    let unit = String.sub(s, String.length(s) - 2, 2);
    let n = String.sub(s, 0, String.length(s) - 2) |> int_of_string;
    switch (unit) {
    | "cm" => n >= 150 && n <= 193 ? Some(Hgt(Cm(n))) : None
    | "in" => n >= 59 && n <= 76 ? Some(Hgt(In(n))) : None
    | _ => None
    };
  }) {
  | _ => None
  };

let parse_hcl = s => {
  String.length(s) == 7
  && Str.string_match(Str.regexp("^#[a-z0-9]+$"), s, 0)
    ? Some(Hcl(s)) : None;
};

let parse_ecl = s => {
  switch (s) {
  | "amb"
  | "blu"
  | "brn"
  | "gry"
  | "grn"
  | "hzl"
  | "oth" => Some(Ecl(s))
  | _ => None
  };
};

let parse_pid = s =>
  try(
    String.length(s) == 9 && Str.string_match(Str.regexp("[0-9]"), s, 0)
      ? Some(Pid(int_of_string(s))) : None
  ) {
  | _ => None
  };

let parse_cid = s => Some(Cid(s));

let parse_field = (k, v) => {
  switch (k) {
  | "byr" => parse_byr(v)
  | "iyr" => parse_iyr(v)
  | "eyr" => parse_eyr(v)
  | "hgt" => parse_hgt(v)
  | "hcl" => parse_hcl(v)
  | "ecl" => parse_ecl(v)
  | "pid" => parse_pid(v)
  | "cid" => parse_cid(v)
  | _ => failwith("Unkown key: " ++ k)
  };
};

let to_passport = row => {
  let hashtbl = Hashtbl.create(8);
  Str.split(Str.regexp("\n\\| "), row)
  |> List.iter(keyValueAsString => {
       let splittedKeyValue = String.split_on_char(':', keyValueAsString);
       Hashtbl.replace(
         hashtbl,
         List.hd(splittedKeyValue),
         List.nth(splittedKeyValue, 1),
       );
     });
  hashtbl;
};

let input =
  Fp.absoluteExn(Sys.getcwd())
  / Fp.relativeExn("src/day4.txt")
  |> Fs.readTextExn
  |> String.concat("\n")
  |> Str.split(Str.regexp("\n\n"))
  |> List.map(to_passport);

let getValidPassportsPart1 = input => {
  let isValid = passport =>
    switch (Hashtbl.length(passport)) {
    | 8 => true
    | 7 => !Hashtbl.mem(passport, "cid")
    | _ => false
    };

  let rec loop = (input, validPassports) => {
    switch (input) {
    | [] => validPassports
    | [passport, ...rest] =>
      if (isValid(passport)) {
        loop(rest, [passport, ...validPassports]);
      } else {
        loop(rest, validPassports);
      }
    };
  };
  loop(input, []);
};

print_endline(
  "Part1: " ++ (List.length(getValidPassportsPart1(input)) |> string_of_int),
);

let getValidPassportsPart2 = input => {
  let isValid = passport => {
    let areAllFieldsValid = passport =>
      Hashtbl.fold(
        (k, v, res) =>
          if (res == false) {
            false;
          } else {
            Option.is_none(parse_field(k, v)) ? false : true;
          },
        passport,
        true,
      );
    switch (Hashtbl.length(passport)) {
    | 8 => areAllFieldsValid(passport)
    | 7 => !Hashtbl.mem(passport, "cid") && areAllFieldsValid(passport)
    | _ => false
    };
  };

  let rec loop = (input, validPassports) => {
    switch (input) {
    | [] => validPassports
    | [passport, ...rest] =>
      if (isValid(passport)) {
        loop(rest, [passport, ...validPassports]);
      } else {
        loop(rest, validPassports);
      }
    };
  };
  loop(input, []);
};

print_endline(
  "Part2: " ++ (List.length(getValidPassportsPart2(input)) |> string_of_int),
);
