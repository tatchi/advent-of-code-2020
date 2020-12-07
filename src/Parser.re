open Angstrom;
let input = "name:Corentin";

type t =
  | Null
  | Undefined;

let to_string =
  fun
  | Null => "NULL"
  | Undefined => "UNDEFINED";

type field =
  | Name(string)
  | Age(int);

let letter =
  take_while1(
    fun
    | 'a' .. 'z' => true
    | _ => false,
  );
let nameParser = Angstrom.string("name:") *> letter >>| (s => Name(s));

let nullParser = Angstrom.string("null") *> return(Null);
let undefinedParser = Angstrom.string("undefined") *> return(Undefined);

let bothParser = nullParser <|> undefinedParser;

let bothParserSep =
  Angstrom.sep_by1(
    Angstrom.char(' '),
    Angstrom.choice([nullParser, undefinedParser]),
  );

let parser =
  Angstrom.sep_by1(Angstrom.many1(Angstrom.char(' ')), bothParser);

switch (
  Angstrom.parse_string(
    ~consume=Angstrom.Consume.All,
    bothParserSep,
    "undefined",
  )
) {
| Ok(res) => res |> List.iter(s => print_endline(to_string(s)))
| Error(e) => print_endline("error: " ++ e)
};
