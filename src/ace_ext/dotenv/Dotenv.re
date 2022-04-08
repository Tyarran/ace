open Base;

let dotenv_regex = Re2.of_string("(\\w+)(?:\\ +)?\\=(?:\\ +)?(.+)?");

let drop_suffix_and_prefix = (str, n) =>
  (String.drop_prefix(str, n) |> String.drop_suffix)(n);

let has_single_quotes = str =>
  String.is_prefix(str, ~prefix="'") && String.is_suffix(str, ~suffix="'");
let has_double_quotes = str =>
  String.is_prefix(str, ~prefix="\"") && String.is_suffix(str, ~suffix="\"");

let strip_quotes = str => {
  switch (has_single_quotes(str), has_double_quotes(str)) {
  | (true, false)
  | (false, true) => drop_suffix_and_prefix(str, 1)
  | _ => str
  };
};

let extract_values = line => {
  switch (Re2.find_submatches(dotenv_regex, line)) {
  | Ok([|Some(_), Some(key), Some(value)|]) =>
    Some((
      key |> String.strip |> strip_quotes,
      value |> String.strip |> strip_quotes,
    ))
  | _ => None
  };
};

let read_dotenv_file = file => {
  Stdio.In_channel.(
    file
    |> input_lines
    |> List.map(~f=extract_values)
    |> List.iter(~f=res => {
         switch (res) {
         | Some((key, value)) => Unix.putenv(key, value)
         | None => ()
         }
       })
  );
};
