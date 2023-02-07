open Base;

type interpolation_error =
  | Mandatory(string);

exception Interpolation_error(interpolation_error);

type interpolation =
  | Simple(Re2.t)
  | WithDefault(Re2.t)
  | Mandatory(Re2.t);

let interpolations = [
  Simple(
    Re2.of_string("\\$((?:[[:upper:]]|_)+)|\\$\\{((?:[[:upper:]]|_)+)\\}"),
  ),
  WithDefault(Re2.of_string("\\$\\{((?:[[:upper:]]|_)+)(:)?-(.*)\\}")),
  Mandatory(Re2.of_string("\\$\\{((?:[[:upper:]]|_)+)(:)?(?:\\?)(.*)\\}")),
];

let replace_simple = match => {
  let key = Re2.Match.get_exn(~sub=`Index(1), match);
  let value_opt = Sys.getenv(key);
  switch (value_opt) {
  | Some(res) => res
  | None => ""
  };
};

let replace_with_default = match => {
  open Re2;

  let key = Match.get_exn(~sub=`Index(1), match);
  let value_opt = Sys.getenv(key);
  let default = Match.get_exn(~sub=`Index(3), match);
  switch (Match.get(~sub=`Index(2), match), value_opt) {
  | (Some(_), Some(res)) when Poly.(res == "") => default
  | (Some(_), None) => default
  | (None, None) => default
  | _ => ""
  };
};

let replace_mandatory = match => {
  open Re2;

  let key = Match.get_exn(~sub=`Index(1), match);
  let value_opt = Sys.getenv(key);
  let error_msg = Match.get_exn(~sub=`Index(3), match);
  switch (Match.get(~sub=`Index(2), match), value_opt) {
  | (_, None) => raise(Interpolation_error(Mandatory(error_msg)))
  | (Some(_), Some(res)) when Poly.(res == "") =>
    raise(Interpolation_error(Mandatory(error_msg)))
  | (_, Some(res)) => res
  };
};

let rec interpolate = (~interpolations=[], value) => {
  switch (interpolations) {
  | [] => value
  | [interpolation, ...rest] =>
    let new_value =
      switch (interpolation) {
      | Simple(regex) => Re2.replace_exn(regex, value, ~f=replace_simple)
      | WithDefault(regex) =>
        Re2.replace_exn(regex, value, ~f=replace_with_default)
      | Mandatory(regex) =>
        Re2.replace_exn(regex, value, ~f=replace_mandatory)
      };
    interpolate(~interpolations=rest, new_value);
  };
};
