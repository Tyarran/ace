open Base;

let rec expand = (name, prefix, count) => {
  switch (String.length(name) <= count) {
  | false => name
  | true => expand(prefix ++ name, prefix, count)
  };
};

let expand_name = (name, name_list) => {
  name_list
  |> List.fold(~init=0, ~f=(acc, name) =>
       Int.max(acc, String.length(name))
     )
  |> expand(name, " ");
};

let space = name =>
  Array.create(~len=String.length(name ++ " > "), ' ')
  |> Array.to_list
  |> String.of_char_list;
