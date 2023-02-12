open Base;

type processor_error =
  | InvalidCommand(string)
  | InvalidCommandName(string);

type input =
  | Command(string, list(string));

type t =
  | Shell(string);

let command_regex = Re.Pcre.regexp("^([\\w]|_-)+$", ~flags=[`CASELESS]);

module Action = {
  type runner =
    | Internal(string);

  type trigger =
    | Command(string)
    | Unknown;

  type origin =
    | Shell;

  type t = {
    name: string,
    only_from: option(list(origin)),
    runner,
    trigger,
  };
};

let actions = [
  Action.{
    name: "ping",
    only_from: None,
    runner: Internal("ping"),
    trigger: Command("ping"),
  },
  {
    name: "help",
    only_from: None,
    runner: Internal("help"),
    trigger: Command("help"),
  },
];

let default_action =
  Action.{
    name: "help",
    only_from: None,
    runner: Internal("help"),
    trigger: Unknown,
  };

module ShellProcessor = {
  type message = string;

  let split_regex = Re.Pcre.regexp("\".*\"", ~flags=[`CASELESS]);

  let read_args = args => {
    args
    |> List.fold(~init="", ~f=(acc, item) => acc ++ " " ++ item)
    |> Re.Pcre.full_split(~rex=split_regex)
    |> List.map(~f=split_result => {
         switch (split_result) {
         | Re.Pcre.Text(text) => String.split_on_chars(text, ~on=[' '])
         | Re.Pcre.Delim(delim) => [
             String.Search_pattern.replace_all(
               String.Search_pattern.create("\""),
               ~in_=delim,
               ~with_="",
             ),
           ]
         | _ => [""]
         }
       })
    |> Caml.List.flatten
    |> List.filter(~f=arg => Poly.(arg != ""));
  };

  let valid_command_name = name => Re.Pcre.pmatch(name, ~rex=command_regex);

  let valid_command = (name, ~args=[], ()) =>
    if (valid_command_name(name)) {
      Ok(Command(name, read_args(args)));
    } else {
      Error(InvalidCommandName(name));
    };

  let parse_command = raw_command => {
    switch (String.split(raw_command, ~on=' ')) {
    | [] => Error(InvalidCommand(raw_command))
    | [name] => valid_command(name, ())
    | [name, ...args] => valid_command(name, ~args, ())
    };
  };

  let process_message = message => {
    switch (String.chop_prefix(message, ~prefix="!")) {
    | Some(raw_command) => parse_command(raw_command)
    | None => Error(InvalidCommand(message))
    };
  };
};

let process = message => {
  switch (message) {
  | Shell(input) => ShellProcessor.process_message(input)
  };
};

let find_action = (input, actions: list(Action.t), default: Action.t) => {
  let found_action =
    List.find(actions, ~f=action => {
      switch (input, action.trigger) {
      | (Command(command_name, _), Command(trigger_name))
          when Poly.(command_name == trigger_name) =>
        true
      | _ => false
      }
    });

  switch (found_action) {
  | Some(found) => found
  | None => default
  };
};
