open Base;

module Input = {
  type t =
    | Shell(string);

  let get_raw = input => {
    switch (input) {
    | Shell(raw_input) => raw_input
    };
  };

  let from_shell = raw_input => Shell(raw_input);
};

type message_type =
  | Command(string, list(string));

type t = {
  input: Input.t,
  value: message_type,
};

type processor_error =
  | InvalidCommand(string)
  | InvalidCommandName(string);

let command_regex = Re.Pcre.regexp("^([\\w]|_-)+$", ~flags=[`CASELESS]);

module Action = {
  type internal_command =
    | Help
    | Debug
    | Ping;

  type runner =
    | Internal(internal_command);

  type trigger =
    | Command(string)
    | Unknown;

  type provider =
    | Shell;
  /* | Slack; */

  type destination =
    | Provider(provider)
    | SameAsOrigin;

  type t = {
    name: string,
    only_from: option(list(provider)),
    runner,
    trigger,
    destination,
  };

  let make = (name, only_from, runner, trigger, ~destination=?, ()) => {
    {
      name,
      only_from,
      runner,
      trigger,
      destination:
        switch (destination) {
        | Some(destination) => destination
        | None => SameAsOrigin
        },
    };
  };
};

module Config = {
  type t = {
    actions: list(Action.t),
    default_action: Action.t,
  };
};

module ShellProcessor = {
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
    |> Stdlib.List.flatten
    |> List.filter(~f=arg => Poly.(arg != ""));
  };

  let is_valid_command_name = name =>
    Re.Pcre.pmatch(name, ~rex=command_regex);

  let build_message = (input, name, args) =>
    if (is_valid_command_name(name)) {
      Ok({input, value: Command(name, read_args(args))});
    } else {
      Error(InvalidCommandName(name));
    };

  let create_message = (input, raw_command) => {
    switch (String.split(raw_command, ~on=' ')) {
    | [] => Error(InvalidCommand(raw_command))
    | [name] => build_message(input, name, [])
    | [name, ...args] => build_message(input, name, args)
    };
  };

  let process_input = input => {
    let raw_input = Input.get_raw(input);
    switch (String.chop_prefix(raw_input, ~prefix="!")) {
    | Some(raw_command) => create_message(input, raw_command)
    | None => Error(InvalidCommand(raw_input))
    };
  };
};

let process = input => {
  switch (input) {
  | Input.Shell(_) => ShellProcessor.process_input(input)
  };
};

let is_valid_origin = (input, only_from) => {
  switch (input, only_from) {
  | (_, None) => true
  | (Input.Shell(_message), Some(from)) =>
    from
    |> List.find(~f=(origin: Action.provider) => {
         switch (origin) {
         | Shell => true
         /* | _ => false */
         }
       })
    |> Option.is_some
  };
};

let find_action = (config: Config.t, message) => {
  let found_action =
    List.find(config.actions, ~f=action => {
      switch (message.value, action.trigger) {
      | (Command(command_name, _), Command(trigger_name)) =>
        Poly.(command_name == trigger_name)
        && is_valid_origin(message.input, action.only_from)
      | _ => false
      }
    });

  let action =
    switch (found_action) {
    | Some(found) => found
    | None => config.default_action
    };
  action;
};
